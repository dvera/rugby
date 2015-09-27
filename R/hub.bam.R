hub.bam <-
function( trackfiles, minAliQual = 0 , indelDoubleInsert = TRUE , indelQueryInsert = TRUE , bamColorMode = "gray" , bamGrayMode = "aliQual" , paired = TRUE , parentname=NULL, composite=FALSE, maxitems=1000, densecoverage=NULL, colorbystrand=NULL, tracknames=basename(removeext(trackfiles)), hubloc="dvera@epsilon.bio.fsu.edu:public_html/hubs/dlv/hg19",printtrack=F){
	library(tools)
	if(length(which(file_ext(trackfiles) %in% c("bed")) > 0)){stop("cannot create track from bed files, convert to bigBed")}
	print(data.frame("file"=trackfiles,"names"=tracknames))
	if(composite==TRUE & is.null(parentname)){stop("must specify parent name if composite=TRUE")}
	#check if dense coverage is numeric and not logical

	bais<-paste0(trackfiles,".bai")
	if(any(file.exists(bais)==FALSE) ) {stop("cannot find tabix index")}
	trackfiles2<-basename(trackfiles)

	if(sum(file.exists(bais)) != length(trackfiles) ) {stop ("one or more bam indices not found!")}

	if(composite){
		track<-c(
			"",
			"",
			paste("track",parentname),
			paste("shortLabel",parentname),
			paste("longLabel",parentname),
			"type bam",
			"compositeTrack on",
			"visibility hide",
			"allButtonPair on",
			"dragAndDrop on",
			"showNames off",
			paste("minAliQual",minAliQual),
			paste("bamColorMode",bamColorMode),
			if(indelDoubleInsert){"indelDoubleInsert on"},
			if(indelQueryInsert){"indelQueryInsert on"},
			if(bamColorMode=="gray"){paste("bamGrayMode",bamGrayMode)},
			if(paired){"pairEndsByName ."},
			""
			)
	} else{
		track<-""
	}

	for(i in 1:length(trackfiles)){
		subtrack<-c(
			paste("track ",if(composite){paste(parentname,"_",sep="")},tracknames[i],sep=""),
			"type bam",
			if(composite){paste("parent",parentname)},
			paste("bigDataUrl bbi/",trackfiles2[i],sep=""),
			paste("shortLabel",tracknames[i]),
			paste("longLabel",tracknames[i]),
			if(composite==F){paste("minAliQual",minAliQual)},
			if(composite==F){paste("bamColorMode",bamColorMode)},
			if(composite==F){"visilbility hide"},
			if(composite==F){"showNames off"},
			if(composite==F & indelDoubleInsert==T){"indelDoubleInsert on"},
			if(composite==F & indelQueryInsert==T){"indelQueryInsert on"},
			if(composite==F & bamColorMode=="gray"){paste("bamGrayMode",bamGrayMode)},
			if(composite==F & paired==T){"pairEndsByName ."},
			""
		)
		track<-append(track,subtrack)
	}


	trackfile<-data.frame("V1"=track,stringsAsFactors=FALSE)
	if(printtrack){cat(unlist(trackfile),sep="\n")} else{write.tsv(trackfile,file="tmphubdb.txt")}
	filelist<-paste(trackfiles,bais,sep=" ",collapse=" ")
	login<-unlist(strsplit(hubloc,":"))[1]
	path<-unlist(strsplit(hubloc,":"))[2]
	cat(paste("scp ",hubloc,"/bbi/",sep=""))
	system(paste("scp ",filelist," ",hubloc,"/bbi/",sep=""))
	system(paste("cat tmphubdb.txt | ssh ",login," 'cat >> ",path,"/hubDb.txt'",sep=""))
}
