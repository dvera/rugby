hub.vcf <-
function( trackfiles, hubloc , descriptions = "" , group = NULL , ra = FALSE , parentname=NULL, composite=FALSE, tracknames=basename(removeext(trackfiles)), printtrack=F){
	if(composite==TRUE & is.null(parentname)){stop("must specify parent name if composite=TRUE")}
	

	if(ra & is.null(group)){stop("must have a group if ra == TRUE")}

	tbis<-paste0(trackfiles,".tbi")
	if(any(file.exists(tbis)==FALSE)) {stop("cannot find tabix index")}


	print(data.frame("file"=trackfiles,"names"=tracknames))
	track=""
	if(composite){
		track<-c(
			"",
			"",
			paste("track",parentname),
			paste("shortLabel",parentname),
			paste("longLabel",parentname),
			"type vcfTabix",
			"configurable on",
			"visilbility hide",
			if(composite){ "allButtonPair on" },
			if(composite){ "dragAndDrop on" },
			if(ra){paste("group",group)},
			paste0("html html/",parentname),
			""
			)
	}

	for(i in 1:length(trackfiles)){
		
		subtrack<-c(
			paste("track ",if(composite){paste(parentname,"-",sep="")},tracknames[i],sep=""),
			if(ra==FALSE){paste("bigDataUrl bbi/",basename(trackfiles[i]),sep="")},
			paste("shortLabel ",tracknames[i],sep=""),
			paste("longLabel ",tracknames[i],sep=""),
			"type vcfTabix",
			if(composite){paste("parent",parentname)},
			if(composite==F & ra){paste("group",group)},
			if(composite==F){"configurable on"},
			if(composite==F){"visilbility hide"},
			if(composite==F){paste0("html html/",tracknames[i])},
			""
		)
		track<-append(track,subtrack)
	}
	
	if(ra){	targetfile="trackDb.ra" } else { targetfile="hubDb.txt" }
	trackfile<-data.frame("V1"=track,stringsAsFactors=FALSE)
	if(printtrack){cat(unlist(trackfile),sep="\n")} else{write.tsv(trackfile,file="tmphubdb.txt")}
	filelist<-paste(trackfiles,tbis,sep=" ",collapse=" ")
	print(filelist)
	login<-unlist(strsplit(hubloc,":"))[1]
	path<-unlist(strsplit(hubloc,":"))[2]
	genome<-basename(path)
	#print(paste("scp ",hubloc,"/bbi/",sep=""))
	print(paste0("scp ",filelist," ",hubloc,"/bbi/"))
	system(paste0("scp ",filelist," ",hubloc,"/bbi/"))
	print(paste0("cat tmphubdb.txt | ssh ",login," 'cat >> ",path,"/",targetfile,"'"))
	system(paste0("cat tmphubdb.txt | ssh ",login," 'cat >> ",path,"/",targetfile,"'"))
	system(paste0("ssh ",login," '",paste(paste0("echo \"",descriptions," > ",path,"/",basename(trackfiles)),collapse=" && "),"'"))
	if(ra){
		print(paste0("ssh ",login," '",paste(paste0("hgBbiDbLink ",genome," ",tracknames," ",path,"/bbi/",basename(trackfiles)),collapse=" && "),"'"))
		system(paste0("ssh ",login," '",paste(paste0("hgBbiDbLink ",genome," ",tracknames," ",path,"/bbi/",basename(trackfiles)),collapse=" && "),"'"))
		print(paste("ssh",login,"'hgTrackDb",genome,path,"/gbdb/kent/src/hg/lib/trackDb.sql",path))
		system(paste("ssh",login,"'hgTrackDb",genome,path,"/gbdb/kent/src/hg/lib/trackDb.sql",path))
	}
}
