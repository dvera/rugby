hub.add <-
function( trackfiles, hubloc , port=22 , group = NULL , ra = FALSE , range=c(0,50), parentname=NULL, parentdesc=parentname, multiwig=FALSE, composite=FALSE, tracknames=basename(removeext(trackfiles)), trackdesc=tracknames, plotcolors=rainbow(length(trackfiles)), altcolors=plotcolors , printtrack=F){


	if(composite==TRUE & is.null(parentname)){stop("must specify parent name if composite=TRUE")}
	if(composite==TRUE & multiwig==TRUE){stop("cannot be both composite and multiwig")}
	if(ra & is.null(group)){stop("must have a group if ra == TRUE")}

	if(multiwig|composite){
		fullnames <- paste0(parentname,"_",tracknames)
	} else{
		fullnames <- tracknames
	}
	color<-paste(col2rgb(plotcolors[i]),collapse=",")

	################ TO DO ###############
	# remove dashes from file names (and track names)

	print(data.frame("file"=trackfiles,"names"=tracknames,"color"=plotcolors,"rgb"=color))

	track=""

	##########################################
	##########################################
	##########################################
	##########################################
	##########################################
	##########################################

	if(multiwig | composite){
		track<-c(
			"",
			"",
			paste("track",parentname),
			paste("shortLabel",parentname),
			paste("longLabel",parentdesc),
			paste("type bigWig",range[1],range[2]),
			if(multiwig){ "container multiWig" } else{ "compositeTrack on" } ,
			"graphType points",
			"configurable on",
			"visilbility hide",
			"maxHeightPixels 200:50:32",
			if(composite){ "allButtonPair on" },
			if(multiwig){ "aggregate transparentOverlay" },
			if(multiwig){ "showSubtrackColorOnUi on" },
			if(composite){ "dragAndDrop on" },
			"autoScale on",
			"windowingFunction mean",
			"yLineOnOff on",
			"yLineMark 0",
			"smoothingWindow off",
			"alwaysZero on",
			if(ra){paste("group",group)},
			paste0("html html/",parentname),
			""
			)
	}



	for(i in 1:length(trackfiles)){
		a<-paste(col2rgb(altcolors[i]),collapse=",")

		subtrack<-c(
			paste("track",fullnames[i]),
			if(ra==FALSE){paste0("bigDataUrl bbi/",basename(trackfiles[i]))},
			paste0("shortLabel ",tracknames[i]),
			paste0("longLabel ",trackdesc[i]),
			if(multiwig | composite){paste("parent",parentname)},
			paste0("color ",color[i]),
			paste0("altColor ",a),
			if(multiwig==F | composite==F){paste0("type bigWig ",range[1]," ",range[2])},
			if(multiwig==F | composite==F){paste("group",group)},
			if(multiwig==F | composite==F){"graphType points"},
			if(multiwig==F | composite==F){"configurable on"},
			if(multiwig==F | composite==F){"visilbility hide"},
			if(multiwig==F | composite==F){"maxHeightPixels 200:50:32"},
			if(multiwig==F | composite==F){"autoScale on"},
			if(multiwig==F | composite==F){"windowingFunction mean"},
			if(multiwig==F | composite==F){"yLineOnOff on"},
			if(multiwig==F | composite==F){"yLineMark 0"},
			if(multiwig==F | composite==F){"smoothingWindow off"},
			if(multiwig==F | composite==F){"alwaysZero on"},
			if(multiwig==F | composite==F){paste0("html html/",fullnames[i])},
			""
		)
		track<-append(track,subtrack)
	}

	##########################################
	##########################################
	##########################################
	##########################################
	##########################################

	if(ra){	targetfile="trackDb.ra" } else { targetfile="hubDb.txt" }
	trackfile<-data.frame("V1"=track,stringsAsFactors=FALSE)
	if(printtrack){cat(unlist(trackfile),sep="\n")} else{write.tsv(trackfile,file="tmphubdb.txt")}
	filelist<-paste(trackfiles,sep=" ",collapse=" ")
	login<-unlist(strsplit(hubloc,":"))[1]
	path<-unlist(strsplit(hubloc,":"))[2]
	genome<-basename(path)
	#print(paste("scp ",hubloc,"/bbi/",sep=""))

	# copy files to server
	cmdString<-paste0("scp -P ",port," ",filelist," ",hubloc,"/bbi/")
	print(cmdString)
	system(cmdString)

	# append track specs to track db


	# ????
	# print(paste0("ssh -p ",port," ",login," '",paste(paste0("echo \"",descriptions,"\" > ",path,"/",basename(trackfiles)),collapse=" && "),"'"))
	# system(paste0("ssh -p ",port," ",login," '",paste(paste0("echo \"",descriptions,"\" > ",path,"/",basename(trackfiles)),collapse=" && "),"'"))
	cmdString<-paste0("cat tmphubdb.txt | ssh -p ",port," ",login," 'cat >> ",path,"/",targetfile,"'")
	print(cmdString)
	system(cmdString)

	if(ra){

		# load tracks in db
		cmdString<-paste0("ssh -p ",port," ",login," '",paste(paste0("hgBbiDbLink ",genome," \"",fullnames,"\" ",path,"/bbi/",basename(trackfiles)),collapse=" && "),"'")
		print(cmdString)
		system(cmdString)

		# reload db
		cmdString<-paste("ssh -p",port," ",login,"'hgTrackDb",path,genome,"trackDb ~/software/kent/src/hg/lib/trackDb.sql",path,"'")
		print(cmdString)
		system(cmdString)
	}

}
