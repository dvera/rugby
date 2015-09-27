hub.bed <-
function(
	trackfiles,
	hubloc,

	tracknames=NULL,
	trackdesc=NULL,
	parentname=NULL,
	parentdesc=NULL,
	composite=FALSE,
	visibility="hide",
	group = NULL ,
	mirror = FALSE ,

	bedcols=3,
	usescore = FALSE ,
	color=FALSE,
	maxitems=1000,
	densecoverage=NULL,
	colorbystrand=NULL,


	port = 22
){



	if(length(which(file_ext(trackfiles) %in% c("bed")) > 0)){stop("cannot create track from bed files, convert to bigBed")}
	if(is.null(tracknames)){tracknames=removeext(trackfiles)}
	if(is.null(trackdesc)){trackdesc=removeext(tracknames)}
	if(composite==TRUE & is.null(parentname)){stop("must specify parent name if composite=TRUE")}
	if(mirror & is.null(group)){stop("must have a group if mirror == TRUE")}
	# if(composite & length(descriptions) >1){stop("can only have 1 description for composite track")}
	# if(composite==FALSE & length(descriptions) != length(trackfiles)){stop("description length should be equal to track #")}
	if(is.null(parentdesc)){parentdesc=parentname}

	if(composite){
		fullnames <- paste0(parentname,"_",tracknames)
	} else{
		fullnames <- tracknames
	}

	print(data.frame("file"=trackfiles,"names"=tracknames))

	track=""

	##########################################
	##########################################
	##########################################
	##########################################
	##########################################
	##########################################


	if(composite){
		track<-c(
			"",
			"",
			paste("track",parentname),
			paste("shortLabel",parentname),
			paste("longLabel",parentdesc),
			paste("visilbility",visibility),
			if(mirror){paste("group",group)},
			if( composite ){ "compositeTrack on"            },
			if( composite ){ "allButtonPair on"             },
			if( composite ){ "dragAndDrop on"               },
			paste0("html html/",parentname),

			paste("type bigBed",bedcols,"."),
			if(usescore){paste("spectrum on")},
			if(color){paste("itemRgb on")},
			if(is.null(colorbystrand)==FALSE){paste("colorByStrand",paste(col2rgb(colorbystrand[1]),collapse=","),paste(col2rgb(colorbystrand[2]),collapse=","),sep=" ")},
			if(is.null(densecoverage)==FALSE){paste("denseCoverage",densecoverage)},
			""
			)
	} else{
		track<-""
	}

	for(i in 1:length(trackfiles)){
		subtrack<-c(
			paste("track",fullnames[i]),
			if(!mirror    ){ paste0("bigDataUrl bbi/",basename(trackfiles[i]))},
			paste0("shortLabel ",tracknames[i]),
			paste0("longLabel ",trackdesc[i]),
			if(  composite ){  paste(  "parent",      parentname    )},
			if( !composite ){  paste(  "visilbility", visibility    )},
			if( !composite ){  paste(  "group",       group         )},
			if( !composite ){ paste0(  "html html/",  fullnames[i]  )},


			paste("type bigBed",bedcols,"."),
			if(composite==FALSE & usescore   ){ paste("spectrum on")             },
			if(color                         ){ paste("itemRgb on")              },
			if(is.null(colorbystrand)==FALSE ){ paste("colorByStrand",paste(col2rgb(colorbystrand[1]),collapse=","),paste(col2rgb(colorbystrand[2]),collapse=","),sep=" ")},
			if(is.null(densecoverage)==FALSE ){ paste("denseCoverage",densecoverage)},
			""
		)
		track<-append(track,subtrack)
	}


	##########################################
	##########################################
	##########################################
	##########################################
	##########################################

	# specify file name based on database type
	if(mirror){	targetfile="trackDb.ra" } else { targetfile="hubDb.txt" }

	# make a file from track data
	trackfile<-data.frame("V1"=track,stringsAsFactors=FALSE)
	write.tsv(trackfile,file="tmphubdb.txt")

	# collapse file names into a single string
	filelist<-paste(trackfiles,sep=" ",collapse=" ")

	# parse hub target string
	login<-unlist(strsplit(hubloc,":"))[1]
	path<-unlist(strsplit(hubloc,":"))[2]
	genome<-basename(path)

	# copy files to server
	cmdString<-paste0("scp -P ",port," ",filelist," ",hubloc,"/bbi/")
	print(cmdString)
	system(cmdString)

	# append track specs to track db
	cmdString<-paste0("cat tmphubdb.txt | ssh -p ",port," ",login," 'cat >> ",path,"/",targetfile,"'")
	print(cmdString)
	system(cmdString)

	if(mirror){

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
