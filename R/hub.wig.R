hub.wig <-
function(
	trackfiles,
	hubloc,

	tracknames=NULL,
	trackdesc=NULL,
	parentname=NULL,
	parentdesc=NULL,
	composite=FALSE,
	visibility="hide",
	group = NULL,
	mirror = FALSE,

	multiwig=FALSE,
	range=c(0,100),
	plotcolors=NULL,
	altcolors=NULL,

	port=22
){

	################ TO DO ###############
	# remove dashes from file names (and track names)
	# check if hub exists


	if(is.null(tracknames)){tracknames=removeext(trackfiles)}
	if(is.null(trackdesc)){trackdesc=removeext(tracknames)}
	if(is.null(plotcolors)){plotcolors=rainbow(length(trackfiles)) }
	if(composite==TRUE & is.null(parentname)){stop("must specify parent name if composite=TRUE")}
	if(mirror & is.null(group)){stop("must have a group if mirror == TRUE")}
	# if(composite & length(descriptions) >1){stop("can only have 1 description for composite track")}
	# if(composite==FALSE & length(descriptions) != length(trackfiles)){stop("description length should be equal to track #")}
	if(is.null(parentdesc)){parentdesc=parentname}
	if(is.null(altcolors)){altcolors=plotcolors}
	if(multiwig|composite){
		fullnames <- paste0(parentname,"_",tracknames)
	} else{
		fullnames <- tracknames
	}
	color<-unlist(lapply(lapply(plotcolors,col2rgb),paste,collapse=","))
	altcolor<-unlist(lapply(lapply(altcolors,col2rgb),paste,collapse=","))

	print(data.frame("file"=trackfiles,"names"=tracknames,"color"=plotcolors,"rgb"=color))

	track=""


	##########################################
	##########################################
	##########################################
	##########################################
	##########################################
	##########################################


	if(composite & multiwig){stop("cannot be both composite and multiwig")}

	if(multiwig|composite){
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

			paste("type bigWig",range[1],range[2]),
			if( multiwig  ){ "container multiWig"           },
			if( multiwig  ){ "aggregate transparentOverlay" },
			if( multiwig  ){ "showSubtrackColorOnUi on"     },
			"graphType points",
			"configurable on",
			"maxHeightPixels 200:50:32",
			"autoScale on",
			"windowingFunction mean",
			"yLineOnOff on",
			"yLineMark 0",
			"smoothingWindow off",
			"alwaysZero on",
			""
			)
	}



	for(i in 1:length(trackfiles)){
		a<-paste(col2rgb(altcolors[i]),collapse=",")

		subtrack<-c(
			paste("track",fullnames[i]),
			if(mirror==FALSE){paste0("bigDataUrl bbi/",basename(trackfiles[i]))},
			paste0("shortLabel ",tracknames[i]),
			paste0("longLabel ",trackdesc[i]),
			if(multiwig | composite){paste("parent",parentname)},
			if(multiwig==F | composite==F){paste("visilbility",visibility)},
			if(multiwig==F | composite==F){paste("group",group)},
			if(multiwig==F | composite==F){paste0("html html/",fullnames[i])},

			paste0("color ",color[i]),
			paste0("altColor ",altcolor[i]),

			if(multiwig==F | composite==F){paste0("type bigWig ",range[1]," ",range[2])},
			if(multiwig==F | composite==F){"graphType points"},
			if(multiwig==F | composite==F){"configurable on"},
			if(multiwig==F | composite==F){"maxHeightPixels 200:50:32"},
			if(multiwig==F | composite==F){"autoScale on"},
			if(multiwig==F | composite==F){"windowingFunction mean"},
			if(multiwig==F | composite==F){"yLineOnOff on"},
			if(multiwig==F | composite==F){"yLineMark 0"},
			if(multiwig==F | composite==F){"smoothingWindow off"},
			if(multiwig==F | composite==F){"alwaysZero on"},
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
