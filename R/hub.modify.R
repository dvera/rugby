hub.modify <-
function(tracknames , hubloc , operation = "remove"){
	t<-grep("track ",a$V1)
	t2<-c(t[2:length(t)]-1,nrow(a))
	b<-strsplit(a$V1," ")
	bl<-unlist(lapply(b,length))
	d<-unlist(lapply(b,"[",1))
	e<-lapply(1:length(b), function(x) b[[x]][2:bl[x]] )
	e<-unlist(lapply(e,paste0,collapse=" "))
	f<-data.frame(V1=d,V2=e,stringsAsFactors=F)
	g<-lapply(1:length(t), function(x) data.frame(x=f[t[x]:t2[x],1] , y = f[t[x]:t2[x],2], stringsAsFactors=F))
	du<-unique(d)
	m<-lapply(1:length(g), function(x) match(g[[x]][,1],du))
	mat<-matrix(nrow=length(g),ncol=length(du))
	mat<-data.frame(mat,stringsAsFactors=F)
	colnames(mat) <- du
	for(i in 1:length(g)){
		mat[i,m[[i]]] = g[[i]][,2]
	}

	if(operation=="remove"){
		selected <- which(mat$track %in% tracknames | mat$parent %in% tracknames)
		if(length(selected) == 0 ){
			stop("no tracks found")
		} else{
			mat <- mat[-selected,]
			m <- m[-selected]
		}
	}

	if(operation=="duplicate"){
		selected <- which(mat$track %in% tracknames | mat$parent %in% tracknames)
		if(length(selected) == 0 ){
			stop("no tracks found")
		} else{
			mat <- mat[nrow(mat)+(1:length(selected)),] <- mat[selected,]
			m <- m[-selected]
		}
	}

	u<-unlist(
		lapply(
			1:nrow(mat), function(x) paste(colnames(mat)[m[[x]]],mat[x,m[[x]]])
		)
	)



	trackfile<-data.frame("V1"=u,stringsAsFactors=FALSE)
	write.tsv(trackfile,file="trackDb.txt")	

	# dont allow 'track' in track names
}
