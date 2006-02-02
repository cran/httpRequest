# copyright (C) 2004 Witold Eryk Wolski.

##==================================================================
## multipartpost
##==================================================================

# changed to post binary files (including \0) by Andreas Westfeld, 2/2/2006
# 
postToHost <- function(host,path,data.to.send,referer="",port=80)
{
	if(missing(path))
		path <- "/"
	#if(missing(ua))
		ua <- "User-Agent: Mozilla/4.05C-SGI [en] (X11; I; IRIX 6.5 IP22)\n"
	if(missing(data.to.send))
		stop("No data to send provided")
	if(!inherits(data.to.send,"list"))
		stop("Data to send have to be a list")

	dc <- 0; #counter for strings
	#make border
	xx <- as.integer(runif(10,min=1,max=10))
	bo<-paste(xx,collapse="")
	bo <- paste("xxx",bo,sep="")
	bol <- "--"

	header <- NULL
	header <- c(header,paste("POST ",path," HTTP/1.1\n",sep=""))
	header <- c(header,paste("Host: ",host,"\n",sep=""))
	#header <- c(header,"Connection: Keep-Alive\n")
	header <- c(header,"Connection: close\n")
	header <- c(header,paste("Referer: ",referer,"\n",sep=""))
	header <- c(header,ua)
	header <- c(header,"Accept: */*\n")
	header <- c(header,paste("Content-type: multipart/form-data; boundary=",bo,"\n",sep=""))

	mcontent <- NULL # keeps the content.

	for(x in 1:length(data.to.send)) {
		val <- data.to.send[[x]]
		key <- names(data.to.send)[x]
		if (typeof(val)=="list") {
			ds <- c(charToRaw(sprintf("%s%s\nContent-Disposition: form-data; name=\"%s\"; filename=\"%s\"\nContent-type: application/octet-stream\n\n", bol, bo, key, val$filename)), val$object, as.raw(10))
		} else {
			ds <- charToRaw(sprintf("%s%s\nContent-Disposition: form-data; name=\"%s\"\n\n%s\n", bol,bo,as.character(key),as.character(val)))
		}
		dc <- dc + length(ds)
		mcontent <- c(mcontent,ds)
	}

	dc <- dc + length(strsplit(bo,"")[[1]])+length(strsplit(bol,"")[[1]])+4;
	header <- c(header,paste("Content-length: ",dc,"\n\n",sep=""))
	mypost <- c(charToRaw(paste(header, collapse="")),mcontent,
		charToRaw(paste(bol,bo,"--\n\n",sep="")))
	rm(header,mcontent)
	#mypost<-paste(mypost,collapse="")

	scon <- socketConnection(host=host,port=port,open="a+b",blocking=TRUE)
	writeBin(mypost, scon, size=1)

	output <- character(0)
	#start <- proc.time()[3]
	repeat{
		ss <- rawToChar(readBin(scon, "raw", 2048))
		output <- paste(output,ss,sep="")
		if(regexpr("\r\n0\r\n\r\n",ss)>-1) break()
		if(ss == "") break()
		#if(proc.time()[3] > start+timeout) break()
	}
	close(scon)
	return(output)
}

##================================================================
## GET
##================================================================

getToHost <- function(host,path,referer,port=80)
{
  if(missing(path))
    path<-"/"
  if(missing(referer))
    referer<-""
  
  fp <- make.socket(host=host, port=port,server=FALSE)
  header <- character(0)
  header <- c(header,paste("GET ",path," HTTP/1.1\n",sep=""))
  header <- c(header,paste("Host: ",host,"\n",sep=""))
  header <- c(header,"Connection: Keep-Alive\n")
  header <- c(header,paste("Referer: ",referer,"\n",sep=""))
  header <- c(header,"User-Agent: Mozilla/4.05C-SGI [en] (X11; I; IRIX 6.5 IP22)\n")
  header <- c(header,"Accept: */*\n\n")
  tmp<-paste(header,collapse="")

  write.socket(fp,tmp)
  output <- character(0)
  #int <- 1
  repeat{
    ss <- read.socket(fp,loop=FALSE)
    output <- paste(output,ss,sep="")
    if(regexpr("\r\n0\r\n\r\n",ss)>-1) break()
    if (ss == "") break()
  }
  close.socket(fp)
  return(output)
}

##================================================================
## Simple Post
##================================================================

simplePostToHost<-function(host,path,referer,datatosend,port=80)
  {
    if(!missing(datatosend))
      {
        lengthdatatosend <- length( strsplit(datatosend,"")[[1]])
        datatosend <- paste(datatosend,"\n",sep="")
      }
    else
      {
        datatosend<-character(0)
        lengthdatatosend <- 0
      }
    if(missing(path)) path<-"/"
    if(missing(referer)) referer <- ""
    #make the header
    header<-character(0)
    header<-c(header,    paste("POST ",path," HTTP/1.1\n",sep=""))
    header<-c(header,    paste("Host: ",host,"\n",sep=""))
    header<-c(header,    paste("Referer: ",referer,"\n",sep=""))
    header<-c(header,    "Content-type: application/x-www-form-urlencoded\n")
    header<-c(header,    paste("Content-length: ",lengthdatatosend,"\n"))
    header<-c(header,    "Connection: Keep-Alive\n\n")
    #add the data.
    header <- paste(c(header,datatosend), collapse="")
    ##establish the connection.
    fp <- make.socket(host=host, port=port,server=FALSE)
    write.socket(fp,header)
    output <- character(0)
    #read as long there is nothing more to read.
    repeat
      {
        ss <- read.socket(fp,loop=FALSE)
        output <- paste(output,ss,sep="")
        if(regexpr("\r\n0\r\n\r\n",ss)>-1) break();
        if (ss == "") break();
      }
    close.socket(fp)
    return(output)
  }
