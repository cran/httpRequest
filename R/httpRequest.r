# copyright (C) 2004 Witold Eryk Wolski.

##==================================================================
## multipartpost
##==================================================================

postToHost <- function(host,path,dataTosend,referer="",port=80)
{
  if(missing(path))
    path <- "/"
  if(missing(dataTosend))
    stop("No data to send provided")
  if(!inherits(dataTosend,"list"))
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
  header <- c(header,"Connection: Keep-Alive\n")
  header <- c(header,paste("Referer: ",referer,"\n",sep=""))
  header <- c(header,"User-Agent: Mozilla/4.05C-SGI [en] (X11; I; IRIX 6.5 IP22)\n")
  header <- c(header,"Accept: */*\n")
  header <- c(header,paste("Content-type: multipart/form-data; boundary=",bo,"\n",sep=""))
  
  mcontent <- NULL # ceeps the content.
  
  for(x in 1:length(dataTosend))
    {
      val <- dataTosend[[x]]
      key <- names(dataTosend)[x]
      ds = sprintf("%s%s\nContent-Disposition: form-data; name=\"%s\"\n\n%s\n", bol,bo,as.character(key),as.character(val))
      dc <- dc + length(strsplit(ds,"")[[1]])
      mcontent <- c(mcontent,ds)
    }
  
  dc <- dc + length(strsplit(bo,"")[[1]])+length(strsplit(bol,"")[[1]])+4;
  header <- c(header,paste("Content-length: ",dc,"\n\n",sep=""))
  mypost <- c(header,mcontent); rm(header,mcontent)
  mypost <- c(mypost,paste(bol,bo,"--\n\n",sep=""))
  mypost<-paste(mypost,collapse="")

  fp <- make.socket(host=host, port=port,server=FALSE,fail=TRUE)
  write.socket(fp,mypost)
  output <- character(0)
  repeat{
    ss <- read.socket(fp,loop=FALSE)
    output <- paste(output,ss,sep="")
    if(regexpr("\r\n0\r\n\r\n",ss)>-1) break()
    if(ss == "") break()
  }
  close.socket(fp)
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
