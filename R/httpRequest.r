# copyright (C) 2004 Witold Eryk Wolski.

##==================================================================
## multipartpost
##==================================================================

PostToHost <- function(host,path,dataTosend,referer="",port=80)
{
  dc <- 0;
  bo <- NULL
  xx <- as.integer(runif(10,min=1,max=10))
  for(y in xx)
  {
    bo <- paste(bo,y,sep="")
  }
  bo <- paste("xxx",bo,sep="")
  bol <- "--"
  fp <- make.socket(host=host, port=port,server=FALSE,fail=TRUE)
  mypost <- NULL
  mypost <- c(mypost,paste("POST ",path," HTTP/1.1\n",sep=""))
  mypost <- c(mypost,paste("Host: ",host,"\n",sep=""))
  mypost <- c(mypost,"Connection: Keep-Alive\n")
  mypost <- c(mypost,paste("Referer: ",referer,"\n",sep=""))
  mypost <- c(mypost,"User-Agent: Mozilla/4.05C-SGI [en] (X11; I; IRIX 6.5 IP22)\n")
  mypost <- c(mypost,"Accept: */*\n")
  mypost <- c(mypost,paste("Content-type: multipart/form-data; boundary=",bo,"\n",sep=""))
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
  mypost <- c(mypost,paste("Content-length: ",dc,"\n\n",sep=""))
  mypost <- c(mypost,mcontent)
  mypost <- c(mypost,paste(bol,bo,"--\n\n",sep=""))
  for(x in 1:length(mypost))
    {
      write.socket(fp,mypost[x])
    }

  output <- character(0)
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
## GET
##================================================================

GetToHost <- function(host,path,referer,port=80)
{
  fp <- make.socket(host=host, port=port,server=FALSE)
  write.socket(fp,paste("GET ",path," HTTP/1.1\n",sep=""))
  write.socket(fp,"Accept: text/html\n")
  #write.socket(fp,"Accept-Language: en-us\n")
  write.socket(fp,"Keep-Alive: timeout=15, max=100")
  write.socket(fp,"Connection: Keep-Alive\n")
  write.socket(fp,paste("Host: ",host,"\n",sep=""))
  #write.socket(fp,"User-Agent: Mozilla/4.0 (compatible; MSIE 5.5; Windows NT 5.0\n")
  ref <- paste("Referer: ",referer,"\n\n",sep="")
  #print(ref)
  write.socket(fp,ref)
  output <- character(0)
  #int <- 1
  repeat{
    ss <- read.socket(fp,loop=FALSE)
    #print(ss)
    output <- paste(output,ss,sep="")
    if(regexpr("\r\n0\r\n\r\n",ss)>-1) break()
    if (ss == "") break()
  }
  close.socket(fp)
  return(output)
}

##================================================================
## Simple post
##================================================================


SimplePostToHost<-function(host, path, referer, datatosend,port=80) {
  fp <- make.socket(host=host, port=port,server=FALSE)
  write.socket(fp, paste("POST ",path," HTTP/1.1\n",sep=""))
  write.socket(fp, paste("Host: ",host,"\n",sep=""))
  write.socket(fp,paste("Referer: ",referer,"\n",sep=""))
  write.socket(fp, "Content-type: application/x-www-form-urlencoded\n");
  write.socket(fp, paste("Content-length: ",length(strsplit(datatosend,"")[[1]]),"\n"));
  write.socket(fp, "Connection: Keep-Alive\n\n");
  write.socket(fp, paste(datatosend,"\n",sep=""));
 
  output <- character(0)
  repeat{
    ss <- read.socket(fp,loop=FALSE)
    #print(ss)
    output <- paste(output,ss,sep="")
    if(regexpr("\r\n0\r\n\r\n",ss)>-1) break()
    if (ss == "") break
  }
  close.socket(fp)
  return(output)
}


#simplsock<-function(filename,host,port)
#{
#  test <- readLines(filename)
#  fp <- make.socket(host=host, port=port,server=FALSE,fail=FALSE)
#  
#  for(x in test)
#  {
#    write.socket(fp,x)
#  }
#  output <- character(0)
#  repeat{
#    ss <- read.socket(fp)
#    if (ss == "") break#
    #print(ss)
 #   output <- paste(output,ss,sep="\n")
 # }
 # close.socket(fp)
 # return(output)
#}

