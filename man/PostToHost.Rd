\name{postToHost}
\alias{postToHost}
\title{Sends Multiform Post Request to HTTP Server}
\description{
  Sends Multiform Post Request to HTTP Server
}
\usage{
postToHost(host,path,dataTosend,referer="",port=80)
}

\arguments{
        \item{host}{The host to which to send the Request eg.: www.spiegel.de,127.0.01}
        \item{path}{The path to the file eg. /cgi/getpasswords.pl or /index.html}
        \item{dataTosend}{its a list of name value pairs e.g. list(name=value,name1=value1,name2=value2)}
        \item{referer}{something like www.myhome.org}
        \item{port}{its 80 or 8080 most frequently}
}

\value{document which the server returns.}
\details{
It is a simple http client. So it does not take care of special chars e.g. whitspaces.
For details see e.g. perl HTTP::Request::Common documentation.
and \code{http://www.w3.org/Protocols/rfc1341/0_TableOfContents.html}

}
\author{E.W.Wolski wolski@molgen.mpg.de}
\seealso{postObjectToHost}
\examples{

#to test uncomment. First check that the host ist running.
#port <- 80
#test2 <- list(
#            "fruit"="apple"
#           ,"dat_defs"="20021204/F113213.dat"
#           ,"upsa"="test"
#           )
#postToHost("www.molgen.mpg.de","/~wolski/test.php4",test2,referer="www.test.de",port=port)
}

\keyword{misc}
