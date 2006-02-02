\name{simplePostToHost}
\alias{simplePostToHost}
\title{Sends Simple Post Request to HTTP Server (host)}
\description{
  Sends Post Request to HTTP Server
}
\usage{
        simplePostToHost(host, path, referer, datatosend, port=80)
}
\arguments{
  \item{host}{The host to which to send the Request eg.: www.spiegel.de, localhost}
  \item{path}{The path to the file eg. \samp{/cgi-bin/paramlist.cgi} or \samp{/index.html}}
  \item{referer}{something like www.myhome.org}
  \item{datatosend}{key value pairs pairs separated by \& \code{"pid=14\&pollvotenumber=2\n4"}}
  \item{port}{its 80 or 8080 most frequently}
}
\value{The document which the server returns as a string.}
\details{
It is a simple http client. So it does not take care of special chars e.g. whitspaces.
For details see e.g. perl HTTP::Request::Common documentation.
and \code{http://www.w3.org/Protocols/rfc1341/0_TableOfContents.html}
}
\author{E.W.Wolski wolski@molgen.mpg.de}
\seealso{getToHost, postToHost}
\examples{
#before you use the example check that the server www.molgen.mpg.de is running.
#port <- 80
#data = "pid=14&poll_vote_number=2";
#simplePostToHost("www.molgen.mpg.de","/~wolski/test.php4","",data,port=port)
}
\keyword{misc}
