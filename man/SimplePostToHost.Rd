\name{SimplePostToHost}
\alias{SimplePostToHost}
\title{Sends Simple Post Request to HTTP Server (host)}
\description{
  Sends Post Request to HTTP Server
}
\usage{
        SimplePostToHost(host, path, referer, datatosend,port=80)
}
\arguments{
  \item{host}{The host to which to send the Request eg.: www.spiegel.de, localhost}
  \item{path}{The path to the file eg. \samp{/cgi-bin/paramlist.cgi} or \samp{/index.html}}
  \item{referer}{something like www.myhome.org}
  \item{datatosend}{key value pairs pairs separated by \& \code{"pid=14\&pollvotenumber=2\n4"}}
  \item{port}{its 80 or 8080 most frequently}
}
\value{The document which the server returns as a string.}
\details{For details see e.g. perl HTTP::Request::Common documentation.
}
\author{E.W.Wolski wolski@molgen.mpg.de}
\seealso{}
\examples{
#for sample code look in the help directory of the package
}
\keyword{misc}
