\name{GetToHost}
\alias{GetToHost}
\title{Sends Get Request to HTTP Server (host)}
\description{
  Sends Get Request to HTTP Server
}
\usage{
GetToHost(host,path,referer,port=80)
}
\arguments{
\item{host}{The host to which to send the Request eg.: www.spiegel.de, 127.0.01}
\item{path}{The path to the file eg. /cgi/getpasswords.pl?test=best or /index.html}
\item{referer}{something like www.myhome.org}
\item{port}{its 80 or 8080 most frequently}
}
\value{The document which the server returns as a string.}
\details{
For details see e.g. perl HTTP::Request::Common documentation.
}
\author{E.W.Wolski wolski@molgen.mpg.de}
\seealso{}
\examples{
#for sample code look in the help directory of the package
}
\keyword{misc}
