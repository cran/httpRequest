\name{PostToHost}
\alias{PostToHost}
\title{Sends Multiform Post Request to HTTP Server}
\description{
  Sends Multiform Post Request to HTTP Server
}
\usage{
PostToHost(host,path,dataTosend,referer="",port=80)
}

\arguments{
        \item{host}{The host to which to send the Request eg.: www.spiegel.de,127.0.01}
        \item{path}{The path to the file eg. /cgi/getpasswords.pl or /index.html}
        \item{dataTosend}{its a list of name value pairs e.g. list(name=value,name1=value1,name2=value2)}
        \item{referer}{something like www.myhome.org}
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
