\name{static_text_service}
\alias{static_text_service}
\title{
Serve a static web page from a file.
}
\description{
This utility function closure serves a static web page from a character
string.
}
\usage{
static_text_service(text)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{The HTML web page as a character string.}
}
\details{
A function of two arguments, \code{socket} and \code{header} is returned.
The \code{socket} argument indicates the low-level
socket descriptor of the websocket client. The \code{header} argument
contains a parsed HTTP GET request. When invoked, the function may
examine the GET request and respond accordingly, usually with a valid
HTTP 200 response, or a 40* response indicating an error.
}
\value{
The function returned by this closure must return a valid HTTP
response.
}
\author{
B. W. Lewis <blewis@illposed.net>
}

\seealso{
\code{\link{create_server}},
\code{\link{static_file_service}}
}
\examples{
# See the detailed example in help('websockets').
}
