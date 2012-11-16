\name{http_response}
\alias{http_response}
\title{Form and send an http response to a socket}
\description{Form and send an http response to a socket}
\usage{
http_response(socket, status=200, content_type="text/html; charset=UTF-8", content="", headers=c())
}
\arguments{
  \item{socket}{A socket descriptor.}
  \item{status}{An http numeric status code.}
  \item{content_type}{The http content type.}
  \item{content}{The http content (normally of type raw).}
  \item{headers}{List or vector of additional headers to send with the response. All items must be named. Do not include \code{Date}, \code{Content-Type} or \code{Content-Length} headers.}
}
\details{Form an http response and send it to the specified socket, closing the
socket connection when done.  }
\value{
NULL is invisibly returned
}
\author{
B. W. Lewis <blewis@illposed.net>
}
\seealso{
\code{\link{createContext}}
\code{\link{create_server}}
}
