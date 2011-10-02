\name{service}
\alias{service}
\title{ Service events of a websocket server}
\description{
Service events queued for a websocket server.
}
\usage{
service(context, timeout = 1000L, server=context)
}
\arguments{
  \item{context}{The websocket environment returned from \code{createContext}
or \code{create_server}.}
  \item{timeout}{An integer value indicating the service timeout interval
in milliseconds. The default value is one second.}
  \item{server}{Future versions of this package will drop the context
terminology, replacing it with server. The context argument is maintained
in this package version for backwards compatibility with previous package
versions. }
}
\details{
Websocket events are placed in a queue and are serviced on a first-come,
first-served basis by the \code{service} function. This function is normally
placed in a loop. The function times out after the specified interval if
events do not occur.
}
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
\examples{
# See the detailed example in help('websockets')
}
