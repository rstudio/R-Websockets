\name{websockets-package}
\alias{websockets-package}
\alias{websockets}
\docType{package}
\title{R/Websockets Interface}
\description{
The HTML 5 Websocket API defines lightweight socket-like communication protocol.
The websockets package is a simple and portable Websocket implementation
for R.
}
\details{
The R/Websockets interface provides exposes the basic HTML 5 Websocket API
to the R language. It lets R directly interact with
web pages without going through server middleware layers.

The basic procedure for initializing the R/Websockets interface is:
\enumerate{
\item Load the library.
\item Initialize with \code{createContext}.
\item Define desired callback functions to respond to events with \code{setCallback}.
\item Service events with \code{service}.
}
}
\author{
B. W. Lewis <blewis@illposed.net>
Maintainer: B. W. Lewis <blewis@illposed.net>
}
\references{
http://dev.w3.org/html5/websockets/
}
\examples{
# Run the demos to see a few examples:
# demo(package='websockets')
}
