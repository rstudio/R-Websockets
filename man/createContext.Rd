\name{createContext}
\alias{createContext}
\title{
createContext
}
\description{
Initialize the R/Websocket service.
}
\usage{
createContext(port = 7681L, webpage = paste(system.file(package = "websockets"), "basic.html", sep = "//"))
}
\arguments{
  \item{port}{The service port number. }
  \item{webpage}{Optionally, specifiy a file to service HTTP requests. This value may be NULL.}
}
\details{
One must initialize the R/Websocket service by calling createContext.
}
\value{
An environment is returned. The R/Websocket service will search the environment
for callback functions named:
\code{closed}, \code{established}, \code{broadcast}, and \code{receive}.
Each callback function must take three arguments: DATA, WS, and COOKIE.
The callback functions are optional. When a websocket event occurs, the
corresponding function will be evaluated if it exists.
The DATA parameter holds a RAW vector with incoming data from broadcast
or receive events, and may be of length zero for other events.
The WS parameter is an external pointer reference to the client web socket
corresponding to the event.
The COOKIE parameter is an external pointer reference to a data cookie
associated with the client corresponding to the event.
}
\author{
B. W. Lewis <blewis@illposed.net>
}

\seealso{
\code{\link{setCookie}},
\code{\link{getCookie}},
\code{\link{service}},
\code{\link{websocket_broadcast}}
\code{\link{websocket_write}}
\code{\link{setCallback}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# See the detailed example in help('websockets').
}
