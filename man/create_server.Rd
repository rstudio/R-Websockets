\name{create_server}
\alias{create_server}
\alias{createContext}
\title{ Create a websocket server.  }
\description{
Create a websocket server and bind it to a port.
}
\usage{
create_server(port = 7681L, webpage = static_file_service(paste(system.file(package = "websockets"), "basic.html", sep = "//")), is.binary = FALSE)
createContext(port = 7681L, webpage = static_file_service(paste(system.file(package = "websockets"), "basic.html", sep = "//")), server = TRUE, is.binary = FALSE)
}
\arguments{
  \item{port}{The service port number. }
  \item{webpage}{Optionally, specifiy a function to service HTTP requests. This value may be NULL.}
  \item{server}{Set FALSE if the environment will represent a client connection (used internally by the \code{websocket} function.}
  \item{is.binary}{The websocket connection binary transport flag.}
}
\details{
The websockets package supports an arbitrary number of servers, each listening
on a distinct TCP port number. An environment is associated with each running
server. Server events must be serviced with the \code{service} function.

The webpage callback must be a function of two arguments, \code{socket} and
\code{header}.  The \code{socket} argument indicates the low-level socket
descriptor of the client. The \code{header} argument contains a parsed HTTP
request header. When invoked, the function may examine the request and respond
using the \code{http_response} and \code{http_vars} functions.
}
\value{
An environment is returned. The R/Websocket service will search the environment
for callback functions named:
\code{closed}, \code{established}, and \code{receive}.
The \code{established} and \code{closed} functions must take a single
argument (a websocket).
The \code{receive} function must take three arguments: DATA, WS, and '...',
holding the message data, websocket client, and possible 
ancillary information received.
The callback functions are optional. When a websocket event occurs, the
corresponding function will be evaluated if it exists.
The DATA parameter holds a RAW vector with incoming data from broadcast
or receive events, and may be of length zero for other events.
The WS parameter is the client web socket corresponding to the event.

See the \code{\link{set_callback}} function for more information.
}
\author{
B. W. Lewis <blewis@illposed.net>
}

\seealso{
\code{\link{service}},
\code{\link{websocket_broadcast}}
\code{\link{websocket_write}}
\code{\link{set_callback}}
\code{\link{http_response}}
\code{\link{http_vars}}
}
\examples{
# See the detailed example in help('websockets').
}

