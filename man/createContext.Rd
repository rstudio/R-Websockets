\name{createContext}
\alias{createContext}
\title{ createContext }
\description{
Create a websocket server and bind it to a port.
}
\usage{
createContext(port = 7681L, webpage = static_file_service(paste(system.file(package = "websockets"), "basic.html", sep = "//")))
}
\arguments{
  \item{port}{The service port number. }
  \item{webpage}{Optionally, specifiy a function to service HTTP requests. This value may be NULL.}
}
\details{
The websockets package supports an arbitrary number of servers, each listening
on a distinct TCP port number. An environment is associated with each running
server. Server events must be serviced with the \code{service} function.
}
\value{
An environment is returned. The R/Websocket service will search the environment
for callback functions named:
\code{closed}, \code{established}, and \code{receive}.
Each callback function must take three arguments: DATA, WS, and '...'.
The callback functions are optional. When a websocket event occurs, the
corresponding function will be evaluated if it exists.
The DATA parameter holds a RAW vector with incoming data from broadcast
or receive events, and may be of length zero for other events.
The WS parameter is an external pointer reference to the client web socket
corresponding to the event.
Previous package versions included a COOKIE parameter which has been dropped.
}
\author{
B. W. Lewis <blewis@illposed.net>
}

\seealso{
\code{\link{service}},
\code{\link{websocket_broadcast}}
\code{\link{websocket_write}}
\code{\link{setCallback}}
}
\examples{
# See the detailed example in help('websockets').
}
