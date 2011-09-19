\name{setCallback}
\alias{setCallback}
\title{
setCallback
}
\description{
Assign an R function to service an HTML 5 Websocket event type.
}
\usage{
setCallback(id, f, envir)
}
\arguments{
  \item{id}{The callback name. Presently supported names are
\code{closed}, \code{established}, and \code{receive}.
}
  \item{f}{
The callback function. It must take three arguments.
}
  \item{envir}{
The environment associated with the R websocket context returned by
\code{createContext}.
}
}
\details{
Define one or more of the callback functions to respond to websocket events
(all of them are optional). A callback function is evaluated whenever
a matching event is serviced by the \code{service} function. Each callback
function must have three arguments:
\enumerate{
\item \code{DATA}  A RAW vector that holds any incoming data associated with the
event, for example from broadcast or receive.
\item \code{WS}   The websocket file descriptor.
\item \code{COOKIE} DEPRECATED: this parameter will disappear in the next version.
The \code{closed} callback is fired whenever a websocket client socket is closed.
The \code{receive} callback occurs when data is received on a client websocket.
The \code{established} callback occurs when a new websocket client socket connection
is successfully negotiated.
}
The arguments will be populated by data corresponding to the event.
The \code{WS} socket can be used by the \code{websocket_write}
function to write data to the client through the socket.
}
\value{
The \code{setCallback} function is invoked for its side effect of
assigning the function to the variable name in the websocket context
evnironment. NULL is invisible returned.
}
\author{
B. W. Lewis <blewis@illposed.net>
}
\seealso{
\code{\link{createContext}}
}
\examples{
# See the detailed example in help('websockets').
}
