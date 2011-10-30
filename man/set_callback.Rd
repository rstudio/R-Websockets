\name{set_callback}
\alias{set_callback}
\alias{setCallback}
\title{ set_callback }
\description{
Assign an R function to service an HTML 5 Websocket event type.
}
\usage{
set_callback(id, f, envir)
setCallback(id, f, envir)
}
\arguments{
  \item{id}{The callback name. Presently supported names are
\code{closed}, \code{established}, and \code{receive}.
}
  \item{f}{
The callback function. See the details section for required function
arguments.
}
  \item{envir}{
The environment associated with the R websocket context returned by
\code{createContext}.
}
}
\details{
Define one or more of the callback functions to respond to websocket events
(all of them are optional). A callback function is evaluated whenever
a matching event is serviced by the \code{service} function. There are
three supported callback function names:
\code{established}, \code{receive}, and \code{closed}, that are evaluated
when corresponding events occur for the websocket.

The \code{receive} function must take three arguments:
\enumerate{
\item \code{DATA}  A RAW vector that holds any incoming data associated with the
event, for example from broadcast or receive.
\item \code{WS}   The websocket file descriptor.
\item \code{HEADER} Contains message header information for
newer protocol messages. The vignette describes this parameter in detail. Although
it is a required parameter, it is usually ignored in practice.
The \code{closed} callback is fired whenever a websocket client socket is closed.
The \code{receive} callback occurs when data is received on a client websocket.
The \code{established} callback occurs when a new websocket client socket connection
is successfully negotiated.
}

The \code{established} and \code{closed} functions must each take one argument:
\enumerate{
\item \code{WS}   The websocket file descriptor.
}.
\strong{NOTE THAT THE CALLBACK FUNCTION PARAMETERS HAVE CHANGED AS OF PACKAGE
VERSION 1.1.0 AND ARE NOT COMPATIBLE WITH PREVIOUS PACKAGE VERSIONS.}

The arguments will be populated by data corresponding to the event.
The \code{WS} socket can be used by the \code{websocket_write}
function to write data to the client through the socket.

Non-websocket HTTP requests are serviced by the \code{static} function callback
defined by the \code{webpage} argument described in the \code{create_server}
help pages.  The \code{static} function may also be redefined with
\code{set_callback}.
}
\value{
The \code{set_callback} function is invoked for its side effect of
assigning the function to the variable name in the websocket context
evnironment. NULL is invisible returned.
}
\author{
B. W. Lewis <blewis@illposed.net>
}
\seealso{
\code{\link{createContext}}
\code{\link{create_server}}
}
\examples{
# See the detailed example in help('websockets').
}
