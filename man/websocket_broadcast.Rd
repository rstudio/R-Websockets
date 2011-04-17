\name{websocket_broadcast}
\alias{websocket_broadcast}
\title{
websocket_broadcast
}
\description{
Invoke a send to all connected websockets.
}
\usage{
websocket_broadcast(DATA)
}
\arguments{
  \item{DATA}{A character string or RAW vector to broadcast.
}
}
\details{
The HTML 5 Websocket API emulates a connection-oriented socket without
a true broadcast capability. The \code{websocket_broadcast} function
emulates a broadcast by triggering a broadcast event from every
connected client with the DATA payload. A broadcast is emulated
by responding to each event
with a \code{broadcast} callback function and transmitting the
DATA payload.
}
\value{
The number of bytes sent is returned when successful (otherwise an
error is thrown).
}
\author{
B. W. Lewis <blewis@illposed.net>
}
\examples{
##
}
