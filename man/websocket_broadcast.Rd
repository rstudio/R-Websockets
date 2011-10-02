\name{websocket_broadcast}
\alias{websocket_broadcast}
\title{
websocket_broadcast
}
\description{
Write data to all websocket clients associated with one websocket server.
}
\usage{
websocket_broadcast(DATA, server)
}
\arguments{
  \item{DATA}{A character string or RAW vector to broadcast.  }
  \item{server}{A websocket server environment returned by \code{create_server}.}
}
\details{
The HTML 5 Websocket API emulates a connection-oriented socket without
a true broadcast capability. The \code{websocket_broadcast} function
emulates a broadcast by sending the DATA payload to each connected client
in a loop.
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
