\name{websocket_write}
\alias{websocket_write}
\title{
websocket_write
}
\description{
Write data to a websocket connection.
}
\usage{
websocket_write(DATA, WS)
}
\arguments{
  \item{DATA}{A character string or RAW vector to write.}
  \item{WS}{A websocket client from a websocket server environment or a
callback function. See the 'details' section below for more information.}
}
\details{
The \code{WS} argument must be a websocket client which is represented as
a list. All connected clients are stored in the environment associated
with a webserver in the \code{client_sockets} list. Because each connected
client is itself a list, be careful to access a single client element,
for example with the '[[' indexing operator, shown here accessing the
first client socket:
\code{WS$client_sockets[[1]]}.
}
\value{
The number of bytes sent or an error. A returned value of -1 indicates
that the websocket client socket should be closed.
}
\author{
B. W. Lewis <blewis@illposed.net>
}

\seealso{
\code{\link{setCallback}}
}
\examples{
##
}
