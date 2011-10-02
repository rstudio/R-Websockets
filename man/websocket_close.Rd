\name{websocket_close}
\alias{websocket_close}
\title{
Close a websocket client connection.
}
\description{
Close a websocket client connection.
}
\usage{
websocket_close(connection)
}
\arguments{
  \item{connection}{A websocket client. See the details section for more
information.  }
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
An integer value returned by the underlying TCP socket close function
which may vary in interpretation from system to system.
}
\author{
B. W. Lewis <blewis@illposed.net>
}
\seealso{
\code{\link{websocket_write}}
}
\examples{
##
}
