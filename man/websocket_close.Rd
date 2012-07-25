\name{websocket_close}
\alias{websocket_close}
\title{
Close a websocket connection.
}
\description{
Close a websocket server or client connection.
}
\usage{
websocket_close(connection)
}
\arguments{
  \item{connection}{A websocket client or server. See the details section for more
information.  }
}
\details{
Use \code{websocket_close} to cleanly shut down a websocket server or client
connection.

If the \code{connection} is a websocket server, all associated client connections
will also be closed.

Connected clients are stored in the environment associated with a websocket
server in the \code{client_sockets} list. Because each connected client is
itself an environment, be careful to access a single client element, for
example with the '[[' indexing operator, shown here accessing the first client
socket: \code{WS$client_sockets[[1]]}.

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
