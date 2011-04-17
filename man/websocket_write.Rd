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
  \item{DATA}{A character string or RAW vector to write. }
  \item{WS}{A pointer to the websocket, usually obtained from a
callback function. }
}
\value{
The number of bytes sent or an error.
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
