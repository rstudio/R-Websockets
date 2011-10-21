\name{websocket}
\alias{websocket}
\title{ Create a websocket client.  }
\description{Create a websocket client connection.  }
\usage{
websocket(url, port=80, subprotocol="chat", version=0)
}
\arguments{
  \item{url}{The websocket url to connect to. }
  \item{port}{The service port number. }
  \item{subprotocol}{A websocket subprotocol id.}
  \item{version}{A websocket version, either 0 or 8.}
}
\details{
Create a context for a websocket client connection. All the functions
available to a websocket server context, including callbacks for handling
events for the connection, may be defined just as with a server
context created with \code{create_server}.
}
\value{
An environment is returned. The service will search the environment
for callback functions named:
\code{closed}, \code{established}, and \code{receive}.
The \code{established} and \code{closed} functions must take a single
argument (a websocket).
The \code{receive} function must take three arguments: DATA, WS, and HEADER,
holding the message data, websocket client, and possible 
ancillary header information received.

The callback functions are optional. When a websocket event occurs, the
corresponding function will be evaluated if it exists.
The DATA parameter holds a raw vector with incoming data from broadcast
or receive events, and may be of length zero for other events.
The WS parameter is the client web socket corresponding to the event.

See the \code{\link{set_callback}} function for more information.
}
\author{
B. W. Lewis <blewis@illposed.net>
}

\seealso{
\code{\link{service}},
\code{\link{websocket_write}}
\code{\link{set_callback}}
}
\examples{
# Connect to the public websocket echo service
\dontrun{
client = websocket("ws://echo.websocket.org")
set_callback("receive", function(DATA, WS, HEADER) cat(rawToChar(DATA),"\n"), client)
websocket_write("Testing, testing", client$client_sockets[[1]])
service(client)
websocket_close(client)
}
}
