\name{daemonize}
\alias{daemonize}
\title{Service websocket events asynchronously}
\description{
Add the specified websocket server to the main R event loop, returning
control to the console.
}
\usage{
daemonize(server)
}
\arguments{
  \item{server}{A websockets server created with \code{create_server}.
}}
\details{
Process websocket events in the main R event loop asynchronously,
returing control to the R console.
}
\value{
NULL is invisibly returned
}
\author{
B. W. Lewis <blewis@illposed.net>
}

\seealso{
\code{\link{createContext}}
\code{\link{create_server}}
}
\examples{
# See the detailed example in help('websockets')
}
