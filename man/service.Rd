\name{service}
\alias{service}
\title{
service
}
\description{
Service the HTML 5 Websocket queue.
}
\usage{
service(contextEnv)
}
\arguments{
  \item{contextEnv}{The websocket environment returned from \code{createContext}.
}
}
\details{
Websocket events are placed in a queue and are serviced on a first-come,
first-served basis by the \code{service} function. This function is normally
placed in a loop.
}
\value{
NULL is invisibly returned
}
\author{
B. W. Lewis <blewis@illposed.net>
}

\seealso{
\code{\link{createContext}}
}
\examples{
# See the detailed example in help('websockets')
}
