\name{http_vars}
\alias{http_vars}
\title{Parse http GET or POST variables, returning a list.}
\description{Parse http GET or POST variables, returning a list.}
\usage{
http_vars(socket, header)
}
\arguments{
  \item{socket}{An open client socket connection.}
  \item{header}{An http header.}
}
\details{Parse http GET or POST variables, returning a list. This function
is designed to be used within the \code{static} webpage callback described
in \code{create_server}.}
\value{A list, possibly NULL, containing the named parsed variables.
}
\author{
B. W. Lewis <blewis@illposed.net>
}
\seealso{
\code{\link{createContext}}
\code{\link{create_server}}
}
