\name{getCookie}
\alias{getCookie}
\title{
getCookie
}
\description{
Retrieve an R character representation of the data cookie associated
with a websocket client connection.
}
\usage{
getCookie(cookie)
}
\arguments{
  \item{cookie}{The external pointer reference to the data cookie
obtained from the callback function. }
}
\details{
This function will normally be called from within a callback function.
}
\value{
A character variable containing the cookie data.
}
\author{
B. W. Lewis <blewis@illposed.net>
}

\examples{
# See the detailed example in help('websockets') for an example.
}
