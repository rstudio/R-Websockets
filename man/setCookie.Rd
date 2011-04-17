\name{setCookie}
\alias{setCookie}
\title{
setCookie
}
\description{
Set the data cookie associated with a websocket client connection.
}
\usage{
setCookie(cookie, DATA)
}
\arguments{
  \item{cookie}{An external pointer reference to the client cookie.
}
  \item{DATA}{
The character or RAW data to store.
}
}
\details{
Call setCookie from within a callback function to set the data cookie
associated with a client websocket connection.
}
\value{
NULL is invisibly returned.
}
\author{
B. W. Lewis <blewis@illposed.net>
}
\examples{
# See the detailed example in help('websockets').
}
