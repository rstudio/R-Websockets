\name{websockets-package}
\alias{websockets-package}
\alias{websockets}
\docType{package}
\title{R/Websockets Interface}
\description{
The HTML 5 Websocket API defines lightweight socket-like communication protocol.
The websockets package is a simple and portable Websocket implementation
for R.
}
\details{
The R/Websockets interface provides exposes the basic HTML 5 Websocket API
to the R language. It lets R directly interact with
web pages without going through server middleware layers.

The basic procedure for initializing the R/Websockets interface is:
\enumerate{
\item Load the library.
\item Initialize with \code{createContext}.
\item Define desired callback functions to respond to events with \code{setCallback}.
\item Service events with \code{service}.
}
}
\author{
B. W. Lewis <blewis@illposed.net>
Maintainer: B. W. Lewis <blewis@illposed.net>
}
\references{
http://dev.w3.org/html5/websockets/
}
\examples{
# NOTE: We embed this example in a global function in order to
# avoid running it during check.
assign("runme",
function(){
require('websockets')

id = 1        # id tracks each connected websocket client.
v =  0        # We will periodically transmit v to each connected client.
w = createContext()
oldopt = options(warn=-1)

# Set receive and broadcast callbacks
f = function(DATA,WS,COOKIE)
{
  x = paste(rawToChar(DATA))
  x = withCallingHandlers(as.numeric(x),error=function(e) NA)
  if(!is.na(x)) {
    cat("Client ID ",getCookie(COOKIE)," sent us some numeric data! (",x,")\n")
    v <<- x    
  }
  websocket_write(paste(v),WS)
}
setCallback("receive",f, w)
setCallback("broadcast",f, w)

# Set a client close callback
h = function(DATA, WS, COOKIE)
{
  cat("Client ID ",getCookie(COOKIE), " closed its connection.\n")
}
setCallback("closed",h, w)

# Set up an established (initialization) callback
g = function(DATA, WS, COOKIE)
{
  setCookie(COOKIE, paste(id))
  websocket_write(paste("Connection established: Your R websocket client ID is",id),WS)
  id <<- id + 1
}
setCallback("established",g, w)

# Run a service loop for a maximum of 5 minutes:
cat("\nOpen your local web browser to http://localhost:7681\n\n")
cat("\nThe web service will run for 5 minutes or until <CTRL>+C is pressed.\n")
j = 0
startTime = proc.time()[3]
while(proc.time()[3] - startTime < 300) {
  j = j + 1
# The broadcast is used here simply to trigger an update and invoke
# the callback function 'f' defined above about every second or so.
# The null data payload itself is ignored. 
  if(j \%\% 20 == 0) {
    v <- v + 1
    websocket_broadcast(raw(1))
  }
  service(w)
  Sys.sleep(0.05)
}
rm(w)
gc()
options(oldopt)
}, envir=globalenv())

cat("\n\nType: runme() to start the example...\n")

}
