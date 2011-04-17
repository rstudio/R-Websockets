# Basic R/websockets example
library('websockets')

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
cat("\nThe web service will run for 5 minutes or until <CTRL>+C is pressed.\n")
cat("Open your local web browser to http://localhost:7681\n")
j = 0
startTime = proc.time()[3]
while(proc.time()[3] - startTime < 300) {
  j = (j + 1) %% 20
# The broadcast is used here simply to trigger an update and invoke
# the callback function 'f' defined above about every second or so.
  if(j == 0) {
    v <- v + 1
    websocket_broadcast("HOMER")
  }
  service(w)
  Sys.sleep(0.05)
}
rm(w)
gc()
options(oldopt)
