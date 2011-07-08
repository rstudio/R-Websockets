library('websockets')
id = 1        # id tracks each connected websocket client.
w = createContext()
oldopt = options(warn=-1)

client_hash = new.env()

# Echo service
f = function(DATA,WS,COOKIE)
{
  x = paste(rawToChar(DATA))
  if(!is.na(x)) {
    cat("Client ID ",getCookie(COOKIE)," sent us some data! (",x,")\n")
  }
  websocket_write(paste(x),WS)
}
setCallback("receive",f, w)
setCallback("broadcast",f, w)

h = function(DATA, WS, COOKIE)
{
  cat("Client ID ",getCookie(COOKIE), " closed its connection.\n")
  rm(paste(id), envir=client_hash)
}
setCallback("closed",h, w)

g = function(DATA, WS, COOKIE)
{
  setCookie(COOKIE, paste(id))
  assign(paste(id),WS, envir=client_hash)
  websocket_write(paste("Connection established: Your R websocket client ID is",id),WS)
  id <<- id + 1
}
setCallback("established",g, w)

j = 0
startTime = proc.time()[3]
while(proc.time()[3] - startTime < 300) {
  j = (j + 1) %% 20
# About every second, randomly pick one client and send a message...
  if(j == 0) {
    k = ceiling(runif(1)*length(as.list(client_hash)))
    if(k>0) {
      WS = get(paste(k),envir=client_hash)
      websocket_write(paste("Your secret number is",runif(1)),WS)
     }
  }
  service(w)
  Sys.sleep(0.05)
}
rm(w)
gc()
options(oldopt)
