# Simple example
require(websockets)
w = create_server()
w$DEBUG=TRUE
f = function(DATA, WS, ...)
{
  cat("Receive callback\n")
  D = ""
  if(is.raw(DATA)) D = rawToChar(DATA)
  websocket_write(DATA=paste("You sent",D,"\n",collapse=" "),WS=WS)
}
set_callback('receive',f,w)
cat("Direct your web browser to http://localhost:7681\n")
while(TRUE)
{
  service(w)
}
