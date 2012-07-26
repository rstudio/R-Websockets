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
cl = function(WS)
{
  cat("Websocket client socket ",WS$socket," has closed.\n")
}
set_callback('closed',cl,w)
es = function(WS)
{
  cat("Websocket client socket ",WS$socket," has been established.\n")
}
set_callback('established',es,w)

cat("Direct your web browser to http://localhost:7681\n")
while(TRUE) service(w)
