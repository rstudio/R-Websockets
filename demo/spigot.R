# Basic R/websockets example
library('websockets')
library('RJSONIO')
# ------------------------------------------------------------------
# Here is the web page that we use for this example (it's saved to a
# temporary file):
webpage = function()
'<html>
<head>
  <title>R/Websockets</title>
</head>
<style type="text/css" media="screen">
.fright {
  float: right;
  margin: 0 0 10px 10px;
  clear: right;
}
</style>
<body>
<table><tr>
<td id="statustd">
<div id="wsdi_status"> Connection not initialized </div>
</td></tr></table>
<hr />
<br />
<div id="output"></div>
<script>
String.prototype.startsWith = function(str){return (this.indexOf(str) === 0);}

var socket = new WebSocket("ws://localhost:7681", "R");
try {
  socket.onopen = function() {
    document.getElementById("wsdi_status").textContent =
      " websocket connection opened ";
    document.getElementById("statustd").style.backgroundColor = "#40ff40";
  } 
  socket.onmessage = function got_packet(msg) {
  document.body.style.cursor = "default";
    document.getElementById("output").textContent = msg.data;
  } 
  socket.onclose = function(){
    document.getElementById("wsdi_status").textContent =
      " websocket connection CLOSED ";
    document.getElementById("statustd").style.backgroundColor = "#ff4040";
  }
}
catch(ex) {document.getElementById("output").textContent = "Error: " + ex;}
</script>
</body>
</html>'

id = 1        # id tracks each connected websocket client.
p = tempfile()
cat(webpage(),file=p)
w = createContext(webpage=p)
oldopt = options(warn=-1)

# Set receive and broadcast callbacks
f = function(DATA,WS,COOKIE)
{
  websocket_write(DATA,WS)
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
cat("Got a new connection: id", id,".\n")
  id <<- id + 1
}
setCallback("established",g, w)

# Run a service loop for a maximum of 5 minutes:
cat("\nThe web service will run for 5 minutes or until <CTRL>+C is pressed.\n")
cat("Open your local web browser to http://localhost:7681\n")
while(TRUE) {
  websocket_broadcast(paste(runif(3), collapse=","))
  service(w)
  Sys.sleep(0.01)
}
rm(w)
gc()
options(oldopt)
