library('websockets')
htmldata = '<html><head><title>R/Websockets</title></head>
<body>
<script>
String.prototype.startsWith = function(str){return (this.indexOf(str) === 0);}
var socket = new WebSocket("ws://localhost:7681", "chat");
var j = 1;
var tm = (new Date()).getTime();
try {
  socket.onopen = function() {
    document.getElementById("wsdi_status").textContent =
      " websocket connection opened ";
    document.getElementById("statustd").style.backgroundColor = "#40ff40";
  } 
  socket.onmessage = function got_packet(msg) {
    document.getElementById("chat").innerHTML = "<pre>" + msg.data + "</pre>";
    if(j>100) {
      var s = j*1000/((new Date()).getTime() - tm);
      tm = (new Date()).getTime();
      j = 1;
      document.getElementById("status").innerHTML = "<pre>" + s + " msg/s</pre>";
    }
  } 
  socket.onclose = function(){
    document.getElementById("wsdi_status").textContent =
      " websocket connection CLOSED ";
    document.getElementById("statustd").style.backgroundColor = "#ff4040";
  }
}
catch(ex) {document.getElementById("chat").innerHTML = "Error: " + ex;}

spigot = function()
{
  socket.send(j + " ");
  j = j + 1;
  setTimeout("spigot();", 10);
}

window.onload = function(){ 
  tm = (new Date()).getTime();
  document.getElementById("chat").innerHTML = "Starting...";
  setTimeout("spigot();", 1000);
}
</script>

<h2>Test: Client rapidly sends messages to server who echoes them back.</h2>
<table><tr>
<td id="statustd">
<div id="wsdi_status"> Connection not initialized </div>
</td></tr></table>
<div id="chat">
</div>
<div id="status">
</div>
<hr />
</body>
</html>
'

w = createContext(webpage=static_text_service(htmldata))
f = function(DATA,WS,...)
{
  d = tryCatch(rawToChar(DATA),error=function(e) "")
  x = paste("<b>WebSocket ",WS$socket," says: </b>",d,sep="")
  websocket_write(x,WS)
}
set_callback("receive",f,w)
cat("Direct your local web browser to http://localhost:7681\n")
while(TRUE) service(w)
