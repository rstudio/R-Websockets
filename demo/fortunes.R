# A chat-like program that also emits R fortunes.
# It requires the fortunes, tm and wordcloud packages.
library('websockets')
htmldata = '<html><head><title>R/Websockets</title></head>
<body>
<style type="text/css">
div.scroll {
height: 200px;
width: 100%;
overflow: auto;
border: 1px solid #666;
padding: 8px;
}
</style>
<script>
chattext = "";
String.prototype.startsWith = function(str){return (this.indexOf(str) === 0);}
var socket = new WebSocket("ws://localhost:7681", "chat");
try {
  socket.onopen = function() {
    document.getElementById("wsdi_status").textContent =
      " websocket connection opened ";
    document.getElementById("statustd").style.backgroundColor = "#40ff40";
  } 
  socket.onmessage = function got_packet(msg) {
    if(msg.data.startsWith("CHAT")) {
      chattext = msg.data.concat("\n");
      chattext = msg.data.concat(chattext);
      document.getElementById("chat").textContent = chattext;
    } 
    else document.getElementById("plot").src = msg.data;
  } 
  socket.onclose = function(){
    document.getElementById("wsdi_status").textContent =
      " websocket connection CLOSED ";
    document.getElementById("statustd").style.backgroundColor = "#ff4040";
  }
}
catch(ex) {document.getElementById("chat").textContent = "Error: " + ex;}

function hello() {
  socket.send(document.getElementById("msg").value);
  document.getElementById("msg").value = "";
}

function checkKey(evt)
{
  if(evt.keyCode == 13) {
    hello();
    return false;
  }
  return true;
}
</script>

<table><tr>
<td>Message:</td>
<td><input type="text" id="msg" value="" size="80" maxlength="150" onkeypress="return checkKey(event);"/>
</td></tr><tr>
<td id="statustd">
<div id="wsdi_status"> Connection not initialized </div>
</td><td>
<div id="chat" class="scroll">
</div>
</td></tr></table>
</body>
</html>
'

#if(require('fortunes') && require('wordcloud') && require('tm'))
w = createContext(webpage=static_text_service(htmldata))
while(TRUE){
  service(w)
}
