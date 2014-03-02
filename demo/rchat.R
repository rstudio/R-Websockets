# A chat-like program for R.
library('websockets')
library('caTools')
library('RJSONIO')
htmldata = '<html><head><title>R Chat</title></head>
<body>
<style type="text/css">
.fig {
  float: right;
  margin: 0 0 10px 10px;
  clear: right;
}
div.scroll {
  height: 300px;
  width: 500px;
  border: 1px solid #666;
  padding: 8px;
  overflow-y: scroll;
  overflow-x: scroll;
  overflow: scroll;
  word-wrap: break-word;
}
input[type="text"], textarea { 
  padding: 0;
  margin: 0;
  width: 518px; 
  border: 1px solid #666;
}
</style>
<script>
var socket = new WebSocket("ws://"+window.location.host, "Rchat");
var last = "";
try {
  socket.onopen = function() {
    document.getElementById("chat").innerHTML = "<b>Websocket connection established</b>";
  }
  socket.onmessage = function got_packet(msg) {
    var ans = JSON.parse(msg.data);
    var log = document.getElementById("chat").innerHTML;
    log = log + "<pre>" + ans.msg + "</pre>";
    document.getElementById("chat").innerHTML = log;
    document.getElementById("chat").scrollTop = 100000;
  
    if(ans.fig != null) document.getElementById("plot").src = ans.fig;
  }
  socket.onclose = function(){
    document.getElementById("chat").innerHTML = "<b>Websocket connection closed</b>";
  }
} catch(ex) {document.getElementById("chat").textContent = "Error: " + ex;}

function checkKey(evt)
{
  if(evt.keyCode == 13) {
    last = document.getElementById("msg").value;
    socket.send(last);
    document.getElementById("msg").value = "";
    return false;
  }
  else if(evt.keyCode == 38) {
    document.getElementById("msg").value = last;
    return false;
  }
  return true;
}
</script>

<h2>R Chat</h2>
<div>
<img class="fig" id="plot" alt="R plot area" width="400" height="400" />
<div id="chat" class="scroll"/>
</div>
<h3>Console:</h3>
<input type="text" id="msg" value="" size="80" maxlength="150" onkeyup="return checkKey(event);"/>
</body>
</html>
'


port = 7681
w = create_server(webpage=static_text_service(htmldata), port=port)

f = function(DATA,WS,...)
{
  d = tryCatch(rawToChar(DATA),error=function(e) "")
  jpeg(file=itmp, width=400,height=400,quality=100)
  devAskNewPage(ask=FALSE)
  sink(file=ctmp,split=TRUE)
  ans = tryCatch(capture.output(eval(parse(text=d),envir=globalenv())),
          error = function(e) cat(as.character(e),file=ctmp))
  dev.off()
  sink()
  ans = paste(ans,collapse="\n")
  if(file.exists(itmp)) {
    p = base64encode(readBin(itmp,what="raw",n=1e6))
    p = paste("data:image/jpg;base64,\n",p,sep="")
    msg = sprintf("<b>Client %d says: </b>%s\n%s",WS$socket,d,ans)
    websocket_broadcast(toJSON(list(msg=msg, fig=p)),WS$server)
  } else {
    msg = sprintf("<b>Client %d says: </b>%s\n%s",WS$socket,d,ans)
    websocket_broadcast(toJSON(list(msg=msg)),WS$server)
  }
}
set_callback("receive",f,w)

ctmp = tempfile()
itmp = tempfile()

cat(sprintf("Direct browsers to http://%s:%s\n",Sys.info()["nodename"],port))
while(TRUE) service(w)
