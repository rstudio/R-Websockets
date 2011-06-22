# R/websockets example
if(any(is.na(packageDescription('caTools'))))
  stop("This demo requires the caTools package.\nRun install.packages('caTools') to install it.\n\n")
if(any(is.na(packageDescription('PerformanceAnalytics'))))
  stop("This demo requires the PerformanceAnalytics package.\nRun install.packages('PerformanceAnalytics') to install it.\n\n")
library('websockets')
library('caTools')
library('PerformanceAnalytics')
require('RJSONIO')
data(managers)

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
<table>
<tr><td style="vertical-align:top;">
<div id="input">
<form name="portfolios">
<select name="plist" size="1" multiple style="width:150px">
</select>
<br />
<select name="chartType" size="1" style="width:150px">
</select>
</form>
</div>
<br />
<input type=button id="send" value="Update" onclick="hello();"/>
</td><td>
<div id="istat"></div>
<img id="plot" src="data:image/jpg;base64,
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" alt="HTML5" width="850" height="500" style="border:1px solid black;"/>
</td></tr></table>
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
var json = new Array();
json.portfolios = new Array();
try {
  socket.onopen = function() {
    document.getElementById("wsdi_status").textContent =
      " websocket connection opened ";
    document.getElementById("statustd").style.backgroundColor = "#40ff40";
  } 

  socket.onmessage = function got_packet(msg) {
  document.body.style.cursor = "default";
    if(msg.data.startsWith("{")) {
      var j;
      json = JSON.parse(msg.data);
      if(json.portfolios) {
        document.portfolios.plist.options.length = 0;
        for(j=0;j<json.portfolios.length;j+=1)
          document.portfolios.plist.options[j]=new Option(json.portfolios[j], json.portfolios[j], true, false);
      }
      if(json.types) {
        document.portfolios.chartType.options.length = 0;
        for(j=0;j<json.types.length;j+=1)
          document.portfolios.chartType.options[j]=new Option(json.types[j], json.types[j], true, false);
      }
    }
    else document.getElementById("plot").src = msg.data;
  } 

  socket.onclose = function(){
    document.getElementById("wsdi_status").textContent =
      " websocket connection CLOSED ";
    document.getElementById("statustd").style.backgroundColor = "#ff4040";
  }

}
catch(ex) {document.getElementById("output").textContent = "Error: " + ex;}

function hello() {
  var j;
  var msg = document.portfolios.chartType.value + ",";
  for(j=0;j<document.portfolios.plist.length;j+=1)
    if(document.portfolios.plist.options[j].selected)
      msg = msg + document.portfolios.plist.options[j].value + ","
  document.body.style.cursor = "wait";
  socket.send(msg);
}

</script>
</body>
</html>'

id = 1           # id tracks each connected websocket client.
p = tempfile()
cat(webpage(),file=p)
w = createContext(webpage=p)
oldopt = options(warn=-1, demo.ask=FALSE)

types=c("PerformanceSummary","BoxPlot","RiskReturnScatter","RollingPerformance")
trailing36.rows = (nrow(managers)-36):nrow(managers)

# Set receive and broadcast callbacks
f = function(DATA,WS,COOKIE)
{
  x = paste(rawToChar(DATA))
  x = withCallingHandlers(strsplit(x,",")[[1]],error=function(e) character())
  tryCatch(
  if(nchar(x)>0) {
    print(x)
    cat("Client ID ",getCookie(COOKIE)," sent us some data!\n")
    f = tempfile()
    jpeg(file=f, width=850,height=500, quality=100)
    devAskNewPage(ask=FALSE)
    chartType = x[1]
    x = x[-1]
    if(chartType == types[1])
      charts.PerformanceSummary(managers[, x], colorset = rich6equal, lwd = 2, ylog = TRUE)
    else if(chartType == types[2])
      chart.Boxplot(managers[trailing36.rows, x], main = "Trailing 36-Month Returns")
    else if(chartType == types[3])
      chart.RiskReturnScatter(managers[trailing36.rows, 1:8], Rf = 0.03/12,  main = "Trailing 36-Month Performance")
    else if(chartType == types[4])
      charts.RollingPerformance(managers[, x], Rf = 0.03/12, colorset = c("red", "blue", "cyan", "yellow", "black", "orange", "green"), lwd = 2)
    dev.off()
    p <- base64encode(readBin(f,what="raw",n=1e6))
    p <- paste("data:image/jpg;base64,\n",p,sep="")
    websocket_write(paste(p),WS)
    file.remove(f)
  }, error=function(e) print(e));
}
setCallback("receive",f, w)

h = function(DATA, WS, COOKIE)
{
  cat("Client ID ",getCookie(COOKIE), " closed its connection.\n")
}
setCallback("closed",h, w)

# Set up an established (initialization) callback
g = function(DATA, WS, COOKIE)
{
  setCookie(COOKIE, paste(id))
  websocket_write(toJSON(list(portfolios=colnames(managers),types=types)),WS);
  id <<- id + 1
}
setCallback("established",g, w)

cat("\nThe web service will run until <CTRL>+C is pressed.\n")
cat("Open your local web browser to http://localhost:7681\n")
while(TRUE) {
  service(w)
  Sys.sleep(0.05)
}
rm(w)
gc()
file.remove(p)
options(oldopt)
