# Self-contained introductory R/websockets example

library('websockets')

# This example serves a basic web page and provides a websocket server.
webpage = function()
  '<html><body><h3>R/Websockets Example</h3>
   <div id="output"></div>
   <script>

   var socket = new WebSocket("ws://localhost:7681", "R");
   try {
     socket.onmessage = function got_packet(msg) {
       document.getElementById("output").textContent = msg.data;
     }
    }
    catch(ex) {document.getElementById("output").textContent = "Error: " + ex;}

  </script></body></html>'

# Create a temporary file and save the 'webpage' to it:
p = tempfile()
cat(webpage(),file=p)

# Initialize a websocket server on the default port, serving our 'webpage':
w = createContext(webpage=p)

# Set up an established (successful connection) callback:
g = function(DATA, WS, COOKIE)
{
  print("Got a new connection")
  websocket_write("GREETINGS FROM R",WS)
}
setCallback("established",g, w)

# Start a loop to service websocket events:
while(TRUE) {
  service(w)
  Sys.sleep(0.05)
}
