# The websocket class holds information on the server and connected clients.
setClass("websocket",
  representation(port="numeric", env="environment")
#  representation(port="numeric", binary="logical", env="environment",
#                 static="closure",receive="closure",closed="closure",
#                 established="closure")
)

setGeneric("cat")
setMethod("cat",signature(file="websocket"), 
  function(..., file)
  {
    if(missing(sep)) sep=" "
    websocket_write(paste(...,sep=sep),WS=file)
  }
)
setGeneric("save")
setMethod("save",signature(file="websocket"),
  function(..., file)
  {
    print("YIKES! This would be easier but for all that .Internal nonsense in connections..")
  }
)
# Add more methods that take connections (write*, serialize, etc.)...
