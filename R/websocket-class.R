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
    print("CAZART!!")
  }
)
setGeneric("save")
setMethod("save",signature(file="websocket"),
  function(..., file)
  {
    print("YIKES")
  }
)
