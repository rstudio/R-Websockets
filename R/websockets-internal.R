# Notes:
# This is a basic implementation of the Websockets protocol.
# - no websocket extensions
# - subprotocol is ignored
# - fragmentation is not supported

.onLoad = function(libname,pkgname)
{
  options(websockets_max_buffer_size=16777216)
}

.onUnload = function(libpath)
{
  options(websockets_max_buffer_size=NULL)
}

# A place to store misc. package state. We bolted on a lot of functions,
# while also maintaining backwards-compatability with older package versions.
# This is the unfortunate result.
.websockets_env = new.env()

# The event handler will evaluate 'websockets_event_handler()' in the global
# environment with activity on the file descriptor.  It returns an external
# pointer reference, a function pointer to an internal handler function. The
# pointer does not need to be de-allocated.
.register_event_handler = function(fd)
{
  .Call("REG_EVENT_HANDLER",as.integer(fd),.package="websockets")
}
.deregister_event_handler = function(handler)
{
  .Call("DEREG_EVENT_HANDLER",handler,.package="websockets")
}

# This is a generic daemon function that services all daemonized
# servers and their client connections. It's called by event handlers
# registered on the server and client sockets.
.websocket_daemon = function()
{
  if(exists("server_list",envir=.websockets_env))
    for(j in .websockets_env$server_list) service(j, timeout=10L)
}

# Remove a server from the daemon watch list
.undaemon = function(server)
{
  socks = sapply(.websockets_env$server_list, function(x) x$server_socket)
  if(length(socks)>0) {
    .websockets_env$server_list = 
      .websockets_env$server_list[!(socks == server$server_socket)]
  }
  invisible()
}

# numToBits can convert large integers to bits.
# It uses the same bit ordering as intToBits.
.numToBits = function(x, fixedLength=NULL)
{
  j = 1;
  r = raw(1);
  if(round(x)!=x) {
    x = round(x)
    warning("Rounding x to whole number")
  }
  if(!is.null(fixedLength)) r = raw(fixedLength)
  one = intToBits(1L)[1]
  while(x>0) {
    if((x %% 2)==1){
      r[j] = one
      x = x - 1
    } else r[j] = raw(1)
    x = x / 2
    j = j + 1
  }
  if(!is.null(fixedLength)) r = r[1:fixedLength]
  r
}

.parse_header = function(msg)
{
# Check to make sure this is a valid GET or POST request. Error out if msg is
# not of the right type, or return null right away if it does not start with
# GET or POS.
  cli_header = list()
  if(!(is.character(msg) || is.raw(msg))) stop("Must be raw or character")
  n = ifelse(is.raw(msg),length(msg),nchar(msg))
  if(n<3) return(c())
  if(is.raw(msg)) if(msg[1]==raw(1)) return(c())
  GET = tryCatch({ifelse(is.raw(msg),rawToChar(msg[1:3]), substr(msg,1,3))},
                 error=function(e) c())
  if(!(GET %in% c("GET","POS"))) return(c())
  rtype = "GET"
  if(GET == "POS") rtype = "POST"
# We are dealing with a GET or POST request, OK to continue.
  if(is.raw(msg)) {
    cli_header$raw = msg
    msg = rawToChar(msg[msg!=0])
  }
  if(n<1) return(cli_header)
  x = gsub("\r","",msg)
  x = strsplit(x,"\n")
  if(length(x)<1) return(cli_header)
  x = x[[1]]
  l = grep(paste("^",rtype,sep=""),x,ignore.case=TRUE)
  cli_header$GET = ifelse(length(l)>0,x[l[1]],"")
  cli_header$TYPE = rtype
  GET = strsplit(cli_header$GET," ")[[1]]
  if(length(GET>2)) {
    cli_header$PROT = tail(GET,1)
    cli_header$RESOURCE = GET[2]
  }
  if(length(l)>0) x = x[-l]
  for(j in 1:length(x)) {
    n = gregexpr(":",text=x[j])[[1]]
    if(!is.na(n) && n[1]>1) {
      key = substr(x[j],1,n-1)
      value = substr(x[j],n+1,nchar(x[j]))
      value = gsub("^ *", "", value)
      cli_header[key] = value
    }
    else break;
  }
  cli_header
}

# Version 00 handshake, which is amazingly lame
.v00_resp_101 = function(cli_header, cli_sock)
{
  er=charToRaw("HTTP/1.1 400 BAD REQUEST\r\n\r\n")
  prot = cli_header["Sec-WebSocket-Protocol"][[1]]
  origin = cli_header["Origin"]
  location = paste("ws://",cli_header["Host"],"/",sep="")
  key1 = cli_header["Sec-WebSocket-Key1"][[1]]
  key2 = cli_header["Sec-WebSocket-Key2"][[1]]
  if(is.null(key1) || is.null(key2)) return(er)
  if(!is.character(key1) || !is.character(key2)) return(er)
  num1 = tryCatch(as.numeric(rawToChar(charToRaw(key1)[gregexpr("[0-9]",text=key1)[[1]]])),
                  error=function(e) return(er))
  num2 = tryCatch(as.numeric(rawToChar(charToRaw(key2)[gregexpr("[0-9]",text=key2)[[1]]])),
                  error=function(e) return(er))
  s1 = length(charToRaw(key1)[gregexpr(" ",text=key1)[[1]]])
  s2 = length(charToRaw(key2)[gregexpr(" ",text=key2)[[1]]])
  v1 = num1/s1
  v2 = num2/s2
  n = length(cli_header$raw)
#  pos = grepRaw(charToRaw("\r\n\r\n"),cli_header$raw,all=TRUE)
#  if(length(pos)<1) return(charToRaw("HTTP/1.1 400 BAD REQUEST\r\n\r\n"))
#  if(length(pos)>1) pos=pos[length(pos)]
#  if(length(cli_header$raw)< pos+11) return(charToRaw("HTTP/1.1 400 BAD REQUEST\r\n\r\n"))
#  key3 = cli_header$raw[(pos+4):(pos+11)]
  check = .SOCK_POLL(cli_sock)
  if(check <1)return(er)
  key3 = .SOCK_RECV(cli_sock)
  if(length(key3)<8) return(er)
  key3 = key3[1:8]
  r1 = packBits(.numToBits(v1,32))[4:1]
  r2 = packBits(.numToBits(v2,32))[4:1]
  val = c(r1,r2,key3)
  hash = digest(val,algo="md5",serialize=FALSE,raw=TRUE)
  resp = "HTTP/1.1 101 Switching Protocols\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\n"
  resp = paste(resp,"Sec-WebSocket-Origin: ",origin,"\r\n",sep="")
  resp = paste(resp,"Sec-WebSocket-Location: ",location,"\r\n",sep="")
  if (!is.null(prot))
    resp = paste(resp,"Sec-WebSocket-Protocol: ",prot,"\r\n\r\n",sep="")
  c(charToRaw(resp),hash)
}

# Version 04 to at least 15 handshake
.v04_resp_101 = function(cli_header)
{
  GUID = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  key = paste(cli_header["Sec-WebSocket-Key"],GUID,sep="") 
  skey = base64encode(digest(charToRaw(key),algo='sha1',serialize=FALSE,raw=TRUE))
  prot = cli_header["Sec-WebSocket-Protocol"][[1]]
  resp = "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n"
  if (!is.null(prot))
    resp = paste(resp,"Sec-WebSocket-Protocol: ",prot,"\r\n",sep="")
  paste(resp,"Sec-WebSocket-Accept: ",skey,"\r\n\r\n",sep="")
}

# Low-level frame header generator, extensions not supported
# Version 01 to at least 15 data framing
# The bit ordering is a bit hard to follow, sorry.
.frame = function(len, FIN=TRUE, opcode=1L, mask=FALSE)
{
  if(is.character(opcode)) opcode = strtoi(opcode, 16L)
  head = rawToBits(raw(1))    # First byte of header
  if(FIN) head[8] = as.raw(1)
  head[1:4] = rawToBits(as.raw(opcode))[1:4]
  head2 = rawToBits(raw(1))    # 2nd byte of header
  rest  = raw(0)              # Optional 3rd -- 6th bytes
  if(mask) head2[8] = as.raw(1)
  if(len < 126) {
    head2[1:7] = rawToBits(as.raw(len))[1:7]
  }
  else if(len > 65535) {
# 8-byte data length, but we only use 4 bytes
    head2[1:7] = rawToBits(as.raw(127))[1:7]
    rest = raw(8)
    rest[8:5] = packBits(intToBits(len),type="raw")
  }
  else {
# 2-byte data length
    head2[1:7] = rawToBits(as.raw(126))[1:7]
    rest = packBits(intToBits(len),type="raw")[2:1]
  }
  c(packBits(head), packBits(head2), rest)
}

# Returns raw message (could be NULL) or FALSE if the client wants
# to close the connection.
.v00_unframe = function(data)
{
  ff = as.raw(255)
  if(data[1]==ff) return(FALSE)
  eof = which(data==ff)
  if(length(eof)<1) return(FALSE)
  data = data[1:eof[1]]
  if(data[length(data)] != ff) warning("End of message missing")
  return(data[2:(length(data)-1)])
}

`.add_client` = function(socket, server)
{
  cs = .SOCK_ACCEPT(socket)
# Check to see if an event handler exists for this server. If so,
# register an event handler for the client socket too.
  handler = NULL
  if(exists("handler",envir=server) && !is.null(server$handler))
  {
    handler = .register_event_handler(cs)
  }
  client_sockets = server$client_sockets
  client_sockets[[as.character(cs)]] = 
    list(socket=cs, wsinfo=NULL, server=server, new=TRUE, handler=handler)
  assign('client_sockets',client_sockets, envir=server)
  invisible()
}

`.remove_client` = function(socket)
{
  server = socket$server
  if(!is.null(socket$handler)) .deregister_event_handler(socket$handler)
  socket$hander = NULL
  cs = socket$server$client_sockets
  cs[[as.character(socket$socket)]] = c()
  j = .SOCK_CLOSE(socket$socket)
  assign('client_sockets',cs, envir=server)
# Trigger client closed callback
  if(exists("closed", envir=server))
    server$closed(socket)
  j
}

.http_400 = function(socket)
{
  .SOCK_SEND(socket,charToRaw("HTTP/1.1 400 BAD REQUEST\r\n\r\n<!DOCTYPE html><html><body><h1>400 Bad request.</h1></body></html>"))
  TRUE
}

# Generic, very basic 200 response.
# other example maybe in response to /favicon.ico for example:
# .http_200(socket, "image/x-icon",.html5ico)
# Or JSON transactions:
# .http_200(socket, "application/json", <content>)
# etc.
.http_200 = function(socket, content_type="text/html; charset=UTF-8",
                    content="<html><body><h1>R Websocket Server</h1></body></html>")
{
  n = ifelse(is.character(content),nchar(content), length(content))
  h="HTTP/1.1 200 OK\r\nServer: R/Websocket\r\n"
  h=paste(h,"Content-Type: ",content_type, "\r\n",sep="")
  h=paste(h,"Date: ",date(),"\r\n",sep="")
  h=paste(h,"Content-Length: ",n,"\r\n\r\n",sep="")
  .SOCK_SEND(socket,charToRaw(h))
  if(is.character(content))
    .SOCK_SEND(socket,charToRaw(content))
  else
    .SOCK_SEND(socket,content)
  .SOCK_CLOSE(socket)
  TRUE
}

.urldecode = function(x)
{
  j = 1
  while(j<nchar(x)) {
    if(substr(x,j,j)=="%") {
      s = substr(x,j,j+2)
      x = sub(s,intToUtf8(sub("\\%","0x",s)),x)
    }
    j = j + 1
  }
  gsub("\\+"," ",x)
}

.websocket_ping = function(connection,frame){
}

.websocket_pong = function(connection,frame){
}
