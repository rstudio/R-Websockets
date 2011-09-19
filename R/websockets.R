# Notes:
# No extensions

parse_header = function(msg)
{
  cli_header = list()
  if(is.raw(msg)) {
    cli_header$raw = msg
    msg = rawToChar(msg)
  }
  x = gsub("\r","",msg)
  x = strsplit(x,"\n")[[1]]
  l = grep("^GET",x,ignore.case=TRUE)
  cli_header$GET = ifelse(length(l)>0,x[l[1]],"")
  cli_header$PROT = tail(strsplit(cli_header$GET," ")[[1]],1)
  if(length(l)>0) x = x[-l]
  for(j in 1:length(x)) {
    n = gregexpr(":",text=x[j])[[1]]
    if(n[1]>1) {
      key = substr(x[j],1,n-1)
      value = substr(x[j],n+1,nchar(x[j]))
      value = gsub("^ *", "", value)
      cli_header[key] = value
    }
  }
  cli_header
}

# Version 00 to 03 handshake
v00_resp_101 = function(cli_header)
{
  prot = cli_header["Sec-WebSocket-Protocol"]
  origin = cli_header["Origin"]
  location = paste("ws://",cli_header["Host"],"/",sep="")
  key1 = cli_header["Sec-WebSocket-Key1"][[1]]
  key2 = cli_header["Sec-WebSocket-Key2"][[1]]
  num1 = as.numeric(rawToChar(charToRaw(key1)[gregexpr("[0-9]",text=key1)[[1]]]))
  num2 = as.numeric(rawToChar(charToRaw(key2)[gregexpr("[0-9]",text=key2)[[1]]]))
  s1 = length(charToRaw(key1)[gregexpr(" ",text=key1)[[1]]])
  s2 = length(charToRaw(key2)[gregexpr(" ",text=key2)[[1]]])
  v1 = as.integer(num1/s1)
  v2 = as.integer(num2/s2)
  n = length(cli_header$raw)
  key3 = cli_header$raw[(n-7):n]
  r1 = packBits(intToBits(v1))[4:1]
  r2 = packBits(intToBits(v2))[4:1]
  val = c(r1,r2,key3)
  hash = digest(val,algo="md5",serialize=FALSE,raw=TRUE)
  resp = "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n"
  resp = paste(resp,"Sec-WebSocket-Origin: ",origin,"\r\n",sep="")
  resp = paste(resp,"Sec-WebSocket-Location: ",location,"\r\n",sep="")
  resp = paste(resp,"Sec-WebSocket-Protocol: ",prot,"\r\n\r\n",sep="")
  c(charToRaw(resp),hash,charToRaw("\r\n\r\n"))
}

# Version 04 to at least 15 handshake
v04_resp_101 = function(cli_header)
{
  GUID = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  key = paste(cli_header["Sec-WebSocket-Key"],GUID,sep="") 
  skey = base64encode(digest(charToRaw(key),algo='sha1',serialize=FALSE,raw=TRUE))
  prot = cli_header["Sec-WebSocket-Protocol"]
  resp = "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n"
  resp = paste(resp,"Sec-WebSocket-Protocol: ",prot,"\r\n",sep="")
  paste(resp,"Sec-WebSocket-Accept: ",skey,"\r\n\r\n",sep="")
}

# Low-level frame header generator, extensions not supported
# Version 01 to at least 15 data framing
# The bit ordering is a bit hard to follow.
frame = function(len, FIN=TRUE, opcode=1L, mask=FALSE)
{
  if(is.character(opcode)) opcode = strtoi(opcode, 16L)
  head = rawToBits(raw(1))    # First byte of header
  if(FIN) head[1] = as.raw(1)
  head[5:8] = rawToBits(as.raw(opcode))[4:1]
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
v00_unframe = function(data)
{
  ff = packBits(intToBits(255)[1:8])
  if(data[1]==ff) return(FALSE)
  if(data[length(data)] != ff) warn("End of message missing")
  return(data[2:(length(data)-1)])
}


# Parse a frame header, data must be a raw vector for now, but external
# pointer will be supported soon (external pointer messages can be unmasked
# in place).
#
# The bit ordering is a bit hard to follow, sorry.
unframe = function(data)
{
  frame=list()
  frame$FIN = 0L
  frame$mask = FALSE
  frame$offset = 3L  # default 2-byte header
  if(is.raw(data)) {
    head = rawToBits(data[1])
    if(head[1]) frame$FIN = 1L
    x = rawToBits(raw(1))
    x[1:4] = head[8:5]
    frame$opcode = as.integer(packBits(x))
    head2 = rawToBits(data[2])
    if(head2[8]) frame$mask = TRUE
    x = rawToBits(raw(1))
    x[1:7] = head2[1:7]
    frame$len = as.integer(packBits(x))
    if(frame$len == 126) {
# 2-byte data length
      x = raw(4)
      x[1:2] = data[4:3]
      frame$len = packBits(rawToBits(x),type="integer")
      frame$offset = frame$offset + 2L
    }
    else if(frame$len == 127) {
# 8-byte data length
      x = raw(4)
      x = data[10:7]
      frame$len = packBits(rawToBits(x),type="integer")
      frame$offset = frame$offset + 8L
      x = raw(4)
      x = data[6:3]
      x = packBits(rawToBits(x),type="integer")
      if(x!=0) warning("Message length exceeds limit.")
    }
    if(frame$mask) {
      frame$key = data[frame$offset:(frame$offset+3)]
      frame$offset = frame$offset + 4L
    }
  }
  else{
    stop("Only raw message types presently supported.")
  }
  frame
}


xxx = function(s)
{
  cs = SOCK_ACCEPT(s)
print(cs)
  x = rawToChar(SOCK_RECV(cs))
  h = parse_header(x)
cat(resp_101(h))
  SOCK_SEND(cs,resp_101(h))
  msg = charToRaw("HELLO")
  SOCK_SEND(cs,c(frame(length(msg)),msg)) 
#  SOCK_CLOSE(cs)
}
