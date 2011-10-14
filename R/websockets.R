`websocket_write` <- function(DATA, WS)
{
  v <- WS$wsinfo$v
# Give up silently. I supressed the warning since broadcast might
# easily hit a non-websocket client (say, a web page request).
  if(is.null(v)) {
#      warning("Invalid websocket")
      return(invisible())
  }
  mask = FALSE
  if(is.null(WS$server)) mask = TRUE
  if(is.character(DATA)) DATA=charToRaw(DATA)
  if(!is.raw(DATA)) stop("DATA must be character or raw")
  if(v==4){
    j <-.SOCK_SEND(WS$socket,.frame(length(DATA),mask=mask))
    if(j<0) {
      websocket_close(WS)
      return(j)
    }
    if(mask) {
      key = as.raw(floor(runif(4)*256))
      j <- .SOCK_SEND(WS$socket, key)
      return(.SOCK_SEND(WS$socket, .MASK(DATA,key)))
    }
    return(.SOCK_SEND(WS$socket, DATA))
  }
  j <- .SOCK_SEND(WS$socket,raw(1))
  if(j<0) {
    websocket_close(WS)
    return(j)
  }
  .SOCK_SEND(WS$socket,DATA)
  .SOCK_SEND(WS$socket,packBits(intToBits(255))[1])
}

`websocket_broadcast` <- function(DATA, server)
{
  lapply(server$client_sockets, function(x) websocket_write(DATA,x))
}

`set_callback` <- function(id, f, envir) assign(id, f, envir=envir)
`setCallback` <- function(id, f, envir)
{
  assign(id, f, envir=envir)
}

# Example static service function closure:
# Supply any function that takes (socket, header) args
# to handle request...
`static_file_service` <- function(fn)
{
  file_name <- fn
  f <- file(fn)
  file_content <- paste(readLines(f),collapse="\n")
  close(f)
  function(socket, header) {
    finf <- file.info(fn)
    if(difftime(finf[,"mtime"],Sys.time(),units="secs")>0){
      f <- file(fn)
      file_content <<- paste(readLines(f),collapse="\n")
      close(f)
    }
    if(is.null(header$RESOURCE)) return(.http_400(socket))
    if(header$RESOURCE == "/favicon.ico") {
      .http_200(socket,"image/x-icon",.html5ico)
    }
    else {
      .http_200(socket,content=file_content)
    }
  }
}

# A simpler example web-page service that serves up static
# text (and an icon).
`static_text_service` <- function(text)
{
  function(socket, header) {
    if(is.null(header$RESOURCE)) return(.http_400(socket))
    if(header$RESOURCE == "/favicon.ico") {
      .http_200(socket,"image/x-icon",.html5ico)
    }
    else {
      .http_200(socket,content=text)
    }
  }
}

# Will eventually switch naming convention, for now this is a doppelganger.
`create_server` <- function(
      port=7681L,
      webpage=static_file_service(
        paste(system.file(package='websockets'), "basic.html",sep="//")))
{
  createContext(port, webpage)
}

# A server (formerly context) is an environment that stores data associated
# with a single websocket server port, including information on all clients
# connected to that server, and a function that serves up static web pages.
`createContext` <- function(
      port=7681L,
      webpage=static_file_service(
        paste(system.file(package='websockets'), "basic.html",sep="//")),
      server=TRUE)
{
  w <- new.env()
  assign('static', webpage, envir=w)
# server_socket is the file descriptor associated with this server
  if(server)
    assign('server_socket', .SOCK_SERVE(port), envir=w)
  else
    assign('server_socket', -1L, envir=w)
# client_sockets is a list of connected clients, each of which is a
# list with at least the following slots:
# socket  (file descriptor)
# wsinfo  (parsed websocket header data -- another list)
# server  (the environment associated with the server for this client)
  assign('client_sockets', list(), envir=w)
# This is not required, but we supply a default recieve function:
  assign('receive', function(WS, DATA, HEADER=NULL) {
                      cat("Received data from client ",WS$socket,":\n")
                      if(is.raw(DATA)) cat(rawToChar(DATA),"\n")
                    },envir=w)
  assign('closed', function(WS) {
                      cat("Client socket",WS$socket," was closed.\n")
                      if(is.null(WS$wsinfo)){cat("(It was not a websocket client, just a static web page.)\n")}
                   },envir=w)
  assign('established', function(WS) {
                      cat("Client socket",WS$socket," has been established.\n")
                   },envir=w)
  return(w)
}

 
# Cleanly close a websocket client connection or server
`websocket_close` <- function(connection)
{
  if(!is.null(connection$socket)) .remove_client(connection)
  else {
# This is not a client socket, perhaps a server?
    for(j in connection$client_sockets) .remove_client(j)
    if(!is.null(connection$server_socket))
      .SOCK_CLOSE(connection$server_socket)
  }
  invisible()
}

# Naming convention will change in a futer version: 'context' will be
# replaced by 'server.' Both are present in this version for compatibility
# with old package versions.
`service` <- function(context, timeout=1000L, server=context)
{
  socks <- c(server$server_socket,
    unlist(lapply(server$client_sockets,function(x) x$socket)))
  if(length(socks)<1) return(invisible())
  s <- .SOCK_POLL(socks, timeout=timeout)
  for(j in s){
    if(j==server$server_socket){
# New client connection
      .add_client(j,server)
    }
    else{
# j holds just the socket file descriptor, or a negated descriptor
# indicating an error condition. Retrieve the client socket from the server
# environment in J. XXX Improve this with a hashed lookup.
      J <- server$client_sockets[[as.character(j)]]
      if(j<0) {
# Poll reports an error condition for this socket. Close it.
        websocket_close(J)
        next
      }
# A connected client is sending us something!
      if(J$new) {
# This is a new client connection, handshake.
        J$new <- FALSE
        x <- .SOCK_RECV_HTTP_HEAD(j)
        h <- .parse_header(x)
        if(is.null(h)) {
# Shucks, something wrong with this client. Drop him.
          websocket_close(J)
          next
        }
        v <- 0
        if(!is.null(h[["Sec-WebSocket-Version"]]))
          if(as.numeric(h[["Sec-WebSocket-Version"]])>=4) v <- 4
        h$v <- v
# Stash this client's header, identifying websocket protocol version, etc.
# in the appropriate client_socket list
        cs <- server$client_sockets
        J$wsinfo <- h
        cs[[as.character(j)]] <- J
        assign("client_sockets",cs,envir=server)

        if(is.null(h$Upgrade)) {
# Not a handshake request, serve a static web page
          if(is.function(server$static)) server$static(j,h)
          .remove_client(J)
          next
        }
# Negotiate a websocket connection
        if(v<4) .SOCK_SEND(j,.v00_resp_101(h, j))
        else .SOCK_SEND(j,.v04_resp_101(h))
# Trigger callback for newly-established connections
        if(is.function(server$established))
          server$established(WS=J)
        next
      } else if(J$wsinfo$v < 4) {
# Old protocol
        x <- .SOCK_RECV_FRAME00(j,max_buffer_size=getOption("websockets_max_buffer_size"))
      } else {
# Try the latest protocol
        x <- .SOCK_RECV_FRAME(j,max_buffer_size=getOption("websockets_max_buffer_size"))
      }
      if(length(x)<1) {
# Can't have an empty transmission, close the socket.
        websocket_close(J)
        next
      }
# Burn payload if we can't use it.
      if(!is.function(server$receive)) next
      if(J$wsinfo$v < 4) {
        server$receive(WS=J, DATA=.v00_unframe(x), HEADER=NULL)
      }
      else{
        DATA <- .unframe(x)
        if(DATA$header$opcode < 3){
          server$receive(WS=J, DATA=DATA$data, HEADER=DATA$header)
        } else if(DATA$header$opcode == 8) {
          websocket_close(J)
          next
        }
      }
    }
  }
}


# Create a websocket client context
# 1. Build a client header.
# 2. Create a context environment
# 3. Connect to the server and establish
#    websocket or fail.
# Use the same callback functions as with server.
`websocket` = function(url,port=80,subprotocol="chat", version=0)
{
  nonce = as.raw(replicate(16,floor(runif(1)*256)))
  h = paste("GET / HTTP/1.1",sep="")
  h = paste(h, "Upgrade: WebSocket", sep="\r\n")
  h = paste(h, "Connection: Upgrade", sep="\r\n")
  u = gsub("^.*://","",url)
  ur = paste("Host:",u)
  h = paste(h, ur, sep="\r\n")
  p = paste("Sec-WebSocket-Protocol:",subprotocol)
  h = paste(h, p, sep="\r\n")
  if(version==0) {
# This is so dumb.
    spaces1 = round(runif(1)*12)+1
    spaces2 = round(runif(1)*12)+1
    max1 = 4294967295/spaces1
    max2 = 4294967295/spaces2
    number1 = round(runif(1)*max1)+1
    number2 = round(runif(1)*max2)+1
    product1 = number1 * spaces1
    product2 = number2 * spaces2
    key1=strsplit(as.character(product1),"")[[1]]
    key2=strsplit(as.character(product2),"")[[1]]
    nchar1 = round(runif(1)*12)+1
    nchar2 = round(runif(1)*12)+1
    char1 = sample(c(letters,LETTERS),nchar1,replace=TRUE)
    char2 = sample(c(letters,LETTERS),nchar2,replace=TRUE)
    nkey = character(length(key1) + length(char1))
    idx = sample(length(nkey),length(char1))
    nkey[idx] = char1
    nkey[-idx] = key1
    key1 = nkey
    nkey = character(length(key2) + length(char2))
    idx = sample(length(nkey),length(char2))
    nkey[idx] = char2
    nkey[-idx] = key2
    key2 = nkey
    len = length(key1) + spaces1
    idx = sample(2:(len - 1), spaces1)
    nkey = character(len)
    nkey[idx] = " "
    nkey[-idx] = key1
    key1 = paste(nkey,collapse="")
    len = length(key2) + spaces2
    idx = sample(2:(len - 1), spaces2)
    nkey = character(len)
    nkey[idx] = " "
    nkey[-idx] = key2
    key2 = paste(nkey,collapse="")
    key3 = as.raw(floor(runif(8)*256))
    h = paste(h, "Origin: r", sep="\r\n")
    k = paste("Sec-WebSocket-Key1:",key1)
    h = paste(h, k, sep="\r\n")
    k = paste("Sec-WebSocket-Key2:",key2)
    h = paste(h, k, sep="\r\n")
    h = paste(h, "\r\n\r\n", sep="")
    h = charToRaw(h)
    h = c(h, key3)
  } else {
    h = paste(h, "Sec-WebSocket-Origin: r", sep="\r\n")
    ver = paste("Sec-WebSocket-Version:",version)
    h = paste(h, ver, sep="\r\n")
    k = paste("Sec-WebSocket-Key:",base64encode(nonce))
    h = paste(h, k, sep="\r\n")
    h = paste(h,"\r\n\r\n")
  }

  context = createContext(port, webpage=url, server=FALSE)
  s = .SOCK_CONNECT (u, port)
  if(s<1) {
    .SOCK_CLOSE(s)
    stop("Connection error")
  }
  j =.SOCK_SEND(s,h)
  j =.SOCK_POLL(s, timeout=2000L)
  if(length(j)==0 || j!=s){
    .SOCK_CLOSE(s)
    stop("Connection timeout")
  }
  x <- .SOCK_RECV_HTTP_HEAD(s)
  if(length(x)<12) stop("Connection error")
  if(!all(x[1:12] == charToRaw("HTTP/1.1 101")))
    stop(paste("Connection error: ",rawToChar(x),sep="\n"))
# XXX XXX parse for valid connection headers to finish handshake...
# XXX ADD ME (NOW, WE DON'T CHECK ANYTHING)
# XXX XXX
  if(version==0) {
    if(.SOCK_POLL(s)>0)
      x <- .SOCK_RECV(s, buf_size=16L, max_buffer_size=16)
  }
  context$client_sockets[[as.character(s)]] <- 
    list(socket=s, wsinfo=list(v=version), server=context, new=FALSE)
  context
}
