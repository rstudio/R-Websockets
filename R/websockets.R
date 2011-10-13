`websocket_write` <- function(DATA, WS)
{
  v <- WS$wsinfo$v
# Give up silently. I supressed the warning since broadcast might
# easily hit a non-websocket client (say, a web page request).
  if(is.null(v)) {
#      warning("Invalid websocket")
      return(invisible())
  }
  if(is.character(DATA)) DATA=charToRaw(DATA)
  if(!is.raw(DATA)) stop("DATA must be character or raw")
  if(v==4){
    j <-.SOCK_SEND(WS$socket,.frame(length(DATA)))
    if(j<0) {
      websocket_close(WS)
      return(j)
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
    if(!is.null(connection$server_socket)) {
      for(j in connection$client_sockets) .remove_client(j)
      .SOCK_CLOSE(connection$server_socket)
    }
  }
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
        if(v<4) .SOCK_SEND(j,.v00_resp_101(h))
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
`websocket` = function(url,port,subprotocol="chat")
{
  nonce = as.raw(replicate(16,floor(runif(1)*256)))
  h = paste("GET / HTTP/1.1",sep="")
  h = paste(h, "Upgrade: websocket", sep="\r\n")
  h = paste(h, "Connection: Upgrade", sep="\r\n")
  u = gsub("^.*://","",url)
  ur = paste("Host:",u)
  h = paste(h, ur, sep="\r\n")
  h = paste(h, "Sec-WebSocket-Origin: R", sep="\r\n")
  p = paste("Sec-WebSocket-Protocol:",subprotocol)
  h = paste(h, p, sep="\r\n")
  k = paste("Sec-WebSocket-Key:",base64encode(nonce))
  h = paste(h, k, sep="\r\n")
  h = paste(h, "Sec-WebSocket-Version: 8",sep="\r\n")
  h = paste(h,"\r\n\r\n")

  context = createContext(port, webpage=url, server=FALSE)
  s = .SOCK_CONNECT (u, port)
  if(s<1) {
    .SOCK_CLOSE(s)
    stop("Connection error")
  }
  j =.SOCK_SEND(s,h)
  j =.SOCK_POLL(s, timeout=5000L)
  if(length(j)==0 || j!=s){
    .SOCK_CLOSE(s)
    stop("Connection timeout")
  }
  x <- .SOCK_RECV_HTTP_HEAD(s)
# XXX XXX parse for valid connection headers to finish handshake...
   context$client_sockets[[as.character(s)]] <- 
    list(socket=s, wsinfo=list(v=8), server=NULL, new=FALSE)
  context
}
