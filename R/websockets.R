`websocket_write` <- function(DATA, WS)
{
  v = WS$wsinfo$v
  if(is.null(v)) {
# Perhaps WS is a nested list, try to unlist?
    WS = WS[[1]]
    v = WS$wsinfo$v
# Give up.
    if(is.null(v)) {
      warning("Invalid websocket")
      return(invisible())
    }
  }
  if(is.character(DATA)) DATA=charToRaw(DATA)
  if(!is.raw(DATA)) stop("DATA must be character or raw")
  if(v==4){
    .SOCK_SEND(WS$socket,.frame(length(DATA)))
    return(.SOCK_SEND(WS$socket, DATA))
  }
  .SOCK_SEND(WS$socket,raw(1))
  .SOCK_SEND(WS$socket,DATA)
  .SOCK_SEND(WS$socket,packBits(intToBits(255))[1])
}

`websocket_broadcast` <- function(DATA, server)
{
  lapply(j$client_sockets, function(x) websocket_write(DATA,x))
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
        paste(system.file(package='websockets'), "basic.html",sep="//")))
{
  w <- new.env()
  assign('static', webpage, envir=w)
# server_socket is the file descriptor associated with this server
  assign('server_socket', .SOCK_SERVE(port), envir=w)
# client_sockets is a list of connected clients, each of which is a
# list with at least the following slots:
# socket  (file descriptor)
# wsinfo  (parsed websocket header data -- another list)
# server  (the environment associated with the server for this client)
  assign('client_sockets', list(), envir=w)
# This is not required, but we supply a default recieve function:
  assign('receive', function(WS, DATA, COOKIE=NULL) {
                      cat("Received data from client ",WS$socket,":\n")
                      if(is.raw(DATA)) cat(rawToChar(DATA),"\n")
                    },envir=w)
  assign('closed', function(WS, DATA, COOKIE=NULL) {
                      cat("Client socket",WS$socket," was closed.\n")
                      if(is.null(WS$wsinfo)){cat("(It was not a websocket client, just a static web page.)\n")}
                   },envir=w)
  assign('established', function(WS, DATA, COOKIE=NULL) {
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
# A connected client is sending something.
# Note: Presently, program copies into a raw vector. Will also
# soon support in place recv via external pointers.
      x <- .SOCK_RECV(j,max_buffer_size=getOption("websockets_max_buffer_size"))
# j holds just the socket file descriptor. Retrieve the client socket
# from the server environment in J. XXX Improve this with a hashed lookup.
      J <- server$client_sockets[
           unlist(lapply(server$client_sockets,function(x) x$socket)) == j][[1]]
      if(length(x)<1) {
        websocket_close(J)
        next
      }
      h <- .parse_header(x)
      if(is.null(h)) {
# Not a GET request, assume an incoming websocket payload.
        if(!is.function(server$receive)){
# Burn payload, nothing to do with it...
          next
        }
        v <- J$wsinfo$v
        if(v<4) {
          server$receive(WS=J, DATA=.v00_unframe(x), COOKIE=NULL)
        }
        else{
          DATA <- .unframe(x)
          if(DATA$header$opcode == 1){
            server$receive(WS=J, DATA=DATA$data, COOKIE=NULL)
          } else if(DATA$header$opcode == 8) {
            websocket_close(J)
            next
          }
        }
      }
      else if(is.null(h$Upgrade))
      {
# A static request, not a websocket
        if(is.function(server$static)) server$static(j,h)
        .remove_client(J)
      }
      else {
# Try to establish a new websocket connection
        v <- 0
        if(!is.null(h[["Sec-WebSocket-Version"]]))
          if(as.numeric(h[["Sec-WebSocket-Version"]])>=4) v <- 4
        h$v <- v
# Stash this client's header, identifying websocket protocol version, etc.
# in the appropriate client_socket list
        cs <- server$client_sockets
        cs[unlist(lapply(cs,function(x) x$socket)) == j][[1]]$wsinfo <- h
        assign("client_sockets",cs,envir=server)
        if(v<4) .SOCK_SEND(j,.v00_resp_101(h))
        else .SOCK_SEND(j,.v04_resp_101(h))
# Trigger callback for newly-established connections
        if(is.function(server$established))
          server$established(WS=J,DATA=NULL,COOKIE=NULL)
# COOKIE will go away in next version...it is useless in this version
# just there for compatibility with older versions...
      }
    }
  }
}
