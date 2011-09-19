`websocket_write` <- function(DATA, WS, CONTEXT=NULL)
{
  if(is.null(CONTEXT)) {
# Try to infer context for backwards compat. with older package versions.
# Need to explicitly specify CONTEXT outside of callback situations.
    frms = sys.frames()
    if(length(frms)>2){
      n = length(frms) - 2
      l = ls(frms[[n]])
      if(any(l=="context")) CONTEXT=frms[[n]]$context
    }
  }
  v=4
  if(is.null(CONTEXT)) warning("CONTEXT not indicated, assuming latest websocket version")
  else {
    if(exists("client_wsinfo",envir=CONTEXT) &&
       any(names(CONTEXT$client_wsinfo)==as.character(WS))) {
       v=CONTEXT$client_wsinfo[[as.character(WS)]]$v
       if(is.null(v)) v=4
    }
  }
  if(is.character(DATA)) DATA=charToRaw(DATA)
  if(!is.raw(DATA)) stop("DATA must be character or raw")
  if(v==4){
    .SOCK_SEND(WS,.frame(length(DATA)))
    .SOCK_SEND(WS, DATA)
    return(invisible())
  }
  .SOCK_SEND(WS,raw(1))
  .SOCK_SEND(WS,DATA)
  .SOCK_SEND(WS,packBits(intToBits(255))[1])
}

`websocket_broadcast` <- function(DATA)
{
  stop("XXX TODO")
}

`setCallback` <- function(id, f, envir)
{
  assign(id, f, envir=envir)
}

# Example static service function
# Supply any function that takes (socket, header) args
# to handle request...
`static_page_service` <- function(fn)
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

# Will eventually switch naming convention, for now this is a doppelganger.
`create_context` <- function(
      port=7681L,
      webpage=static_page_service(
        paste(system.file(package='websockets'), "basic.html",sep="//")))
{
  createContext(port, webpage)
}

# A context is an environment that stores data associated with a single
# websocket server port, including information on all clients connected
# to that server, and a function that serves up static web pages.
`createContext` <- function(
      port=7681L,
      webpage=static_page_service(
        paste(system.file(package='websockets'), "basic.html",sep="//")))
{
  w <- new.env()
  assign('static', webpage, envir=w)
  assign('server_socket', .SOCK_SERVE(port), envir=w)
  assign('client_sockets', c(), envir=w)
  assign('client_wsinfo', c(), envir=w)
  assign('receive', function(WS, DATA, COOKIE=NULL) {cat("Received data from client ",WS,":\n");cat(rawToChar(DATA),"\n");websocket_write("HELLO",WS)},envir=w)
  return(w)
}

`.add_client` <- function(socket, context)
{
  cs <- .SOCK_ACCEPT(socket)
  assign('client_sockets',c(context$client_sockets,cs), envir=context)
  invisible()
}

`.remove_client` <- function(socket, context)
{
  cs <- context$client_sockets
  cs <- cs[cs!=socket]
  wsinfo <- context$client_wsinfo
  wsinfo <- wsinfo[names(wsinfo)!=as.character(socket)]
  .SOCK_CLOSE(socket)
  assign('client_sockets',cs, envir=context)
  assign('client_wsinfo',wsinfo, envir=context)
  if(length(wsinfo)>0 && exists("closed"))
    context$closed(WS, DATA=NULL, COOKIE=NULL)
  invisible()
}
 
# Cleanly close a websocket connection
`websocket_close` <- function(socket, context)
{
cat("websocket_close ",socket,"\n")
# XXX unclean! add close protocol
  .remove_client(socket, context)
}

`service` <- function(context, timeout=1000L)
{
  socks <- c(context$server_socket, context$client_sockets)
  if(length(socks)<1) return(invisible())
  s <- .SOCK_POLL(socks, timeout=timeout)
  for(j in s){
    if(j==context$server_socket){
      .add_client(j,context)
    }
    else{
# Presently, program copies into a raw vector. Will also
# soon support in place recv via external pointers.
      x <- .SOCK_RECV(j)
      if(length(x)<1) {
        websocket_close(j, context)
        next
      }
      h <- .parse_header(x)
      if(is.null(h)) {
# Not a GET request, assume an incoming websocket payload.
        if(!is.function(context$receive)){
# Burn payload, nothing to do with it...
          next
        }
        if(context$client_wsinfo[[as.character(j)]]["v"]<4) {
          context$receive(WS=j, DATA=.v00_unframe(x), COOKIE=NULL)
        }
        else{
          context$receive(WS=j, DATA=.unframe(x), COOKIE=NULL)
        }
      }
      else if(is.null(h$Upgrade))
      {
# A static request, not a websocket
        context$static(j,h)
        .remove_client(j, context)
      }
      else {
# Try to establish a new websocket connection
# 1. Stash this client's header, identifying websocket protocol version, etc.
        context$client_wsinfo[[as.character(j)]] = h
# 2. Respond depending on version:
        v = 0
        if(!is.null(h[["Sec-WebSocket-Version"]]))
          if(as.numeric(h[["Sec-WebSocket-Version"]])>=4) v=4
        context$client_wsinfo[[as.character(j)]]["v"] = v
        if(v<4) .SOCK_SEND(j,.v00_resp_101(h))
        else .SOCK_SEND(j,.v04_resp_101(h))
        if(is.function(context$established))
          context$established(WS=j,DATA=NULL,COOKIE=NULL)
# COOKIE will go away in next version...it is useless in this version
# just there for compatability with older versions...
      }
    }
  }
}
