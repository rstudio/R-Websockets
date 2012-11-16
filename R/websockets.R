daemonize = function(server)
{
  if(.Platform$OS.type != "unix") stop("Sorry, daemonize requires a unix-type operating system.\nUse service in an explicit event loop instead.")
  if(!exists("server_list",envir=.websockets_env))
    assign("server_list",c(),envir=.websockets_env)
  server$handler = .register_event_handler(server$server_socket)
  .websockets_env$server_list = c(.websockets_env$server_list,server)
}

# Write data to a websocket
websocket_write = function(DATA, WS)
{
  mask = FALSE
  if(is.null(WS$server)) {
# Then this is a probably client websocket connection.
    mask = TRUE
    WS = WS$client_sockets[[1]]
  }
  v = WS$wsinfo$v
# Give up silently. I supressed the warning since broadcast might
# easily hit a non-websocket client (say, a web page request).
  if(is.null(v)) {
#      warning("Invalid websocket")
      return(invisible())
  }
  if(is.character(DATA)) DATA=charToRaw(DATA)
  if(!is.raw(DATA)) stop("DATA must be character or raw")
  if(v==4){
    #j =.SOCK_SEND(WS$socket,.frame(length(DATA),mask=mask,
    #                                opcode=(if (WS$server$is.binary) { 2L } else { 1L })))
     frame = .frame(length(DATA),mask=mask, opcode=(if (WS$server$is.binary) { 2L } else { 1L }))
    if(mask) {
      key = as.raw(floor(runif(4)*256))
      #j = .SOCK_SEND(WS$socket, key)
      mask = .MASK(DATA,key)
    } else {
       key = raw(0)
       mask = raw(0)
    }
    j = .SOCK_SEND(WS$socket, c(frame,key,mask,DATA))

    if(j<0) websocket_close(WS)
    return(j)
  }
  if (WS$server$is.binary)
    j = .SOCK_SEND(WS$socket,raw(2))
  else
    j = .SOCK_SEND(WS$socket,raw(1))
  if(j<0) {
    websocket_close(WS)
    return(j)
  }
  .SOCK_SEND(WS$socket,DATA)
  .SOCK_SEND(WS$socket,packBits(intToBits(255))[1])
}

# "Broadcast" data to all the client websockets attached to the server.
websocket_broadcast = function(DATA, server)
{
  lapply(server$client_sockets, function(x) websocket_write(DATA,x))
}

setCallback = function(id, f, envir) set_callback(id,f,envir)
set_callback = function(id, f, envir)
{
  assign(id, f, envir=envir)
}

# Example static service function closure:
# Supply any function that takes (socket, header) args
# to handle request...
static_file_service = function(fn)
{
  file_name = fn
  f = file(fn)
  file_content = paste(readLines(f),collapse="\n")
  close(f)
  function(socket, header) {
    finf = file.info(fn)
    if(difftime(finf[,"mtime"],Sys.time(),units="secs")>0){
      f = file(fn)
      file_content <<- paste(readLines(f),collapse="\n")
      close(f)
    }
    if(is.null(header$RESOURCE))
      return(.http_400(socket))
    .http_200(socket,content=file_content)
    return(TRUE)
  }
}

# A simpler example web-page service that serves up static
# text (and an icon).
static_text_service = function(text)
{
  function(socket, header) {
    if(is.null(header$RESOURCE))
      return(.http_400(socket))
    .http_200(socket,content=text)
    return(TRUE)
  }
}

# Will eventually switch naming convention, for now this is a doppelganger.
create_server = function(
      port=7681L,
      webpage=static_file_service(
        paste(system.file(package='websockets'), "basic.html",sep="//")),
      is.binary=FALSE)
{
  w = createContext(port, webpage, is.binary=is.binary)

  # createContext can fail on binding to a socket
  if (is.null(w))
     stop("Cannot create a server (via createContext())")

  set_callback ("static", webpage, w)
  w
}

# A server (formerly context) is an environment that stores data associated
# with a single websocket server port, including information on all clients
# connected to that server, and a function that returns static web pages.
`createContext` = function(
      port=7681L,
      webpage=static_file_service(
        paste(system.file(package='websockets'), "basic.html",sep="//")),
      server=TRUE,
      is.binary=FALSE)
{
  w = new.env()

  assign('port',port,envir=w)

  assign('static', webpage, envir=w)
# server_socket is the file descriptor associated with this server
  if(server){
    socket = .SOCK_SERVE(port)
    if (socket != -1L)
       assign('server_socket', socket, envir=w)
    else
       stop('Unable to bind socket on port ' , port, '; is it already in use?')
  } else {
    assign('server_socket', -1L, envir=w)
  }
# client_sockets is a list of connected clients, each of which is a
# list with at least the following slots:
# socket  (file descriptor)
# wsinfo  (parsed websocket header data -- another list)
# server  (the environment associated with the server for this client)
  assign('client_sockets', list(), envir=w)
# This is not required, but we supply a default websocket recieve function:
  assign('receive', function(WS, DATA, HEADER=NULL) {
                      cat("Received data from client socket ",WS$socket,":\n")
                      if(is.raw(DATA)) cat(rawToChar(DATA),"\n")
                    },envir=w)
# A default websocket close function:
  assign('closed', function(WS) {
                      cat("Client socket",WS$socket," was closed.\n")
                      if(is.null(WS$wsinfo)){cat("(It was not a websocket client, just a static web page.)\n")}
                   },envir=w)
# A default websocket established function:
  assign('established', function(WS) {
                      cat("Client socket",WS$socket," has been established.\n")
                   },envir=w)
  assign('is.binary', is.binary, envir=w)

  w$accumulate_fragment = function(WS,frame){
    if (is.null(WS$frames))
      WS$frames = list()
    WS$frames[[length(WS$frames)+1]] = frame
  }

  w$coalesce_fragments = function(WS,frame){
    if (is.null(WS$frames)) return(frame)

    WS$frames[[length(WS$frames)+1]] = frame
    
    # First frame contains the opcode, so we will coalesce all data parts into
    # that one.
    frame = WS$frames[[1]]
    frame$data = unlist(lapply(WS$frames,function(f) f$data))

    WS$frames = NULL

    frame
  }

  reg.finalizer(w, function(e) {websocket_close(e)})
  return(w)
}

 
# Cleanly close any websocket connection (client or server)
`websocket_close` = function(connection)
{
  if(!is.null(connection$socket)) .remove_client(connection)
  else {
# This is not a client socket, perhaps a server?
    for(j in connection$client_sockets) .remove_client(j)
    if(!is.null(connection$handler))
    {
      .deregister_event_handler(connection$handler)
      connection$handler = NULL
      .undaemon(connection)
    }
    if(!is.null(connection$server_socket)){
      .SOCK_CLOSE(connection$server_socket)
      connection$server_socket = NULL
    }
  }
  invisible()
}


# Naming convention may change in a futer version: 'context' will be
# replaced by 'server.' Both are present in this version for compatibility
# with old package versions.
`service` = function(context, timeout=1000L, server=context)
{
  socks = c(server$server_socket,
    unlist(lapply(server$client_sockets,function(x) x$socket)))

  if (length(socks)<1) return(invisible())

  s = .SOCK_POLL(socks, timeout=timeout)

  # Loop handles three case:
  # 1. New client connections
  # 2. Client Web or Protocol Upgrade request
  # 3. Client Websocket Frame to read and respond to
  for (j in s){

    # 1. New client connection on listening socket
    if (j==server$server_socket){
      .add_client(j,server)
      next
    }

    # Something to read from connected client

    # j holds just the socket file descriptor, or a negated descriptor
    # indicating an error condition. Retrieve the client socket from the
    # server environment in J.
    J = server$client_sockets[[as.character(abs(j))]]

    # Poll reports an error condition for this socket. Close it.
    if (j<0) {
      websocket_close(J)
      next
    }

    # 2. Client Web or Protocol Upgrade request
    if (J$new) {

      J$new = FALSE
      J$wsinfo = .parse_header(.SOCK_RECV_HTTP_HEAD(j))

      # Something wrong with client: close connection.
      if (is.null(J$wsinfo)) {
        websocket_close(J)
        next
      }

      if (!is.null(J$wsinfo[["Sec-WebSocket-Version"]]) &&
        (as.numeric(J$wsinfo[["Sec-WebSocket-Version"]])>=4) )
        J$wsinfo$v = 4
      else
        J$wsinfo$v = 0

      # Stash this client's header, identifying websocket protocol version, 
      # etc. in the appropriate client_socket list
      server$client_sockets[[as.character(j)]] = J

      # Web request
      if (is.null(J$wsinfo$Upgrade)) {

        # Not a handshake request, serve a static web page
        if (is.function(server$static)) {

          # NOTE: CONNECTION REMAINS OPEN IF STATIC WEB SERVICE DOES NOT 
          # RETURN TRUE. We allow this case to pass connection on to
          # ancillary service, for example. 
          if(server$static(j,J$wsinfo)) .remove_client(J)

        } else {
           .remove_client(J)
        }
        next
      }

      # Protocol Upgrade: negotiate a websocket connection
      if (J$wsinfo$v<4)
        .SOCK_SEND(j,.v00_resp_101(J$wsinfo, j))
      else 
        .SOCK_SEND(j,.v04_resp_101(J$wsinfo))

      # Trigger callback for newly-established connections
      if (is.function(server$established))
        server$established(WS=J)

      next
    } 
    
    # 3. Client Websocket Frame to read and respond to

    # 3.1 Old Protocol

    if (J$wsinfo$v < 4) {
      frame = .SOCK_RECV_FRAME00(j) # Old protocol

      if (length(frame)<1) {
      # Can't have an empty transmission, close the socket.
        websocket_close(J)
        next
      }

      # Drop payload if we can't use it.
      if (!is.function(server$receive)) next

      server$receive(WS=J, DATA=.v00_unframe(frame), HEADER=NULL)

      next
    }

	  # 3.2 New Protocol

    frame = .SOCK_RECV_FRAME(j) # Try the latest protocol

    if (is.null(frame)){
      websocket_close(J)
    } else if (frame$header$fin == 0){
      server$store_fragment(WS=J,frame)
    } else if (frame$header$fin == 1){
      frame = server$coalesce_fragments(WS=J,frame)
      if (frame$header$opcode %in% c(0,1,2) && is.function(server$receive)){
        server$receive(WS=J, DATA=frame$data, HEADER=frame$header)
      } else if (frame$header$opcode == 8) {
        websocket_close(J)
      } else if (frame$header$opcode == 9) {
        .websocket_pong(J,frame)
      } else if (frame$header$opcode == 10) {
        .websocket_ping(J,frame)
      } else {
        # ignore frame
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
  x = .SOCK_RECV_HTTP_HEAD(s)
  if(length(x)<12) stop("Connection error")
  if(!all(x[1:12] == charToRaw("HTTP/1.1 101")))
    stop(paste("Connection error: ",rawToChar(x),sep="\n"))
# XXX XXX parse for valid connection headers to finish handshake...
# XXX ADD ME (NOW, WE DON'T CHECK ANYTHING)
# XXX XXX
  if(version==0) {
    if(.SOCK_POLL(s)>0)
      x = .SOCK_RECV(s, buf_size=16L, max_buffer_size=16)
  }
  context$client_sockets[[as.character(s)]] = 
    list(socket=s, wsinfo=list(v=version), server=context, new=FALSE)
  context
}

# Parse http get/post variables, returning a list.
# We don't really handle the POST case. Use a better web server
# for that!
http_vars = function(socket, header)
{
  res = strsplit(header$RESOURCE,split="\\?")[[1]]
  if(header$TYPE=="POST") {
    N = header[["Content-Length"]]
    if(!is.null(N)) return(rawToChar(.SOCK_RECV_N(socket, N)))
    else stop("Error in POST handling")
  }
  else GET = res[2]
  if(!is.na(GET) && nchar(GET)>1) {
    GET = lapply(strsplit(GET,"&")[[1]],function(x) strsplit(x,"=")[[1]])
    gnams = lapply(GET,function(x) x[[1]])
    GET = lapply(GET,function(x) if(length(x)>1){.urldecode(x[[2]])} else{c()})
    names(GET) = gnams
  } else GET = c()
  GET
}

# A basic and generic http response function
http_response = function(socket, status=200,
                         content_type="text/html; charset=UTF-8", content="")
{
  n = ifelse(is.character(content),nchar(content), length(content))
  h=paste("HTTP/1.1",status,"OK\r\nServer: R/Websocket\r\n")
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
