# Minimalist socket functions
POLLIN  = 1L
POLLPRI = 2L
POLLOUT = 4L

SOCK_POLL = function(fds, timeout=1000, events=POLLIN)
{
  .Call('POLL', as.integer(fds), as.integer(timeout), as.integer(events),
         PACKAGE='websockets')
# XXX
}

SOCK_CLOSE = function(socket)
{
  .Call('CLOSE_FD', as.integer(socket), PACKAGE='websockets')
}

SOCK_ACCEPT = function(socket)
{
  .Call('ACCEPT', as.integer(socket), PACKAGE='websockets')
}

SOCK_RECV = function(socket, external_pointer=FALSE)
{
  .Call('RECV', as.integer(socket), as.integer(external_pointer), PACKAGE='websockets')
}

SOCK_SEND = function(socket, msg)
{
  if(is.raw(msg)) return(.Call('SEND', socket, msg, PACKAGE='websockets'))
  if(is.character(msg)) return(.Call('SEND', socket, charToRaw(msg), PACKAGE='websockets'))
  stop("msg must be of data type 'Raw'")
}

SOCK_GETSOCKNAME = function(socket)
{
  .Call('GETSOCKNAME', as.integer(socket), PACKAGE='websockets')
}

SOCK_SERVE = function(port=0L)
{
  .Call('TCPSERVE', as.integer(port), PACKAGE='websockets')
}

SOCK_CONNECT = function(host, port)
{
  .Call('TCPCONNECT', as.character(host), as.integer(port),
        PACKAGE='websockets')
}
