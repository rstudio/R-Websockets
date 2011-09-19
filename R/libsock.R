# Minimalist socket functions
.SOCK_POLLIN  = 1L
.SOCK_POLLPRI = 2L
.SOCK_POLLOUT = 4L

.SOCK_POLL = function(fds, timeout=1000, events=.SOCK_POLLIN)
{
  x = .Call('SOCK_POLL', as.integer(fds), as.integer(timeout), as.integer(events),
         PACKAGE='websockets')
  fds[x == events]
}

.SOCK_CLOSE = function(socket)
{
  .Call('SOCK_CLOSE', as.integer(socket), PACKAGE='websockets')
}

.SOCK_ACCEPT = function(socket)
{
  .Call('SOCK_ACCEPT', as.integer(socket), PACKAGE='websockets')
}

.SOCK_RECV = function(socket, external_pointer=FALSE)
{
  .Call('SOCK_RECV', as.integer(socket), as.integer(external_pointer), PACKAGE='websockets')
}

.SOCK_SEND = function(socket, msg)
{
  if(is.raw(msg)) return(.Call('SOCK_SEND', socket, msg, PACKAGE='websockets'))
  if(is.character(msg))
    return(.Call('SOCK_SEND', socket, charToRaw(msg), PACKAGE='websockets'))
  stop("msg must be of data type 'Raw'")
}

.SOCK_GETSOCKNAME = function(socket)
{
  .Call('SOCK_NAME', as.integer(socket), PACKAGE='websockets')
}

.SOCK_SERVE = function(port=0L)
{
  .Call('SOCK_SERVE', as.integer(port), PACKAGE='websockets')
}

.SOCK_CONNECT = function(host, port)
{
  .Call('SOCK_CONNECT', as.character(host), as.integer(port),
        PACKAGE='websockets')
}

.MASK = function(data, key)
{
  if(is.character(key)) key = charToRaw(key)
  if(!is.raw(data) && !is.raw(key))
    stop("data must be raw, key must be character or raw")
  .Call('MASK',data,key)
}
