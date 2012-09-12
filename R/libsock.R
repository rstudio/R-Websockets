# Minimalist socket functions
.SOCK_POLLIN  = 1L
.SOCK_POLLPRI = 2L
.SOCK_POLLOUT = 4L

# Return vector of sockets matching requested events. Negative
# socket numbers in returned vector indicates sockets with error
# conditions.
.SOCK_POLL = function(fds, timeout=1000L, events=.SOCK_POLLIN)
{
  x = .Call('SOCK_POLL', as.integer(fds), as.integer(timeout), as.integer(events), PACKAGE='websockets')
  c(fds[x == events], -fds[x>4])
}

.SOCK_CLOSE = function(socket)
{
  .Call('SOCK_CLOSE', as.integer(socket), PACKAGE='websockets')
}

.SOCK_ACCEPT = function(socket)
{
  .Call('SOCK_ACCEPT', as.integer(socket), PACKAGE='websockets')
}

.SOCK_RECV = function(socket, external_pointer=FALSE, buf_size=8192, max_buffer_size=2^24)
{
  .Call('SOCK_RECV', as.integer(socket), as.integer(external_pointer), as.integer(buf_size), as.numeric(max_buffer_size), PACKAGE='websockets')
}

.SOCK_RECV_FRAME = function(socket, max_buffer_size=getOption("websockets_max_buffer_size"))
{
  .Call('SOCK_RECV_FRAME', as.integer(socket), as.numeric(max_buffer_size), PACKAGE='websockets')
}

.SOCK_RECV_FRAME00 = function(socket, max_buffer_size=getOption("websockets_max_buffer_size"))
{
  .Call('SOCK_RECV_FRAME00', as.integer(socket), as.numeric(max_buffer_size), PACKAGE='websockets')
}

.SOCK_RECV_HTTP_HEAD = function(socket)
{
  .Call('SOCK_RECV_HTTP_HEAD', as.integer(socket), PACKAGE='websockets')
}

.SOCK_RECV_N = function(socket, N)
{
  .Call('SOCK_RECV_HTTP_HEAD', as.integer(socket), as.integer(N), PACKAGE='websockets')
}

# We trap the possibility of a SIGPIPE signal error during SOCK_SEND.
.SOCK_SEND = function(socket, msg)
{
  if(is.raw(msg)) return(
    tryCatch(.Call('SOCK_SEND', socket, msg, PACKAGE='websockets'),
      error=function(e) -1, interrupt=function(e) -1))
  if(is.character(msg))
    return(
      tryCatch(
        .Call('SOCK_SEND', socket, charToRaw(msg), PACKAGE='websockets'),
        error=function(e) -1, interrupt=function(e) -1))
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
