#
# Copyright (c) 2011 by Bryan W. Lewis.
#
# This is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
# USA

`websocket_write` <- function(DATA, WS)
{
  if(is.raw(DATA)) return(.Call('wsockwrite', WS, DATA, PACKAGE='websockets'))
  if(is.character(DATA)) return(.Call('wsockwrite', WS, charToRaw(DATA), PACKAGE='websockets'))
  stop('Only character and raw supported right now...')
}

`websocket_broadcast` <- function(DATA)
{
  if(is.raw(DATA)) return(.Call('wsockbcast', DATA, PACKAGE='websockets'))
  if(is.character(DATA)) return(.Call('wsockbcast', charToRaw(DATA), PACKAGE='websockets'))
  stop('Only character and raw supported right now...')
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
  .SOCK_CLOSE(socket)
  assign('client_sockets',cs, envir=context)
  invisible()
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
      x <- .SOCK_RECV(j)
      h <- .parse_header(x)
      if(is.null(h)) {
# Not a GET request, assume an incoming websocket payload.
      }
      else if(is.null(h$Upgrade) || h$Upgrade != "websocket")
      {
# A static request, not a websocket
        context$static(j,h)
        .remove_client(j, context)
      }
      else {
# Try to establish a new websocket connection
      }
    }
  }
}
