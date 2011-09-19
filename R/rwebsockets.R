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

`.webpage_file_reader` <- function(fn)
{
  file_name <- fn
  f <- file(fn)
  file_content <- paste(readLines(f),collapse="\n")
  close(f)
  function() {
    finf <- file.info(fn)
    if(difftime(finf[,"mtime"],Sys.time(),units="secs")>0){
      f <- file(fn)
      file_content <<- paste(readLines(f),collapse="\n")
      close(f)
    }
    file_content
  }
}

`createContext` <- function(
      port=7681L,
      webpage=.webpage_file_reader(
                paste(system.file(package='websockets'),
                        "basic.html",sep="//")))
{
  w <- new.env()
  assign('webpage', webpage, envir=w)
  assign('server_socket', .SOCK_SERVE(port), envir=w)
  assign('client_sockets', c(), envir=w)
  return(w)
}

`.add_client` <- function(socket, context)
{
  cs = .SOCK_ACCEPT(socket)
  assign('client_sockets',c(context$client_sockets,cs), envir=context)
  invisible()
}

`.notawebsocket` <- function(socket, header, context)
{
# XXX 
  .http_200(socket)
}

`service` <- function(context)
{
  socks <- c(w$server_socket, w$client_sockets)
  if(length(socks)<1) return(invisible())
  s <- .SOCK_POLL(socks)
  for(j in s){
    if(j==w$server_socket){
      .add_client(j,context)
    }
    else{
      x <- .SOCK_RECV(j)
      h <- .parse_header(x)
      if(is.null(h$Upgrade) || h$Upgrade != "websocket")
      {
        .notawebsocket(j,h,context)
      }
    }
  }
}
