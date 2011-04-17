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

`.onLoad` <- function(libname, pkgname)
{
  library.dynam('websockets', pkgname, libname)
}

`.onUnload` <- function(libpath)
{
  library.dynam.unload('websockets', libpath)
}

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

`createContext` <- function(
      port=7681L,
      webpage=paste(system.file(package='websockets'),"basic.html",sep="//"))
{
  w <- new.env()
  if(!is.null(webpage)) assign('webpage', webpage, envir=w)
  assign('context', .Call('createContext', w, as.integer(port), PACKAGE='websockets'), envir=w)
  return(w)
}

`service` <- function(contextEnv)
{
  .Call('service', contextEnv$context, PACKAGE='websockets')
}

`setCookie` <- function(cookie, DATA)
{
  if(is.raw(DATA)) return(.Call('setcookie', cookie, DATA, PACKAGE='websockets'))
  if(is.character(DATA)) return(.Call('setcookie', cookie, charToRaw(DATA), PACKAGE='websockets'))
  stop('Only character and raw supported right now...')
}

`getCookie` <- function(cookie)
{
  .Call('getcookie', cookie, PACKAGE='websockets')
}
