#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
//#include <sys/mman.h>
#include <stdlib.h>
#include <string.h>

#include "private-libwebsockets.h"
#include "libwebsockets.h"

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

enum protocols {
  /* always first */
  PROTOCOL_HTTP = 0,
  PROTOCOL_R,
  /* always last */
  COUNT
};

// This protocol is not really needed, but can be convenient for
// self-contained applications.
static int
callback_http(struct libwebsocket_context *context,
              struct libwebsocket *wsi,
              enum libwebsocket_callback_reasons reason, void *user,
              void *in, size_t len)
{
  const char *page;
  SEXP PAGE;
  switch(reason) {
    case LWS_CALLBACK_HTTP:
      PAGE = findVar(install("webpage"), context->renv);
      if(PAGE == R_UnboundValue) break;
      page = CHAR(STRING_ELT(PAGE,0));
      if(libwebsockets_serve_http_file(wsi, page, "text/html"))
        error("Failed to send HTTP file");
      break;
    default:
      break;
  }
  return 0;
}

void
do_callback(struct libwebsocket_context *context,
            struct libwebsocket *wsi, const char *fname,
            void *in, size_t len, void *user)
{
  void *p;
  SEXP fun, R_fcall = R_NilValue, data, WSI, COOKIE;
  fun = findVar(install(fname), context->renv);
//  fun = findFun(install(fname), (SEXP)context->env);
// Would be nice if findVar1 were in the standard R api...
//  if(fun == R_UnboundValue || TYPEOF(fun) != FUNSXP)
  if(fun == R_UnboundValue)
  {
    warning("undefined callback function: %s",fname);
    return;
  }
  PROTECT(R_fcall=lang4(fun, R_NilValue, R_NilValue, R_NilValue));
  PROTECT(data = allocVector(RAWSXP, len));
  p = (void *)RAW(data);
  memcpy(p, in, len);
  // Pass a copy of the socket to the callback function...
  WSI = R_MakeExternalPtr((void *)wsi, R_NilValue, R_NilValue);
  COOKIE = R_MakeExternalPtr((void *)user, R_NilValue, R_NilValue);
  SETCADR(R_fcall, data);
  SETCADDR(R_fcall, WSI);
  SETCADDDR(R_fcall, COOKIE);
  eval(R_fcall, context->renv);
  UNPROTECT(2);
}

// XXX Add more callbacks...
static int
callback_R(struct libwebsocket_context *context,
           struct libwebsocket *wsi,
           enum libwebsocket_callback_reasons reason,
           void *user, void *in, size_t len)
{
  switch (reason) {
    case LWS_CALLBACK_CLOSED:
      do_callback(context, wsi, "closed", in, len, user);
    break;
    case LWS_CALLBACK_ESTABLISHED:
      do_callback(context, wsi, "established", in, len, user);
    break;
    case LWS_CALLBACK_BROADCAST:
      do_callback(context, wsi, "broadcast", in, len, user);
    break;
    case LWS_CALLBACK_RECEIVE:
      do_callback(context, wsi, "receive", in, len, user);
    break;
    default:
    break;
  }
  return 0;
}

/* Per-session extra data. This can really be anything.
 * One of these allocated per session.
 */
struct cookie{
  char data[1024];
};

static struct libwebsocket_protocols protocols[] = {
  {
    "http-only",            /* name */
     callback_http,         /* callback */
     0                      /* per_session_data_size */
  },
  {
    "R",
    callback_R,
    sizeof(struct cookie)
  },
  {
    NULL, NULL, 0 
  }
};

SEXP setcookie(SEXP COOKIE, SEXP DATA)
{
  void *p = R_ExternalPtrAddr(COOKIE);
  void *q = (void *)RAW(DATA);
  int n = LENGTH(DATA);
  if (n>sizeof(struct cookie)-1) n = sizeof(struct cookie)-1;
  memset(p, 0, n+1);
  memcpy(p, q, n);
  return R_NilValue;
}

SEXP getcookie(SEXP COOKIE)
{
  SEXP ans;
  void *p = R_ExternalPtrAddr(COOKIE);
  PROTECT(ans = allocVector(STRSXP,1));
  SET_STRING_ELT(ans, 0, mkChar(p));
  UNPROTECT(1);
  return ans;
}

// Write DATA to WSI
SEXP wsockwrite(SEXP WSI, SEXP DATA)
{
  unsigned char *p, *d;
  int n;
  struct libwebsocket *wsi = (struct libwebsocket*)R_ExternalPtrAddr(WSI);
  unsigned char *buf;
  n = LENGTH(DATA);
  d = (unsigned char *)RAW(DATA);
  buf = (unsigned char *)calloc(LWS_SEND_BUFFER_PRE_PADDING + n +
                                LWS_SEND_BUFFER_POST_PADDING, sizeof(char));
  p = buf + LWS_SEND_BUFFER_PRE_PADDING;
// Unfortunate copy here due to buffer requirements. Is there a better way?
  memcpy(p, d, n);
  n = libwebsocket_write(wsi, p, n, LWS_WRITE_TEXT);
  free(buf);
  if(n<0) error("ERROR writing to socket\n");
  return ScalarInteger(n);
}

// Broadcast to protocol
SEXP wsockbcast(SEXP DATA)
{
  unsigned char *p, *d;
  int n;
  unsigned char *buf;
  n = LENGTH(DATA);
  d = (unsigned char *)RAW(DATA);
  buf = (unsigned char *)calloc(LWS_SEND_BUFFER_PRE_PADDING + n +
                                LWS_SEND_BUFFER_POST_PADDING, sizeof(char));
  p = buf + LWS_SEND_BUFFER_PRE_PADDING;
  memcpy(p, d, n);
  n = libwebsockets_broadcast(&protocols[PROTOCOL_R],buf,n);
  free(buf);
  if(n<0) error("ERROR writing to socket\n");
  return ScalarInteger(n);
}

/* Service sockets (usually place in a loop) */
SEXP service(SEXP CONTEXT)
{
  struct libwebsocket_context *context = 
    (struct libwebsocket_context *)R_ExternalPtrAddr(CONTEXT);
  libwebsocket_service(context, 50);
  return R_NilValue;
}

/* context_finalize: Clean up the websockets context. */
static void
context_finalize (SEXP C)
{
  struct libwebsocket_context *context = 
    (struct libwebsocket_context *)R_ExternalPtrAddr(C);
  libwebsocket_context_destroy(context);
}


// XXX add more options (ssl, select interface, ...)
SEXP createContext(SEXP env, SEXP PORT)
{
  struct libwebsocket_context *context;
  SEXP ans;
  int port = INTEGER(PORT)[0];
  int opts = 0;
  const char *cert_path = 0;
  const char *key_path = 0;
  const char * interface = 0;
  opts = LWS_SERVER_OPTION_DEFEAT_CLIENT_MASK;
  context = libwebsocket_create_context(port, interface, protocols,
              libwebsocket_internal_extensions,
              cert_path, key_path, -1, -1, opts);
  if(!context) return (R_NilValue);
  context->renv = env;
  ans = R_MakeExternalPtr((void *)context, R_NilValue, R_NilValue);
  R_RegisterCFinalizer(ans, context_finalize);
  return (ans);
}
