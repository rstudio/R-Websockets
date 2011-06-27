#include "websockets.h"
#include <Rconfig.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

static const
R_CallMethodDef callMethods[] = {
  {"setcookie",         (DL_FUNC) &setcookie,               2},
  {"getcookie",         (DL_FUNC) &getcookie,               1},
  {"wsockwrite",      (DL_FUNC) &wsockwrite,            2},
  {"wsockbcast",      (DL_FUNC) &wsockbcast,            1},
  {"service",           (DL_FUNC) &service,                 1},
  {NULL,                NULL,                               0}
};

void R_init_websockets(DllInfo *info)
{
  R_registerRoutines(info,
                     NULL,
                     callMethods,
                     NULL,
                     NULL);
  R_useDynamicSymbols(info, TRUE);

  R_RegisterCCallable("websockets", "setcookie", (DL_FUNC) &setcookie);
  R_RegisterCCallable("websockets", "getcookie", (DL_FUNC) &getcookie);
  R_RegisterCCallable("websockets", "wsockwrite", (DL_FUNC) &wsockwrite);
  R_RegisterCCallable("websockets", "wsockbcast", (DL_FUNC) &wsockbcast);
  R_RegisterCCallable("websockets", "service", (DL_FUNC) &service);
}

