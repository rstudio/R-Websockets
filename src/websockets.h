#include <R.h>
#include <Rinternals.h>

SEXP setcookie(SEXP COOKIE,SEXP DATA);
SEXP getcookie(SEXP COOKIE);
SEXP wsockwrite(SEXP WSI,SEXP DATA);
SEXP wsockbcast(SEXP DATA);
SEXP service(SEXP CONTEXT);

