#include <R.h>
#include <Rinternals.h>

#define BACKLOG 100
#define RXBUF 16384             /* TCP receive buffer */
#define MBUF 1048576            /* Message buffer base size */

int tcpserv (int);
int tcpconnect (char *, int);
SEXP SOCK_POLL (SEXP, SEXP, SEXP);
SEXP SOCK_CLOSE (SEXP);
SEXP SOCK_ACCEPT (SEXP);
SEXP SOCK_RECV (SEXP, SEXP);
SEXP SOCK_SEND (SEXP, SEXP);
SEXP SOCK_NAME (SEXP);
SEXP SOCK_SERVE (SEXP);
SEXP SOCK_CONNECT (SEXP, SEXP);
