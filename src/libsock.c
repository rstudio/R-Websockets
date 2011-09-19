/* Minimalist socket functions */
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <stdio.h>
#include <unistd.h>
#include <poll.h>

#include <R.h>
#include <Rinternals.h>
#include "libsock.h"

/* tcpserv
 * Set up a tcp/ip server. Set lport to 0 for an OS-assigned port. The
 * socket is returned (-1 on error). 
 */
int
tcpserv (int lport)
{
  int s, n;
  struct sockaddr_in sin;

  bzero (&sin, sizeof (sin));
  sin.sin_family = AF_INET;
  sin.sin_addr.s_addr = htonl (INADDR_ANY);     /* listen on all interfaces */
  sin.sin_port = htons (lport); /* OS assigns port if lport=0 */
  sin.sin_port = htons (lport); /* OS assigns port if lport=0 */

  s = socket (AF_INET, SOCK_STREAM, 0);
  if (s < 0)
    {
      return -1;
    } 
  n = 1;
  if (setsockopt (s, SOL_SOCKET, SO_REUSEADDR, (const void *) &n, sizeof (n))
      < 0)
    {
      close (s);
      return -1;
    }
  if (bind (s, (struct sockaddr *) &sin, sizeof (sin)) < 0)
    { 
      close (s);
      return -1;
    }
  if (listen (s, BACKLOG) < 0)
    { 
      close (s);
      return -1;
    }
  return s;
}

/* tcpconnect
 * connect to the specified host and port, returning a socket
 */
int
tcpconnect (char *host, int port)
{
  struct hostent *h;
  struct sockaddr_in sa;
  int s;

  h = gethostbyname (host);
  if (!h)
    {
      s = -1;
    }
  else
    {
      s = socket (AF_INET, SOCK_STREAM, 0);
      bzero (&sa, sizeof (sa));
      sa.sin_family = AF_INET;
      sa.sin_port = htons (port);
      sa.sin_addr = *(struct in_addr *) h->h_addr;
      if (connect (s, (struct sockaddr *) &sa, sizeof (sa)) < 0)
	{
	  close (s);
	  return -1;
	}
    }
  return s;
}

static void
recv_finalize (SEXP M)
{
  free((void *)R_ExternalPtrAddr(M));
}

/* in-place mask/unmask */
void
mask (int len, char *msg, char *key)
{
  int j,k;
  for(j=0;j<len;++j){
    k = j % 4;
    msg[j] = msg[j] ^ key[k];
  }
}

/* XXX
 * The present MASK function makes a copy of the data into a new raw vector.
 * A future version will allow in-place masking using an external pointer.
 */
SEXP MASK (SEXP DATA, SEXP KEY)
{
  SEXP ans = R_NilValue;
  int len = length(DATA);
  char *key = (char *)RAW(KEY);
  char *msg = (char *)malloc(len);
  memcpy((void *)msg, (void *)RAW(DATA), len);
  mask(len, msg, key);
  PROTECT(ans = allocVector(RAWSXP, len));
  memcpy((void *)RAW(ans), msg, len);
  UNPROTECT(1);
  free(msg);
  return ans;
}

SEXP SOCK_CLOSE (SEXP S)
{ 
  return ScalarInteger(close(INTEGER(S)[0]));
}

SEXP SOCK_ACCEPT (SEXP S)
{ 
  struct sockaddr_in sa;
  socklen_t slen;
  memset(&sa, 0, sizeof (sa));
  return ScalarInteger(accept(INTEGER(S)[0], (struct sockaddr*)&sa, &slen));
}

SEXP SOCK_NAME(SEXP S)
{
  struct sockaddr_in sin;
  socklen_t slen;
  int s = INTEGER(S)[0];
  slen = sizeof(sin);
  memset(&sin, 0, sizeof (sin));
  getsockname (s, (struct sockaddr *) &sin, &slen);
  return ScalarInteger(ntohs(sin.sin_port));
}

SEXP SOCK_CONNECT(SEXP HOST, SEXP PORT)
{
  char *host =  (char *)CHAR(STRING_ELT(HOST, 0));
  int port = INTEGER(PORT)[0];
  return ScalarInteger(tcpconnect(host, port));
}

SEXP SOCK_SERVE(SEXP PORT)
{
  int port = INTEGER(PORT)[0];
  return ScalarInteger(tcpserv(port));
}

SEXP SOCK_POLL (SEXP FDS, SEXP TIMEOUT, SEXP EVENTS)
{ 
  struct pollfd *pfds;
  int j;
  SEXP ans = R_NilValue;
  int *fds = INTEGER(FDS);
  int t = INTEGER(TIMEOUT)[0];
  short events = (short) INTEGER(EVENTS)[0];
  int n = length(FDS);
  pfds = (struct pollfd *)malloc(n * sizeof(struct pollfd));
  for(j=0;j<n;++j){
    pfds[j].fd = fds[j];
    pfds[j].events = events;
  }
  poll(pfds, (nfds_t) n, t);
  PROTECT(ans = allocVector(INTSXP, n));
  for(j=0;j<n;++j) {
    fds = INTEGER(ans);
    fds[j] = (int)pfds[j].revents;  
  }
  UNPROTECT(1);
  free(pfds);
  return ans;
}

SEXP SOCK_SEND(SEXP S, SEXP DATA)
{ 
  const void *data = (const void *)RAW(DATA);
  size_t len = (size_t)length(DATA);
  int s = INTEGER(S)[0];
  return ScalarInteger(send(s, data, len, 0));
}

SEXP SOCK_RECV(SEXP S, SEXP EXT)
{
  SEXP ans = R_NilValue;
  void *msg, *buf, *p;
  struct pollfd pfds;
  int h,j,k=0, s = INTEGER(S)[0];
  int bufsize = MBUF;
  buf = (void *)malloc(RXBUF);
  msg = (void *)malloc(MBUF);
  p = msg;
  pfds.fd = s;
  pfds.events = POLLIN;
  h = poll(&pfds, 1, 50);
  while(h>0) {
    j = recv(s, buf, RXBUF, 0);
    if(j<1) break;
    if(k + j > bufsize) {
      bufsize = bufsize + MBUF;
      msg = realloc(msg, bufsize);  
    }
    p = msg + k;
    memcpy(p, buf, j);
    k = k + j;
    h=poll(&pfds, (nfds_t)1, 50);
  }
  if(INTEGER(EXT)[0]) {
/* return a pointer to the recv buffer */
    ans = R_MakeExternalPtr ((void *)msg, R_NilValue, R_NilValue);
    R_RegisterCFinalizer (ans, recv_finalize);
  }
  else {
/* Copy to a raw vector */
    PROTECT(ans=allocVector(RAWSXP,k));
    p = RAW(ans);
    memcpy(p, msg, k);
    free(msg);
    UNPROTECT(1);
  }
  free(buf);
  return ans;
}
