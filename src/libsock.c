/* Minimalist socket functions */
#include <stdio.h>
#ifdef WIN32
#include <winsock2.h>
#include <windows.h>
#else
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#include <poll.h>
#define INVALID_SOCKET -1
#endif

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>
#include <R_ext/Rdynload.h>
#include "libsock.h"

#ifdef WIN32
WSADATA wsaData;
#endif

void
R_init_websockets(DllInfo * info)
{
#ifdef WIN32
  int j = WSAStartup(MAKEWORD(2,2),&wsaData);
  if(j!=0) error("Windows socket initialization error");
#endif
}

#ifdef WIN32
void
R_unload_websockets(DllInfo * info)
{
  WSACleanup();
}
#endif

/* tcpserv
 * Set up a tcp/ip server. Set lport to 0 for an OS-assigned port. The
 * socket is returned (-1 on error). 
 */
int
tcpserv (int lport)
{
#ifdef WIN32
  SOCKET s;
#else
  int s;
#endif
  int n;
  struct sockaddr_in sin;

  memset(&sin, 0, sizeof(sin));
  sin.sin_family = AF_INET;
  sin.sin_addr.s_addr = htonl (INADDR_ANY);     /* listen on all interfaces */
  sin.sin_port = htons (lport); /* OS assigns port if lport=0 */
  sin.sin_port = htons (lport); /* OS assigns port if lport=0 */

  s = socket (AF_INET, SOCK_STREAM, 0);
  if (s == INVALID_SOCKET)
    {
      return -1;
    } 
  n = 1;
  if (setsockopt (s, SOL_SOCKET, SO_REUSEADDR, (const void *) &n, sizeof (n))
      < 0)
    {
#ifdef WIN32
      closesocket(s);
#else
      close (s);
#endif
      return -1;
    }
  if (bind (s, (struct sockaddr *) &sin, sizeof (sin)) < 0)
    { 
#ifdef WIN32
      closesocket(s);
#else
      close (s);
#endif
      return -1;
    }
  if (listen (s, BACKLOG) < 0)
    { 
#ifdef WIN32
      closesocket(s);
#else
      close (s);
#endif
      return -1;
    }
  return (int)s;
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
      memset(&sa, 0, sizeof(sa));
      sa.sin_family = AF_INET;
      sa.sin_port = htons (port);
      sa.sin_addr = *(struct in_addr *) h->h_addr;
      if (connect (s, (struct sockaddr *) &sa, sizeof (sa)) < 0)
	{
#ifdef WIN32
          closesocket(s);
#else
          close (s);
#endif
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
#ifdef WIN32
  return ScalarInteger(closesocket(INTEGER(S)[0]));
#else
  return ScalarInteger(close(INTEGER(S)[0]));
#endif
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
  R_CheckUserInterrupt();
  poll(pfds, n, t);
  R_CheckUserInterrupt();
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
  struct pollfd pfds;
  int h;
  const void *data = (const void *)RAW(DATA);
  size_t len = (size_t)length(DATA);
  int s = INTEGER(S)[0];
  pfds.fd = s;
  pfds.events = POLLOUT;
  h = poll(&pfds, 1, 500);
  if(pfds.events & POLLOUT)
    return ScalarInteger(send(s, data, len, 0));
  return ScalarInteger(-1);
}

SEXP SOCK_RECV(SEXP S, SEXP EXT, SEXP MAXBUFSIZE)
{
  SEXP ans = R_NilValue;
  void *buf;
  char *msg, *p;
  struct pollfd pfds;
  int h, j, s = INTEGER(S)[0];
  size_t k = 0;
  double maxbufsize = REAL(MAXBUFSIZE)[0];
  int bufsize = MBUF;
  if(maxbufsize < RXBUF) maxbufsize = RXBUF;
  buf = (void *)malloc(RXBUF);
  msg = (char *)malloc(MBUF);
  p = msg;
  pfds.fd = s;
  pfds.events = POLLIN;
  h = poll(&pfds, 1, 50);
  while(h>0) {
    j = recv(s, buf, RXBUF, 0);
    if(j<1) break;
/* If we exceed the maxbufsize, break. This leaves data
 * in the TCP RX buffer. XXX We need to tell R that this
 * is an incomplete read so this can be handled at a high
 * level, for example by closing the connection or whatever.
 * The code is here for basic protection from DoS and memory
 * overcommit attacks. 
 */
    if(k+j > maxbufsize) break;
    if(k + j > bufsize) {
      bufsize = bufsize + MBUF;
      msg = (char *)realloc(msg, bufsize);  
    }
    p = msg + k;
    memcpy((void *)p, buf, j);
    k = k + j;
    h=poll(&pfds, 1, 50);
  }
  if(INTEGER(EXT)[0]) {
/* return a pointer to the recv buffer */
    ans = R_MakeExternalPtr ((void *)msg, R_NilValue, R_NilValue);
    R_RegisterCFinalizer (ans, recv_finalize);
  }
  else {
/* Copy to a raw vector */
    PROTECT(ans=allocVector(RAWSXP,k));
    p = (char *)RAW(ans);
    memcpy((void *)p, (void *)msg, k);
    free(msg);
    UNPROTECT(1);
  }
  free(buf);
  return ans;
}


#ifdef WIN32
int
mingw_poll (struct pollfd *fds, unsigned int nfds, int timo)
{
  struct timeval timeout, *toptr;
  fd_set ifds, ofds, efds, *ip, *op;
  int i, rc;

  /* Set up the file-descriptor sets in ifds, ofds and efds. */
  FD_ZERO (&ifds);
  FD_ZERO (&ofds);
  FD_ZERO (&efds);
  for (i = 0, op = ip = 0; i < nfds; ++i)
    {
      fds[i].revents = 0;
      if (fds[i].events & (POLLIN | POLLPRI))
        {
          ip = &ifds;
          FD_SET (fds[i].fd, ip);
        }
      if (fds[i].events & POLLOUT)
        {
          op = &ofds;
          FD_SET (fds[i].fd, op);
        }
      FD_SET (fds[i].fd, &efds);
    }

  /* Set up the timeval structure for the timeout parameter */
  if (timo < 0)
    {
      toptr = 0;
    }
  else
    {
      toptr = &timeout;
      timeout.tv_sec = timo / 1000;
      timeout.tv_usec = (timo - timeout.tv_sec * 1000) * 1000;
    }

#ifdef DEBUG_POLL
  printf ("Entering select() sec=%ld usec=%ld ip=%lx op=%lx\n",
          (long) timeout.tv_sec, (long) timeout.tv_usec, (long) ip,
          (long) op);
#endif
  rc = select (0, ip, op, &efds, toptr);
#ifdef DEBUG_POLL
  printf ("Exiting select rc=%d\n", rc);
#endif

  if (rc <= 0)
    return rc;

  if (rc > 0)
    {
      for (i = 0; i < nfds; ++i)
        {
          int fd = fds[i].fd;
          if (fds[i].events & (POLLIN | POLLPRI) && FD_ISSET (fd, &ifds))
            fds[i].revents |= POLLIN;
          if (fds[i].events & POLLOUT && FD_ISSET (fd, &ofds))
            fds[i].revents |= POLLOUT;
          if (FD_ISSET (fd, &efds))
            /* Some error was detected ... should be some way to know. */
            fds[i].revents |= POLLHUP;
#ifdef DEBUG_POLL
          printf ("%d %d %d revent = %x\n",
                  FD_ISSET (fd, &ifds), FD_ISSET (fd, &ofds), FD_ISSET (fd,
                                                                        &efds),
                  fds[i].revents);
#endif
        }
    }
  return rc;
}
#endif
