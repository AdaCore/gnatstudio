#ifdef _WIN32

#include <windows.h>
#include "system/windows.h"

int
__gnat_expect_fork () {
  return 0;
}

void
__gnat_expect_portable_execvp (char* cmd, char* argv[]) {
  int pid = spawnve (_P_NOWAIT, cmd, argv, NULL);
}

int
__gnat_pipe (int *fd)
{
  HANDLE read, write;

  CreatePipe (&read, &write, NULL, 0);
  fd[0]=_open_osfhandle (read, 0);
  fd[1]=_open_osfhandle (write, 0);
  return 0;  /* always success */
}

int
__gnat_expect_poll (int *fd, int num_fd, int timeout, int *is_set)
{
  int i, num;
  DWORD avail;
  HANDLE handles[num_fd];

  for (i=0; i<num_fd; i++)
    is_set[i]=0;

  for (i=0; i<num_fd; i++)
    handles[i] = (HANDLE) _get_osfhandle (fd [i]);

  num = timeout / 10;

  for (;;)
    {
      for (i=0; i<num_fd; i++)
	{
	  if (!PeekNamedPipe (handles [i], NULL, 0, NULL, &avail, NULL))
	    return -1;
	  if (avail > 0)
	    {
	      is_set[i] = 1;
	      return 1;
	    }
	}

      if (num == 0)
	return 0;
      Sleep (10);
      num--;
    }
}

#else

/* The following implementation of poll() comes from the GNU C Library.
 * Copyright (C) 1994, 1996, 1997 Free Software Foundation, Inc.
 */

#include <string.h> /* for bzero on BSD systems */
#include <time.h>
#include <sys/time.h>
#include <unistd.h>

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif /* HAVE_SYS_SELECT_H_ */

#ifndef NO_FD_SET
#  define SELECT_MASK fd_set
#else /* !NO_FD_SET */
#  ifndef _AIX
typedef long fd_mask;
#  endif /* _AIX */
#  ifdef _IBMR2
#    define SELECT_MASK void
#  else /* !_IBMR2 */
#    define SELECT_MASK int
#  endif /* !_IBMR2 */
#endif /* !NO_FD_SET */

int
__gnat_pipe (int *fd)
{
  return pipe (fd);
}

int
__gnat_expect_fork () {
  return fork ();
}

void
__gnat_expect_portable_execvp (char* cmd, char* argv[]) {
  execvp (cmd, argv);
}

int
__gnat_expect_poll (int* fd, int num_fd, int timeout, int* is_set)
{
  struct timeval tv;
  SELECT_MASK rset;
  int max_fd = 0;
  int ready;
  int i;

  FD_ZERO (&rset);
  
  for (i = 0; i < num_fd; i++) {
    FD_SET (fd [i], &rset);
    if (fd [i] > max_fd)
      {
	max_fd = fd [i];
      }
  }

  tv.tv_sec  = timeout / 1000;
  tv.tv_usec = (timeout % 1000) * 1000;

  ready = select (max_fd + 1, &rset, NULL, NULL, timeout == -1 ? NULL : &tv);

  if (ready > 0) {
    for (i = 0; i < num_fd; i++) {
      is_set [i] = (FD_ISSET (fd [i], &rset)  ? 1 : 0);
    }
  }
  
  return ready;
}

#endif
