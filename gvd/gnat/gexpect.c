
/* The following implementation of poll() comes from the GNU C Library.
 * Copyright (C) 1994, 1996, 1997 Free Software Foundation, Inc.
 */

#include <string.h> /* for bzero on BSD systems */
#include <time.h>
#include <sys/time.h>

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
