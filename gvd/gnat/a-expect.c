/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               E X P E C T                                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *           Copyright (C) 2001-2003 Ada Core Technologies, Inc.            *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). *
 *                                                                          *
 ****************************************************************************/

/* This file provides the low level functionalities needed to implement Expect
   capabilities in GNAT.Expect.
   Implementations for unix and windows systems is provided.
   Dummy stubs are also provided for other systems. */

#ifdef _AIX
/* Work around the fact that gcc/cpp does not define "unix" under AiX.  */
#define unix
#endif

#ifdef _WIN32

#include <windows.h>
#include <process.h>

void
__gnat_kill (int pid, int sig)
{
  HANDLE process_handle;
  
  if (sig == 9)
    {
      process_handle = OpenProcess (PROCESS_TERMINATE, FALSE, pid);
      if (process_handle != NULL)
	TerminateProcess (process_handle, 0);
    }
}

int
__gnat_expect_fork ()
{
  return 0;
}

void
__gnat_expect_portable_execvp (int *pid, char *cmd, char *argv[])
{
  *pid = (int) spawnve (_P_NOWAIT, cmd, argv, NULL);
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
#define MAX_DELAY 100

  int i, delay, infinite = 0;
  DWORD avail;
  HANDLE handles[num_fd];

  for (i = 0; i < num_fd; i++)
    is_set[i] = 0;

  for (i = 0; i < num_fd; i++)
    handles[i] = (HANDLE) _get_osfhandle (fd [i]);

  /* Start with small delays, and then increase them, to avoid polling too
     much when waiting a long time */
  delay = 5;

  if (timeout < 0)
    infinite = 1;

  while (1)
    {
      for (i = 0; i < num_fd; i++)
        {
          if (!PeekNamedPipe (handles [i], NULL, 0, NULL, &avail, NULL))
            return -1;

          if (avail > 0)
            {
              is_set[i] = 1;
              return 1;
            }
        }

      if (!infinite && timeout <= 0)
        return 0;

      Sleep (delay);
      timeout -= delay;

      if (delay < MAX_DELAY)
        delay += 10;
    }
}

#elif defined (VMS)
#include <unistd.h>
#include <stdio.h>
#include <unixio.h>
#include <stdlib.h>
#include <string.h>
#include <descrip.h>
#include <stdio.h>
#include <stsdef.h>
#include <iodef.h>

int
__gnat_pipe (int *fd)
{
  return pipe (fd);
}

int
__gnat_expect_fork ()
{
  return -1;
}

void
__gnat_expect_portable_execvp (int *pid, char *cmd, char *argv[])
{
  *pid = (int) getpid ();
  /* Since cmd is fully qualified, it is incorrect to call execvp */
  execv (cmd, argv);
  _exit (1);
}

int
__gnat_expect_poll (int *fd, int num_fd, int timeout, int *is_set)
{
  int i, num, ready = 0;
  unsigned int status;
  int mbxchans [num_fd];
  struct dsc$descriptor_s mbxname;
  struct io_status_block {
    short int condition;
    short int count;
    int dev;
  } iosb;
  char buf [256];

  for (i = 0; i < num_fd; i++)
    is_set[i] = 0;

  for (i = 0; i < num_fd; i++)
    {

      /* Get name of the mailbox used in the pipe */
      getname (fd [i], buf);

      /* Assign a channel to the mailbox */
      if (strlen (buf) > 0)
	{
	  mbxname.dsc$w_length = strlen (buf);
	  mbxname.dsc$b_dtype = DSC$K_DTYPE_T;
	  mbxname.dsc$b_class = DSC$K_CLASS_S;
	  mbxname.dsc$a_pointer = buf;

	  status = SYS$ASSIGN (&mbxname, &mbxchans[i], 0, 0, 0);
	}
    }

  num = timeout / 100;

  while (1)
    {
      for (i = 0; i < num_fd; i++)
	{
	  if (mbxchans[i] > 0)
	    {

	      /* Peek in the mailbox to see if there's data */
	      status = SYS$QIOW
		(0, mbxchans[i], IO$_SENSEMODE|IO$M_READERCHECK,
		 &iosb, 0, 0, 0, 0, 0, 0, 0, 0);

	      if (iosb.count > 0)
		{
		  is_set[i] = 1;
		  ready = 1;
		  goto deassign;
		}
	    }
	}

      if (timeout >= 0 && num == 0)
	{
	  ready = 0;
	  goto deassign;
	}

      usleep (100000);
      num--;
    }

 deassign:

  /* Deassign channels assigned above */
  for (i = 0; i < num_fd; i++)
    {
      if (mbxchans[i] > 0)
	status = SYS$DASSGN (mbxchans[i]);
    }

  return ready;
}

#elif defined (unix)

#ifdef hpux
#include <sys/ptyio.h>
#endif

#include <sys/time.h>
#include <unistd.h>

#ifndef NO_FD_SET
#define SELECT_MASK fd_set
#else /* !NO_FD_SET */
#ifndef _AIX
typedef long fd_mask;
#endif /* _AIX */
#ifdef _IBMR2
#define SELECT_MASK void
#else /* !_IBMR2 */
#define SELECT_MASK int
#endif /* !_IBMR2 */
#endif /* !NO_FD_SET */

void
__gnat_kill (int pid, int sig)
{
  kill (pid, sig);
}

int
__gnat_pipe (int *fd)
{
  return pipe (fd);
}

int
__gnat_expect_fork ()
{
  return fork ();
}

void
__gnat_expect_portable_execvp (int *pid, char *cmd, char *argv[])
{
  *pid = (int) getpid ();
  /* Since cmd is fully qualified, it is incorrect to call execvp */
  execv (cmd, argv);
  _exit (1);
}

int
__gnat_expect_poll (int *fd, int num_fd, int timeout, int *is_set)
{
  struct timeval tv;
  SELECT_MASK rset;
  SELECT_MASK eset;

  int max_fd = 0;
  int ready;
  int i;

  FD_ZERO (&rset);
  FD_ZERO (&eset);

  for (i = 0; i < num_fd; i++)
    {
      FD_SET (fd[i], &rset);
      FD_SET (fd[i], &eset);

      if (fd[i] > max_fd)
	max_fd = fd[i];
    }

  tv.tv_sec  = timeout / 1000;
  tv.tv_usec = (timeout % 1000) * 1000;

  ready = select (max_fd + 1, &rset, NULL, &eset, timeout == -1 ? NULL : &tv);

  if (ready > 0)
    {
      for (i = 0; i < num_fd; i++)
	{
	  is_set[i] = (FD_ISSET (fd[i], &rset) ? 1 : 0);

#ifdef hpux
	  if (FD_ISSET (fd[i], &eset))
	    {
	      struct request_info ei;

	      ioctl (fd[i], TIOCREQCHECK, &ei);

	      if (ei.request == TIOCCLOSE)
		{
		  ioctl (fd[i], TIOCREQSET, &ei);
		  return -1;
		}

	      ioctl (fd[i], TIOCREQSET, &ei);
	      ready--;
	    }
#endif
	}
    }

  return ready;
}

#else

void
__gnat_kill (int pid, int sig)
{
}

int
__gnat_pipe (int *fd)
{
  return -1;
}

int
__gnat_expect_fork ()
{
  return -1;
}

void
__gnat_expect_portable_execvp (int *pid, char *cmd, char *argv[])
{
  *pid = 0;
}

int
__gnat_expect_poll (int *fd, int num_fd, int timeout, int *is_set)
{
  return -1;
}
#endif
