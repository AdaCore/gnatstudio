/*********************************************************************
 *                               G P S                               *
 *                                                                   *
 *                      Copyright (C) 2005                           *
 *                             AdaCore                               *
 *                                                                   *
 * GPS is free  software;  you can redistribute it and/or modify  it *
 * under the terms of the GNU General Public License as published by *
 * the Free Software Foundation; either version 2 of the License, or *
 * (at your option) any later version.                               *
 *                                                                   *
 * This program is  distributed in the hope that it will be  useful, *
 * but  WITHOUT ANY WARRANTY;  without even the  implied warranty of *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *
 * General Public License for more details. You should have received *
 * a copy of the GNU General Public License along with this program; *
 * if not,  write to the  Free Software Foundation, Inc.,  59 Temple *
 * Place - Suite 330, Boston, MA 02111-1307, USA.                    *
 *********************************************************************/

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/param.h>
#include <unistd.h>
#include <fcntl.h>

#ifdef _WIN32
#include <windows.h>
#include <process.h>
#endif

int
__gnat_set_close_on_exec (int fd, int close_on_exec_p)
{
#if defined (F_GETFD) && defined (FD_CLOEXEC)
  int flags = fcntl (fd, F_GETFD, 0);
  if (flags < 0)
    return flags;
  if (close_on_exec_p)
    flags |= FD_CLOEXEC;
  else
    flags &= ~FD_CLOEXEC;
  return fcntl (fd, F_SETFD, flags | FD_CLOEXEC);
#else
  return -1;
  /* For the Windows case, we should use SetHandleInformation to remove
     the HANDLE_INHERIT property from fd. This is not implemented yet,
     but for our purposes (support of GNAT.Expect) this does not matter,
     as by default handles are *not* inherited. */
#endif
}
