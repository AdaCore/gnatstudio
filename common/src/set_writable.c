/*********************************************************************
 *                               G P S                               *
 *                                                                   *
 *                      Copyright (C) 2002-2004                      *
 *                            ACT-Europe                             *
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

extern int __gnat_stat (char *, struct stat *);

void
__gnat_set_writable (char *name)
{
  struct stat statbuf;

  if (__gnat_stat (name, &statbuf) == 0)
  {
    statbuf.st_mode = statbuf.st_mode | S_IWUSR;
    chmod (name, statbuf.st_mode);
  }
}
