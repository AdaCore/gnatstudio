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

#ifndef O_TEXT
#define O_TEXT 0
#endif

#if defined (__MINGW32__)
#define PERM (S_IREAD | S_IWRITE)
#elif defined (VMS)
#define PERM 0777
#else
#define PERM (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)
#endif

int
__gnat_create_output_file (char *path)
{
  int fd;
#if defined (VMS)
  fd = open (path, O_WRONLY | O_CREAT | O_TRUNC | O_TEXT, PERM,
             "rfm=stmlf", "ctx=rec", "rat=none", "rop=nlk",
             "shr=del,get,put,upd");
#else
  fd = open (path, O_WRONLY | O_CREAT | O_TRUNC | O_TEXT, PERM);
#endif

  return fd < 0 ? -1 : fd;
}
