/*********************************************************************
 *                               G P S                               *
 *                                                                   *
 *                        Copyright (C) 2002                         *
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
#ifdef _WIN32
#include <windows.h>
#endif

int
__gnat_subdirectories_count (name)
{
  struct stat statbuf;
  int ret;
  
  ret = __gnat_stat (name, &statbuf);
  
#ifdef _WIN32
  /* On windows, stat(2) always return 1 for the number of links, and thus can
     not be used reliably.
     However, for GPS we only want to know if there is at least one
     subdirectory, so we just pretend this is always true.
  */
  if (ret || !S_ISDIR (statbuf.st_mode))
    return -1;
  else
    return 1;
  
#else
  if (ret || !S_ISDIR (statbuf.st_mode))
    return -1;
  else
    /* Do not count the subdirectories . and .. */
    return statbuf.st_nlink - 2;
#endif
}

int __gnat_get_logical_drive_strings (char *buffer, int len)
{
#ifdef _WIN32
  return GetLogicalDriveStringsA ((DWORD)len, (LPSTR)buffer);
#else
  return 0;
#endif
}

