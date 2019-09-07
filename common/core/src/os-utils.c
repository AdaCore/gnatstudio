/*********************************************************************
 *                          GNAT Studio                              *
 *                                                                   *
 *                Copyright (C) 2002-2019, AdaCore                   *
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <windows.h>
#include <lmcons.h>
#include <malloc.h>
#else
#include <pwd.h>
#endif

int
__gps_subdirectories_count (name)
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

void
__gps_set_writable (char *file, int set)
{
  struct stat statbuf;

  if (!__gnat_stat (file, &statbuf))
    {
      if (set)
	chmod (file, statbuf.st_mode | S_IWRITE);
      else
	chmod (file, statbuf.st_mode & (~S_IWRITE));
    }
}

void
__gps_set_readable (char *file, int set)
{
  struct stat statbuf;

  if (!__gnat_stat (file, &statbuf))
    {
      if (set)
	chmod (file, statbuf.st_mode | S_IREAD);
      else
	chmod (file, statbuf.st_mode & (~S_IREAD));
    }
}

void
__gps_ensure_valid_output (void)
{
#ifdef _WIN32
  static int alloc_console_called = 0;
  static HINSTANCE kernel32_dll;
  HANDLE handle;
  typedef BOOL (FAR PASCAL *AttachConsoleFunc) (DWORD dwProcessId);
  AttachConsoleFunc proc;
#define ATTACH_PARENT_CONSOLE -1

  if (!alloc_console_called)
    {
      handle = (HANDLE) _get_osfhandle (fileno (stdout));

      if (handle == INVALID_HANDLE_VALUE)
        {
          kernel32_dll = LoadLibrary ("kernel32.dll");
          proc = (AttachConsoleFunc) GetProcAddress
            (kernel32_dll, "AttachConsole");
          if (proc)
	    {
	      if (! (*proc) (ATTACH_PARENT_CONSOLE))
		AllocConsole ();
	    }
	  else
	    AllocConsole ();

	  alloc_console_called = 1;
	  freopen ("CONOUT$", "w", stdout);
	  freopen ("CONOUT$", "w", stderr);
	}
    }
#endif
}

/*********************************************************
 ** __gps_user_login_name ()
 ** Return the real user name.
 ** Return value must be freed by caller
 *********************************************************/

char*
__gps_user_login_name (void)
{
#ifdef _WIN32
  DWORD size = UNLEN;
  char *str = alloca (UNLEN + 1);
  BOOL res;
#else
  struct passwd* pw;
#endif
  char* result;

  result = getenv ("LOGNAME");
  if (result) {
    return strdup (result);
  }

  result = getenv ("USERNAME");  /* mostly windows users */
  if (result) {
     return strdup (result);
  }

  result = getenv ("USER");
  if (result) {
     return strdup (result);
  }

#ifdef _WIN32
  res = GetUserName (str, &size);
  result = strdup ((res == TRUE) ? str : "unknown");
#else
  pw = (struct passwd*) getpwuid (geteuid());
  result = strdup (pw ? pw->pw_name : "unknown");
  free (pw);
#endif

  return result;
}

