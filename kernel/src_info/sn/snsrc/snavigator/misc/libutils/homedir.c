/*

Copyright (c) 2000, Red Hat, Inc.

This file is part of Source-Navigator.

Source-Navigator is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2, or (at your option)
any later version.

Source-Navigator is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along
with Source-Navigator; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA.



*/

/*
 * homedir.c
 *
 * Copyright (C) 1998 Cygnus Solutions
 *
 * Description:
 * Provides a (seemingly) platform independent way to find out a
 * user's home directory.
 */

#include <config.h>

#include "mxdefine.h"
#include "mxfuncs.h"
#include "mxlogger.h"
#include <time.h>

char *
GetHomeDir()
{
  char *home;
#if _WINDOWS
  if ((home = getenv("HOME")))
    {
      LOGGER((LOGFP,"Using HOME (%s) from environment\n",home));
      return(home);
    }
  return("");
#else
  struct passwd *usrp;
  
  usrp = getpwuid(getuid());
  endpwent();
  
  if (usrp == NULL)
    {
      LOGGER((LOGFP,"Cannot find home dir: %d\n", errno));
      if ((home = getenv("HOME")))
	{
	  LOGGER((LOGFP,"Using HOME (%s) from environment\n", home));
	  return(home);
	}
      return("");
    }
  return ((char *) usrp->pw_dir);
#endif /* _WINDOWS */
}


