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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>

#ifdef __MSVC__
#include <shlobj.h>
#endif

#include <ctype.h>
#include <signal.h>
#include <time.h>
#include <locale.h>

#include "mxdefine.h"
#include "fileutils.h"

#ifndef _WINDOWS

#undef USE_TCLGETCWD
#ifdef USE_TCLGETCWD
#include "tclInt.h"
extern char * TclGetCwd(Tcl_Interp *interp);
#endif

/*
 * returns the full path name by deleting all relative path names from
 * the original path
 *
 * "/home/user/../user2/./.sn/rc.tcl" ==> "/home/usr2/.sn/rc.tcl"
 */
int
absolutepath (char *rpath, char *fullpath, char *pwd)
{
	char relpath[MAXPATHLEN+1];
	int i, j;
	char name[256], *fp;
	
	/* We got a path like "../foo", join pwd with it */
	if (! IS_ROOT(rpath[0]))
	{
		if (pwd == NULL)
		{
			return 0;           /* Path must be specified */
		}
		strcpy (relpath, pwd);
		strcat (relpath, "/");
		strcat (relpath, rpath);
	}
	else
	{
		strcpy (relpath, rpath);
	}
	
	/* now we filter the path */
	strcpy (fullpath, "/"); /* start with root */
	fp = fullpath+1;
	for (i=0; relpath[i]; )
	{
		/* pick the next (nacked) name */
		for (j=0;relpath[i] && ! IS_ROOT (relpath[i]); i++, j++)
		{
			name[j] = relpath[i];
		}
		name[j] = 0;
		
		if (strcmp (name, "..") == 0)          /* delete basename */
		{
			while (fp > fullpath && ! IS_ROOT(*fp))
			{
				fp--;
			}
			if (fp > fullpath)           /* don't override root */
			{
				*fp = 0;
			}
		}
		else if (strcmp (name, ".") == 0)           /* skip "./" */
		{
			;
		}
		else if (name [0])          /* add name to the full path */
		{
			if (! IS_ROOT (*(fp-1)))
			{
				strcpy (fp++, "/");
			}
			strcpy (fp, name);
			fp += strlen (name);
		}
		while (IS_ROOT (relpath[i])) /* skip all "/" or "\\" **/
		{
			i++;
		}
	}
	return 1;
}
#endif

#if _WINDOWS
#ifndef __MSVC__
/* Get the realpath() from Cygwin.*/
int
win32_realpath(char* p,char *realnm)
{
	char	temp_realnm[MAXPATHLEN + 1];
	int	i;
	int	ret;

    /* Get realpath (cygwin) */
	ret=realpath (p, temp_realnm);

	/* Convert path to Win32 */
	cygwin32_conv_to_full_win32_path(temp_realnm,realnm);

	/*
	 * Convert "\" to "/" (just now!)
	 */
	sn_internal_convert_path (realnm, SN_PATH_UNIX);
	
	/*
	 * It could happen, that we get pathes like "//foo.c"
	 * Convert "//foo.c" to "C:/foo.c"
	 */
	if (IS_ROOT (realnm[0]) && IS_ROOT (realnm[1]))
		{
			int i = strlen (realnm) + 1;
			for (; i>0; i--)
			{
				realnm[i] = realnm[i-1];
			}
			realnm[1] = ':';
			realnm[0] = 'C';
		}
		
	/* make sure drive letter is uppercase to avoid other problems */
	realnm[0] = _toupper(realnm[0]);
	
	return 1;
}
#else /* __MSVC__ */

/* Get the realpath() from WIN32.*/
int
win32_realpath(char* p,char *realnm)
{
	char	temp_realnm[MAXPATHLEN + 1];
	DWORD	ret;

    /*
     * convert TCL path to WIN32 path
     */
    sn_internal_convert_path (p, SN_PATH_WINDOWS);

    /*
     * Get fullpath
     */
/*	ret = _fullpath(p,temp_realnm,MAXPATHLEN); */
    ret = GetFullPathName (p, MAXPATHLEN, temp_realnm, NULL);

	if ((ret==0) || (strlen(temp_realnm) != ret))
		return 0; /* error!!! */
	/*
	 * Convert "\" to "/" (just now!)
	 */
	sn_internal_convert_path (temp_realnm, SN_PATH_UNIX);
	
	strcpy(realnm,temp_realnm);

	return 1;
}

#endif /* __MSVC__ */

#endif /* _WINDOWS */

char * file_lastroot (char *path)
{
	char *q = path + strlen (path) - 1;
	for (; q >= path; q--)
	{
		if (IS_ROOT (*q))
		{
			return q;
		}
	}
	return NULL;
}
char * file_firstroot (char *path)
{
	char *q = path;
	for (; *q; q++)
	{
		if (IS_ROOT (*q))
		{
			return q;
		}
	}
	return NULL;
}

/*
 * Converts pathes between unix & windows
 * mode: 0: converts unix --> windows
 * mode: 1: converts windows --> unix
 */
void sn_internal_convert_path (char*path, int mode)
{
	char *p;
	char slash1, slash2;
	if (mode == SN_PATH_WINDOWS)
	{
		slash1 = '/';
		slash2 = '\\';
	}
	else
	{
		slash1 = '\\';
		slash2 = '/';
	}
	for (p = path; *p; p++)
	{
		if (*p == slash1)
			*p = slash2;
	}
}

void
sn_append_option_to_command_line (char*cmd, char *arg)
{
	int have_blanks;
	if (*arg == 0)
	{
		return;
	}
	if (*cmd)
	{
		strcat (cmd, " ");
	}
	if (strchr (arg, ' ') != NULL)
	{
		strcat (cmd, "\"");
		have_blanks = 1;
	}
	else
	{
		have_blanks = 0;
	}
	strcat (cmd, arg);
	if (have_blanks)
	{
		strcat (cmd, "\"");
	}
}

/*
 * compare two paths, on windows it ignores upper/lower case
 *
 * len != -1: emulates "strncasecmp" for windows and "strncmp" for unix
 * len == 0:  emulates strcasecmp on windows and strcmp on unix.
 */
int native_compare_paths (char *path1, char*path2, int path1len)
{
#if _WINDOWS
	char *p = path1, *q = path2;
	int ret;
	int len = path1len;
	
	for (ret = 0; *p && *q; p++, q++, len--)
	{
		if (len == 0)
		{
			return ret;
		}
		if (IS_ROOT (*p) && IS_ROOT(*q))
		{
			continue; /* don't compare roots */
		}
		if ((ret = (tolower(*p) - tolower (*q))) != 0)
		{
			return ret; /* different */
		}
	}
	if (len == 0)
	{
		return 0;
	}
	if (*p == 0 && *q == 0)
	{
		return 0; /* is equal */
	}
	return (tolower (*p) - tolower (*q)); /* different */
	
#else
	if (path1len != -1)
	{
		return strncmp (path1, path2, path1len);
	}
	else
	{
		return strcmp (path1, path2);
	}
#endif
}

