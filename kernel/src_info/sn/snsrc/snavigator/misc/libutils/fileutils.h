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
 * fileutils.h
 */
#ifndef FILEUTILS_H
#define FILEUTILS_H

#if _WINDOWS
#define IS_ROOT(c) ((c) == '/' || (c) == '\\')
#else
#define IS_ROOT(c) ((c) == '/')
#endif

enum
{
	SN_PATH_WINDOWS, /* convert path to windows native */
	SN_PATH_UNIX,    /* convert path to unix native */
	SN_PATH_NATIVE
};

#ifdef WIN32
int win32_realpath(char* p,char *realnm);
#endif

void sn_internal_convert_path (char*path, int mode);
void sn_append_option_to_command_line (char*cmd, char *arg);
char * file_lastroot (char *path);
char * file_firstroot (char *path);
int  absolutepath (char *rpath, char *fullpath, char *pwd);
int  native_compare_paths (char *path1, char*path2, int path1len);

#endif /* FILEUTILS_H */

