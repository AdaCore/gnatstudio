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

#ifndef _MSC_VER
#include "config.h"
#endif

#if HAVE_STRING_H | defined _MSC_VER | defined MSDOS
#include <string.h>
#else
#if HAVE_STRINGS_H
#include <strings.h>
#else
#include <sys/types.h>
extern char *strcpy (), *strncpy (), *strchr (), *strrchr (), *strtok ();
extern char *strcat (), *strncat (), *strerror (), *strstr ();
extern char *strpbrk ();
extern int strcmp (), strcasecmp (), strncasecmp ();
extern size_t strlen (), strspn (), strcspn ();
#endif
#endif

