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
 * common.c
 *
 * Copyright (C) 1998 Cygnus Solutions
 *
 * Description:
 * Commonly used routine implementations.
 */

#include <stdlib.h>
#include <stdio.h>
#include <tcl.h>

#include "snptools.h"

char *
strjoin(char *s1, char *s2)
{
        char *buf;

        buf = (char *) ckalloc(strlen(s1) + strlen(s2) + 2); /* space and NUL */
        if (buf == NULL) return buf;

        sprintf(buf, "%s %s", s1, s2);

        return buf;
}

