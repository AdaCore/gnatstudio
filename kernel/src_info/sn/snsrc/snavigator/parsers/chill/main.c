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
 * main.c
 *
 * Copyright (C) 1998 Cygnus Solutions
 *
 * Description:
 * Entry point and associated callback functions.
 */

#include "snptools.h"

static char group[] = "chill";

extern int errors;
extern int yydebug;

extern FILE * yyin;

extern int yyparse();

/*
 * Called at the end of each source file.
 */
static void
reset()
{
        sn_reset_column();
        sn_reset_line();
}

/*
 * Called by the parser toolbox library.
 */
static int
parse()
{
        while (yyparse() != 0) {}

#ifdef DEBUG
	if (errors > 0)
	{
            printf("%d errors encountered.\n", errors);
	}
	else
	{
	     printf("done.\n");
	}
#endif /* DEBUG */

        return errors;
}

int
main(int argc, char *argv[])
{
#if 0
	yydebug = 1;
#endif
        return sn_main(argc, argv, group, &yyin, parse, reset);
}

