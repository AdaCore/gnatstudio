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
 * dbquery.c
 *
 * Copyright (C) 1998 Cygnus Solutions
 *
 * Description:
 * This example shows how to query the Source-Navigator database in C.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>

#include "db.h"

int
main(int argc, char *argv[])
{
	DB	*db;
	DBT	data;
	DBT	key;
	int	flag;
	int	len;
	char	*pattern;

	if (argc != 3)
	{
		printf("usage: %s database pattern\n",argv[0]);
		exit(1);
	}
	if (!(db = dbopen(argv[1],O_RDONLY,0644,DB_BTREE,NULL)))
	{
		fprintf(stderr,"Could not open \"%s\",%s\n",argv[1],
			strerror(errno));
		exit(2);
	}

	pattern = argv[2];
	len = strlen(pattern);
	key.data = (void *)pattern;
	key.size = len;
	for(flag = R_CURSOR;
		db->seq(db,&key,&data,flag) == 0 &&
		strncmp(key.data,pattern,len) == 0; flag = R_NEXT)
	{
		printf("key:  %s\n", (char *) key.data);
		printf("data: %s\n", (char *) data.data);
	}
	db->close(db);

	exit (0);
}

