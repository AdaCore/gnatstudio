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

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>
#include <limits.h>
#include <string.h>

#include "db.h"

int
main(int argc, char *argv[])
{
	DB	*dbi;
	DB	*dbo;
	DBT	data;
	DBT	key;
	int	flag;
	long	copy_cou;
	long	rec_cou;
	int	dbtype = DB_BTREE;

	if (argc != 3)
	{
		printf("usage: dbcp input_table output_table\n");
		return(1);
	}
	dbi = dbopen(argv[1],O_RDONLY,0666,DB_BTREE,NULL);
	if (!dbi)
	{
		dbtype = DB_HASH;
		dbi = dbopen(argv[1],O_RDONLY,0666,DB_HASH,NULL);
	}
	if (!dbi)
	{
		fprintf(stderr,"Could not open \"%s\",%s\n",argv[1],
			strerror(errno));
		return(2);
	}
	dbo = dbopen(argv[2],O_RDWR|O_CREAT|O_TRUNC,0666,dbtype,NULL);
	if (!dbo)
	{
		fprintf(stderr,"Could not open \"%s\",%s\n",argv[2],
			strerror(errno));
		return(2);
	}

	for(copy_cou = 0, rec_cou = 0, flag = R_FIRST;
		dbi->seq(dbi,&key,&data,flag) == 0; flag = R_NEXT, rec_cou++)
	{
		if (data.size > 1)
		{
			dbo->put(dbo,&key,&data,R_NOOVERWRITE);
			copy_cou++;
		}
	}
	dbi->close(dbi);
	dbo->close(dbo);
	printf("%ld records of %ld have been copied.\n",copy_cou,rec_cou);

	return(0);
}

