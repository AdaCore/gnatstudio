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

#include "dbutils.h"

extern	int	optind;
#ifdef WIN32
extern	const char	*optarg;
#else
extern	char	*optarg;
#endif /* WIN32 */

static	char	*sep = ";";
extern	int	db_case_compare __P((const DBT *, const DBT *));

static	void
db_export(char *db_name, int csv, int data_flag, char fld_sep)
{
	DB	*dbi;
	DBT	data;
	DBT	key;
	int	flag;
	long	rec_cou;
	BTREEINFO info;
	char	*poi;
	char	*cvt;
	int		len;

	memset((char *)&info,0,sizeof(info));

	info.compare = db_case_compare;

	dbi = dbopen(db_name,O_RDONLY,0666,DB_BTREE,&info);
	if (!dbi)
	{
		HASHINFO	hinf;

		memset((char *)&hinf,0,sizeof(hinf));
		dbi = dbopen(db_name,O_RDONLY,0666,DB_HASH,&hinf);
		if (dbi && !csv)
		{
			printf("nelem: %d\n",hinf.nelem);
		}
	}

	if (!dbi)
	{
		fprintf(stderr,"dbopen error: %s\n",strerror(errno));
		exit(2);
	}
	key.data = NULL;
	key.size = 0;
	data.data = NULL;
	data.size = 0;
	for(rec_cou = 0, flag = R_FIRST;
		dbi->seq(dbi,&key,&data,flag) == 0; flag = R_NEXT, rec_cou++)
	{
		if (key.size <= 0 || !key.data)
		{
			printf("**** EMPTY KEY ****\n");
			continue;
		}
		
		poi = (char *)key.data + key.size - 1;
		*poi = '\0';
		if (data.size > 0)
		{
			poi = (char *)data.data + data.size - 1;
			*poi = '\0';
		}
		for (cvt = key.data, len = key.size; len--; cvt++)
		{
			if (*cvt == DB_FLDSEP_CHR)
				*cvt = fld_sep;
		}
		for (cvt = data.data, len = data.size; len--; cvt++)
		{
			if (*cvt == DB_FLDSEP_CHR)
				*cvt = fld_sep;
		}
		if (csv)
		{
			printf("%s%s%s\n",
				(char *)key.data, sep,
				data_flag && data.data && data.size ? (char *)data.data : "");
		}
		else
		{
			printf("%s|%s| Size: %d, Data_Len: %d\n",
				(char *)key.data,
				data_flag && data.data && data.size ? (char *)data.data : "",
				key.size,data.size);
		}
	}
	dbi->close(dbi);
}

static	int
get_record(fp,key,key_size,data,data_size)
FILE	*fp;
char	*key;
int	key_size;
char	*data;
int	data_size;
{
	int	c;

	for(; (c = getc(fp)) != EOF && c != ';' && c != '\n' && --key_size > 0; *key++ = c);
	*key = '\0';

	if (c != EOF && c != '\n')
	{
		for(; (c = getc(fp)) != EOF && c != '\n' && --data_size > 0; *data++ = c);
	}
	*data = '\0';

	return c;
}

static	void
db_import(db_name,db_type,trunc)
char	*db_name;
DBTYPE	db_type;
int	trunc;
{
	DB	*dbi;
	DBT	data;
	DBT	key;
	BTREEINFO info;
	char	keybuf[5000];
	char	databuf[100000];
	int	ret;
	int	flag = O_RDWR|O_CREAT;

	if (trunc)
		flag |= O_TRUNC;
	memset((char *)&info,0,sizeof(info));

	info.compare = db_case_compare;

	if (db_type == DB_BTREE)
	{
		info.cachesize = 3 * 1024 * 1024;
		dbi = dbopen(db_name,flag,0666,db_type,&info);
	}
	else
		dbi = dbopen(db_name,flag,0666,db_type,NULL);

	if (!dbi)
	{
		fprintf(stderr,"dbopen error: %s, errno:%d\n",
			strerror(errno),errno);
		exit(2);
	}

	while(get_record(stdin,keybuf,sizeof(keybuf),databuf,sizeof(databuf)) != EOF)
	{
		key.data = keybuf;
		key.size = strlen(key.data) + 1;
		data.data = databuf;
		data.size = strlen(data.data) + 1;

		ret = dbi->put(dbi,&key,&data,0);
		if (ret != 0)
		{
			printf("write error: ret: %d, %s\n",ret,strerror(errno));
			break;
		}
	}
	dbi->close(dbi);
}

int
main(int argc, char *argv[])
{
	int	csv = 1;
	int	import = 0;
	int	flag;
	DBTYPE	db_type = DB_HASH;
	int	data_flag = TRUE;
	int	trunc = TRUE;
	char fld_sep = ' ';

	Tcl_FindExecutable(argv[0]);

	while((flag = getopt(argc,argv,"s:libf:daSc:")) != EOF)
	{
		switch (flag)
		{
		case 'i':
			import = 1;
			break;

		case 's':
			sep=optarg;
			break;

		case 'l':
			csv = 0;
			break;

		case 'b':
			db_type = DB_BTREE;
			break;

		case 'f':
			freopen(optarg,"r",stdin);
			break;

		case 'd':
			data_flag = FALSE;
			break;

		case 'a':
			trunc = FALSE;
			break;

		case 'S':
			fld_sep = ' ';
			break;

		case 'c':
			fld_sep = *optarg;
			break;
		}
	}
	if (argc == optind)
	{
		fprintf(stderr,"Usage: %s [-s field-separator] [-d] [-l] file\n",
			argv[0]);
		return(2);
	}
	
	if (import)
		db_import(argv[optind],db_type,trunc);
	else
		db_export(argv[optind],csv,data_flag,fld_sep);

	return(0); 
}

