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
 * dbutils.c
 *
 * This version has been modified to support blanks and backslashes in file names
 * to do this the split character has been modified to be != SPACE
 */

#ifdef __MSVC__
#include <stdio.h>
#include <stdlib.h>
#endif
#include <unistd.h>
#include <stdlib.h>
#include "dbutils.h"
#include <setjmp.h>
#include <ctype.h>
#include "longstr.h"
#include "fileutils.h"

#include <tcl.h>

#ifndef MY_DEBUG2
#define MY_DEBUG(x)
#define MY_DEBUG2(x)
#define MY_DEBUG(x)
#endif

#if !defined (__MSVC__) && ! defined (HAVE_STRTOUL)
#define strtoul(s,p,b) (unsigned long)strtol(s,p,b)
#endif

#ifdef __MSVC__
#define use_STRNCASECMP strnicmp
#else
#define use_STRNCASECMP strncasecmp
#endif

/*
 * Don't exceed buffer size
 */
#define MY_STRNCPY(str1, str2, len) strncpy (str1, str2, len); str1[len-1] = 0;

#define	DEL_SEQ_VALUE	0			/* Search even when deleting sequentially!*/
/*#define	DEL_SEQ_VALUE	R_CURSOR */

#define	BUG_TRACE	0		/* Should be always 0 */

static	DB	*db_class_tree;
static	DB	*db_cached_classes;

#define	INH_AC_TYPE	(PAF_PRIVATE|PAF_PROTECTED|PAF_PUBLIC)

#ifndef WIN32
typedef FILE*   HANDLE;
#define INVALID_HANDLE_VALUE    (FILE *)0
#else
static  HANDLE  process_handle;
#endif /* WIN32 */

static  HANDLE  pipe_handle = INVALID_HANDLE_VALUE;

static DB *db_include;
static	char	**include_array;

int	Paf_dbimp_running = FALSE;
int	xref_fastupdate = FALSE;

int	 report_local_vars = FALSE;
FILE *cross_ref_fp;
int  comment_database = FALSE;

static int Paf_Pipe_Write MX_VARARGS(char *,str);
static int Paf_Pipe_Flush ();

static DB *db_syms[50];           /* Keep enough space */

#define CREATE_ALWAYS_BTREES 1

static	char	db_project_dir[MAXPATHLEN];
static	u_int	db_cachesize;
static	u_int	db_cross_cachesize;

#define DB_NO_CASE_COMPARE 1
int	db_case_compare (const DBT *, const DBT *);
int db_no_case_compare(const DBT *a,const DBT *b);
int db_compare_nocase = 0;
int db_action_is_fetching = 0; /* marks if we are in a fetching routine */

char *SN_StrDup(char*);

static  char    *acc_strings[] = {
	"r","w","p","u"
};

/* See paf.h for definitions! */
static char *SN_symbol_types[] = {
	"f", "t", "cl", "mi", "iv", "e", "con", "ma", "fu", "su",
	"gv", "com", "cov", "in", "fil", "by", "to","md","fd","ec",
	"un","fr","na","ex","lv","vd","iu","rem","cpp","ud",
	"xfi",NULL,NULL,NULL,NULL,NULL,NULL
};

static  DB      *db_scopes;

/*
 * get database permission from an environment, set in the gui
 */
int get_db_permission()
{
	char *p;
	int perm = 0660;
	p = getenv ("SN_DB_PERMS");
	if (p != NULL && *p != 0)
	{
		sscanf (p, "%o", &perm);
	}
	return perm;
}

/*
 *      We have to conver '\\' to '/'. That's all.
 */
char *
Paf_tempnam(char *dir,char *pref)
{
	static	char	tmpnm[MAXPATHLEN + 1];
	char    *nm;

	nm = tempnam(dir,pref);

	if (!nm)
		return nm;

	unlink(nm);     /* Just to be very sure !!! */

	strcpy (tmpnm, nm);
	sn_internal_convert_path (tmpnm, SN_PATH_UNIX);
	
	/* DON'T USE ckfree */
	free(nm);

	return tmpnm;
}

/*
 * We can't use Tcl_SplitList anymore, however we could use a trick here
 *
 *
 * WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
 *
 * This procedure uses a static string variable to store the results.
 * If you want to use it more than one time, you have to copy the
 * results before calling this function again.
 */
int
my_SplitList (char *str, int *num, char ***argvPtr, char sep)
{
	static char *lst=NULL;
	char *p, **argv;
	int cnt, i;
	int have_bracket;
	
	MY_DEBUG ((Output, "ping..<%s>\n", str));
	
	/*
	 * Use a static string pointer to store the splited string
	 */
	if (lst)
	{
		ckfree (lst);
		lst=NULL;
	}
	lst = SN_StrDup (str);
	
	/*
	 * Count how many fields
	 */
	for (have_bracket = 0, cnt=1, p=lst; *p; p++)
	{
		if (*p == '{')
		{
			have_bracket = 1;
		}
		if (*p == '}')
		{
			have_bracket = 0;
		}
		if (! have_bracket && *p == sep)
		{
			cnt ++;
		}
	}
	argv = (char**)ckalloc (sizeof (char*) * cnt);
	/*
	 * Set pointers and skip leading '{'
	 */
	for (argv[0] = lst, p=lst, i=0; *p; p++)
	{
		if (*p == '{')
		{
			have_bracket = 1;
			argv[i] ++; /* skip opening bracket */
			*p = 0;
		}
		if (*p == '}')
		{
			have_bracket = 0;
			*p = 0;
		}
		if (! have_bracket && *p == sep)
		{
			*p = 0;                        /* terminate string */
			argv[++i] = p+1;
		}
	}

	*argvPtr = argv;
	*num = cnt;

	return TCL_OK;
}

/*
 * Read next field in a database record, this is up to the end
 * of the string or when a split-chr has been reached.
 *
 * If nothing has been read, the same pointer will be return.
 *
 * str:  Points to the string with the db-fields.
 *		 It could contain \null pointer, then the last specified
 *       pointer will be taken to continue parsing
 * size: buffer size without terminated \null
 *       if size is equal 0, all characters will be reverted, this
 *       is usefull when a field must be skiped.
 * buf:  could be \null pointer
 */
static char *oldPtr=NULL;
char *
read_next_field (char *str, char *buf, int size, char sep)
{
	char *p=str, *q=buf;
	int i;
	
	if (q)
	{
		q[0] = 0;
	}
	
	/*
	 * Use stored pointer to continue parsing */
	if (p == NULL)
	{
		if (oldPtr == NULL)
		{
			return NULL;
		}
		p = oldPtr;
	}
	
	for(i=0; *p && *p != sep; p++)
	{
		/* skip the rest of string, if buffer exceeded */
		if (q && i < size)
		{
			*q++ = *p; i++;
		}
	}
	/*
	 * terminate constructed buffer
	 */
	if (q)
	{
		*q = 0;
	}
	
	/*
	 * Skip the separator
	 */
	if (*p == sep)
		p ++;
	
	/*
	 * Store last position for next stips */
	oldPtr = p;
	
	return p;
}
int
read_next_int_field (char *str, char sep)
{
	char tmp[64];
	read_next_field (str, tmp, sizeof(tmp), sep);
	return atoi (tmp);
}

/*
 * Create a new database file that reflect a table
 * Comments:
 * It's too bad to create a database for each used table in SN,
 * this is to be changed in 5.0
 */
static	DB *
create_table(int type,int mode,int cachesize)
{
	DB	*dbp;
	char	fname[MAXPATHLEN + 1];
	const void	*db_inf;
	int	db_type;
	HASHINFO	db_hash_info;
	BTREEINFO       db_btree_info;

	if (db_syms[type])	/* Don't open it twice ! */
		return db_syms[type];

#if CREATE_ALWAYS_BTREES
	db_type = DB_BTREE;
	memset((char *)&db_btree_info,0,sizeof(db_btree_info));
	db_btree_info.cachesize = cachesize;
	
#if _WINDOWS
	/*
	 * On windows we do support lower/upper case pathnames
	 */
	if (type == PAF_FILE)
	{
		db_btree_info.compare = db_no_case_compare;
	}
	else
	{
		db_btree_info.compare = db_case_compare;
	}
#else
	db_btree_info.compare = db_case_compare;
#endif

	db_inf = (void *)&db_btree_info;

	sprintf(fname,"%s.%s",db_project_dir,SN_symbol_types[type]);

	dbp = dbopen(fname, mode, get_db_permission(),db_type,db_inf);
	
	/*
	 * Backward compatiblility:
	 * if file exists it could be a hash table
	 */
	if (!dbp && access (fname, R_OK) == 0)
	{
		db_type = DB_HASH;
		memset((char *)&db_hash_info,0,sizeof(db_hash_info));
		db_hash_info.cachesize = cachesize;
		db_inf = (void *)&db_hash_info;
		dbp = dbopen (fname, mode, get_db_permission(), db_type, db_inf);
	}
#else
	if (type == PAF_FILE)
	{
		db_type = DB_HASH;

		memset((char *)&db_hash_info,0,sizeof(db_hash_info));

		db_hash_info.cachesize = cachesize;

		db_inf = (void *)&db_hash_info;
	}
	else
	{
		db_type = DB_BTREE;

		memset((char *)&db_btree_info,0,sizeof(db_btree_info));

		db_btree_info.cachesize = cachesize;
		db_btree_info.compare = db_case_compare;

		db_inf = (void *)&db_btree_info;
	}

	sprintf(fname,"%s.%s",db_project_dir,SN_symbol_types[type]);

	dbp = dbopen (fname, mode, get_db_permission(), db_type, db_inf);
#endif
	if (!dbp)
	{
		/* Display error only if we could not open the
		 * file and that should have not been created either.
		 */
		if ((errno != ENOENT || (mode & O_CREAT)))
		{
			fprintf(stderr,"create table %s: %s\n",fname,strerror(errno));

			Paf_panic(PAF_PANIC_SIMPLE);
		}
	}
	db_syms[type] = dbp;

	return dbp;
}

#define	PUR_DIR_TOO	0

/*
 * Adds a file into the file database "<db name>.f"
 * Format:
 *         <file>;<type> <modify time> ?<highlight file>?
 *
 * <highlight file> is optional
 *
 * Return values:
 *	1	Successful, and the file was not part of the project yet.
 *	0	Successful, and the file was already part of the project.
 *	else error.
 */
static	int
put_file_db(char *file_name,char *group,char *highlight_file)
{
	Tcl_DString filenameDStr;

	DB      *dbp = db_syms[PAF_FILE];
	DBT     data;
	DBT     key;
	int		found;
	int     ret;
	char		buf[MAXPATHLEN + 200];	/* 200: group + mtime */
#if PUR_DIR_TOO
	char    *p;
#endif /* PUR_DIR_TOO */

#ifdef  __MSVC__
	struct  _stat   stb;
#else
	struct  stat    stb;
#endif /* __MSVC__ */

	if (!dbp)
	{
		dbp = create_table(PAF_FILE,O_RDWR|O_CREAT,db_cachesize);
		if (!dbp)
			return -1;
	}

	Tcl_DStringInit(&filenameDStr);
	Tcl_UtfToExternalDString(NULL,
				 file_name, strlen(file_name), &filenameDStr);

#ifdef  __MSVC__
	if (_stat(Tcl_DStringValue(&filenameDStr), &stb) != 0)
#else
	if (stat(Tcl_DStringValue(&filenameDStr), &stb) != 0)
#endif
		 stb.st_mtime = 0;

	Tcl_DStringFree(&filenameDStr);

	/*
	 * Note: No need to add the special separator in this
	 * file, however to not be confused, the fields are
	 * separated with the defined separator
	 */
	sprintf(buf,"%s%c%lu%c%s",
		group,                        DB_FLDSEP_CHR,
		(unsigned long)stb.st_mtime,  DB_FLDSEP_CHR,
		highlight_file ? highlight_file : "");
	key.data = file_name;
	key.size = strlen(file_name) + 1;

	/*
	 * Is the file already in the table ? */
	found = dbp->get(dbp,&key,&data,0);
	if (found == -1)
	{
		return -1;
	}
	else if (found == 0)
	{
		char highfile[MAXPATHLEN];
		char *pData;
		
		/*
		 * skip type
		 */
		pData = read_next_field (data.data, NULL, 0, DB_FLDSEP_CHR);
		/*
		 * skip mtime
		 */
		pData = read_next_field (pData, NULL, 0, DB_FLDSEP_CHR);
		/*
		 * get the highlight file to remove it
		 */
		pData = read_next_field (pData, highfile, MAXPATHLEN-1, DB_FLDSEP_CHR);
		if (highfile[0])
		{
			unlink(highfile);       /* Old highlighting file. */
		}
	}

	data.data = buf;
	data.size = strlen(buf) + 1;

	ret = dbp->put(dbp,&key,&data,0);

	if (ret != 0)
	{
		fprintf(stderr,"put_file_db error: %s\n",strerror(errno));
		return ret;
	}
	return found;
}

/* This functions puts the file and its directory names
 * into the appropriate tables.
 */

int
put_file(char *file_name,char *group,char *highlight_file)
{
	if (!file_name)
	{
		fprintf(stderr,"put_file: empty file name\n");
		return -1;
	}

	if (pipe_handle != INVALID_HANDLE_VALUE)
	{
		Paf_Pipe_Write("%d%c%s%c%s%c%s\n",
			PAF_FILE,      KEY_DATA_SEP_CHR,
			file_name,     KEY_DATA_SEP_CHR,
			group,         DB_FLDSEP_CHR,
			highlight_file ? highlight_file : "");

		return Paf_Pipe_Flush();       /* 'dbimp' can start deleting. */
	}

	/*
	 * If the database import process (dbimp) is not running
	 * we insert the name of the highlighting file directly into
	 * the database.
	 */
	if (highlight_file && db_project_dir[0])
	{
		return put_file_db(file_name, group, highlight_file);
	}

	return 0;
}

int
put_comment(char *classn,char *func,char *filename,char *comment,int beg_line,int beg_char)
{
	register unsigned char  *p;

	if (!comment_database || !comment || !cross_ref_fp)
	{
		return 0;
	}

	/* We use cross_ref_fp because the comments should be inserted
	 * only during the second phase with the cross reference together.
	 */
	for (p = (unsigned char *)comment; *p; p++)
	{
		if (*p == '\n')
		{
			*p = 0xff;
		}
	}

	MY_DEBUG((Output, "put comment into file <%s>\n", filename));

	fprintf(cross_ref_fp,
		"%d%c%s%c%06d.%03d%c%s%c%s%c%s\n",
		PAF_COMMENT_DEF,                  KEY_DATA_SEP_CHR,
		filename,                         DB_FLDSEP_CHR,
		beg_line, beg_char,               DB_FLDSEP_CHR,
		classn && *classn ? classn : "#", DB_FLDSEP_CHR,
		func && *func ? func : "#",       KEY_DATA_SEP_CHR,
		comment);

	return 0;
}

int
put_symbol(
int     sym_type,
char    *scope_name,
char    *symbol_name,
char    *file_name,
int     start_lineno,
int     start_colpos,
int     end_lineno,
int     end_colpos,
unsigned long   attr,
char    *ret,
char    *arg_types,
char    *args,
char    *comment,
int     high_start_lineno,
int     high_start_colpos,
int     high_end_lineno,
int     high_end_colpos)
{
	char	*sym_str_type = SN_symbol_types[sym_type];

	if (!file_name)
	{
		fprintf(stderr,"Error: put_symbol argument file_name must not be NULL\n");
		fflush(stderr);
		return -1;
	}
	for (; isspace(*file_name); file_name++);

	if (!sym_str_type)
	{
		fprintf(stderr,"Error: put_symbol unknown type: %d file: %s\n",
			sym_type,file_name);
		fflush(stderr);
		return -1;
	}

	if (!symbol_name || !*symbol_name)
	{
		fprintf(stderr,
			"Error: put_symbol argument #3 must not be empty, type: (%s), line: %d file: %s\n",
			sym_str_type,start_lineno,file_name);
		fflush(stderr);
		return -1;
	}

	for (; isspace(*symbol_name); symbol_name++);

	/* If args is just a string we take a special name. */
	if (args && *args == '"')
	{
		args = "%STRING%";
	}

	if (comment_database && comment && *comment)
	{
		register char *p;

		for (p = comment; *p; p++)
		{
			switch (*p)
			{
			case    '\n':
				*p = (char )0xff;
				break;

			case    '{':
			case    '}':
				*p = DB_FLDSEP_CHR;
				break;
			}
		}
	}
	else
		comment = "";

	switch (sym_type)
	{
	case	PAF_TYPE_DEF:
	case	PAF_CLASS_DEF:
	case	PAF_ENUM_DEF:
	case	PAF_CONS_DEF:
	case	PAF_MACRO_DEF:
	case	PAF_FUNC_DEF:
	case	PAF_GLOB_VAR_DEF:
	case	PAF_FUNC_DCL:
	case	PAF_UNION_DEF:
	case	PAF_ENUM_CONST_DEF:
		scope_name = NULL;
		break;
	}

	/* Skip leading blanks */
	for (;scope_name && isspace(*scope_name); scope_name++);

	if (!scope_name || *scope_name == '\0')
	{
		scope_name = NULL;

		switch (sym_type)
		{
		case PAF_MBR_FUNC_DEF:
		case PAF_MBR_VAR_DEF:
		case PAF_COMMON_MBR_VAR_DEF:
		case PAF_CLASS_INHERIT:
		case PAF_MBR_FUNC_DCL:
/*		case PAF_ENUM_CONST_DEF: */
			fprintf(stderr,
				"Error: put_symbol argument #2 must not be empty, type: (%s), line: %d file: %s\n",
				sym_str_type,start_lineno,file_name);
			fflush(stderr);
			return -1;
			break;
		}
	}

	if (high_start_lineno == 0)	/* Take the symbol definition. */
	{
		high_start_lineno = start_lineno;
		high_start_colpos = start_colpos;
		high_end_lineno = start_lineno;
		high_end_colpos = start_colpos + strlen(symbol_name);
	}
	Paf_Pipe_Write("%d%c%s%s%s%c%06d.%03d%c%s%c%d.%d%c0x%x%c{%s}%c{%s}%c{%s}%c{%s}\n",
		sym_type,                        KEY_DATA_SEP_CHR,
		scope_name ? scope_name : "",
		scope_name ? DB_FLDSEP_STR : "",
		symbol_name,                     DB_FLDSEP_CHR,
		start_lineno, start_colpos,      DB_FLDSEP_CHR,
		file_name,                       KEY_DATA_SEP_CHR,
		end_lineno, end_colpos,          DB_FLDSEP_CHR,
		attr,                            DB_FLDSEP_CHR,
		ret ? ret : "",                  DB_FLDSEP_CHR,
		arg_types ? arg_types : "",      DB_FLDSEP_CHR,
		args ? args : "",                DB_FLDSEP_CHR,
		comment
		);

	Paf_Pipe_Write("%d%c%s%c%06d.%03d%c%s%c%s%c%s%c%d.%d%c%d.%d%c%d.%d%c{%s}\n",
		PAF_FILE_SYMBOLS,                     KEY_DATA_SEP_CHR,
		file_name,                            DB_FLDSEP_CHR,
		start_lineno, start_colpos,           DB_FLDSEP_CHR,
		scope_name ? scope_name : "#",        DB_FLDSEP_CHR,
		symbol_name,                          DB_FLDSEP_CHR,
		sym_str_type,                         KEY_DATA_SEP_CHR,
		end_lineno,        end_colpos,        DB_FLDSEP_CHR,
		high_start_lineno, high_start_colpos, DB_FLDSEP_CHR,
		high_end_lineno,   high_end_colpos,   DB_FLDSEP_CHR,
		arg_types ? arg_types : ""
		);

	return 0;
}

int
put_cross_ref(
int     type,
int     scope_type,
int     scope_lev,
char    *fnc_cls,              /* caller class */
char    *fnc,                   /* caller function/method */
char	*fnc_arg_types,         /* caller function/method argument types */
char    *scope,                 /* referenced class */
char	*what,                  /* referenced member */
char	*arg_types,		/* referenced function/method argument types */
char	*file,
int	lineno,
int	acc)
{
	LongString	key_value;
	LongString	data_value;
	char	lineno_buf[10];

	if ((!cross_ref_fp && !Paf_dbimp_running) || !fnc || *fnc == '\0' ||
		(scope_lev == PAF_REF_SCOPE_LOCAL && !report_local_vars))
	{
		return -1;
	}

	MY_DEBUG2 ((Output, "put_cross_ref (%i, %i, %i, %s, %s, %s, %s, %s, %s, %s, %i, %i)\n",
					type, scope_type, scope_lev,
					fnc_cls?fnc_cls:"?",
					fnc?fnc:"?",
					fnc_arg_types?fnc_arg_types:"?", scope?scope:"?", what?what:"?",
					arg_types?arg_types:"?",	file?file:"?", lineno, acc));

	if (scope && *scope == '\0')
		scope = NULL;
	else if (scope)
	{
		char *_p;
		for (; isspace(*scope); scope++);
		
		/*
		 * It can happen, that the scope contains "class fld ",
		 * so then terminate the rest after "class" */
		if ((_p=strchr (scope, DB_FLDSEP_CHR)))
		{
			*_p = 0;
		}
		/**/
		if (*scope == '\0')
			scope = NULL;
	}
	if (fnc_cls && *fnc_cls == '\0')
		fnc_cls = NULL;
	else if (fnc_cls)
	{
		for (; isspace(*fnc_cls); fnc_cls++);
	}

	if (!file)
	{
		fprintf(stderr,"Filename must not be NULL\n");
		return -1;
	}
	if (!what || !*what)
	{
		fprintf(stderr,"Input parameter (#8) must not be NULL file: %s line: %d\n",
				file,lineno);
		abort();
		return -1;
	}

	for (; isspace(*what); what++);
	for (; isspace(*file); file++);

	if (type <= 0 || type > PAF_REF_UNDEFINED)
	{
		fprintf(stderr,"Input parameter (#1) %d is not allowed\n",
			type);
		return -1;
	}

	if (scope_type <= 0 || scope_type >= PAF_VAR_DCL)
	{
		fprintf(stderr,"Input parameter (#2) %d is not allowed\n",
			scope_type);
		return -1;
	}

	LongStringInit(&key_value,0);
	LongStringInit(&data_value,0);

	if (type == PAF_MBR_FUNC_DCL)
		type = PAF_MBR_FUNC_DEF;

	sprintf(lineno_buf,"%06d",lineno);
	key_value.copystrings(&key_value,
		fnc_cls ? fnc_cls : "#",            DB_FLDSEP_STR,
		fnc,                                DB_FLDSEP_STR,
		SN_symbol_types[scope_type],       DB_FLDSEP_STR,
		scope ? scope : "#",                DB_FLDSEP_STR,
		what,                               DB_FLDSEP_STR,
		SN_symbol_types[type],             DB_FLDSEP_STR,
		acc_strings[acc],                   DB_FLDSEP_STR,
		lineno_buf,                         DB_FLDSEP_STR,
		file,
		NULL);
	data_value.copystrings(&data_value,
		fnc_arg_types ? fnc_arg_types : "", DB_FLDSEP_STR,
		arg_types ? arg_types : NULL,
		NULL);

	if (Paf_dbimp_running)
	{
		MY_DEBUG2((Output, "xref entry <%s> <%s>\n", key_value.buf, data_value.buf));
	
		db_insert_entry(PAF_CROSS_REF, key_value.buf, data_value.buf);
	}
	else if (!scope || *scope != '?')
	{
		fprintf(cross_ref_fp, "%d%c%s%c%s\n",
			PAF_CROSS_REF, KEY_DATA_SEP_CHR,
			key_value.buf, KEY_DATA_SEP_CHR,
			data_value.buf);
	}

	key_value.free(&key_value);
	data_value.free(&data_value);

	return 0;
}

void Paf_Close_Include_Dirs()
{
	if (include_array)
	{
		char	**ip;

		for (ip = include_array; *ip; ip++)
		{
			ckfree(*ip);
		}
		ckfree ((char *)include_array);
		include_array = NULL;
	}

	if (db_include)
	{
		db_include->close(db_include);
		db_include = NULL;
	}
}

int
Paf_db_close_tables()
{
	DB      *dbp;
	int     cou;
	int     max;
	int		ret = 0;
	int		saved_errno = 0;
	int		fd;

	for (cou = 0, max = sizeof(db_syms) / sizeof(*db_syms); cou < max; cou++)
	{
		dbp = db_syms[cou];

		if (dbp)
		{
			fd = dbp->fd(dbp);
			if (dbp->close(dbp) == -1)
			{
				ret = -1;
				saved_errno = errno;
				close(fd);
			}
			db_syms[cou] = NULL;
		}
	}
	if (db_class_tree)
	{
		char	fname[MAXPATHLEN];

		fd = db_class_tree->fd(db_class_tree);
		if (db_class_tree->close(db_class_tree) == -1)
		{
			ret = -1;
			saved_errno = errno;
			close(fd);
		}

		sprintf(fname,"%s.ctr",db_project_dir);
/*		unlink(fname); */
	}
	if (db_cached_classes)
	{
		char	fname[MAXPATHLEN];

		fd = db_cached_classes->fd(db_cached_classes);
		if (db_cached_classes->close(db_cached_classes) == -1)
		{
			ret = -1;
			saved_errno = errno;
			close(fd);
		}

		sprintf(fname,"%s.xhs",db_project_dir);
		unlink(fname);
	}

	if (db_scopes)
	{
		fd = db_scopes->fd(db_scopes);
		if (db_scopes->close(db_scopes) == -1)
		{
			ret = -1;
			saved_errno = errno;
			close(fd);
		}
	}

	errno = saved_errno;

	return ret;
}

static void
db_remove_comment_def(int softdel,char *file)
{
	DB      *dbp = db_syms[PAF_COMMENT_DEF];
	DBT     data;
	DBT     key;
	char    filename[MAXPATHLEN];
	int     flag;
	unsigned int    cmp_len;

	if (!dbp)
		return;

	sprintf (filename, "%s%c",file, DB_FLDSEP_CHR);
	cmp_len = strlen(filename);
	key.data = filename;
	key.size = cmp_len;
	for (flag = R_CURSOR; dbp->seq(dbp,&key,&data,flag) == 0; flag = R_NEXT)
	{
		if ((int)key.size < cmp_len || memcmp(key.data,filename,cmp_len) != 0)
		{
			break;
		}
		dbp->del(dbp,&key,DEL_SEQ_VALUE);
	}
}

void
db_remove_file_def(int softdel,char *file)
{
	DB      *dbp = db_syms[PAF_FILE_SYMBOLS];
	DB      *db_del = NULL;
	DBT     data;
	DBT     key;
	DBT     sc_key;
	DBT     sc_data;
	int     flag;
	unsigned int    cmp_len;
	char    filename[MAXPATHLEN + 1];
	LongString	delkey;
	LongString	pars;
	int     scope_val = -1;
	int     del;
	int     del_fil;

	printf("Deleting %s\n",file);	/* Informs SN which files is being deleted. */
	fflush(stdout);

	if (!dbp)
	{
		dbp = create_table(PAF_FILE_SYMBOLS,O_RDWR,db_cachesize);
		if (!dbp)
			return;

		if (!db_syms[PAF_COMMENT_DEF])
			create_table(PAF_COMMENT_DEF,O_RDWR,db_cachesize);
	}


	db_remove_comment_def(softdel,file);

	if (!db_scopes)
	{
		char	**scopep;

		db_scopes = dbopen (NULL, O_RDWR|O_CREAT, get_db_permission(), DB_HASH, NULL);
		data.data = (char *)&scope_val;
		data.size = sizeof(scope_val);
		for (scope_val = PAF_FILE, scopep = SN_symbol_types; *scopep;
			scopep++,scope_val++)
		{
			key.data = *scopep;
			key.size = strlen(key.data);
			if (key.size > 0)
			{
				if (db_scopes->put(db_scopes,&key,&data,R_NOOVERWRITE) == -1)
				{
					fprintf(stderr,"Write error: %s\n",strerror(errno));
				}
			}
		}
	}

	LongStringInit(&delkey,0);
	LongStringInit(&pars,0);

	/*
	 * Add separator at the end of file to unique identify it's records
	 */
	sprintf(filename,"%s%c", file, DB_FLDSEP_CHR);
	cmp_len = strlen(filename);
	key.data = filename;
	key.size = cmp_len;
	data.data = NULL;
	data.size = 0;
	for (flag = R_CURSOR; dbp->seq(dbp,&key,&data,flag) == 0; flag = R_NEXT)
	{
		if ((int)key.size < cmp_len || memcmp(key.data,filename,cmp_len) != 0)
		{
			break;
		}
		
		/*
		 * get scope name (at the end of the key field list)
		 */
		sc_key.data = strrchr(key.data, DB_FLDSEP_CHR) + 1;
		sc_key.size = strlen(sc_key.data);
		/*
		 * verify if this scope exists
		 */
		if (db_scopes->get(db_scopes,&sc_key,&sc_data,0) != 0)
		{
			fprintf(stderr,"unknown scope: \"%s\" in \"%s\"\n",
				(char *)sc_key.data,(char *)key.data);
			continue;
		}

		pars.split(&pars, key.data, key.size - 1, 0, DB_FLDSEP_CHR, -1);

		memcpy((char *)&scope_val,sc_data.data,sc_data.size);
		switch (scope_val)
		{
		case PAF_EXCEPTION_DEF:
		case PAF_MBR_FUNC_DEF:
		case PAF_MBR_FUNC_DCL:
		case PAF_FRIEND_DCL:
		case PAF_MBR_VAR_DEF:
		case PAF_COMMON_MBR_VAR_DEF:
		case PAF_CLASS_INHERIT:
		case PAF_LOCAL_VAR_DEF:
			/* Key format: class member lineno filename */
			delkey.copy(&delkey,
				pars.field_value[2],
				pars.field_size[2]);

			delkey.append (&delkey, DB_FLDSEP_STR, -1); /* Separator */

			delkey.append(&delkey,
				pars.field_value[3],
				pars.field_size[3]);

			delkey.append(&delkey, DB_FLDSEP_STR, -1); /* Separator */

			delkey.append(&delkey,
				pars.field_value[1],
				pars.field_size[1]);

			delkey.append(&delkey, DB_FLDSEP_STR, -1); /* Separator */

			delkey.append(&delkey,
				file,
				cmp_len -1);
			break;

		default:
			/* Key format: symbol lineno filename */
			delkey.copy(&delkey,
				pars.field_value[3],
				pars.field_size[3]);
			delkey.append(&delkey, DB_FLDSEP_STR, -1); /* Separator */

			delkey.append(&delkey,
				pars.field_value[1],
				pars.field_size[1]);
			delkey.append(&delkey, DB_FLDSEP_STR, -1); /* Separator */

			delkey.append(&delkey,
				file,
				cmp_len -1);
			break;
		}

		sc_key.data = (void *)delkey.buf;
		sc_key.size = delkey.len + 1;

		if (!(db_del = db_syms[scope_val]))
		{
			db_del = create_table(scope_val,O_RDWR,db_cachesize);
			if (!db_del)
			{
				continue;
			}
		}
		del_fil = dbp->del(dbp,&key,DEL_SEQ_VALUE);
		del = db_del->del(db_del,&sc_key,0);

		/* Send message if something went wrong except we could
		 * not delete a local variable from its table. */
		if (del_fil != 0 || (del != 0 && scope_val != PAF_LOCAL_VAR_DEF))
		{
			fprintf(stderr,"DELETING of <%s> with size: %d type <%s> returned: %d,%d\n",
				(char *)sc_key.data,
				sc_key.size,
				SN_symbol_types[scope_val],
				del_fil,
				del);
		}
	}

	delkey.free(&delkey);
	pars.free(&pars);
}

#define DB_XREF_FLD_SEP_STR DB_FLDSEP_STR

void
db_remove_file_xfer_using_keys(int softdel, char *key_files)
{
	FILE	*fp;
	DB		*dbp = db_syms[PAF_CROSS_REF];
	DB		*dbp_by = db_syms[PAF_CROSS_REF_BY];
	DBT		key;
	DBT		by_key;
	DBT		data;
	LongString key_to;
	LongString key_by;
	LongString file_del_key;
	LongString data_buf;
	char	last_del_fname[MAXPATHLEN];
	int flag;
	char *pfn;
	char *fn;
	char *ref_type;
	void *key_buf;
	int key_len;
	int line_cou = 1;
	int delete_file = TRUE;
	
	LongStringInit(&key_to,0);
	LongStringInit(&data_buf,0);
	LongStringInit(&key_by,0);
	LongStringInit(&file_del_key,0);

	if (!key_files || *key_files == '\0' || (fp = fopen(key_files, "r")) == NULL)
		return;	/* Should never happen. */

	if (!dbp)
	{
		dbp = create_table(PAF_CROSS_REF,O_RDWR,
			db_cross_cachesize);		/* Open the table ! */

		if (!dbp)
			return;
	}

	if (!dbp_by)
	{
		dbp_by = create_table(PAF_CROSS_REF_BY,O_RDWR,
			db_cross_cachesize);		/* Open the table ! */
	}

	for (last_del_fname[0] = '\0', line_cou = 1; file_del_key.fgets(&file_del_key, fp); line_cou++)
	{
		if (file_del_key.split(&file_del_key,
			file_del_key.buf,
			file_del_key.len,
			TRUE,
			(int)';',
			-1) != 5)
		{
			continue;
		}

		fn = file_del_key.field_value[3];
		if (strcmp(last_del_fname, fn) != 0)
		{
			printf("Deleting %s\n", fn);
			fflush(stdout);
			strcpy(last_del_fname, fn);
		}

		ref_type = file_del_key.field_value[2];
		/*
		 * Remove references in the bodies of "md" | "mi" | "fu" | "su" !
		 */
		if ((ref_type[0] == 'm' && (ref_type[1] == 'd' || ref_type[1] == 'i')) ||
			((ref_type[0] == 'f' || ref_type[0] == 's') && ref_type[1] == 'u'))
		{
			key_to.copystrings(&key_to,
				file_del_key.field_value[0], DB_FLDSEP_STR,
				file_del_key.field_value[1], DB_FLDSEP_STR,
				file_del_key.field_value[2], DB_FLDSEP_STR,
				NULL);
			
			key.data = (void *)key_to.buf;
			key.size = key_to.len;
			key_buf = key_to.buf;
			key_len = key_to.len;

			for (flag = R_CURSOR; dbp->seq(dbp,&key,&data,flag) == 0; flag = R_NEXT)
			{
				if ((int)key.size < key_len || memcmp(key_buf, key.data, key_len) != 0)
				{
					break;
				}
				/* The filename starts after the last separator in the key. */
				for (pfn = (char *)key.data + key.size - 1; pfn > (char*)key.data && *pfn != DB_FLDSEP_CHR; pfn--);
				if (strcmp(pfn + 1, fn) != 0)
				{
					continue;		/* An other file, don't delete the record! */
				}

				/* Create the key for the "by" record !*/
				data_buf.split(&data_buf,
					key.data, key.size - 1, FALSE, DB_FLDSEP_CHR, -1);

				ref_type = data_buf.field_value[5];
				
				/*
				 * There is no xref "by" info for local variables. !
				 */
				if (dbp_by && (ref_type[0] != 'l' || ref_type[1] != 'v'))
				{
					key_by.copy(&key_by,
						data_buf.field_value[3],
						data_buf.field_size[3] + data_buf.field_size[4] + data_buf.field_size[5] + 3);

					key_by.append(&key_by,
						data_buf.field_value[0],
						data_buf.field_size[0] + data_buf.field_size[1] + data_buf.field_size[2] + 3);

					key_by.append(&key_by,
						data_buf.field_value[6],
						data_buf.field_size[6] + data_buf.field_size[7] + data_buf.field_size[8] + 2);

					by_key.data = (void *)key_by.buf;
					by_key.size = key_by.len + 1;

					if (dbp_by->del(dbp_by,&by_key,0) != 0)
					{
						fprintf(stdout,"Delete (BY) not found <%s>\n",(char *)by_key.data);
					}
				}

				dbp->del(dbp,&key,DEL_SEQ_VALUE);
			}
		}
	}
	fclose(fp);

	file_del_key.free(&file_del_key);
	key_to.free(&key_to);
	data_buf.free(&data_buf);
	key_by.free(&key_by);

	/* Delete the file only if its contains was ok, otherwise we need a chance
	 * too be able to look. */
	if (delete_file)
		unlink(key_files);
}

void
db_insert_entry(int type,char *key_buf,char *data_buf)
{
	register unsigned char *p;
	DB      *dbp = db_syms[type];
	DBT     data;
	DBT     key;
	LongString tmp;
	LongString xref_data;
	LongString xref;
	LongString xref_data_fields;

	if (type == PAF_FILE)
	{
		char    group[80];
		char    highfile[MAXPATHLEN];
		int		state;

		group[0] = '\0';
		highfile[0] = '\0';

		/*
		 * read group and highlight file */
		read_next_field (data_buf, group, 80, DB_FLDSEP_CHR);
		read_next_field (NULL, highfile, MAXPATHLEN, DB_FLDSEP_CHR);
		if (highfile[0])
			p = (unsigned char *)highfile;
		else
			p = NULL;

		state = put_file_db(key_buf,group,p);
		if (state == 0)
		{
			db_remove_file_def(0,key_buf);
		}

		return;
	}

	if (!dbp)
	{
		int csize;

		if (type == PAF_CROSS_REF)
		{
			csize = db_cross_cachesize;
		}
		else
		{
			csize = db_cachesize;
		}

		dbp = create_table(type,O_RDWR|O_CREAT,csize);		/* Open the table ! */
		if (!dbp)
			return;
	}

	if (type != PAF_CROSS_REF)
	{
		key.data = key_buf;
		key.size = strlen(key.data) + 1;
		data.data = data_buf;
		for (p = (unsigned char *)data.data; *p; p++)
		{
			if (*p == 0xff)
				*p = '\n';
		}
		data.size = (char *)p - (char *)data.data + 1;
		if(dbp->put(dbp,&key,&data,0) == -1)
		{
			Paf_panic(PAF_PANIC_EMERGENCY);
		}
		return;
	}

	LongStringInit(&xref,0);
	LongStringInit(&xref_data,0);
	LongStringInit(&xref_data_fields,0);
	LongStringInit(&tmp,0);

	xref_data_fields.split (&xref_data_fields,
		data_buf,
		-1,
		FALSE,
		DB_FLDSEP_CHR,
		-1);

	xref_data.copy(&xref_data,
		"{", -1);

	xref_data.append (&xref_data,
		xref_data_fields.field_value[0],
		xref_data_fields.field_size[0]);

	xref_data.appendstrings(&xref_data, "}", DB_FLDSEP_STR, "{", NULL);

	xref_data.append (&xref_data,
		xref_data_fields.field_value[1],
		xref_data_fields.field_size[1]);

	xref_data.append (&xref_data, "}", -1);

	data.data = (void *)xref_data.buf;
	data.size = xref_data.len + 1;

	/* If a parser could not figure out a scope of a member because
	 * for example an inherited class was not known, we can fix it
	 * here.
	 */
	if (*key_buf == '?')
	{
		static int opened = FALSE;
		char arg_types[10000] = {0};
		char ret_type [1024] = {0};
		char this_cls [1024] = {0};
		char name     [1024] = {0};
		char scope_buf[1024] = {0};

		if (!opened)
		{
			open_tables_for_cross_ref();
			opened = TRUE;
		}

		MY_DEBUG((Output, "%s\n", key_buf));

		/*
		 * skip field
		 */
		read_next_field (key_buf, NULL, 0, DB_FLDSEP_CHR);
		/*
		 * Name */
		read_next_field (NULL, name, sizeof(name), DB_FLDSEP_CHR);
		/*
		 * Skip field */
		read_next_field (NULL, NULL, 0, DB_FLDSEP_CHR);
		/*
		 * Class */
		read_next_field (NULL, this_cls, sizeof (this_cls), DB_FLDSEP_CHR);

		get_symbol (this_cls,
			NULL,
			name,
			arg_types,
			scope_buf,
			ret_type,
			NULL,
			1);

		/* switch "?" with the returned class name */
		if (*scope_buf != '\0' && *scope_buf != '?')
		{
			int len = strlen(scope_buf);
			memmove (key_buf+len-1, key_buf, strlen (key_buf) - len);
			memcpy(key_buf, scope_buf, len);
		}

		MY_DEBUG2 ((Output, "split key <%s>\n", key_buf));

		/* Format:
		 * Class Method Type DeclClass Var VarType Line File
		 * Example:RectShape draw mi ? chr1 iv 000003 RectShape.java
		 */
		tmp.split(&tmp, key_buf, key.size, FALSE, DB_FLDSEP_CHR, 7);

		tmp.append(&tmp,
			tmp.field_value[3],
			tmp.field_size[3] + tmp.field_size[4] + tmp.field_size[5] + 3);

		tmp.append(&tmp,
			tmp.field_value[0],
			tmp.field_size[0] + 1);

		tmp.append(&tmp,
			tmp.field_value[1],
			tmp.field_size[1] + tmp.field_size[2] + 2);

		tmp.append(&tmp,
			tmp.field_value[6],
			tmp.field_size[6]);

		key.data = tmp.buf;
		key.size = tmp.len + 1;
	}
	else
	{
		key.data = key_buf;
		key.size = strlen(key.data) + 1;
	}

	if(dbp->put(dbp,&key,&data,0) == -1)
	{
		Paf_panic(PAF_PANIC_EMERGENCY);
	}


	MY_DEBUG2 ((Output, "split data <%s>\n", key.data));

	tmp.split (&tmp,key.data, key.size -1, FALSE, DB_FLDSEP_CHR, -1);

	if (tmp.field_value[5][0] != SN_symbol_types[PAF_REF_TO_LOCAL_VAR][0] ||
		tmp.field_value[5][1] != SN_symbol_types[PAF_REF_TO_LOCAL_VAR][1])
	{
		dbp = db_syms[PAF_CROSS_REF_BY];
		if (!dbp)
		{
			dbp = create_table(PAF_CROSS_REF_BY,O_RDWR|O_CREAT,
				db_cross_cachesize);		/* Open the table ! */
			if (!dbp)
			{
				xref_data.free(&xref_data);
				xref.free(&xref);
				tmp.free(&tmp);

				return;
			}
		}

		xref.copy(&xref,
			tmp.field_value[3],
			tmp.field_size[3] + tmp.field_size[4] + tmp.field_size[5] + 3);

		xref.append(&xref,
			tmp.field_value[0],
			tmp.field_size[0] + tmp.field_size[1] + tmp.field_size[2] + 3);

		xref.append(&xref,
			tmp.field_value[6],
			tmp.field_size[6] + tmp.field_size[7] + tmp.field_size[8] + 2);

		key.data = (void *)xref.buf;
		key.size = xref.len + 1;

		xref_data.copy(&xref_data,
			"{", -1);

		xref_data.append(&xref_data,
			xref_data_fields.field_value[1],
			xref_data_fields.field_size[1]);

		xref_data.appendstrings (&xref_data, "}", DB_FLDSEP_STR, "{", NULL);

		xref_data.append(&xref_data,
			xref_data_fields.field_value[0],
			xref_data_fields.field_size[0]);

		xref_data.append(&xref_data, "}", -1);

		data.data = (void *)xref_data.buf;
		data.size = xref_data.len + 1;

		if(dbp->put(dbp,&key,&data,0) == -1)
		{
			Paf_panic(PAF_PANIC_EMERGENCY);
		}
	}
	xref_data_fields.free(&xref_data_fields);
	xref_data.free(&xref_data);
	xref.free(&xref);
	tmp.free(&tmp);
}

void
Paf_Open_Include_Dirs(char *inf_name,char *db_prefix)
{
	FILE	*include_fp;
	int	opt;
	char	*fname;
	char	tmp[MAXPATHLEN];

	include_fp = fopen(inf_name,"r");
	if (!include_fp)
		return;

#define INC_ARR_ICR 50

	for (opt = 0; fgets(tmp,sizeof(tmp) -1,include_fp);)
	{
		if ((fname = strchr(tmp,'\n')))
		{
			*fname = '\0';
		}
		if (!*tmp || *tmp == '#')
			continue;

		if ((opt % INC_ARR_ICR) == 0)
		{
			if (!include_array)
			{
				include_array = (char **)ckalloc((INC_ARR_ICR + 1) * sizeof(char *));
			}
			else
			{
				include_array = (char **)ckrealloc((char*)include_array,
								(opt + INC_ARR_ICR + 1) * sizeof(char *));
			}
		}
		include_array[opt++] = SN_StrDup(tmp);
		include_array[opt] = NULL;
	}
	fclose(include_fp);

	if (include_array)
	{
		BTREEINFO	inf;

		include_array[opt] = NULL;

		memset((char *)&inf,0,sizeof(inf));
		inf.cachesize = (u_int)db_cachesize;
		inf.compare = db_case_compare;

		sprintf(tmp,"%s.icl",db_prefix);
		db_include = dbopen (tmp, O_RDWR, get_db_permission(), DB_BTREE, &inf);
	}
}

char *
Paf_Search_Include_dir(char *name)
{
	char   *incl_with_path = NULL;
	static char incl_file[MAXPATHLEN];
	DBT key;
	DBT data;
	char *p;
	char *bfn;		/* Base filename */
	char	dirn[MAXPATHLEN];
	char	filename[MAXPATHLEN];
	char **ip;
	int path_argc = 0;
	char **path_argv = NULL;
	int	cou;
	int	len;
	int flag;
	int	cpy_len;
	
	sn_internal_convert_path (name, SN_PATH_UNIX);
	if (! (bfn = file_lastroot (name)))
	{
		bfn = name;
	}
	else
	{
		bfn ++;
	}

	if (!db_include)
	{
		return name;
	}

	if (bfn == name)
	{
		dirn[0] = '\0';
	}
	else
	{
		len = bfn - name - 1;
		memcpy(dirn,name,len);
		dirn[len] = '\0';
	}
	sprintf(filename,"%s%c", bfn, DB_FLDSEP_CHR);
	key.data = filename;
	key.size = strlen(filename);
	len = key.size;
	for (flag = R_CURSOR; db_include->seq(db_include,&key,&data,flag) == 0; flag = R_NEXT)
	{
		if ((int)key.size < len || memcmp(filename,key.data,len) != 0)
		{
			break;
		}

		p = strchr((char *)key.data, DB_FLDSEP_CHR);
		if (!p)
			continue;		/* Should never happen !!! */
		*p = '\0';
		sprintf (incl_file, "%s/%s", p + 1, (char *)key.data);
		*p++ = DB_FLDSEP_CHR;		/* Restore separator and point to the directory name ! */

		/* Does it still exist ? */
		if (access(incl_file,0) != 0)
		{
			db_include->del(db_include,&key,0);
			db_include->sync(db_include,0);	/* Now, we can even crash. */

			continue;
		}
		if (path_argv)
		{
			path_argv = (char **)ckrealloc((char*)path_argv,(path_argc + 1) * sizeof(char *));
		}
		else
		{
			path_argv = (char **)ckalloc(sizeof(char *));
		}
		cpy_len = key.size - (p - (char *)key.data);
		path_argv[path_argc] = ckalloc(cpy_len);
		memcpy(path_argv[path_argc],p,cpy_len);
		path_argc++;
	}

	
	/*
	if (!path_argc)
	{
		return name;
	}
	*/

	if (path_argc == 1)
	{
		/* Onle one file has been found, we just take it. */
		if (path_argv[0][0] == '.')
		{
			strcpy(incl_file,bfn);
		}
		else
		{
			sprintf(incl_file,"%s/%s",path_argv[0],bfn);
		}
		incl_with_path = incl_file;
	}
	else if (path_argc > 1)
	{
		int found = 0;
		/*
		 * More than one file have been found, we have to find the
		 * right one.
		 */
		for (ip = include_array; *ip; ip++)
		{
			for (cou = 0; cou < path_argc; cou++)
			{
				if (**ip != path_argv[cou][0])
				{
					continue;
				}

				if (dirn[0])
				{
					sprintf(filename,"%s/%s",*ip,dirn);
					p = filename;
				}
				else
				{
					p = *ip;
				}
				if (strcmp(p,path_argv[cou]) == 0)
				{
					if (**ip == '.')
						strcpy(incl_file,bfn);
					else
						sprintf(incl_file,"%s/%s",p,bfn);
					incl_with_path = incl_file;

					found = 1;
					break;
				}
			}
			if (found)
				break;
		}
		/*
		 * take the first one, if nothing found
		 */
		if (! found)
		{
			sprintf (incl_file, "%s/%s", path_argv[0], bfn);
			incl_with_path = incl_file;
		}
	}
	else
	{
		/*
		 * as last possibility, we should look at the file if it
		 * exists on the desk
		 */
		for (ip = include_array; *ip; ip++)
		{
			sprintf (incl_file, "%s/%s", *ip, bfn);
			if (access (incl_file, R_OK) == 0)
			{
				incl_with_path = incl_file;
				break;
			}
		}
	}
	
	if (path_argv)
	{
		for (cou = 0; cou < path_argc; cou++)
			ckfree(path_argv[cou]);
		ckfree((char*)path_argv);
	}
	
	if (incl_with_path)
	{
		return incl_with_path;
	}

	return name;
}

#define	OPT_CLASS_TREE	0

/*
 * This function copies the exisinting members of the inherited
 * class. The newly created members will be known in the new class
 * too.
 */
static void
inherit_members(char *class_name,char *inherited_class, char *inh_buf)
{
	DB	*dbp = db_class_tree;
#if OPT_CLASS_TREE
	DB *db_mbr = db_syms[PAF_MBR_FUNC_DCL];
#endif /* OPT_CLASS_TREE */
	DBT	key;
	DBT	data;
	DBT	mbr_key;
	DBT	mbr_data;
	char	*inh_cls_name;
	char	*inh_symbol_type;
	char	*inh_access;
	char	*inh_mbr_type;
	char	*nm;
	char	*mbr_name;
	char	*rest;
	unsigned int	flag;
	unsigned int	len;
	unsigned long inh_ac_tp;
	unsigned	long output_ac_tp;
	LongString	tmp_class_name;
	LongString	data_buf;
	LongString	key_buf;
	LongString	key_pars;
	LongString	inh_pars;
	char	inh_access_tmp[100];

	output_ac_tp = strtoul(inh_buf,NULL,16);

	LongStringInit(&tmp_class_name,0);
	LongStringInit(&key_buf,0);
	LongStringInit(&data_buf,0);
	LongStringInit(&key_pars,0);
	LongStringInit(&inh_pars,0);

	tmp_class_name.appendstrings(&tmp_class_name, inherited_class, DB_FLDSEP_STR,
		NULL);
	nm = tmp_class_name.buf;
	len = tmp_class_name.len;

	key.data = (void *)nm;
	key.size = len;
	for (flag = R_CURSOR; dbp->seq(dbp,&key,&data,flag) == 0; flag = R_NEXT)
	{
		if ((int)key.size < len || memcmp(nm,key.data,len) != 0)
		{
			break;
		}
		inh_pars.split (&inh_pars, (char*)data.data, data.size-1, TRUE,
						DB_FLDSEP_CHR, 4);

		inh_cls_name = inh_pars.field_value[0];
		inh_symbol_type = inh_pars.field_value[1];
		inh_access = inh_pars.field_value[2];
		inh_mbr_type = inh_pars.field_value[3];

		/* Private members cannot be inherited ! */
		inh_ac_tp = strtoul(inh_access,NULL,16);
		if ((inh_ac_tp & INH_AC_TYPE) == PAF_PRIVATE)
		{
			continue;
		}

		switch (output_ac_tp)
		{
		case PAF_PRIVATE:
			inh_ac_tp = PAF_PRIVATE;
			break;

		case PAF_PROTECTED:
			inh_ac_tp = PAF_PROTECTED;
			break;

		case PAF_PUBLIC:	/* Take the read value ! */
			break;
		}

		key_pars.split (&key_pars, key.data, key.size-1, TRUE, DB_FLDSEP_CHR, 3);
		mbr_name = key_pars.field_value[1];
		rest = key_pars.field_value[2];

		key_buf.copystrings(&key_buf,
			class_name, DB_FLDSEP_STR,
			mbr_name,   DB_FLDSEP_STR,
			NULL);

#if OPT_CLASS_TREE
		/* Don't load members of base classes that exist in the superclass . */
		if (inh_symbol_type[0] == SN_symbol_types[PAF_MBR_FUNC_DCL][0]
			&& db_mbr)		/* Search for method of the superclass. */
		{
			/* We load (inherit) the member only if the superclass
			 * does not contain such a member.
			 */
			char	*keyp = key_buf.buf;
			int	kln = key_buf.len;

			mbr_key.data = (void *)keyp;
			mbr_key.size = kln;
			mbr_data.data = NULL;
			mbr_data.size = 0;
			if (db_mbr->seq(db_mbr,&mbr_key,&mbr_data,R_CURSOR) == 0 &&
				mbr_key.size >= kln && memcmp(mbr_key.data,keyp,kln) == 0)
			{
				/* The superclass has such a member. */
				continue;
			}
		}
#endif /* OPT_CLASS_TREE */
		key_buf.append(&key_buf,rest,-1);

		sprintf(inh_access_tmp, "0x%lx%c", inh_ac_tp, DB_FLDSEP_CHR);
		data_buf.copystrings (&data_buf,
			inh_cls_name,    DB_FLDSEP_STR,
			inh_symbol_type, DB_FLDSEP_STR,
			inh_access_tmp,
			inh_mbr_type,
			NULL);

		mbr_key.data = (void *)key_buf.buf;
		mbr_key.size = key_buf.len + 1;
		mbr_data.data = (void *)data_buf.buf;
		mbr_data.size = data_buf.len + 1;

		if (dbp->put(dbp,&mbr_key,&mbr_data,0) == -1)
		{
			Paf_panic(PAF_PANIC_EMERGENCY);
		}
	}
	key_pars.free(&key_pars);
	inh_pars.free(&inh_pars);
	key_buf.free(&key_buf);
	key_buf.free(&key_buf);
	data_buf.free(&data_buf);
	tmp_class_name.free(&tmp_class_name);
}

/*
 * This functions reads the members of a class from a '.iv' or '.md'
 * tables.
 */
static void
load_class_members(int type, char *class_name)
{
	DB	*dbp = db_syms[type];
	DBT	key;
	DBT	data;
	DBT	mbr_key;
	DBT	mbr_data;
	char	*symbol_type = SN_symbol_types[type];
	char	*mbr_type;
	int	mbr_type_len;
	char	*mbr_pars;
	int	mbr_pars_len;
	char	*inher_access;
	int	inher_access_len;
	LongString	tmp_class_name;
	LongString key_buf;
	LongString data_buf;
	char	*nm;
	u_int	flag;
	unsigned int	len;
	char	*pend;
	int	mbr_name_beg,mbr_name_end;

	if (!dbp)
		return;

	LongStringInit(&tmp_class_name,0);
	LongStringInit(&key_buf,0);
	LongStringInit(&data_buf,0);

	tmp_class_name.copystrings(&tmp_class_name,class_name," ",NULL);
	nm = tmp_class_name.buf;
	len = tmp_class_name.len;

	key.data = (void *)nm;
	key.size = len;
	data.data = NULL;
	data.size = 0;
	for (flag = R_CURSOR; dbp->seq(dbp,&key,&data,flag) == 0; flag = R_NEXT)
	{
		if ((int)key.size < len || memcmp(nm,key.data,len) != 0)
		{
			break;
		}
		/*
		 * Skip field */
		read_next_field (key.data, NULL, 0, DB_FLDSEP_CHR);
		/*
		 * read integer field */
		mbr_name_beg = read_next_int_field (NULL, DB_FLDSEP_CHR);
		/*
		 * Skip field */
		read_next_field (NULL, NULL, 0, DB_FLDSEP_CHR);
		/*
		 * read integer field */
		mbr_name_end = read_next_int_field (NULL, DB_FLDSEP_CHR);

		inher_access = strchr (data.data, DB_FLDSEP_CHR) + 1;
		pend = strchr (inher_access, DB_FLDSEP_CHR);
		inher_access_len = pend - inher_access;

		mbr_type = strchr(pend + 1,'{') + 1;
		pend = strchr(mbr_type,'}');
		mbr_type_len = pend - mbr_type;

		mbr_pars = strchr(pend + 1,'{') + 1;
		pend = strchr(mbr_pars,'}');
		mbr_pars_len = pend - mbr_pars;
/*
 * Key format: class_name member_name parameter_list
 * Data format: class_name sybmbol_type (e.g. md or iv)	inher_access type (e.g int)
 */
		key_buf.copystrings(&key_buf, class_name, DB_FLDSEP_STR, NULL);

		key_buf.append(&key_buf,
			(char *)key.data + mbr_name_beg, mbr_name_end - mbr_name_beg);

		key_buf.append(&key_buf,mbr_pars,mbr_pars_len);

		data_buf.copystrings(&data_buf,
			class_name,  DB_FLDSEP_STR,
			symbol_type, DB_FLDSEP_STR,
			NULL);

		data_buf.append(&data_buf,
			inher_access, inher_access_len);

		data_buf.append(&data_buf, DB_FLDSEP_STR, -1);
		data_buf.append(&data_buf, mbr_type, mbr_type_len);
		
		mbr_key.data = (void *)key_buf.buf;
		mbr_key.size = key_buf.len + 1;
		mbr_data.data = (void *)data_buf.buf;
		mbr_data.size = data_buf.len + 1;

		if (db_class_tree->put(db_class_tree,&mbr_key,&mbr_data,0) == -1)
		{
			Paf_panic(PAF_PANIC_EMERGENCY);
		}
	}
	key_buf.free(&key_buf);
	data_buf.free(&data_buf);
	tmp_class_name.free(&tmp_class_name);
}

static int
store_symbol_to_cache(char *name,int sym_type,char *origin)
{
	DBT	key;
	DBT	data;
	int ret;
	LongString buf;
	char	smt = (char)sym_type;

	LongStringInit(&buf,0);

	buf.append(&buf,&smt,1);	/* A '\0' will be appended too. */
	if (origin)
	{
		buf.append(&buf,origin,-1);
		data.size = buf.len + 1;	/* Include '\0' too ! */
	}
	else
	{
		data.size = 2;		/* Include '\0' too! */
	}
	key.data = (void *)name;
	key.size = strlen(name);
	data.data = (void *)buf.buf;

	ret = db_cached_classes->put(db_cached_classes,&key,&data,
		R_NOOVERWRITE);
	if (ret == -1)
	{
		Paf_panic(PAF_PANIC_EMERGENCY);
	}

	buf.free(&buf);

	return ret;
}

/*
 * This function loads a class including its base classes.
 */
static void
load_class(char *class_name)
{
	DB	*dbp = db_cached_classes;
	DBT	key;
	DBT	data;
	LongString	tmp_class_name;
	char	*nm;
	unsigned int	len;

	key.data = (void *)class_name;
	key.size = strlen(class_name);

	if (dbp->get(dbp,&key,&data,0) == 0)
	{
		return;		/* It has already been loaded, don't do it again! */
	}

	dbp = db_syms[PAF_CLASS_DEF];
	if (!dbp)
		return;

	LongStringInit(&tmp_class_name,0);

	tmp_class_name.copystrings(&tmp_class_name,class_name," ",NULL);
	nm = tmp_class_name.buf;
	len = tmp_class_name.len;

	key.data = (void *)nm;
	key.size = len;
	if (dbp->seq(dbp,&key,&data,R_CURSOR) != 0 ||
		(int)key.size < len  || memcmp(nm,key.data,len) != 0)
	{
		tmp_class_name.free(&tmp_class_name);
		return;		/* The class does not exist. */
	}

	store_symbol_to_cache(class_name,PAF_CLASS_DEF,NULL);

	dbp = db_syms[PAF_CLASS_INHERIT];
	if (dbp)
	{
		LongString	tmp_inherited_class;
		LongString	inh_buf;
		char	*icls;
		char	*inhb;
		unsigned int	flag;

		LongStringInit(&tmp_inherited_class,0);
		LongStringInit(&inh_buf,0);

		for (flag = R_CURSOR;
			dbp->seq(dbp,&key,&data,flag) == 0 &&
			key.size >= len && memcmp(nm,key.data,len) == 0; flag = R_NEXT)
		{
			tmp_inherited_class.split(&tmp_inherited_class,
				key.data,
				key.size -1,
				TRUE,
				DB_FLDSEP_CHR,
				-1);
			icls = tmp_inherited_class.field_value[1];

			inh_buf.split(&inh_buf,
				data.data,
				data.size -1,
				TRUE,
				DB_FLDSEP_CHR,
				-1);
			inhb = inh_buf.field_value[1];

			load_class(icls);

			inherit_members(class_name,icls,inhb);

			dbp->seq(dbp,&key,&data,R_CURSOR);	/* Restore the cursor ! */
		}

		tmp_inherited_class.free(&tmp_inherited_class);
		inh_buf.free(&inh_buf);
	}
	tmp_class_name.free(&tmp_class_name);

	load_class_members(PAF_MBR_VAR_DEF, class_name);
	load_class_members(PAF_MBR_FUNC_DCL, class_name);
}

void
open_tables_for_cross_ref()
{
	char	fname[MAXPATHLEN];
	BTREEINFO	db_inf;
	HASHINFO	ha_inf;

	memset((char *)&db_inf,0,sizeof(db_inf));
	db_inf.cachesize = (u_int)db_cross_cachesize;
	db_inf.compare = db_case_compare;

	sprintf(fname,"%s.ctr",db_project_dir);

	db_class_tree = dbopen(fname, O_RDWR|O_CREAT|O_TRUNC, get_db_permission(), DB_BTREE,
		&db_inf);
	if (!db_class_tree)
	{
		fprintf(stderr,"dbimp: %s, %s",fname,strerror(errno));
		fflush(stderr);

		Paf_panic(PAF_PANIC_SIMPLE);
	}

	memset((char *)&ha_inf,0,sizeof(ha_inf));
	ha_inf.cachesize = (u_int)db_cross_cachesize;
	ha_inf.nelem = 10000;

	sprintf(fname,"%s.xhs",db_project_dir);
	db_cached_classes = dbopen(fname,O_RDWR|O_CREAT|O_TRUNC, get_db_permission(), DB_HASH,
		&ha_inf);
	if (!db_cached_classes)
	{
		fprintf(stderr,"dbimp: %s, %s\n",fname,strerror(errno));
		fflush(stderr);

		Paf_panic(PAF_PANIC_SIMPLE);
	}

	create_table(PAF_CLASS_DEF,O_RDONLY,db_cachesize);
	create_table(PAF_TYPE_DEF,O_RDONLY,db_cachesize);
	create_table(PAF_MBR_VAR_DEF,O_RDONLY,db_cachesize);
	create_table(PAF_CONS_DEF,O_RDONLY,db_cachesize);
	create_table(PAF_MACRO_DEF,O_RDONLY,db_cachesize);
	create_table(PAF_FUNC_DEF,O_RDONLY,db_cachesize);
	create_table(PAF_FUNC_DCL,O_RDONLY,db_cachesize);
	create_table(PAF_GLOB_VAR_DEF,O_RDONLY,db_cachesize);
	create_table(PAF_CLASS_INHERIT,O_RDONLY,db_cachesize);
	create_table(PAF_MBR_FUNC_DCL,O_RDONLY,db_cachesize);
	create_table(PAF_ENUM_CONST_DEF,O_RDONLY,db_cachesize);
	create_table(PAF_ENUM_DEF,O_RDONLY,db_cachesize);
#if PAF_UNION_DEF != PAF_CLASS_DEF
	create_table(PAF_UNION_DEF,O_RDONLY,db_cachesize);
#endif /* PAF_UNION_DEF != PAF_CLASS_DEF */
}

#undef USE_LEVELS
#ifdef USE_LEVELS
#define LEVELS 4
#else
#define LEVELS 2
#endif

struct	CIgnored_Words {
	char	*wrd;
	int	size;
};

static struct CIgnored_Words CIgnoredWords[] =
{
	{"const",5},
	{"unsigned",8},
	{NULL,0}
};

static int RemoveIgnoredWords (char *target_buf, char *pb)
{
	char	*target = target_buf;
	char	*pe;
	int len;
	char	fc;
	struct	CIgnored_Words	*ci;

	for (pe = pb; *pe && (len = strcspn(pb, " ,")); pb = pe + 1)
	{
		pe = pb + len;
		fc = *pb;
		for (ci = &CIgnoredWords[0]; ci->size; ci++)
		{
			if (*ci->wrd == fc && ci->size == len &&
				memcmp (pb, ci->wrd,len) == 0)
			{
				goto next;
			}
		}
		memcpy(target,pb,len + 1);
		target += len + 1;
next:
		;
	}

	if (target == target_buf)
	{
		*target = '\0';
		return 0;
	}
	return target - target_buf - 1;
}

#define	PAF_CLASS_TREE -1

#define	RETURN_FROM_SEARCH(value) \
	{\
		buf.free(&buf);\
		rounded_arg_types.free(&rounded_arg_types);\
		cls_name.free(&cls_name);\
		tmp.free(&tmp);\
		return (value);\
	}

static int
search_for_symbol(char *global_class_name,char *local_class_name,
	char *name,char *arg_types,int db_type,char *scope,char *ret_type,
	char *macro_value,int exact)
{
	DB	*dbp;
	DBT	key;
	DBT	data;
	int	length;
	char	in_access_val[80];
	char	sym_type[50];
	char param_args[2048];
	char	*bufval;
	LongString buf;
	LongString rounded_arg_types;
	LongString cls_name;
	LongString tmp;
	int	flag;
	int	cmp = 1;
	int	fetch;
	int arg_types_len;
	int num;
	char **fields;
#if BUG_TRACE
	static	FILE	*trace_fp;

	if (!trace_fp)
	{
		trace_fp = fopen("/tmp/dbutils.log","w+");
		chmod("/tmp/dbutils.log",0666);
	}
#endif /* BUG_TRACE */

	LongStringInit(&buf,0);
	LongStringInit(&rounded_arg_types,0);
	LongStringInit(&cls_name,0);
	LongStringInit(&tmp,0);

	if (db_type == PAF_CLASS_TREE)
	{
		dbp = db_class_tree;
	}
	else
	{
		dbp = db_syms[db_type];
	}

	if (!dbp)
		return FALSE;

	if (db_type == PAF_CLASS_TREE)
	{
		buf.copystrings(&buf,
			local_class_name
				? local_class_name
				: global_class_name, DB_FLDSEP_STR,
			name,                    DB_FLDSEP_STR,
			arg_types ? arg_types : NULL,
			NULL);
		length = buf.len + 1;
	}
	else
	{
		buf.appendstrings (&buf,name, DB_FLDSEP_STR, NULL);
		length = buf.len;
	}

	bufval = buf.buf;
	arg_types_len = arg_types ? strlen(arg_types) : 0;
	key.data = (void *)bufval;
	key.size = (u_int)length;
	data.data = NULL;
	data.size = 0;
	fetch = dbp->seq(dbp,&key,&data,R_CURSOR);

#if BUG_TRACE
	fprintf(trace_fp,"Search for: <%s> return: %d ARGS: <%s>\n",
		 buf.buf,
		 fetch,
		 arg_types ? arg_types : "NULL");
#endif /* BUG_TRACE */

	if (fetch == -1)		/* Error */
	{
		RETURN_FROM_SEARCH(FALSE);
	}
	if (fetch == 1 || (int)key.size <  length ||
		memcmp(buf.buf,key.data,length) != 0)
	{
		if (db_type == PAF_CLASS_TREE && !exact && arg_types)
		{
#if !OPT_CLASS_TREE
			DB	*db_md = db_syms[PAF_MBR_FUNC_DCL];
#endif /* !OPT_CLASS_TREE */
			char	*cl_nm_p;
			int Round;
			/* If might happen that we cannot find a method
			 * because of typecasting problems. In this case, we
			 * check whether the fetched or the previous
			 * record matches the target method name. */
			cls_name.copystrings(&cls_name,
				local_class_name
					? local_class_name
					: global_class_name, DB_FLDSEP_STR,
				name,                    DB_FLDSEP_STR,
				NULL);
			length = cls_name.len;
			cl_nm_p = cls_name.buf;

			rounded_arg_types.makespace (&rounded_arg_types, arg_types_len);
			rounded_arg_types.len = RemoveIgnoredWords(rounded_arg_types.buf,arg_types);

#if !OPT_CLASS_TREE
			/* Search for the method in the current class, (not in base classes) */
			if (db_md)
			{
				int function_avail = 0;

				for (Round = 1; Round < LEVELS; Round++)
				{
					key.data = (void *)cl_nm_p;
					key.size = (u_int)length;
					data.data = NULL;
					data.size = 0;

					/* search for method in the base class! */
					for (flag = R_CURSOR; db_md->seq(db_md, &key, &data, flag)== 0; flag = R_NEXT)
					{
						/* Still target class and method name ? */
						if ((int)key.size < length + 1 || memcmp (cl_nm_p, key.data, length) != 0)
							break;

						function_avail = 1;

						/*
						 * Format:
						 *
						 *          <line> <access> <return type> <parameters> <parameter names>
						 */
						my_SplitList (data.data, &num, &fields, DB_FLDSEP_CHR);
						if (num >= 4)
						{
							MY_STRNCPY (in_access_val, fields[1], sizeof (in_access_val));
							MY_STRNCPY (ret_type,      fields[2], 1024); /* FIXME. */
							MY_STRNCPY (param_args,    fields[3], sizeof (param_args));
						}
						else
						{
							in_access_val[0] = 0;
							ret_type     [0] = 0;
							param_args   [0] = 0;
						}
						ckfree ((char*)fields);
						
						tmp.makespace (&tmp,data.size);
						tmp.len = RemoveIgnoredWords (tmp.buf, param_args);
#if BUG_TRACE
						fprintf(trace_fp,"method type: <%s> <%s> <%s>\n",
							tmp.buf,cl_nm_p,(char *)data.data);
#endif /* BUG_TRACE */
						if (tmp.len == rounded_arg_types.len &&
							memcmp(tmp.buf,rounded_arg_types.buf,tmp.len) == 0)
						{
							/* method found and arguments are similar. */
							memcpy(arg_types, param_args, strlen (param_args)+1);

							if (scope)
							{
								sscanf(cl_nm_p,"%s",scope);
								MY_DEBUG ((Output, "sscanf used by cl_nm_p <%s> Scope <%s>\n", cl_nm_p, scope));
							}
#if BUG_TRACE
							fprintf(trace_fp,"1 scope: <%s> access: <%s> ret_type: <%s> arg_types: <%s>\n",
								scope ? scope : NULL,in_access_val,ret_type,arg_types);
#endif /* BUG_TRACE */
							RETURN_FROM_SEARCH(PAF_MBR_FUNC_DEF);
						}
					}
				}
				/*
				 * If method is availiable, we don't search in base classes,
				 * we just take the first method of the class.
				 */
				if (function_avail)
				{
					key.data = (void *)cl_nm_p;
					key.size = (u_int)length;
					data.data = NULL;
					data.size = 0;
					if (db_md->seq (db_md, &key, &data, R_CURSOR) != 0)
					{
						RETURN_FROM_SEARCH(FALSE);
					}

					/*
					 * format:
					 *          <line> <access> <return type> <parameters> <parameter names>
					 */
					my_SplitList (data.data, &num, &fields, DB_FLDSEP_CHR);
					if (num >= 4)
					{
						MY_STRNCPY (in_access_val, fields[1], sizeof (in_access_val));
						MY_STRNCPY (ret_type,      fields[2], 1024);
						MY_STRNCPY (arg_types,     fields[3], 10000);
					}
					else
					{
						in_access_val[0] = 0;
						ret_type     [0] = 0;
						arg_types    [0] = 0;
					}
					ckfree ((char*)fields);

					if (scope)
					{
						sscanf(cl_nm_p,"%s",scope);
						MY_DEBUG ((Output, "sscanf used 2 by cl_nm_p <%s> scope <%s>\n", cl_nm_p, scope));
					}
#if BUG_TRACE
					fprintf(trace_fp,"2 scope: <%s> access: <%s> ret_type: <%s> arg_types: <%s>\n",
						scope ? scope : NULL,in_access_val,ret_type,arg_types);
#endif /* BUG_TRACE */

					RETURN_FROM_SEARCH(PAF_MBR_FUNC_DEF);
				}
			}
#endif /* !OPT_CLASS_TREE */

			/* Search for the method in the base classes with similar parameters! */
			for (Round = 1; Round < LEVELS; Round++)
			{
				/* search method exactly in the base classes */
				key.data  = (void *)cl_nm_p;
				key.size  = (u_int)length;
				data.data = NULL;
				data.size = 0;
				for (flag = R_CURSOR; dbp->seq(dbp, &key, &data, flag)== 0; flag = R_NEXT)
				{
					/* Still the method being searching for ? */
					if ((int)key.size < length + 1 || memcmp (cl_nm_p, key.data, length) != 0)
						break;

					tmp.makespace(&tmp,key.size);
					tmp.len = RemoveIgnoredWords (tmp.buf,(char *)key.data + length);

					if (tmp.len == rounded_arg_types.len &&
						memcmp(tmp.buf,rounded_arg_types.buf,tmp.len) == 0)
					{
						/* It will copy the terminating '\0' too. */
						memcpy(arg_types, (char*)key.data + length, key.size - length);

						/*
						 * format
						 *
						 *           <scope> <type> <access> <return type> <parameters> <parameter names>
						 */
						my_SplitList (data.data, &num, &fields, DB_FLDSEP_CHR);
						if (num >= 4)
						{
							MY_STRNCPY (scope,         fields[0], sizeof(scope));
							MY_STRNCPY (sym_type,      fields[1], sizeof(sym_type));
							MY_STRNCPY (in_access_val, fields[2], sizeof(in_access_val));
							MY_STRNCPY (ret_type,      fields[3], 1024);
						}
						else
						{
							scope        [0] = 0;
							sym_type     [0] = 0;
							in_access_val[0] = 0;
							ret_type     [0] = 0;
						}
						ckfree ((char*)fields);
						
#if BUG_TRACE
						fprintf(trace_fp,"3 scope: <%s> access: <%s> ret_type: <%s> arg_types: <%s>\n",
							scope ? scope : NULL,in_access_val,ret_type,arg_types);
#endif /* BUG_TRACE */

						RETURN_FROM_SEARCH(PAF_MBR_FUNC_DEF);
					}
				}
			}

			/* No method matches the argument list, thus we just take the
			 * first one.
			 */
			key.data  = (void *)cl_nm_p;
			key.size  = (u_int)length;
			data.data = NULL;
			data.size = 0;
			if (dbp->seq(dbp, &key, &data, R_CURSOR) != 0 ||
				(int)key.size < length || memcmp(cl_nm_p, key.data, length) != 0)
			{
				RETURN_FROM_SEARCH(FALSE);
			}

			/* The terminating '\0' will be copied too. */
			memcpy(arg_types,(char*)key.data + length,key.size - length);
		}
		else
		{
			RETURN_FROM_SEARCH(FALSE);
		}
	}

	if (db_type == PAF_CLASS_TREE)
	{
		LongString pars;

		LongStringInit(&pars,0);

		pars.split (&pars, data.data, data.size-1, FALSE, DB_FLDSEP_CHR, 4);

		memcpy(scope, pars.field_value[0], pars.field_size[0]);
		scope[pars.field_size[0]] = '\0';

		memcpy(sym_type, pars.field_value[1], pars.field_size[1]);
		sym_type[pars.field_size[1]] = '\0';

		memcpy (in_access_val, pars.field_value[2], pars.field_size[2]);
		in_access_val[pars.field_size[2]] = '\0';

		memcpy(ret_type,
			pars.field_value[3],
			pars.field_size[3]);
		ret_type[pars.field_size[3]] = '\0';

		pars.free(&pars);

		if (sym_type[0] == SN_symbol_types[PAF_MBR_VAR_DEF][0])		/* "iv" ? */
		{
			RETURN_FROM_SEARCH(PAF_MBR_VAR_DEF);
		}
#if BUG_TRACE
		fprintf(trace_fp,"4 name: <%s> scope: <%s> access: <%s> ret_type: <%s> arg_types: <%s>\n",
			buf.buf,
			scope ? scope : NULL,
			in_access_val,
			ret_type,
			arg_types ? arg_types : "NULL");
#endif /* BUG_TRACE */
		/* At this point we might return a method with wrong
		 * argument list, but it is still better than saying that
		 * the method is not known.
		 */
		RETURN_FROM_SEARCH(PAF_MBR_FUNC_DEF);
	}

	/* If we are searching for a function we have to check its
	 * input arguments' types. (ANSI).
	 */
	fetch = 0;
	do
	{
		/* Parse for example: line.col attr {struct fp *} {unsigned char *,const int} */
		*in_access_val = '\0';
		*ret_type = '\0';
		
		/*
		 * Field has the format, separated with DB_FLDSEP_CHR:
		 *
		 *    <line> <access> <return type> <paramaeter list> <parameter variables>
		 *
		 */
		my_SplitList (data.data, &num, &fields, DB_FLDSEP_CHR);
		if (num >= 4)
		{
			MY_STRNCPY (in_access_val, fields[1], sizeof (in_access_val));
			MY_STRNCPY (ret_type,      fields[2], 1024);
			MY_STRNCPY (param_args,    fields[3], sizeof (param_args));
			if (arg_types && arg_types_len == (int)strlen(param_args) &&
				memcmp (param_args, arg_types, arg_types_len) == 0)
			{
				ckfree ((char*)fields);
				cmp = 0;
				break;
			}
			else
				cmp = 1;
		}
		ckfree ((char*)fields);
	} while(arg_types &&
		(fetch = dbp->seq(dbp,&key,&data,R_NEXT)) == 0 &&
		(int)key.size >= length &&
		(cmp = memcmp(bufval,key.data,length)) == 0);

#define	MACRO_XREF 1
#if MACRO_XREF
	/* If the macro name and its contents are identical, we don't
	 * return the contents because that could cause an end less loop.
	 */
	if (db_type == PAF_MACRO_DEF && macro_value)
	{
		strcpy(macro_value, ret_type);
	}
#endif /* MACRO_XREF */

	if (arg_types)
	{
		/*
		 * Here we take the last found function with its arguments.
		 */
		strcpy (arg_types, param_args);
	}

#if BUG_TRACE
		fprintf(trace_fp,"5 func: <%s> access: <%s> ret_type: <%s> arg_types: <%s>\n",
			buf.buf,in_access_val,ret_type,arg_types ? arg_types : "NULL");
		fflush(trace_fp);
#endif /* BUG_TRACE */

	if (exact && arg_types && (fetch != 0 || cmp != 0))
	{
		RETURN_FROM_SEARCH(FALSE);
	}

	/* At this point we might return a function with wrong
	 * argument list, but it is still better than saying that
	 * the function is not known.
	 */
	RETURN_FROM_SEARCH(db_type);
}

static struct check_symbol_types {
	int	db_type;
	int	needs_class;
	int	needs_args;
} symbol_types_seq[] = {
	{PAF_CLASS_TREE, TRUE, TRUE},			/* Methods. */
	{PAF_CLASS_TREE, TRUE, FALSE},		/* Inst. variables. */
	{PAF_FUNC_DEF, FALSE, TRUE},
	{PAF_FUNC_DCL, FALSE, TRUE},
	{PAF_MACRO_DEF, FALSE, TRUE},		/* Macros suchas DEF() */
	{PAF_MACRO_DEF, FALSE, FALSE},	/* Macros such as BUFSIZE */
	{PAF_GLOB_VAR_DEF, FALSE, FALSE},
	{PAF_CONS_DEF, FALSE, FALSE},
	{PAF_ENUM_CONST_DEF, FALSE, FALSE}
};

static int check_class_typedeff_enum_union[] = {
	PAF_CLASS_DEF,PAF_ENUM_DEF,PAF_UNION_DEF,PAF_TYPE_DEF,0
};

int
get_class_or_typedef(char *name, char *origin)
{
	int	sym_type;
	char	dummy[1000];
	DBT	key;
	DBT	data;
	int	*tp;

/* Check it in the hash table. */
	key.data = (void *)name;
	key.size = strlen(name);
	if (db_cached_classes->get(db_cached_classes,&key,&data,0) == 0)
	{
		char	*p = (char *)data.data;
		sym_type = (int)p[0];
		memcpy(origin,&p[1],data.size - 1);	/* That will copy a '\0' too. */

		return sym_type;
	}

	*origin = '\0';
	*dummy = '\0';
	for (sym_type = 0, tp = check_class_typedeff_enum_union;
		!sym_type && *tp; tp++)
	{
		sym_type = search_for_symbol(
			NULL,
			NULL,
			name,
			NULL,
			*tp,
			dummy,
			origin,
			NULL,
			TRUE);
	}

	if (sym_type == PAF_CLASS_DEF)
	{
		load_class(name);
	}

	return sym_type;
}

int
get_symbol(char *global_class_name,char *local_class_name,char *name,
	char *arg_types,char *scope,char *ret_type,char *macro_value,int exact)
{
	int	cou;
	int	sym_type;
	struct	check_symbol_types	*sym;

	*scope = '\0';
	*ret_type = '\0';

	if (local_class_name)
	{
		if (*local_class_name == '\0')
			local_class_name = NULL;
		else
			load_class(local_class_name);
	}
	if (global_class_name)
	{
		if (*global_class_name == '\0')
			global_class_name = NULL;
		else
			load_class(global_class_name);
	}

	cou = sizeof(symbol_types_seq) / sizeof(struct check_symbol_types);
	for (sym_type = FALSE, sym = &symbol_types_seq[0]; !sym_type && cou-- > 0; sym++)
	{
		if ((sym->needs_class && !global_class_name && !local_class_name) ||
			(sym->needs_args && !arg_types) || (local_class_name && !sym->needs_class))
		{
			continue;
		}

		sym_type = search_for_symbol(
			sym->needs_class ? global_class_name : NULL,
			sym->needs_class ? local_class_name : NULL,
			name,
			arg_types,
			sym->db_type,
			scope,
			ret_type,
			macro_value,
			exact);
	}

/*	It still can be a constructor. */
	if (!sym_type && arg_types)
	{
		sym_type = search_for_symbol(
			NULL,
			name,
			name,
			arg_types,
			PAF_CLASS_TREE,
			scope,
			ret_type,
			macro_value,
			exact);
	}

	return sym_type;
}

void
Paf_db_init_tables(char *proj_dir,char *cache,char *cross_cache)
{
	if (proj_dir)
	{
		strcpy(db_project_dir,proj_dir);
	}
	if (cache)
	{
		db_cachesize = (u_int)(atoi(cache) * 1024);
	}
	else
	{
		db_cachesize = 0;
	}
	if (cross_cache)
	{
		db_cross_cachesize = (u_int)(atoi(cross_cache) * 1024);
	}
	else
	{
		db_cross_cachesize = 0;
	}
}

int
Paf_Pipe_Create(char *pipe_cmd,char *db_prefix,char *incl_to_pipe,
	char *cache,char *sn_host, char *sn_pid)
{
	pid_t   pid = -1;
	char    tmp[1024];
	
#ifdef WIN32
	int             err;
	HANDLE          read_handle;
	STARTUPINFO startInfo;
	PROCESS_INFORMATION procInfo;
	SECURITY_ATTRIBUTES secAtts;
#else
	int             fd;
	int             pfd[2];
	pid_t   fork();
	char	*argv[50];
	int	argc;
#endif /* WIN32 */

	/* Check whether the file exists! */
	if (access(pipe_cmd,F_OK) != 0)
	{
		return -1;
	}

#ifdef WIN32
	secAtts.nLength = sizeof(SECURITY_ATTRIBUTES);
	secAtts.lpSecurityDescriptor = NULL;
	secAtts.bInheritHandle = FALSE;

	if (!CreatePipe(&read_handle, &pipe_handle, &secAtts, 0))
	{
		errno = GetLastError();
		printf("Error: (CreatePipe) error: %d\n",errno);
		fflush(stdout);
		return -1;
	}
	startInfo.cb = sizeof(startInfo);
	startInfo.lpReserved = NULL;
	startInfo.lpDesktop = NULL;
	startInfo.lpTitle = NULL;
	startInfo.dwX = startInfo.dwY = 0;
	startInfo.dwXSize = startInfo.dwYSize = 0;
	startInfo.dwXCountChars = startInfo.dwYCountChars = 0;
	startInfo.dwFillAttribute = 0;
	startInfo.dwFlags = STARTF_USESTDHANDLES;
	startInfo.wShowWindow = 0;
	startInfo.cbReserved2 = 0;
	startInfo.lpReserved2 = NULL;

	secAtts.nLength = sizeof(SECURITY_ATTRIBUTES);
	secAtts.lpSecurityDescriptor = NULL;
	secAtts.bInheritHandle = TRUE;

	DuplicateHandle(GetCurrentProcess(),
			 (HANDLE) read_handle,
			 GetCurrentProcess(), &startInfo.hStdInput, 0, TRUE,
			 DUPLICATE_SAME_ACCESS);

	DuplicateHandle(GetCurrentProcess(),
			 GetStdHandle(STD_OUTPUT_HANDLE),
			 GetCurrentProcess(), &startInfo.hStdOutput, 0, TRUE,
			 DUPLICATE_SAME_ACCESS);

	DuplicateHandle(GetCurrentProcess(),
			 GetStdHandle(STD_ERROR_HANDLE),
			 GetCurrentProcess(), &startInfo.hStdError, 0, TRUE,
			 DUPLICATE_SAME_ACCESS);

	/*
	 * Constract the command line so that argmuents or the command
	 * itself is masked with ".." when it contains blanks
	 */
	tmp[0] = 0;
	sn_append_option_to_command_line (tmp, pipe_cmd);

	if (cache)
	{
		sn_append_option_to_command_line (tmp, "-c");
		sn_append_option_to_command_line (tmp, cache);
	}

	if (sn_host)
	{
		sn_append_option_to_command_line (tmp, "-H");
		sn_append_option_to_command_line (tmp, sn_host);
	}

	if (sn_pid)
	{
		sn_append_option_to_command_line (tmp, "-P");
		sn_append_option_to_command_line (tmp, sn_pid);
	}

	sn_append_option_to_command_line (tmp, db_prefix);

	memset((char *)&procInfo,0,sizeof(procInfo));

	if (!CreateProcess(NULL,tmp,NULL,NULL,TRUE,DETACHED_PROCESS,
		NULL,NULL,&startInfo, &procInfo))
	{
		err = GetLastError();
	}
	else
		err = 0;
	CloseHandle(read_handle);
	CloseHandle(startInfo.hStdInput);
	CloseHandle(startInfo.hStdOutput);
	CloseHandle(startInfo.hStdError);
	CloseHandle(procInfo.hThread);

	pid = (int) procInfo.hProcess;
	process_handle = procInfo.hProcess;
	if (err)
	{
		errno = err;
		printf("Error: (CreateProcess) \"%s\", error: %d\n",tmp,errno);
		fflush(stdout);
		return -1;
	}
#else /* WIN32 */
	if (pipe(pfd) == -1)
	{
		printf("Error: (pipe) \"%s\", errno: %d\n",pipe_cmd,errno);
		fflush(stdout);
		return -1;
	}
	pid = fork();
	switch (pid)
	{
	case 0:
		close(0);
		dup(pfd[0]);            /* stdin is the pipe */
		close(pfd[0]);
		close(pfd[1]);

	/* Close every file except stdin, stdout and stderr! */
		for (dup(0), fd = dup(0); fd > 2; fd--)
			close(fd);

		argc = 0;
		argv[argc++] = pipe_cmd;

		if (cache)
		{
			argv[argc++] = "-c";
			argv[argc++] = cache;
		}

		if (sn_host)
		{
			argv[argc++] = "-H";
			argv[argc++] = sn_host;
		}

		if (sn_pid)
		{
			argv[argc++] = "-P";
			argv[argc++] = sn_pid;
		}

		argv[argc++] = db_prefix;

		argv[argc++] = NULL;

		execvp(pipe_cmd,argv);

		return -1;	/* Error happend. */
		break;

	case -1:
		printf("Error: fork error: %d\n",errno);
		exit(1);
		break;
	}

	if ((pipe_handle = fdopen(pfd[1],"w")) == NULL)
	{
		printf("Error:fdopen error, errno: %d\n",errno);
		exit(1);
	}
	close(pfd[0]);
#endif /* WIN32 */

	if (incl_to_pipe)
	{
		FILE    *ifp;

		if ((ifp = fopen(incl_to_pipe,"r")) == NULL)
		{
			fprintf(stderr,"Error:couldn't load \"%s\",errno: %d\n",
				incl_to_pipe,errno);
			exit(1);
		}
		while (fgets(tmp,sizeof(tmp) -1,ifp))
		{
			Paf_Pipe_Write(tmp);
		}
		Paf_Pipe_Flush();

		fclose(ifp);

		unlink(incl_to_pipe);   /* Nobody needs it. */
	}
	return (int)pid;
}

#if DB_NO_CASE_COMPARE
/*
 * Database (without case) comparison routine.
 *
 * Parameters:
 *      a:      DBT #1
 *      b:      DBT #2
 *
 * Returns:
 *      < 0 if a is < b
 *      = 0 if a is = b
 *      > 0 if a is > b
 */
int
db_no_case_compare(const DBT *a,const DBT *b)
{
	register int cmp;
	
	cmp = use_STRNCASECMP (a->data, b->data, a->size < b->size ? a->size : b->size);
	if (cmp == 0)
	{
		/*
		 * This is something tricky,
		 * if "Foo" == "foo" and we are in a fetching routine,
		 * "foo" must replace "Foo", so the original text must
		 * be returned.
		 */
		if (db_action_is_fetching &&
			a->size == b->size && 
			memcmp(a->data, b->data, a->size < b->size ? a->size : b->size) != 0)
		{
			memcpy (a->data, b->data, b->size);
		}
		return ((int)a->size - (int)b->size); /* return only if it is equal to a string */
	}
	if (cmp != 0)
	{
		return cmp;
	}
	return ((int)a->size - (int)b->size);
}
#endif /* DB_NO_CASE_COMPARE */

/*
 * Database comparison routine.
 *
 * Parameters:
 *      a:      DBT #1
 *      b:      DBT #2
 *
 * Returns:
 *      < 0 if a is < b
 *      = 0 if a is = b
 *      > 0 if a is > b
 */
int
db_case_compare(register const DBT *a,register const DBT *b)
{
	register int cmp;
	
	if (db_compare_nocase && a->size == b->size)
	{
		cmp = use_STRNCASECMP (a->data, b->data, a->size < b->size ? a->size : b->size);
		if (cmp == 0)
		{
			return ((int)a->size - (int)b->size); /* return only if it is equal to a string */
		}
	}

	cmp = memcmp(a->data,b->data,a->size < b->size ? a->size : b->size);
	if (cmp != 0)
	{
		return cmp;
	}
	return ((int)a->size - (int)b->size);
}

/* very bad solution for error handling */
jmp_buf	BAD_IMPL_jmp_buf;

void
Paf_panic(int level)
{
	if (level <= 0)
		level = 1;
	longjmp(BAD_IMPL_jmp_buf,level);
}

static int
Paf_Pipe_Flush()
{
	if (pipe_handle == INVALID_HANDLE_VALUE)
		return -1;
#ifdef  __MSVC__
	return (int)FlushFileBuffers(pipe_handle);
#else
	return fflush(pipe_handle);
#endif /* __MSVC__ */
}

int
Paf_Pipe_Close()
{
	int     ret = 0;

	if (pipe_handle == INVALID_HANDLE_VALUE)
	{
		Paf_db_close_tables();

		return 0;
	}
#ifdef  WIN32
	CloseHandle(pipe_handle);

	ret = WaitForSingleObject(process_handle,INFINITE);
	process_handle = INVALID_HANDLE_VALUE;
#else
	fclose(pipe_handle);

	wait(&ret);
#endif /* WIN32 */

	pipe_handle = INVALID_HANDLE_VALUE;

	return ret;
}

static int
Paf_Pipe_Write MX_VARARGS_DEF(char *, arg1)
{
	va_list args;
	char    *fmt;
	char    tmp[10000];
	int     len;
	int     cou;
	Tcl_DString utfBuffer;

	if (pipe_handle == INVALID_HANDLE_VALUE)
		return -1;

	fmt = (char *)MX_VARARGS_START(char *,arg1,args);

	len = vsprintf(tmp,fmt, args);
	for (fmt = tmp; len > 0;)
	{
		cou = 0;
		Tcl_ExternalToUtfDString(NULL, tmp, len, &utfBuffer);
#ifdef _WINDOWS
		if (WriteFile(pipe_handle,Tcl_DStringValue(&utfBuffer),
		    Tcl_DStringLength(&utfBuffer),&cou,NULL) == FALSE)
		{
			Tcl_DStringFree(&utfBuffer);
			return FALSE;
		}
#else
		cou = fprintf(pipe_handle,"%s",Tcl_DStringValue(&utfBuffer));
		if (cou == -1) {
			Tcl_DStringFree(&utfBuffer);
			return FALSE;
		}
#endif

		if (cou > 0)
		{
			fmt += cou;
			len -= cou;
		}
		Tcl_DStringFree(&utfBuffer);
	}
	return TRUE;
}

#ifdef __MSVC__
/* This function checks for a process (with pid - proc_id).
   Returns:
        -1 if not there
         1  if there
         NB: this won't kill the process.
  [irox:3.3.98]
*/

int kill(int dumy, DWORD proc_id) /*sn_win32_ping*/
{
        HANDLE hProcess;
        char debug_str[200];


        hProcess = OpenProcess(PROCESS_ALL_ACCESS, TRUE, proc_id);

        if (hProcess!=NULL)
        {
        sprintf(debug_str,"Opened process %d.",proc_id);
/*      MessageBox(NULL,debug_str,"debug InFo",MB_OK);*/
                /* It's alive! */
                return 1;
        } else {
        sprintf(debug_str,"Couldn't opened process %d.",proc_id);
/*      MessageBox(NULL,debug_str,"debug InFo",MB_OK);*/
                /* I think it's dead Jim. */
                return -1;
        }
}
#endif



