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

#ifndef _DBUTILS_

#define  _DBUTILS_

#ifndef WIN32
/* Once we start using confiure we can get rid of this. */
#include <config.h>
#endif

#include "mxdefine.h"
#include "mxfuncs.h"

#include "db.h"
#include "sn.h"
#include "tcl.h"

void db_remove_file_xfer(int softdel,char *files);
void db_remove_file_def(int softdel, char *file);
void db_insert_entry(int type,char *key_buf,char *data_buf);
int Paf_db_close_tables(void);
void open_tables_for_cross_ref(void);
int my_SplitList (char *str, int *num, char ***argvPtr, char sep);
char * read_next_field (char *str, char *buf, int size, char sep);
int read_next_int_field (char *str, char sep);
void db_remove_file_xfer_using_keys(int softdel, char *key_files);
void MacroReadFile(char *pcFilename);

#endif /* _DBUTILS_ */

