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
 * sn.h
 *
 * Copyright (C) 1998 Cygnus Solutions
 *
 * Description:
 * Source-Navigator constants and function prototypes for publically visible
 * API functions.
 */ 

#ifndef SN_H
#define SN_H
#include "config.h"

#define  PAF_SYM_FILENAME  1
#define  PAF_HIGH_FILENAME 2
#define  PAF_HIGH    3

#define PAF_FILE        0
#define PAF_TYPE_DEF    1
#define PAF_CLASS_DEF      2
#define PAF_MBR_FUNC_DEF   3
#define PAF_MBR_VAR_DEF    4
#define PAF_ENUM_DEF    5
#define PAF_CONS_DEF    6
#define PAF_MACRO_DEF      7
#define PAF_FUNC_DEF    8
#define PAF_SUBR_DEF    9
#define PAF_GLOB_VAR_DEF   10
#define PAF_COMMON_DEF     11
#define PAF_COMMON_MBR_VAR_DEF   12
#define PAF_CLASS_INHERIT  13
#define PAF_FILE_SYMBOLS   14
#define PAF_CROSS_REF_BY   15
#define PAF_CROSS_REF      16
#define PAF_MBR_FUNC_DCL   17
#define PAF_FUNC_DCL    18
#define PAF_ENUM_CONST_DEF 19
#define PAF_UNION_DEF      20
#define PAF_FRIEND_DCL     21
#define PAF_NAMESPACE_DEF  22
#define PAF_EXCEPTION_DEF  23
#define  PAF_LOCAL_VAR_DEF 24
#define  PAF_VAR_DCL    25
#define  PAF_INCLUDE_DEF      26
#define PAF_COMMENT_DEF    27
#define PAF_CROSS_REF_CPP  28
#define PAF_TEMPLATE_ARG_DEF   29
#define  PAF_REF_UNDEFINED 30
#define PAF_CROSS_REF_FILE 31


/* Cross reference values. */
#define PAF_REF_TO_TYPEDEF PAF_TYPE_DEF
#define PAF_REF_TO_DEFINE  PAF_MACRO_DEF
#define PAF_REF_TO_ENUM    PAF_ENUM_CONST_DEF
#define PAF_REF_TO_STRUCT  PAF_STRUCT_DEF
#define PAF_REF_TO_UNION   PAF_UNION_DEF
#define PAF_REF_TO_CLASS   PAF_CLASS_DEF
#define PAF_REF_TO_FUNCTION   PAF_FUNC_DEF
#define PAF_REF_TO_MBR_FUNC   PAF_MBR_FUNC_DEF
#define PAF_REF_TO_MBR_VAR PAF_MBR_VAR_DEF
#define PAF_REF_TO_COMM_VAR   PAF_COMMON_MBR_VAR_DEF
#define PAF_REF_TO_CONSTANT   PAF_CONS_DEF
#define PAF_REF_TO_SUBROUTINE PAF_SUBR_DEF
#define PAF_REF_TO_GLOB_VAR   PAF_GLOB_VAR_DEF
#define PAF_REF_TO_LOCAL_VAR  PAF_LOCAL_VAR_DEF
#define  PAF_REF_TO_TEMPLATE  PAF_TEMPLATE_DEF
#define  PAF_REF_TO_NAMESPACE PAF_NAMESPACE_DEF
#define  PAF_REF_TO_EXCEPTION PAF_EXCEPTION_DEF
#define  PAF_REF_TO_LABEL  PAF_SUBR_DEF
#define PARF_REF_TO_TEMPLATE_ARG PAF_TEMPLATE_DEF

#define PAF_REF_SCOPE_LOCAL  0
#define PAF_REF_SCOPE_GLOBAL 1


/* Variable references */
#define PAF_REF_READ      0
#define PAF_REF_WRITE     1
#define PAF_REF_PASS      2
#define PAF_REF_UNUSED    3

/* put_symbol attributums. */
#define PAF_PRIVATE             0x000001
#define PAF_PROTECTED           0x000002
#define PAF_PUBLIC              0x000004
#define PAF_STATIC              0x000008
#define PAF_VIRTUAL             0x001000

#define PAF_ABSTRACT            0x000010
#define PAF_FINAL               0x000020
#define PAF_NATIVE              0x000040
#define PAF_SYNCHRONIZED        0x000080
#define PAF_VOLATILE            0x000100
#define PAF_TRANSIENT           0x000200
#define PAF_INTERFACE           0x000400
#define PAF_IMPLEMENTS          0x000800
#define PAF_INLINE              0x002000
#define PAF_CONSTRUCTOR         0x004000
#define PAF_DESTRUCTOR              PAF_CONSTRUCTOR
#define PAF_PUREVIRTUAL           (0x008000 | PAF_VIRTUAL)
#define PAF_STRUCT_DEF             0x010000

#define PAF_OVERRIDE            0x20000
#define PAF_OVERLOADED          0x40000
#define PAF_TEMPLATE            0x80000

#define PAF_TA_TYPE             0
#define PAF_TA_VALUE            1
#define PAF_TA_TEMPLATE         2

/* end of put_symbol attributums. */

#define  PAF_PANIC_SOFT 1
#define  PAF_PANIC_SIMPLE  2
#define  PAF_PANIC_EMERGENCY  3

#define PAF_WORD_DEFINE    1
#define PAF_WORD_UNDEF     2
#define  PAF_WORD_REPLACE  3
#define PAF_WORD_NONE      4

#ifndef TRUE
#define TRUE    1
#endif

#ifndef FALSE
#define FALSE   0
#endif

/*
 * Separator for the database fields
 * The string and character representations must have the same value.
 */
#define DB_FLDSEP_CHR 0x1
#define DB_FLDSEP_STR "\001"
#define KEY_DATA_SEP_STR ";"
#define KEY_DATA_SEP_CHR ';'

#ifndef  _ANSI_ARGS_
#if ((defined(__STDC__) || defined(SABER)) && !defined(NO_PROTOTYPE)) || defined(__cplusplus)

#define _ANSI_ARGS_(x)  x
#else
#define _ANSI_ARGS_(x)  ()
#endif
#endif /* _ANSI_ARGS_ */

#if (__cplusplus)
extern "C" {
#endif /* (__cplusplus) */

extern int put_symbol _ANSI_ARGS_((int type,char *scope,char *sym_name,char *file,
   int start_lineno,int start_colpos,int end_lineno,int end_colpos,unsigned long attr,
   char *ret,char *arg_types,char *args, char *comment,
   int high_start_lineno,int high_start_colpos,int high_end_lineno, int high_end_colpos));

extern int get_symbol _ANSI_ARGS_((char *class_name,char *exp_scope,
   char *name,char *arg_types, char *scope,
   char *ret_type, char *ret_define, int exact));

extern int get_class_or_typedef _ANSI_ARGS_((char *name,char *origin));

extern int put_cross_ref _ANSI_ARGS_((int type,int scope_type,int scope_lev,
   char *fnc_cls,char *fnc,char *fnc_arg_types,char *scope,char *what,
   char *arg_types,char *file,int lineno,int charno,int acc));

extern int put_file _ANSI_ARGS_((char *file_name,char *group, char *highlight_file));

extern int put_comment _ANSI_ARGS_((char *classn,char *func,char *filename,
   char *comment,int beg_line,int beg_char));

extern int Paf_Pipe_Create _ANSI_ARGS_((char *pipe_cmd,char *db_prefix,char *incl_to_pipe,char *cache,char *sn_host,char *sn_pid));

extern void Paf_Open_Include_Dirs _ANSI_ARGS_((char *file_name,char *db_prefix));
extern void Paf_Close_Include_Dirs _ANSI_ARGS_(());

extern char *Paf_Search_Include_dir _ANSI_ARGS_((char *dir));

extern int Paf_Pipe_Close _ANSI_ARGS_ (());

extern char *Paf_tempnam _ANSI_ARGS_((char *dir,char *pref));

extern int (*Paf_Macro) _ANSI_ARGS_ ((char *word,int len,char **parameter_list,char **macro));

extern void Paf_db_init_tables _ANSI_ARGS_((char *arg,char *cache,char *cross_cache));

extern void Paf_insert_cross_ref_qry _ANSI_ARGS_ ((char *pcLine));

extern void Paf_panic (int level);

#if !HAVE_GETOPT
extern int getopt _ANSI_ARGS_((int argc,char **argv,char *opt));
#endif /* HAVE_GETOPT */

#if (__cplusplus)
}
#endif /* (__cplusplus) */

#endif /* SN_H */

