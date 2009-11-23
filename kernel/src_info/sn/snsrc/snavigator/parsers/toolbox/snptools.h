/*
 * snptools.h
 *
 * Copyright (C) 1997 Cygnus Solutions, Inc.
 *
 * Description:
 * Interface for the Source-Navigator parser toolbox.  This toolbox is designed
 * to be as independent of the parser as possible, although it does tend to sit
 * together well with lex/GNU flex.
 */

#ifndef SNPTOOLS_H
#define SNPTOOLS_H

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "sn.h"

enum sn_options
{
  SN_OPT_CACHESIZE,
  SN_OPT_CASE_SENSITIVE,
  SN_OPT_COMMENTS,
  SN_OPT_DIALECT,
  SN_OPT_DBPREFIX,
  SN_OPT_DROP_USR_HEADERS,
  SN_OPT_GROUP,
  SN_OPT_HOSTNAME,
  SN_OPT_INCL_TO_PIPE,
  SN_OPT_INCLUDE_LIST,
  SN_OPT_LISTFILE,
  SN_OPT_LOCAL_VARS,
  SN_OPT_PID,
  SN_OPT_PIPECMD,
  SN_OPT_TREAT_AS_CPLUSPLUS,
  SN_OPT_XREF_FILENAME
};

/*
 * The following #defines just map onto the old PAF_ constants.
 */

#define SN_SYM_FILENAME       PAF_SYM_FILENAME
#define SN_HIGH_FILENAME      PAF_HIGH_FILENAME
#define SN_HIGH               PAF_HIGH

#define SN_FILE               PAF_FILE
#define SN_TYPE_DEF           PAF_TYPE_DEF
#define SN_CLASS_DEF          PAF_CLASS_DEF
#define SN_MBR_FUNC_DEF       PAF_MBR_FUNC_DEF
#define SN_MBR_VAR_DEF        PAF_MBR_VAR_DEF
#define SN_ENUM_DEF           PAF_ENUM_DEF
#define SN_CONS_DEF           PAF_CONS_DEF
#define SN_MACRO_DEF          PAF_MACRO_DEF
#define SN_FUNC_DEF           PAF_FUNC_DEF
#define SN_SUBR_DEF           PAF_SUBR_DEF
#define SN_GLOB_VAR_DEF       PAF_GLOB_VAR_DEF
#define SN_COMMON_DEF         PAF_COMMON_DEF
#define SN_COMMON_MBR_VAR_DEF PAF_COMMON_MBR_VAR_DEF
#define SN_CLASS_INHERIT      PAF_CLASS_INHERIT
#define SN_FILE_SYMBOLS       PAF_FILE_SYMBOLS
#define SN_CROSS_REF_BY       PAF_CROSS_REF_BY
#define SN_CROSS_REF          PAF_CROSS_REF
#define SN_MBR_FUNC_DCL       PAF_MBR_FUNC_DCL
#define SN_FUNC_DCL           PAF_FUNC_DCL
#define SN_ENUM_CONST_DEF     PAF_ENUM_CONST_DEF
#define SN_UNION_DEF          PAF_UNION_DEF
#define SN_FRIEND_DCL         PAF_FRIEND_DCL
#define SN_NAMESPACE_DEF      PAF_NAMESPACE_DEF
#define SN_EXCEPTION_DEF      PAF_EXCEPTION_DEF
#define SN_LOCAL_VAR_DEF      PAF_LOCAL_VAR_DEF
#define SN_VAR_DCL            PAF_VAR_DCL
#define SN_INCLUDE_DEF        PAF_INCLUDE_DEF
#define SN_COMMENT_DEF        PAF_COMMENT_DEF
#define SN_CROSS_REF_CPP      PAF_CROSS_REF_CPP
#define SN_REF_UNDEFINED      PAF_REF_UNDEFINED
#define SN_CROSS_REF_FILE     PAF_CROSS_REF_FILE

#define SN_REF_TO_TYPEDEF     PAF_REF_TO_TYPEDEF
#define SN_REF_TO_DEFINE      PAF_REF_TO_DEFINE
#define SN_REF_TO_ENUM        PAF_REF_TO_ENUM
#define SN_REF_TO_STRUCT      PAF_REF_TO_STRUCT
#define SN_REF_TO_UNION       PAF_REF_TO_UNION
#define SN_REF_TO_CLASS       PAF_REF_TO_CLASS
#define SN_REF_TO_FUNCTION    PAF_REF_TO_FUNCTION
#define SN_REF_TO_MBR_FUNC    PAF_REF_TO_MBR_FUNC
#define SN_REF_TO_MBR_VAR     PAF_REF_TO_MBR_VAR
#define SN_REF_TO_COMM_VAR    PAF_REF_TO_COMM_VAR
#define SN_REF_TO_CONSTANT    PAF_REF_TO_CONSTANT
#define SN_REF_TO_SUBROUTINE  PAF_REF_TO_SUBROUTINE
#define SN_REF_TO_GLOB_VAR    PAF_REF_TO_GLOB_VAR
#define SN_REF_TO_LOCAL_VAR   PAF_REF_TO_LOCAL_VAR
#define SN_REF_TO_TEMPLATE    PAF_REF_TO_TEMPLATE
#define SN_REF_TO_NAMESPACE   PAF_REF_TO_NAMESPACE
#define SN_REF_TO_EXCEPTION   PAF_REF_TO_EXCEPTION
#define SN_REF_TO_LABEL       PAF_REF_TO_LABEL

#define SN_REF_SCOPE_LOCAL    PAF_REF_SCOPE_LOCAL
#define SN_REF_SCOPE_GLOBAL   PAF_REF_SCOPE_GLOBAL

#define SN_REF_READ           PAF_REF_READ
#define SN_REF_WRITE          PAF_REF_WRITE
#define SN_REF_PASS           PAF_REF_PASS
#define SN_REF_UNUSED         PAF_REF_UNUSED

#define SN_PRIVATE            PAF_PRIVATE
#define SN_PROTECTED          PAF_PROTECTED
#define SN_PUBLIC             PAF_PUBLIC
#define SN_STATIC             PAF_STATIC
#define SN_VIRTUAL            PAF_VIRTUAL

#define SN_ABSTRACT           PAF_ABSTRACT
#define SN_FINAL              PAF_FINAL
#define SN_NATIVE             PAF_NATIVE
#define SN_SYNCHRONIZED       PAF_SYNCHRONIZED
#define SN_VOLATILE           PAF_VOLATILE
#define SN_TRANSIENT          PAF_TRANSIENT
#define SN_INTERFACE          PAF_INTERFACE
#define SN_IMPLEMENTS         PAF_IMPLEMENTS
#define SN_INLINE             PAF_INLINE
#define SN_CONSTRUCTOR        PAF_CONSTRUCTOR
#define SN_DESTRUCTOR         PAF_DESTRUCTOR
#define SN_PUREVIRTUAL        PAF_PUREVIRTUAL
#define SN_STRUCT_DEF         PAF_STRUCT_DEF

#define SN_OVERRIDE           PAF_OVERRIDE
#define SN_OVERLOADED         PAF_OVERLOADED

#define SN_PANIC_SOFT         PAF_PANIC_SOFT
#define SN_PANIC_SIMPLE       PAF_PANIC_SIMPLE
#define SN_PANIC_EMERGENCY    PAF_PANIC_EMERGENCY

#define SN_WORD_DEFINE        PAF_WORD_DEFINE
#define SN_WORD_UNDEF         PAF_WORD_UNDEF
#define SN_WORD_REPLACE       PAF_WORD_REPLACE
#define SN_WORD_NONE          PAF_WORD_NONE

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
 * A boilerplate main routine that does all the work for most parsers.
 */
int sn_main(int argc, char *argv[], char * group, FILE ** lexstream, int (*lexer)(), void (*reset)());

/*
 * Insert a symbol into the database.
 */
int sn_insert_symbol(int id_type, char *classname, char *identifier,
                    char *filename, int start_lineno, int start_colpos,
                    int end_lineno, int end_colpos, unsigned long attr,
                    char *ret, char *arg_types, char *arg_names, char *comment,
                    int high_start_lineno, int high_start_colpos, 
                    int high_end_lineno, int high_end_colpos);

/*
 * Insert a cross-reference into the project database. 
 */
int sn_insert_xref(int type, int scope_type, int scope_level,
                   char *classname, char *funcname, char *argtypes, 
                   char *refclass, char *refsymbol, char *ref_arg_types,
                   char *filename, int lineno, int acc); 

/*
 * Insert a comment into the project database.
 */
int sn_insert_comment(char *classname, char *funcname, char *filename,
                      char *comment, int beg_line, int beg_col);

/*
 * Retrieve the first include path (including a trailing '/')
 * Returns NULL if no include paths were specified.
 */
char * sn_includepath_first();

/*
 * Retrieve the next include path (with a trailing '/')
 * Returns NULL if there are no more include paths.
 */
char * sn_includepath_next();

/*
 * Retrieve command line options (specified by the enum sn_options).
 */
void * sn_getopt(enum sn_options);

/*
 * Make the parser exit due to some error condition.
 */
void sn_exit();

/*
 * Issue a message that will appear in the "scanning" dialog box within S-N.
 */
int sn_message(char * format, ...);

/*
 * Issue an error message to stderr.
 */
int sn_error(char * format, ...);

/*
 * Process command line options to be later retrieved using sn_getopt().
 */
void sn_process_options(int argc, char *argv[]);

/*
 * Set the language description tag.
 */
void sn_set_group(char * newGroup);

/*
 * Open hooks to the database such that the S-N put_* API calls will work.
 */ 
int sn_init();

/*
 * Close the database and any other open files prior to the parser exiting.
 */
void sn_close();

/*
 * Register a new source file and reassign the "lexstream" stream variable
 * to point to the new file so that the lexer/parser can work on it.
 */
int sn_register_filename(FILE ** lexstream, char *filename);

/*
 * Line/column manipulators; this library looks after keeping count of them.
 */
void sn_set_column(long column);
void sn_reset_column();
void sn_advance_column(int num);

void sn_set_line(long line);
void sn_reset_line();
void sn_advance_line();

/*
 * Functions for saving at most one line/column number.  The *pop() 
 * functions return -1 if too many pops are done.
 */
void sn_push_line();
long sn_pop_line();

void sn_push_column();
long sn_pop_column();

/*
 * Count the number of characters in a null-terminated buffer.  Increment the
 * internal line/column counters accordingly.
 */ 
void sn_count_chars(char *buf, int length);

/*
 * Functions to get the current line/column numbers.
 */
long sn_column();
long sn_line();

/* 
 * If a list of source files is given on the command line, then open each
 * named file and parse it using the user-defined parser entry point.
 */
void sn_parse_all(FILE ** lexstream, int (*parse)(), void (*reset)());

/*
 * Return the name of the currently parsed source file. 
 */
char * sn_current_file();

/*
 * Are we generating cross-referencing information?  If so, this function
 * returns 1, 0 otherwise.
 */
int sn_cross_referencing();

/* 
 * Locate the position of the end of the last whitespace region in a buffer.
 * e.g.    +--------------------+
 *         |..abcd         efgh_|
 *         +--------------------+
 *                returns -'   `-null 
 */
char * sn_afterlastwspace(char * buf);

/*
 * Search the include path for an include file.
 */
int sn_find_file(char * filename, char * buf);

#endif /* SNPTOOLS_H */

#ifdef __cplusplus
}
#endif /* __cplusplus */
