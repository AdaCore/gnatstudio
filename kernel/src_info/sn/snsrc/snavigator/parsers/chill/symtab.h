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
 * symtab.h
 *
 * Copyright (C) 1998 Cygnus Solutions
 *
 * Description:
 * Pseudo symbol table management for the CHILL parser.
 */

#ifndef SYMTAB_H
#define SYMTAB_H

enum types {
    unknown,
    call,
    array,
    structure,
    simple,
    numTypes
};

struct punctuation {
    unsigned line;
    unsigned startcol;
    unsigned endcol;
};

struct blockregion {
    unsigned startline;
    unsigned startcol;
    unsigned endline;
    unsigned endcol;
};

struct identifier {
    unsigned line;
    unsigned startcol;
    unsigned endcol;
    char * name;
    enum types type;
};

struct kword {
    unsigned line;
    unsigned startcol;
    unsigned endcol;
};

struct datatype {
    char * text;
};

struct attribute {
    char * text;
};

struct sig {
    char * rettype;
    char * args;
};

struct identifierlist {
    unsigned line;
    unsigned startcol;
    unsigned endcol;
    char * name;
    struct identifierlist * next;
};

/* Get the name of the current module. */
char * get_module_name();

/* Get the name of the current spec module. */ 
char * get_spec_module_name();

/* Get the name of the current in-scope procedure. */
char * get_proc_name();

/* Get the name of the current in-scope process. */
char * get_process_name();

/* Sets the name of the current module. */
void set_module_name(char * name);

/* Sets the name of the current spec module. */
void set_spec_module_name(char * name);

/* Sets the name of the current in-scope procedure. */
void set_proc_name(char * name);

/* Sets the name of the current in-scope process. */
void set_process_name(char * name);

/* Store the names of locals for later identification. */
int remember_locals(struct identifierlist * ids);

/* Drop the symbol table structure. */
void forget_locals();

/* Activate a WITH clause. */
void activate_with();

/* Is there a current WITH clause in scope? Returns 1, otherwise 0. */
int with_active();

/* Set the current WITH scope. */
void set_with_scope(char * scope);

/* Get the current WITH scope.  Returns NULL if there is no
   current activation. */
char * get_with_scope();

/* Leave the current WITH activation. */
void leave_with();

#endif /* SYMTAB_H */

