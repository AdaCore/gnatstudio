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

#ifndef __SEARCHTABLE

#define  __SEARCHTABLE

#define  SEARCH_DUP_KEY 1
#define  SEARCH_DUP_DATA   2
#define  SEARCH_FIRST   4

#define  SEARCH_HASH_TABLE 0
#define  SEARCH_BTREE_TABLE   1

typedef struct {
   char  *key;    /* Pointer to an ascii key. */
   int      key_len; /* -1 means a '\0' terminated string. */
   void  *data;      /* Pointer to a user data. */
   int      data_len;   /* -1 means a '\0' terminated string. */
   unsigned char flag;  /* can contain: 0, SEARCH_DUP_KEY|SEARCH_DUP_DATA */
} SearchEntry;

typedef struct {
   SearchEntry entry;
   unsigned int used;
} DATA;

typedef struct __SearchTable {
   DATA *data;
   unsigned int hsize;
   unsigned int filled;
   int   cursor;
   SearchEntry *(*add)(struct __SearchTable **table,SearchEntry entry);
   SearchEntry *(*delete)(struct __SearchTable **table,SearchEntry entry);
   SearchEntry *(*search)(struct __SearchTable **table,SearchEntry entry);
   SearchEntry *(*seq)(struct __SearchTable **table,int pos);  /* pos 0: first */
   void (*destroy)(struct __SearchTable **table);
   void (*_delete_entry)(SearchEntry *entry);   /* Never call it direclty! */
} SearchTable;

SearchTable *SearchTableCreate(unsigned int nel,int type,
   void (*_delete_entry)(SearchEntry *entry));
#endif /* __HASHTABLE */

