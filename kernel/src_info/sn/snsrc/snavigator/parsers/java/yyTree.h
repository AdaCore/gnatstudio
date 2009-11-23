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

#ifndef beginint
#define beginint(a)		
#endif
#ifndef closeint
#define closeint(a)		
#endif
#ifndef readint
#define readint(a)		(void) fscanf (yyf, "%d", & a);
#endif
#ifndef writeint
#define writeint(a)		(void) fprintf (yyf, "%d", a);
#endif
#ifndef getint
#define getint(a)		yyGet ((char *) & a, sizeof (a));
#endif
#ifndef putint
#define putint(a)		yyPut ((char *) & a, sizeof (a));
#endif
#ifndef copyint
#define copyint(a, b)		
#endif
#ifndef equalint
#define equalint(a, b)		(a) == (b)
#endif
#ifndef beginshort
#define beginshort(a)		
#endif
#ifndef closeshort
#define closeshort(a)		
#endif
#ifndef readshort
#define readshort(a)		(void) fscanf (yyf, "%hd", & a);
#endif
#ifndef writeshort
#define writeshort(a)		(void) fprintf (yyf, "%hd", a);
#endif
#ifndef getshort
#define getshort(a)		yyGet ((char *) & a, sizeof (a));
#endif
#ifndef putshort
#define putshort(a)		yyPut ((char *) & a, sizeof (a));
#endif
#ifndef copyshort
#define copyshort(a, b)		
#endif
#ifndef equalshort
#define equalshort(a, b)	(a) == (b)
#endif
#ifndef beginlong
#define beginlong(a)		
#endif
#ifndef closelong
#define closelong(a)		
#endif
#ifndef readlong
#define readlong(a)		(void) fscanf (yyf, "%ld", & a);
#endif
#ifndef writelong
#define writelong(a)		(void) fprintf (yyf, "%ld", a);
#endif
#ifndef getlong
#define getlong(a)		yyGet ((char *) & a, sizeof (a));
#endif
#ifndef putlong
#define putlong(a)		yyPut ((char *) & a, sizeof (a));
#endif
#ifndef copylong
#define copylong(a, b)		
#endif
#ifndef equallong
#define equallong(a, b)	(a) == (b)
#endif
#ifndef beginrbool
#define beginrbool(a)		
#endif
#ifndef closerbool
#define closerbool(a)		
#endif
#ifndef readrbool
#define readrbool(a)		a = fgetc (yyf) == 'T';
#endif
#ifndef writerbool
#define writerbool(a)		(void) fputc (a ? 'T' : 'F', yyf);
#endif
#ifndef getrbool
#define getrbool(a)		yyGet ((char *) & a, sizeof (a));
#endif
#ifndef putrbool
#define putrbool(a)		yyPut ((char *) & a, sizeof (a));
#endif
#ifndef copyrbool
#define copyrbool(a, b)		
#endif
#ifndef equalrbool
#define equalrbool(a, b)	(a) == (b)
#endif
#ifndef begintStringRef
#define begintStringRef(a)	
#endif
#ifndef closetStringRef
#define closetStringRef(a)	
#endif
#ifndef readtStringRef
#define readtStringRef(a)	{ char yys [1024]; (void) fgets (yys, 1024, yyf); (void) ungetc ('\n', yyf); a = PutString (yys, strlen (yys) - 1); }
#endif
#ifndef writetStringRef
#define writetStringRef(a)	WriteString (yyf, a);
#endif
#ifndef gettStringRef
#define gettStringRef(a)	{ char yys [1024]; (void) fgets (yys, 1024, yyf); a = PutString (yys, strlen (yys) - 1); }
#endif
#ifndef puttStringRef
#define puttStringRef(a)	{ WriteString (yyf, a); (void) fputc ('\n', yyf); }
#endif
#ifndef copytStringRef
#define copytStringRef(a, b)		
#endif
#ifndef equaltStringRef
#define equaltStringRef(a, b)	(a) == (b)
#endif
#ifndef begintIdent
#define begintIdent(a)		a = NoIdent;
#endif
#ifndef closetIdent
#define closetIdent(a)		
#endif
#ifndef readtIdent
#define readtIdent(a)		a = yyReadIdent ();
#endif
#ifndef writetIdent
#define writetIdent(a)		WriteIdent (yyf, a);
#endif
#ifndef gettIdent
#define gettIdent(a)		yyGetIdent (& a);
#endif
#ifndef puttIdent
#define puttIdent(a)		yyPutIdent (a);
#endif
#ifndef copytIdent
#define copytIdent(a, b)		
#endif
#ifndef equaltIdent
#define equaltIdent(a, b)	(a) == (b)
#endif
#ifndef begintPosition
#define begintPosition(a)	a = NoPosition;
#endif
#ifndef closetPosition
#define closetPosition(a)	
#endif
#ifndef readtPosition
#define readtPosition(a)	ReadPosition (yyf, & a);
#endif
#ifndef writetPosition
#define writetPosition(a)	WritePosition (yyf, a);
#endif
#ifndef gettPosition
#define gettPosition(a)	yyGet ((char *) & a, sizeof (a));
#endif
#ifndef puttPosition
#define puttPosition(a)	yyPut ((char *) & a, sizeof (a));
#endif
#ifndef copytPosition
#define copytPosition(a, b)	
#endif
#ifndef equaltPosition
#define equaltPosition(a, b)	Compare (a, b) == 0
#endif
#ifndef beginNodeHead
#define beginNodeHead(a)	
#endif
#ifndef closeNodeHead
#define closeNodeHead(a)	
#endif
#ifndef readNodeHead
#define readNodeHead(a)	
#endif
#ifndef writeNodeHead
#define writeNodeHead(a)	
#endif
#ifndef getNodeHead
#define getNodeHead(a)		
#endif
#ifndef putNodeHead
#define putNodeHead(a)		
#endif
#ifndef copyNodeHead
#define copyNodeHead(a, b)	
#endif
#ifndef equalNodeHead
#define equalNodeHead(a, b)	rtrue
#endif
#ifndef beginttype
#define beginttype(a)
#endif
#ifndef closettype
#define closettype(a)
#endif
#ifndef readttype
#define readttype(a)	\
  yyReadHex ((unsigned char *) & a, sizeof (a));
#endif
#ifndef writettype
#define writettype(a)	\
  yyWriteHex ((unsigned char *) & a, sizeof (a));
#endif
#ifndef getttype
#define getttype(a)	yyGet ((char *) & a, sizeof (a));
#endif
#ifndef putttype
#define putttype(a)	yyPut ((char *) & a, sizeof (a));
#endif
#ifndef copyttype
#define copyttype(a, b)
#endif
#ifndef equalttype
#define equalttype(a, b)	\
  memcmp ((char *) & a, (char *) & b, sizeof (a)) == 0
#endif
#ifndef begintoperator
#define begintoperator(a)
#endif
#ifndef closetoperator
#define closetoperator(a)
#endif
#ifndef readtoperator
#define readtoperator(a)	\
  yyReadHex ((unsigned char *) & a, sizeof (a));
#endif
#ifndef writetoperator
#define writetoperator(a)	\
  yyWriteHex ((unsigned char *) & a, sizeof (a));
#endif
#ifndef gettoperator
#define gettoperator(a)	yyGet ((char *) & a, sizeof (a));
#endif
#ifndef puttoperator
#define puttoperator(a)	yyPut ((char *) & a, sizeof (a));
#endif
#ifndef copytoperator
#define copytoperator(a, b)
#endif
#ifndef equaltoperator
#define equaltoperator(a, b)	\
  memcmp ((char *) & a, (char *) & b, sizeof (a)) == 0
#endif
#ifndef begintHashTable
#define begintHashTable(a)
#endif
#ifndef closetHashTable
#define closetHashTable(a)
#endif
#ifndef readtHashTable
#define readtHashTable(a)	\
  yyReadHex ((unsigned char *) & a, sizeof (a));
#endif
#ifndef writetHashTable
#define writetHashTable(a)	\
  yyWriteHex ((unsigned char *) & a, sizeof (a));
#endif
#ifndef gettHashTable
#define gettHashTable(a)	yyGet ((char *) & a, sizeof (a));
#endif
#ifndef puttHashTable
#define puttHashTable(a)	yyPut ((char *) & a, sizeof (a));
#endif
#ifndef copytHashTable
#define copytHashTable(a, b)
#endif
#ifndef equaltHashTable
#define equaltHashTable(a, b)	\
  memcmp ((char *) & a, (char *) & b, sizeof (a)) == 0
#endif
#ifndef begintTree
#define begintTree(a)	a = NoTree;
#endif
#ifndef closetTree
#define closetTree(a)
#endif
#ifndef readtTree
#define readtTree(a)	(void) fscanf (yyf, "%lx +", (unsigned long *) & a);
#endif
#ifndef writetTree
#define writetTree(a)	(void) fprintf (yyf, "%08lx +", (unsigned long) a);
#endif
#ifndef gettTree
#define gettTree(a)	yyGet ((char *) & a, sizeof (a));
#endif
#ifndef puttTree
#define puttTree(a)	yyPut ((char *) & a, sizeof (a));
#endif
#ifndef copytTree
#define copytTree(a, b)	a = b;
#endif
#ifndef equaltTree
#define equaltTree(a, b)	a == b
#endif

