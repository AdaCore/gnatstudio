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
#ifndef equalint
#define equalint(a, b)		(a) == (b)
#endif
#ifndef beginshort
#define beginshort(a)		
#endif
#ifndef equalshort
#define equalshort(a, b)	(a) == (b)
#endif
#ifndef beginlong
#define beginlong(a)		
#endif
#ifndef equallong
#define equallong(a, b)	(a) == (b)
#endif
#ifndef beginrbool
#define beginrbool(a)		
#endif
#ifndef equalrbool
#define equalrbool(a, b)	(a) == (b)
#endif
#ifndef begintString
#define begintString(a)	
#endif
#ifndef equaltString
#define equaltString(a, b)	strcmp (a, (b)) == 0
#endif
#ifndef begintStringRef
#define begintStringRef(a)	
#endif
#ifndef equaltStringRef
#define equaltStringRef(a, b)	(a) == (b)
#endif
#ifndef begintIdent
#define begintIdent(a)		a = NoIdent;
#endif
#ifndef equaltIdent
#define equaltIdent(a, b)	(a) == (b)
#endif
#ifndef begintPosition
#define begintPosition(a)	a = NoPosition;
#endif
#ifndef equaltPosition
#define equaltPosition(a, b)	Compare (a, b) == 0
#endif
#ifndef beginNodeHead
#define beginNodeHead(a)	
#endif
#ifndef equalNodeHead
#define equalNodeHead(a, b)	rtrue
#endif
#ifndef beginttype
#define beginttype(a)
#endif
#ifndef equalttype
#define equalttype(a, b)	\
  memcmp ((char *) & a, (char *) & b, sizeof (a)) == 0
#endif
#ifndef begintoperator
#define begintoperator(a)
#endif
#ifndef equaltoperator
#define equaltoperator(a, b)	\
  memcmp ((char *) & a, (char *) & b, sizeof (a)) == 0
#endif
#ifndef begintHashTable
#define begintHashTable(a)
#endif
#ifndef equaltHashTable
#define equaltHashTable(a, b)	\
  memcmp ((char *) & a, (char *) & b, sizeof (a)) == 0
#endif
#ifndef begintTree
#define begintTree(a)	a = NULL;
#endif
#ifndef equaltTree
#define equaltTree(a, b)	IsEqualTree (a, b)
#endif

