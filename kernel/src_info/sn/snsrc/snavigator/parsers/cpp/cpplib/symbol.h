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


typedef struct sSymtab sSymtab_t, *Symtab_t;

struct sSymtab;

extern int SymtabInsert( Symtab_t Symtab, char *name, void *pvData, int iNiveau );
extern void *SymtabFind( Symtab_t Symtab, char *name );
extern Symtab_t SymtabCreate( void (*pfFree)(void *));
extern void SymtabDestroy( Symtab_t Symtab );
extern void SymtabClear( Symtab_t Symtab );
extern void SymtabPrint( Symtab_t Symtab, int tab );
extern void SymtabNiveauDestroy( Symtab_t Symtab, int iNiveau );


