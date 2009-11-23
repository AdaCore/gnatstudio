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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tcl.h>

#include "symbol.h"
#include "srchtbl.h"

#undef TRACE

#ifndef Malloc
extern void *Malloc( int );
#endif /* Malloc */

#define False 0
#define True (!False)

typedef struct sSymbol sSymbol_t, *Symbol_t;
typedef struct sDatasp sDatasp_t, *Datasp_t;

struct sSymtab
{
   SearchTable *ptable;
   void (*pfFree)(void *);
};

struct sSymbol
{
   char *name;
   Datasp_t Datasp;
};

struct sDatasp
{
   int iNiveau;
   void *pvData;
   Datasp_t DataspNext;
};

static Symbol_t SymbolCreate( char *name, void *pvData, int iNiveau );
static void SymbolDestroy( Symbol_t Symbol, void (*pfFree)(void *));
static Datasp_t DataspCreate( void *pvData, int iNiveau );
static void DataspDestroy( Datasp_t Datasp, void (*pfFree)(void *));
static Symbol_t SymbolInsert( Symtab_t Symtab, Symbol_t Symbol );
static void f_EntryDestroy( SearchEntry *pentry );
static void SymbolPrint( Symbol_t Symbol, int tab );

static Symtab_t Symtab_g;

extern int SymtabInsert( Symtab_t Symtab, char *name, void *pvData, int iNiveau )
{
   Symbol_t Symbol;
   Symbol_t SymbolOld;

   Symtab_g = Symtab;

#ifdef TRACE
   printf( "SymbolInsert: %s\n", name );
#endif

   Symbol = SymbolCreate( name, pvData, iNiveau );

   if(( SymbolOld = SymbolInsert( Symtab, Symbol )))
   {
      Datasp_t Datasp;

#ifdef TRACE
      printf( "SymbolInsert: old value: %s niveau: %d\n", name, iNiveau );
#endif

      SymbolDestroy( Symbol, (void(*)(void*)) 0 );
      Datasp = DataspCreate( pvData, iNiveau );
      Datasp->DataspNext = SymbolOld->Datasp;
      SymbolOld->Datasp = Datasp;

      Symtab_g = 0;
      return True;
   }
   else
   {
#ifdef TRACE
      printf( "SymbolInsert: new value: %s niveau: %d\n", name, iNiveau );
#endif
      Symtab_g = 0;
      return True;
   }
}

static Symbol_t SymbolCreate( char *name, void *pvData, int iNiveau )
{
   Symbol_t Symbol = (Symbol_t) Malloc( sizeof( Symbol[0] ));

   Symbol->name = name;
   Symbol->Datasp = DataspCreate( pvData, iNiveau );

   return Symbol;
}

static Datasp_t DataspCreate( void *pvData, int iNiveau )
{
   Datasp_t Datasp = (Datasp_t) Malloc( sizeof( Datasp[0] ));

   Datasp->pvData     = pvData;
   Datasp->iNiveau    = iNiveau;
   Datasp->DataspNext = 0;

   return Datasp;
}

static void SymbolDestroy( Symbol_t Symbol, void (*pfFree)(void *))
{
   if( Symbol )
   {
      DataspDestroy( Symbol->Datasp, pfFree );
      ckfree( (char*)Symbol );
   }
}

static void DataspDestroy( Datasp_t Datasp, void (*pfFree)(void *))
{
   if( Datasp )
   {
      if( pfFree && Datasp->pvData )
      {
         (*pfFree)( Datasp->pvData );
      }
      DataspDestroy( Datasp->DataspNext, pfFree );

      ckfree( (char*)Datasp );
   }
}

static Symbol_t SymbolInsert( Symtab_t Symtab, Symbol_t Symbol )
{
   SearchEntry entry;
   SearchEntry *pentry;

   entry.key = Symbol->name;
   entry.key_len = strlen( Symbol->name );

   if(( pentry = Symtab->ptable->search( &(Symtab->ptable), entry )))
   {
      return (Symbol_t) pentry->data;
   }

   entry.data = (void *) Symbol;
   entry.data_len = sizeof(Symbol);
   entry.flag = 0;
   Symtab->ptable->add( &(Symtab->ptable), entry );

   return 0;
}

extern void *SymtabFind( Symtab_t Symtab, char *name )
{
   SearchEntry entry;
   SearchEntry *pentry;

   Symtab_g = Symtab;

   entry.key = name;
   entry.key_len = strlen( name );

   if(( pentry = Symtab->ptable->search( &(Symtab->ptable), entry )))
   {
      Symbol_t Symbol = (Symbol_t) pentry->data;
      Symtab_g = 0;
      return Symbol->Datasp->pvData;
   }
   else
   {
      Symtab_g = 0;
      return 0;
   }
}

extern Symtab_t SymtabCreate( void (*pfFree)(void *))
{
   Symtab_t Symtab = (Symtab_t) Malloc( sizeof( Symtab[0] ));
   Symtab->ptable = SearchTableCreate( 100
                                     , SEARCH_BTREE_TABLE
                                     , f_EntryDestroy );
   Symtab->pfFree = pfFree;
   return Symtab;
}

extern void SymtabDestroy( Symtab_t Symtab )
{
   if( Symtab )
   {
      Symtab_g = Symtab;
      Symtab->ptable->destroy(&(Symtab->ptable));
      ckfree( (char*)Symtab );
      Symtab_g = 0;
   }
}

extern void SymtabClear( Symtab_t Symtab )
{
   if( Symtab )
   {
      Symtab_g = Symtab;
      Symtab->ptable->destroy(&(Symtab->ptable));
      Symtab->ptable = SearchTableCreate( 100
                                        , SEARCH_BTREE_TABLE
                                        , f_EntryDestroy );
      Symtab_g = 0;
   }
}

static void f_EntryDestroy( SearchEntry *pentry )
{
   Symbol_t Symbol = (Symbol_t) pentry->data;
   SymbolDestroy( Symbol, Symtab_g->pfFree );
}

extern void SymtabPrint( Symtab_t Symtab, int tab )
{
   if( Symtab )
   {
      int iCount;
      SearchEntry *pentry;

      for( iCount = 0
         ; ( pentry = Symtab->ptable->seq(&(Symtab->ptable), iCount ))
         ; iCount++ )
      {
         Symbol_t Symbol = (Symbol_t) pentry->data;
         SymbolPrint( Symbol, tab );
      }
   }
}

static void SymbolPrint( Symbol_t Symbol, int tab )
{
   printf( "%*.*sSymbol: %s\n"
         , 4 * tab
         , 4 * tab
         , ""
         , Symbol->name
         );
}

extern void SymtabNiveauDestroy( Symtab_t Symtab, int iNiveau )
{
   if( Symtab )
   {
      int iCount;
      SearchEntry *pentry;

      Symtab_g = Symtab;

      for( iCount = 0
         ; ( pentry = Symtab->ptable->seq(&(Symtab->ptable), iCount ))
         ; iCount++ )
      {
         Symbol_t Symbol = (Symbol_t) pentry->data;

         if( Symbol->Datasp->iNiveau > iNiveau )
         {
            Datasp_t Datasp = Symbol->Datasp;
            Symbol->Datasp = Datasp->DataspNext;
            if( Symbol->Datasp == 0 )
            {
	       SearchEntry entry;

	       entry.key = Symbol->name;
	       entry.key_len = strlen( Symbol->name );
	       entry.flag = 0;

               Symtab->ptable->delete( &(Symtab->ptable), entry );
               iCount--;
            }
            /* a Datasp-t csak a Symbol torlese utan szabad torolni, mert
               a Symbol->name a Datasp->pvData alatt is megvan, es ott torlodik.
               Igy ha a Datasp-t torlom, akkor a Symbol->name a levegobe mutat.
               22.01.98 rigo
            */

            Datasp->DataspNext = 0;
            DataspDestroy( Datasp, Symtab->pfFree );
         }
      }
      Symtab_g = 0;
   }
}



