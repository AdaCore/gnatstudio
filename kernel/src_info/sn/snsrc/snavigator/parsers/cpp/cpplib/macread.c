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
#include <string.h>
#include <ctype.h>

#include <tcl.h>
#include "srchtbl.h"
#include "longstr.h"
#include "sn.h"

static SearchTable *pbtrMacro;
int (*Paf_Word)(char *word,int len,char **parameter_list,char **macro);
static int paf_word( char *word, int len, char **parameter_list, char **macro );

typedef struct sMacro sMacro_t, *Macro_t;

struct sMacro
{
	int typ;
	char *name;
	char *parameter_list;
	char *macro;
	char *pcLine;
};

static void FreeMacroEntry(SearchEntry *entry)
{
	Macro_t Macro = entry->data;

	ckfree ((char*)Macro->pcLine);
	ckfree ((char*)Macro);
}

/* This function deletes unnecessary white spaces. */
static void remove_unneded_chars(char *buf)
{
	char  *p;      /* Point to the output area. */
	char  *pc;
	char  c;
	char  prev_ch;
	int   brac_lev;

	/* Skip leading white spaces and '#' ! */
	for (p = pc = buf; isspace(*pc) || *pc == '#'; pc++);

	for (brac_lev = 0,prev_ch = '\0'; *pc; pc++)
	{
		c = *pc;
		if (isspace(c))
		{
			/* Drop duplicated white spaces ! */
			if (isspace(prev_ch))
				continue;

			c = '\040'; /* Map every white space into blank ! */
		}
		switch (c)
		{
		case ')':
			brac_lev--;
			if (isspace(prev_ch) && p < pc)
				p--;
			break;

		case '(':
			brac_lev++;
		case ',':
			if (brac_lev)
			{
				if (isspace(prev_ch) && p < pc)
					p--;
				for (pc++; isspace(*pc); pc++);
				pc--;
			}
			break;
		}

		*p++ = c;
		prev_ch = c;
	}
	*p = '\0';
}

static Macro_t f_MacroReadLine(char *pcLine, int *name_len, SearchTable *tbl)
{
	Macro_t Macro = (Macro_t) ckalloc(sizeof(*Macro));
	char *pc;
	int   par_list;

	pc = Macro->pcLine = SN_StrDup(pcLine);

	if( strncmp( pc, "define", 6 ) == 0 )
	{
		pc += 6;
		Macro->typ = PAF_WORD_DEFINE;
	}
	else if( strncmp( pc, "undef", 5 ) == 0 )
	{
		pc += 5;
		Macro->typ = PAF_WORD_UNDEF;
	}
	else if( strncmp( pc, "replace", 7 ) == 0 )
	{
		pc += 7;
		Macro->typ = PAF_WORD_REPLACE;
	}
	else if( strncmp( pc, "delete", 6 ) == 0 )
	{
		SearchEntry entry;

		for (pc += 6; isspace(*pc);pc++);

		entry.key = pc;
		entry.key_len = -1;
		tbl->delete(&tbl,entry);

		ckfree ((char*)Macro->pcLine);
		ckfree ((char*)Macro);
		return 0;
	}
	else
	{
		fprintf(stderr,"Unknown command: %s\n",pc);

		ckfree ((char*)Macro->pcLine);
		ckfree ((char*)Macro);
		return 0;
	}

	/* Skip white spaces ! */
	for (;isspace(*pc); pc++);

	Macro->name = pc;
	Macro->parameter_list = 0;
	Macro->macro = 0;

	while (*pc != '\0' && !isspace(*pc) && *pc != '(')
	{
		pc++;
	}
	*name_len = pc - Macro->name;

	for (; isspace(*pc); pc++);

	if (*pc == '(')
	{
	  *pc++ = '\0';
		par_list = 1;
	}
	else
		par_list = 0;

	Macro->name[*name_len] = '\0';

	if (par_list)  /* parameter_list */
	{
		int par = 1;

		for (Macro->parameter_list = pc; par && *pc; pc++)
		{
			switch (*pc)
			{
			case '(':
				par++;
				break;

			case ')':
				par--;
				if (par == 0)
				{
					*pc = '\0';
				}
				break;
			}
		}
		if (par != 0)  /* Syntax error */
		{
			fprintf(stderr,"Macro syntax error: %s,%s\n",
				Macro->pcLine,Macro->parameter_list);

			ckfree ((char*)Macro->pcLine);
			ckfree ((char*)Macro );
			return 0;
		}
	}

	if (*pc)
	{
		/* Skip white spaces ! */
		for (;isspace(*pc); pc++);

		if (*pc != '\0')
		{
			Macro->macro = pc;
		}
	}

#if 0
	if( Macro->parameter_list )
	{
		printf( "Name: <%s> Parameters:<%s> Macro: <%s>\n"
				, Macro->name
				, Macro->parameter_list
				, Macro->macro ? Macro->macro : ""
				);
	}
	else
	{
		printf( "Name: <%s> Macro: <%s>\n"
				, Macro->name
				, Macro->macro ? Macro->macro : "<empty>"
				);
	}
#endif

	return Macro;
}

void MacroReadFile(char *pcFilename)
{
	FILE *pfile;
	LongString bf;
	char *acBuffer;
	SearchEntry entry;

	if(( pfile = fopen( pcFilename, "r" )) == 0 )
	{
		perror( pcFilename );
		return;
	}

	LongStringInit(&bf,0);
	/* Create hash table !*/
	if (!pbtrMacro)
		pbtrMacro = SearchTableCreate(1000,SEARCH_HASH_TABLE,FreeMacroEntry);

	entry.key_len = -1;
	entry.data_len = sizeof(sMacro_t);
	entry.flag = 0;
	while ((acBuffer = bf.fgets(&bf,pfile)))
	{
		Macro_t Macro;

		acBuffer = bf.trim(&bf,NULL);

		/* Skip empty lines and comments ! */
		if (*acBuffer == '\0' || *acBuffer == '\'')
			continue;

		/* Continuous line ? */
		if (bf.buf[bf.len - 1] == '\\')
		{
			LongString b2;

			LongStringInit(&b2,0);

			bf.len--;
			while (b2.fgets(&b2,pfile) && b2.len > 0)
			{
				b2.trim(&b2,NULL);

				if (b2.len == 0)
					continue;

				if (b2.buf[b2.len - 1] == '\\')
				{
					bf.append(&bf,b2.buf,b2.len - 1);
				}
				else
				{
					bf.append(&bf,b2.buf,b2.len);
					break;
				}
			}
			b2.free(&b2);
		}
		acBuffer = bf.buf;

		remove_unneded_chars(acBuffer);

		Macro = f_MacroReadLine( acBuffer,&entry.key_len,pbtrMacro);
		if (!Macro)
			continue;

		entry.key = Macro->name;
		entry.data = Macro;
		pbtrMacro->delete(&pbtrMacro,entry);
		pbtrMacro->add(&pbtrMacro,entry);
	}

	fclose( pfile );

	bf.free(&bf);

	/* Check whether the table is empty ! */
	if (pbtrMacro->filled)
	{
		Paf_Word = paf_word;
	}
	else
	{
		/* Now we just destroy the table, because it is empty. */
		pbtrMacro->destroy(&pbtrMacro);
		pbtrMacro = NULL;
	}
}

extern void MacroFreeTable( void )
{
	if (pbtrMacro)
		pbtrMacro->destroy(&pbtrMacro);
}

static int paf_word( char *word, int len, char **parameter_list, char **macro )
{
	Macro_t Macro;
	int retval;
	SearchEntry entry,*found;

	if( pbtrMacro != 0 )
	{
		entry.key = word;
		entry.key_len = len;
		found = pbtrMacro->search(&pbtrMacro,entry);

		if (found)
		{
			Macro = found->data;

			if (parameter_list)
				*parameter_list = Macro->parameter_list;
			if (macro)
				*macro = Macro->macro;

			retval = Macro->typ;
		}
		else
		{
			if (parameter_list)
				*parameter_list = 0;
			if (macro)
				*macro = 0;

			retval = PAF_WORD_NONE;
		}
	}
	else
	{
		if (parameter_list)
			*parameter_list = 0;
		if (macro)
			*macro = 0;

		retval = PAF_WORD_NONE;
	}

	return retval;
}

