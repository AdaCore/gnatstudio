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

/* Ich, Doktor Josef Grosch, Informatiker, Dez. 1996 */

#include <string.h>
#include "def.h"
#include "deftab.h"
#include "rMemory.h"
#include "Errors.h"
#include "StringM.h"
#include "keywords.h"
#include "cobol.h"
#include "sn.h"

#define null (char *) NULL
#define attr (unsigned long) PAF_PUBLIC

static	char *	section_name []	= { 0,
   "CONFIGURATION_SECTION",
   "INPUT-OUTPUT_SECTION",
   "FILE_SECTION",
   "WORKING-STORAGE_SECTION",
   "LOCAL-STORAGE_SECTION",
   "LINKAGE_SECTION",
   "COMMUNICATION_SECTION",
   "REPORT_SECTION",
   "SCREEN_SECTION",
   0,
   0,
   0,
   0,
   0,
   0,
   0,
   0,
   0,
   0,
   0,
   "IDENTIFICATION_DIVISION",
   "ENVIRONMENT_DIVISION",
   "DATA_DIVISION",
   "PROCEDURE_DIVISION",
};

typedef struct {
   char		level	;
   tpdecl *	decls	;
} tstack;

	char	Section		= 0;
	rbool	NameCheck	= rfalse;
	int	acc		= PAF_REF_READ;

extern	int	error_count	;

static	tpdecl	decls		= NULL;		/* declared data	*/
static	tpuse	forwards	= NULL;		/* used     data	*/
static	tpdecl	sections	= NULL;		/* declared labels	*/
static	tplabel	labels		= NULL;		/* used     labels	*/

static	tpdecl	stack_2 [52]	;
static	tstack	stack [52]	= {{01, & decls}};
static	int	stack_ptr	= 0;
static	int	cur_level	= 01;
static	tpdecl	field [52]	;
static	int	field_ptr	= 0;
static	tpdecl	cpl_dcl [52]	;
static	int	cpl_dcl_ptr	= 0;
static	tpdecl	cpl_use [52]	;
static	int	cpl_use_ptr	= 0;

static	char *	current_ident, * current_class;
static	char *	current_subr, * ref_class;

static void Predef
#if defined __STDC__ | defined __cplusplus
   (char * name, unsigned long mask)
#else
   (name, mask) char * name; unsigned long mask;
#endif
{
   if (dialect & mask) {
      register tpdecl decl = (tpdecl) Alloc ((unsigned long) sizeof (tdecl));
      decl->position	= NoPosition;
      decl->end_pos	= NoPosition;
      decl->e_pos	= NoPosition;
      decl->level	= 01;
      decl->name	= MakeIdent (name, strlen (name));
      decl->next	= NULL;
      decl->fields	= NULL;
      * stack [0].decls	= decl;
      stack [0].decls	= & decl->next;
   }
}

void BeginDeftab ARGS ((void))
{
   decls	= NULL;		/* declared data	*/
   forwards	= NULL;		/* used     data	*/
   sections	= NULL;		/* declared labels	*/
   labels	= NULL;		/* used     labels	*/

   stack [0].level = 01;
   stack [0].decls = & decls;
   stack_ptr	= 0;
   cur_level	= 01;
   field_ptr	= 0;
   cpl_dcl_ptr	= 0;
   cpl_use_ptr	= 0;

/*
   Predef ("PRINTER"	, all);
   Predef ("PRINTER-1"	, all);
   Predef ("CONSOLE"	, all);
#include "predef.c"
*/
}

static void WriteDecls
#if defined __STDC__ | defined __cplusplus
   (register tpdecl decls, int k)
#else
   (decls, k) register tpdecl decls; int k;
#endif
{
   register int i;
   for (; decls; decls = decls->next) {
      WritePosition (stdout, decls->position);
      for (i = 1; i <= k; i ++) (void) printf ("  ");
      (void) printf ("  %02d  ", decls->level);
      WriteIdent (stdout, decls->name);
      (void) printf ("\n");
      WriteDecls (decls->fields, k + 1);
   }
}

static void WriteForwards
#if defined __STDC__ | defined __cplusplus
   (register tpuse forwards, int k)
#else
   (forwards, k) register tpuse forwards; int k;
#endif
{
   register int i;
   for (; forwards; forwards = forwards->next) {
      WritePosition (stdout, forwards->position);
      for (i = 0; i <= k; i ++) (void) printf ("  ");
      WriteIdent (stdout, forwards->name);
      (void) printf ("\n");
      WriteForwards (forwards->fields, k + 1);
   }
}

static void WriteSections
#if defined __STDC__ | defined __cplusplus
   (register tpdecl sections, int k)
#else
   (sections, k) register tpdecl sections; int k;
#endif
{
   register int i;
   for (; sections; sections = sections->next) {
      WritePosition (stdout, sections->position);
      (void) printf ("  ");
      sections->end_pos.FileName = NoIdent;
      WritePosition (stdout, sections->end_pos);
      (void) printf ("  ");
      sections->e_pos.FileName = NoIdent;
      WritePosition (stdout, sections->e_pos);
      for (i = 0; i <= k; i ++) (void) printf ("  ");
      WriteIdent (stdout, sections->name);
      (void) printf ("\n");
      if (k == 0) WriteSections (sections->fields, 1);
   }
}

static void WriteLabels
#if defined __STDC__ | defined __cplusplus
   (register tplabel labels)
#else
   (labels) register tplabel labels;
#endif
{
   for (; labels; labels = labels->next) {
      WritePosition (stdout, labels->position);
      (void) printf ("  ");
      WriteIdent (stdout, labels->name);
      (void) printf ("  ");
      WriteIdent (stdout, labels->name2);
      (void) printf ("\n");
   }
}

void WriteDeftab ARGS ((void))
{
   (void) printf ("decls\n");	 WriteDecls	(decls, 0);
   (void) printf ("forwards\n"); WriteForwards	(forwards, 0);
   (void) printf ("sections\n"); WriteSections	(sections, 0);
   (void) printf ("labels\n");	 WriteLabels	(labels);
}
/*
*/

tpdecl Declare
#if defined __STDC__ | defined __cplusplus
   (register int level, tScanAttribute Attribute, int Type, tPosition e_pos)
#else
   (level, Attribute, Type, e_pos)
   register int		level		;
   tScanAttribute	Attribute	;
   int			Type		;
   tPosition		e_pos		;
#endif
{
   register tpdecl decl	= (tpdecl) Alloc ((unsigned long) sizeof (tdecl));
   decl->position	= Attribute.Position;
   decl->end_pos	= Attribute.name.EPos;
   decl->e_pos		= e_pos;
   decl->level		= level;
   decl->name		= Attribute.name.Ident;
   decl->next		= NULL;
   decl->fields		= NULL;

   switch (level) {
   case MN:		/* mnemonic  name of SPECIAL-NAMES	*/
   case CN:		/* condition name of SPECIAL-NAMES	*/
   case FD:
   case SD:
   case CD:
   case RD: stack_ptr = 0; break;

   case 01: if (Section == cFILE_SCT || Section == cCOMM_SCT) {
	       if (stack_ptr == 0) {
		  stack [1].level = level;
		  stack [1].decls = & cpl_dcl [1]->fields;
	       }
	       stack_ptr = 1;
	    } else {
	       stack_ptr = 0;
	    }
	    cur_level = level; break;

   case 66: if (Section == cFILE_SCT) {
	       if (stack_ptr == 0) {		/* if fields are missing: */
		  stack [1].level = level;	/* should not occur	  */
		  stack [1].decls = & cpl_dcl [1]->fields;
		  stack_ptr = 1;
		  break;
	       }
	       if (stack_ptr == 1) {
		  stack [2].level = level;
		  stack [2].decls = & cpl_dcl [2]->fields;
	       }
	       stack_ptr = 2;
	    } else {
	       if (stack_ptr == 0) {
		  stack [1].level = level;
		  stack [1].decls = & cpl_dcl [1]->fields;
	       }
	       stack_ptr = 1;
	    } break;

   case 77:
   case 78: stack_ptr = 0; break;

   case IB:		/* INDEXED BY of data description	*/
	    {
	       tpdecl prev_cpl_dcl	= cpl_dcl [1];
	       int prev_cpl_dcl_ptr	= cpl_dcl_ptr;

	       * stack [0].decls = decl;
	       stack [0].decls = & decl->next;
	       cpl_dcl [cpl_dcl_ptr = 1] = decl;
	       cpl_dcl [1] = prev_cpl_dcl;
	       cpl_dcl_ptr = prev_cpl_dcl_ptr;
	       return decl;
	    }

   case 88:
   default:		/* levels 02 - 49			*/
      if (level < cur_level) {
	 while (level < stack [-- stack_ptr].level);
	 if (level > stack [stack_ptr].level)
	    stack [++ stack_ptr].level = level;
	 cur_level = level;
      } else if (level > cur_level) {
	 if (cpl_dcl_ptr > 0) {
	    stack_ptr ++;
	    stack [stack_ptr].decls = & cpl_dcl [stack_ptr]->fields;
	 }
	 stack [stack_ptr].level = level;
	 cur_level = level;
      }
   }

   * stack [stack_ptr].decls = decl;
   stack [stack_ptr].decls = & decl->next;
   cpl_dcl [cpl_dcl_ptr = stack_ptr + 1] = decl;
   return decl;
}

tpdecl DeclareLabel
#if defined __STDC__ | defined __cplusplus
   (tScanAttribute Attribute, int Type, tPosition e_pos)
#else
   (Attribute, Type, e_pos) tScanAttribute Attribute; int Type; tPosition e_pos;
#endif
{
   register tpdecl decl	= (tpdecl) Alloc ((unsigned long) sizeof (tdecl));
   decl->position	= Attribute.Position;
   decl->end_pos	= Attribute.name.EPos;
   decl->e_pos		= e_pos;
   decl->level		= Type;
   decl->name		= Attribute.name.Ident;
   if (Type == lSECTION || sections == NULL || sections->level != lSECTION) {
      decl->fields	= NULL;
      decl->next	= sections;
      sections		= decl;
      cpl_dcl_ptr	= 1;
   } else {
      decl->fields	= sections;
      decl->next	= sections->fields;
      sections->fields	= decl;
      cpl_dcl_ptr	= 2;
   }
   if (Type != lENTRY) current_subr = GetCStr (Attribute.name.Ident);
   return decl;
}

void DeclareEnd
#if defined __STDC__ | defined __cplusplus
   (int Type, tPosition e_pos)
#else
   (Type, e_pos) int Type; tPosition e_pos;
#endif
{
   register tpdecl decl = sections;

   while (decl->level == lENTRY) decl = decl->next;

   if (Type == lSECTION) {
      if (decl->level == lSECTION) decl->e_pos = e_pos;
   } else {
      if (decl->level == lSECTION) {
	 decl = decl->fields;
	 while (decl->level == lENTRY) decl = decl->next;
      }
      decl->e_pos = e_pos;
   }
}

tpdecl search_next
#if defined __STDC__ | defined __cplusplus
   (register tIdent name, register tpdecl decls)
#else
   (name, decls) register tIdent name; register tpdecl decls;
#endif
{
   while (decls) if (decls->name == name) return decls; else decls = decls->next;
   return NULL;
}

tpdecl search_all
#if defined __STDC__ | defined __cplusplus
   (register tIdent name, register tpdecl decls, int nest)
#else
   (name, decls, nest)
   register tIdent	name		;
   register tpdecl	decls		;
	    int		nest		;
#endif
{
   while (decls) {
      if (decls->name == name) return cpl_use [cpl_use_ptr = nest] = decls;
      if (decls->fields) {
	 register tpdecl decl = search_all (name, decls->fields, nest + 1);
	 if (decl) { cpl_use [nest] = decls; return decl; }
      }
      decls = decls->next;
   }
   return NULL;
}

void CloseForwards ARGS ((void))
{
   tpuse forward = forwards;
   tpdecl decl;
   tpuse field;
   tScanAttribute Attribute;

   while (forward) {
      current_subr = section_name [forward->section];
      Attribute.name.Ident = forward->name;
      Attribute.Position   = forward->position;
      Attribute.name.EPos  = forward->end_pos;
      decl = UseName (Attribute);
      for (field = forward->fields; field && decl; field = field->fields) {
	 Attribute.name.Ident = field->name;
	 Attribute.Position   = field->position;
	 Attribute.name.EPos  = field->end_pos;
	 decl = UseField (Attribute, decl->fields);
      }
      forward = forward->next;
   }
}

tpdecl UseName
#if defined __STDC__ | defined __cplusplus
   (tScanAttribute Attribute)
#else
   (Attribute) tScanAttribute Attribute;
#endif
{
   tpdecl decl = search_next (Attribute.name.Ident, decls);

   if (decl) {
      field [field_ptr = 1] = cpl_use [cpl_use_ptr = 1] = decl;
   } else {
      decl = search_all (Attribute.name.Ident, decls, 1);
      if (decl) {
	 field [field_ptr = 1] = decl;
      } else {
	 int PrevSection = Section;
	 Section = cWS_SCT;
	 field [field_ptr = 1] =
	 cpl_use [cpl_use_ptr = 1] = Declare (1, Attribute, oDATA, Attribute.name.EPos);
	 Section = PrevSection;
	 if (NameCheck) {
	    MessageI ("name not declared", xxError, Attribute.Position, xxIdent, (char *) & Attribute.name.Ident);
	    error_count ++;
	 }
      }
   }

   if (cross_ref_fp) {
      if (cpl_use_ptr == 1) {
	 put_cross_ref (PAF_REF_TO_GLOB_VAR, PAF_FUNC_DEF, PAF_REF_SCOPE_GLOBAL,
	    null, current_subr, null, null, GetCStr (Attribute.name.Ident),
	    null, current_file, (int) Attribute.Position.Line, acc);
      } else {
	 ref_class = GetCStr (cpl_use [cpl_use_ptr - 1]->name);
	 if (ref_class [0] == '\0') ref_class = "FILLER";
	 put_cross_ref (PAF_REF_TO_MBR_VAR, PAF_FUNC_DEF, PAF_REF_SCOPE_GLOBAL,
	    null, current_subr, null, ref_class, GetCStr (Attribute.name.Ident),
	    null, current_file, (int) Attribute.Position.Line, acc);
      }
   }

   return decl;
}

tpdecl UseField
#if defined __STDC__ | defined __cplusplus
   (tScanAttribute Attribute, tpdecl decls)
#else
   (Attribute, decls) tScanAttribute Attribute; tpdecl decls;
#endif
{
   tpdecl decl = search_next (Attribute.name.Ident, decls);

   if (decl) {
      field [++ field_ptr] = cpl_use [++ cpl_use_ptr] = decl;
   } else {
      decl = search_all (Attribute.name.Ident, decls, cpl_use_ptr + 1);
      if (decl) {
	 field [++ field_ptr] = decl;
      } else {
	 if (NameCheck) {
	    MessageI ("field not declared", xxError, Attribute.Position, xxIdent, (char *) & Attribute.name.Ident);
	    error_count ++;
	 }
      }
   }

   if (cross_ref_fp) {
      if (cpl_use_ptr == 1) {
	 put_cross_ref (PAF_REF_TO_MBR_VAR, PAF_FUNC_DEF, PAF_REF_SCOPE_GLOBAL,
	    null, current_subr, null, null, GetCStr (Attribute.name.Ident),
	    null, current_file, (int) Attribute.Position.Line, acc);
      } else {
	 ref_class = GetCStr (cpl_use [cpl_use_ptr - 1]->name);
	 if (ref_class [0] == '\0') ref_class = "FILLER";
	 put_cross_ref (PAF_REF_TO_MBR_VAR, PAF_FUNC_DEF, PAF_REF_SCOPE_GLOBAL,
	    null, current_subr, null, ref_class, GetCStr (Attribute.name.Ident),
	    null, current_file, (int) Attribute.Position.Line, acc);
      }
   }

   return decl;
}

void UseForward
#if defined __STDC__ | defined __cplusplus
   (tScanAttribute Attribute)
#else
   (Attribute) tScanAttribute Attribute;
#endif
{
   register tpuse use	= (tpuse) Alloc ((unsigned long) sizeof (tuse));
   use->name		= Attribute.name.Ident;
   use->position	= Attribute.Position;
   use->end_pos		= Attribute.name.EPos;
   use->next		= forwards;
   use->fields		= NULL;
   use->section		= Section;
   forwards		= use;
}

void UseFieldForward
#if defined __STDC__ | defined __cplusplus
   (tScanAttribute Attribute)
#else
   (Attribute) tScanAttribute Attribute;
#endif
{
   register tpuse p, use = (tpuse) Alloc ((unsigned long) sizeof (tuse));
   use->name		= Attribute.name.Ident;
   use->position	= Attribute.Position;
   use->end_pos		= Attribute.name.EPos;
   use->next		= NULL;
   use->fields		= NULL;
   use->section		= Section;
   forwards->position	= Attribute.Position;
   for (p = forwards; p->fields != NULL; p = p->fields);
   p->fields		= use;
}

void UseLabel
#if defined __STDC__ | defined __cplusplus
   (tScanAttribute Attribute)
#else
   (Attribute) tScanAttribute Attribute;
#endif
{
   register tplabel label = (tplabel) Alloc ((unsigned long) sizeof (tlabel));
   label->name		= Attribute.name.Ident;
   label->name2		= NoIdent;
   label->scope		= sections;
   label->position	= Attribute.Position;
   label->end_pos	= Attribute.name.EPos;
   label->next		= labels;
   labels		= label;

   if (cross_ref_fp) {
      put_cross_ref (PAF_REF_TO_FUNCTION, PAF_FUNC_DEF, PAF_REF_SCOPE_GLOBAL,
	 null, current_subr, null, null, GetCStr (Attribute.name.Ident),
	 null, current_file, (int) Attribute.Position.Line, acc);
   }
}

void UseLabel2
#if defined __STDC__ | defined __cplusplus
   (tScanAttribute Attribute, tScanAttribute Attribute2)
#else
   (Attribute, Attribute2) tScanAttribute Attribute; tScanAttribute Attribute2;
#endif
{
   UseLabel (Attribute);
   labels->name2 = Attribute2.name.Ident;

   if (cross_ref_fp) {
      put_cross_ref (PAF_REF_TO_FUNCTION, PAF_FUNC_DEF, PAF_REF_SCOPE_GLOBAL,
	 null, current_subr, null, null, GetCStr (Attribute2.name.Ident),
	 null, current_file, (int) Attribute2.Position.Line, acc);
   }
}

void UseLabelExtern
#if defined __STDC__ | defined __cplusplus
   (tScanAttribute Attribute)
#else
   (Attribute) tScanAttribute Attribute;
#endif
{
   if (cross_ref_fp) {
      put_cross_ref (PAF_REF_TO_FUNCTION, PAF_FUNC_DEF, PAF_REF_SCOPE_GLOBAL,
	 null, current_subr, null, null, GetCStr (Attribute.name.Ident),
	 null, current_file, (int) Attribute.Position.Line, acc);
   }
}

tpdecl search_next_label
#if defined __STDC__ | defined __cplusplus
   (register tIdent name, register tpdecl decls)
#else
   (name, decls) register tIdent name; register tpdecl decls;
#endif
{
   while (decls) if (decls->name == name) return decls; else decls = decls->next;
   return NULL;
}

tpdecl search_all_label
#if defined __STDC__ | defined __cplusplus
   (register tIdent name, register tpdecl decls)
#else
   (name, decls) register tIdent name; register tpdecl decls;
#endif
{
   register tpdecl decl;
   while (decls) {
      if (decls->name == name) return decls;
      decl = search_next_label (name, decls->fields);
      if (decl) return decl;
      decls = decls->next;
   }
   return NULL;
}

void CloseLabels ARGS ((void))
{
   tplabel label = labels;
   register tpdecl decl, decl2;
   if (! NameCheck) return;
   while (label) {
      if (label->name2 == NoIdent) {
	 decl = search_next_label (label->name, label->scope);
	 if (! decl) decl = search_all_label (label->name, sections);
	 if (decl) {
	    field_ptr = 1;
	    field [1] = decl;
	    if (decl->level == lPARAGRPH && decl->fields != NULL) {
	       cpl_use_ptr = 2;
	       cpl_use [1] = decl->fields;
	       cpl_use [2] = decl;
	    } else {
	       cpl_use_ptr = 1;
	       cpl_use [1] = decl;
	    }
	 } else {
	    MessageI ("label not declared", xxError, label->position, xxIdent, (char *) & label->name);
	    error_count ++;
	 }
      } else {
	 decl = search_next_label (label->name2, sections);
	 if (decl && decl->level == lSECTION) {
	    decl2 = search_next_label (label->name, decl->fields);
	    if (decl2) {
	       field_ptr = cpl_use_ptr = 2;
	       field [1] = cpl_use [1] = decl;
	       field [2] = cpl_use [2] = decl2;
	    } else {
	       MessageI ("paragraph not declared", xxError, label->position, xxIdent, (char *) & label->name);
	       error_count ++;
	    }
	 } else {
	    MessageI ("section not declared", xxError, label->position, xxIdent, (char *) & label->name2);
	    error_count ++;
	 }
      }
      label = label->next;
   }
}

void CloseDeftab ARGS ((void))
{
   CloseForwards	();
   CloseLabels		();
   /* WriteDeftab	(); */
}

static void ReleaseDecls
#if defined __STDC__ | defined __cplusplus
   (register tpdecl decls)
#else
   (decls) register tpdecl decls;
#endif
{
   while (decls) {
      tpdecl next = decls->next;
      ReleaseDecls (decls->fields);
      Free ((unsigned long) sizeof (tdecl), (char *) decls);
      decls = next;
   }
}

static void ReleaseForwards
#if defined __STDC__ | defined __cplusplus
   (register tpuse forwards)
#else
   (forwards) register tpuse forwards;
#endif
{
   while (forwards) {
      tpuse next = forwards->next;
      ReleaseForwards (forwards->fields);
      Free ((unsigned long) sizeof (tuse), (char *) forwards);
      forwards = next;
   }
}

static void ReleaseSections
#if defined __STDC__ | defined __cplusplus
   (register tpdecl sections, int k)
#else
   (sections, k) register tpdecl sections; int k;
#endif
{
   while (sections) {
      tpdecl next = sections->next;
      if (k == 0) ReleaseSections (sections->fields, 1);
      Free ((unsigned long) sizeof (tdecl), (char *) sections);
      sections = next;
   }
}

static void ReleaseLabels
#if defined __STDC__ | defined __cplusplus
   (register tplabel labels)
#else
   (labels) register tplabel labels;
#endif
{
   while (labels) {
      tplabel next = labels->next;
      Free ((unsigned long) sizeof (tlabel), (char *) labels);
      labels = next;
   }
}

void ReleaseDeftab ARGS ((void))
{
   ReleaseForwards	(forwards);
   ReleaseSections	(sections, 0);
   ReleaseLabels	(labels);
   ReleaseDecls		(decls);
}

static void GetEPos
#if defined __STDC__ | defined __cplusplus
   (register tpdecl decls, tPosition * e_pos)
#else
   (decls, e_pos) register tpdecl decls; tPosition * e_pos;
#endif
{
	if (decls->next  ) GetEPos (decls->next  , e_pos);
   else if (decls->fields) GetEPos (decls->fields, e_pos);
   else * e_pos = decls->e_pos;
}

static void PutDecls
#if defined __STDC__ | defined __cplusplus
   (register tpdecl decls, int k)
#else
   (decls, k) register tpdecl decls; int k;
#endif
{
   for (; decls; decls = decls->next) {
      if (decls->name != NoIdent) {
	 current_ident = GetCStr (decls->name);
	 if (decls->level == 78 || decls->level == 88) {

	    /* constant */

	    put_symbol (PAF_CONS_DEF, null, current_ident, current_file,
	       (int) decls->position.Line, (int) decls->position.Column - 1,
	       (int) decls->end_pos.Line, (int) decls->end_pos.Column,
	       attr, null, null, null, null, 0, 0, 0, 0);
	 } else {
	    if (k == 0) {

	       /* variable declared on outer level */
	       
	       put_symbol (PAF_GLOB_VAR_DEF, null, current_ident, current_file,
		  (int) decls->position.Line, (int) decls->position.Column - 1,
		  (int) decls->end_pos.Line, (int) decls->end_pos.Column,
		  attr, null, null, null, null, 0, 0, 0, 0);
	    } else {

	       /* variable declared on inner level */
		  
	       current_class = GetCStr (stack_2 [k - 1]->name);
	       if (current_class [0] == '\0') current_class = "FILLER";
	       put_symbol (PAF_MBR_VAR_DEF, current_class, current_ident, current_file,
		  (int) decls->position.Line, (int) decls->position.Column - 1,
		  (int) decls->end_pos.Line, (int) decls->end_pos.Column,
		  attr, null, null, null, null, 0, 0, 0, 0);
	    }
	    if (decls->fields) {

	       /* variable has members */
		  
	       tPosition e_pos; GetEPos (decls->fields, & e_pos);
	   /* was PAF_STRUCT_DEF, does not work any more because of new paf.h */
	       put_symbol (PAF_CLASS_DEF, null, current_ident, current_file,
		  (int) decls->position.Line, (int) decls->position.Column - 1,
		  (int) e_pos.Line, (int) e_pos.Column,
		  attr | PAF_STRUCT_DEF, null, null, null, null, 0, 0, 0, 0);
	       stack_2 [k] = decls;
	       PutDecls (decls->fields, k + 1);
	    } else {

	       /* variable has no members */
		  
	    }
	 }
      } else {						/* FILLER */
	 if (decls->level == 78 || decls->level == 88) {

	    /* constant */

	 } else {
	    if (decls->fields) {

	       /* FILLER has members */
		  
	       stack_2 [k] = decls;
	       PutDecls (decls->fields, k + 1);
	    }
	 }
      }
   }
}

static void PutSections
#if defined __STDC__ | defined __cplusplus
   (register tpdecl sections, int k)
#else
   (sections, k) register tpdecl sections; int k;
#endif
{
   for (; sections; sections = sections->next) {
      current_ident = GetCStr (sections->name);
      put_symbol (PAF_FUNC_DEF, null, current_ident, current_file,
	 (int) sections->position.Line, (int) sections->position.Column - 1,
	 (int) sections->e_pos.Line, (int) sections->e_pos.Column,
	 attr, null, null, null, null, 0, 0, 0, 0);
   /* put_symbol (PAF_FUNC_DCL, null, current_ident, current_file,
	 (int) sections->position.Line, (int) sections->position.Column - 1,
	 (int) sections->e_pos.Line, (int) sections->e_pos.Column,
	 attr, null, null, null, null, 0, 0, 0, 0);
   */
   /* put_symbol (PAF_SUBR_DEF, null, current_ident, current_file,
	 (int) sections->position.Line, (int) sections->position.Column - 1,
	 (int) sections->e_pos.Line, (int) sections->e_pos.Column,
	 attr, null, null, null, null, 0, 0, 0, 0);
   */
      if (k == 0) PutSections (sections->fields, 1);
   }
}

void PutDeftab ARGS ((void))
{
   PutSections	(sections, 0);
   PutDecls	(decls, 0);
}

