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

#ifndef MX_VARARGS
#if defined (__MSVC__) || defined(__STDC__) || defined(HAS_STDARG)
#include <stdarg.h>
#   define MX_VARARGS(type, name) (type name, ...)
#   define MX_VARARGS_DEF(type, name) (type name, ...)
#   define MX_VARARGS_START(str, type, name, list) \
      va_start(list, name); str = name;
#else
#include <varargs.h>
#   ifdef __cplusplus
#       define MX_VARARGS(type, name) (type name, ...)
#       define MX_VARARGS_DEF(type, name) (type va_alist, ...)
#   else
#       define MX_VARARGS(type, name) ()
#       define MX_VARARGS_DEF(type, name) (va_alist)
#   endif
#   define MX_VARARGS_START(str, type, name, list) \
        va_start(list); str = va_arg(list, type);
#endif
#endif /* MX_VARARGS */

typedef struct __LongString
{
   unsigned int   allocated;
   unsigned short slot_size;
   unsigned short total_fields;
   unsigned short allocated_field_pointers;
   int len;
   char  *buf;
   char  **field_value;
   short *field_size;
   char  *(*append)(struct __LongString *,char *string,unsigned int len);
   char  *(*appendstrings)MX_VARARGS(struct __LongString *,str);
   char  *(*copystrings)MX_VARARGS(struct __LongString *,str);
   char  *(*copy)(struct __LongString *,char *string,unsigned int len);
   char  *(*makespace)(struct __LongString *,unsigned int len);
   char  *(*min)(struct __LongString *str);
   char  *(*dup)(struct __LongString *ostr,struct __LongString *istr);
   void  (*free)(struct __LongString *str);
   char  *(*fgets)(struct __LongString *str,FILE *stream);
   char  *(*trim)(struct __LongString *str,char *trim_pat);
   char  *(*readfile)(struct __LongString *str,char *filename);
   unsigned int   (*split)(struct __LongString *str,void *string,int length,int dup_flag,int separator,int maxfields);
} LongString;

void LongStringInit(LongString *str,int slot_size);
char *SN_StrDup(char*);

