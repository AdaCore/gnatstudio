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
#include <memory.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#ifndef WIN32
#include <unistd.h>
#endif /* WIN32 */
#ifdef __MSVC__
#include <io.h>
#endif

#include "longstr.h"
#include "tcl.h"

#define  STRING_SLOT_SIZE  500      /* It must be greater then 0 ! */
#define  FIELD_POINTER_SLOT_SIZE 15 /* It must be greater then 0 ! */

static unsigned int
LongStringSplit(LongString *str, void *string, int length, int dup_flag, int separator, int maxfields)
{
   int   cou;
   char  sep = (char)separator;
   char  *p = (char *)string;

   if (dup_flag)
   {
      if (str->buf == (char *)string)
	  {
	  	p = str->buf;
	  }
	  else
	  {
        p = str->copy(str,(char *)string,length);
	  }
      string = (void *)p;     /* We have to remember to the beginning. */
   }

   if (!str->field_value)
   {
      str->allocated_field_pointers = FIELD_POINTER_SLOT_SIZE;
      str->field_value = (char**)ckalloc(str->allocated_field_pointers * sizeof(char *));
      str->field_size = (short*)ckalloc(str->allocated_field_pointers * sizeof(unsigned short));
   }
   for (cou = 0, str->field_value[0] = p; *p; p++)
   {
      if (*p == sep)
      {
         if (dup_flag)
            *p = '\0';

         if (cou + 1 == str->allocated_field_pointers)
         {
            str->allocated_field_pointers += FIELD_POINTER_SLOT_SIZE;

            str->field_value = (char**)ckrealloc((char*)str->field_value,
               str->allocated_field_pointers * sizeof(char *));

            str->field_size = (short*)ckrealloc((char*)str->field_size,
               str->allocated_field_pointers * sizeof(unsigned short));
         }
         str->field_size[cou] = (unsigned short)(p - str->field_value[cou]);
         str->field_value[++cou] = p + 1;

         /* The rest will be taken as one field. */
         if (maxfields > 0 && cou == maxfields - 1)
         {
            /* We have to point to the end to compute correct size! */
            if (dup_flag)
            {
               p = str->buf + str->len;
            }
            else if (length > 0)
            {
               p = (char *)string + length + 1;
            }
            else
            {
               p += strlen(p) + 1;
            }
            break;
         }
      }
   }
   str->field_size[cou] = (unsigned short)(p - str->field_value[cou]);

   str->total_fields = cou + 1;
   return (unsigned int)str->total_fields;
}

/* Duplicates the item. */
static   char *
LongStringDup(LongString *ostr,LongString *istr)
{
   memcpy((void *)ostr,(void *)istr,sizeof(LongString));

   if (istr->buf)
   {
      int   len = istr->len + 1; /* Include '\0' too ! */
      ostr->buf = (char*)ckalloc(len);
      if (!ostr->buf)
      {
         ostr->free(ostr);

         return NULL;
      }
      memcpy((void *)ostr->buf,(void *)istr->buf,len);
      ostr->allocated = len;
   }

   if (istr->field_value)
   {
      int   len;

      istr->allocated_field_pointers = istr->total_fields;
      len = istr->allocated_field_pointers * sizeof(char *);

      ostr->field_value = (char**)ckalloc(len);
      memcpy((void *)ostr->field_value,(void *)istr->field_value,len);

      len = istr->allocated_field_pointers * sizeof(unsigned short);

      ostr->field_size = (short*)ckalloc(len);
      memcpy((void *)ostr->field_size,(void *)istr->field_size,len);
   }

   return ostr->buf;
}

static void
LongStringFree(LongString *str)
{
   if (str->buf)
   {
      ckfree(str->buf);
      str->buf = NULL;
   }
   if (str->field_value)
   {
      ckfree((char*)str->field_value);
      str->field_value = NULL;

      ckfree((char*)str->field_size);
      str->field_size = NULL;
   }

   str->allocated = 0;
   str->allocated_field_pointers = 0;
   str->total_fields = 0;
   str->len = 0;
}

/* Increases the buffer size when necessary */
static   char *
LongStringMakespace(LongString *str,unsigned int siz)
{
   siz++;   /* Include '\0'! */

   if (str->buf == NULL)
   {
      str->allocated = siz + str->slot_size;
      str->buf = (char*)ckalloc(str->allocated);
      *str->buf = '\0';
   }
   else if (str->allocated < siz)
   {
      char  *pb;

      str->allocated = siz + str->slot_size;
      pb = (char*)ckrealloc(str->buf,str->allocated);
      if (!pb)
      {
         str->free(str);

         return NULL;
      }
      str->buf = pb;
   }

   return str->buf;
}

/* Frees the not needed memory. */
static   char *
LongStringMin(LongString *str)
{
   if (str->buf)
   {
      char  *bp = (char*)ckrealloc(str->buf,str->len + 1);
      if (bp)
      {
         str->buf = bp;
      }
   }
   return str->buf;
}

/* Append the string, and reallocate buffer when necessary! */
static   char *
LongStringAppend(LongString *str,char *string,int len)
{
   if (len == 0)
   {
      return str->buf;
   }

   if (len < 0)      /* Copy until '\0'! */
   {
      char  *pi;
      char  *po;
      int   s;

      if (!str->buf)
      {
         str->allocated = str->slot_size;
         str->buf = (char*)ckalloc(str->allocated);
         if (!str->buf)
         {
            str->allocated = 0;
            return NULL;
         }
      }
      for (pi = string, po = &str->buf[str->len], s = str->allocated - (po - str->buf) - 1;;)
      {
         char  *bf;

         /* Copy as much as possible! */
         while (s-- > 0 && *pi)
         {
            *po++ = *pi++;
         }

         if (*pi == '\0')  /* We have reached the end. */
            break;

         /* The buffer must be reallocated! */
         str->allocated += str->slot_size;
         bf = (char*)ckrealloc(str->buf,str->allocated);
         if (!bf)
         {
            str->free(str);

            return NULL;
         }
         po = bf + (po - str->buf);
         str->buf = bf;
         s = str->allocated - (po - str->buf) - 1;
      }
      *po = '\0';
      str->len = po - str->buf;
   }
   else
   {
      /* +1 is necessary because we will append '\0' too. */
      if (len >= (int)(str->allocated - str->len))
      {
         char  *bf;

         /* Allocate always bit more to reduce the number of reallocs! */
         if (str->buf)
         {
            str->allocated += len + str->slot_size;
            bf = (char*)ckrealloc(str->buf,str->allocated);
         }
         else
         {
            str->allocated = len + str->slot_size;
            bf = (char*)ckalloc(str->allocated);
         }
         if (!bf)
         {
            str->free(str);

            return NULL;
         }
         str->buf = bf;
      }
      /* printf("COPY: <%*.*s>,%d\n",len,len,string,len); */
      memcpy(&str->buf[str->len],string,len);
      str->len += len;
      str->buf[str->len] = '\0';
   }

   return str->buf;
}

/* Copy the string! */
static   char *
LongStringCopy(LongString *str,char *string,int len)
{
   str->len = 0;

   return LongStringAppend(str, string, len);
}

/* Adds multiple strings, the list must be terminated by NULL. */
static   char *
LongStringAppendStrings MX_VARARGS_DEF(LongString *, arg1)
{
   LongString *str;
   va_list  args;
   char  *string;
   char  *pe = NULL;

/* str = MX_VARARGS_START(LongString *,arg1,args); rigo */
   MX_VARARGS_START(str,LongString *,arg1,args); /* rigo */
   while (1)
   {
      string = va_arg(args, char *);
      if (!string)
         break;

      pe = LongStringAppend(str,string,-1);
   }

   va_end(args);

   return pe;
}

/* Copies multiple strings, the list must be terminated by NULL. */
static   char *
LongStringCopyStrings MX_VARARGS_DEF(LongString *, arg1)
{
   LongString *str;
   va_list  args;
   char  *string;
   char  *pe = NULL;

/* str = MX_VARARGS_START(LongString *,arg1,args); rigo */
   MX_VARARGS_START(str,LongString *,arg1,args); /* rigo */
   str->len = 0;
   while (1)
   {
      string = va_arg(args, char *);
      if (!string)
         break;

      pe = LongStringAppend(str,string,-1);
   }

   va_end(args);

   return pe;
}

/* Reads the contents of a file into a dynamically allocated buffer. */
static   char *
LongStringReadFile(LongString *str,char *filename)
{
   struct   stat  statBuf;
   register unsigned char *p;
   register size_t len;
   int   om = O_RDONLY;
   int   fd;
   int   rdb;
#ifdef WIN32
   om |= O_BINARY;
#endif /* WIN32 */

   fd = open(filename,om);
   if (fd < 0)
   {
      str->free(str);

      return NULL;
   }

   if (fstat(fd, &statBuf) == -1)
   {
      str->free(str);

      close(fd);

      return NULL;
   }

   if (!str->buf)
   {
      p = (char *)ckalloc((size_t)statBuf.st_size+1);
   }
   else
   {
      p = (char *)ckrealloc(str->buf,(size_t)statBuf.st_size+1);

   }
   if (!p)
   {
      str->free(str);

      close(fd);

      return NULL;
   }

   str->buf = p;

   str->len = (int)statBuf.st_size;
   for (p = str->buf,len = (size_t)str->len, rdb = 1;
      rdb > 0 && len > 0;)
   {
      rdb = (size_t)read(fd, p, len);
      if (rdb > 0)
      {
         len -= rdb;
         p += rdb;
      }
   }

   close(fd);

   str->buf[str->len] = '\0';

   return str->buf;
}

/* Reads a line into a dynamically allocated buffer. */
static   char *
LongStringTrim(LongString *str,char *trim_pat)
{
	char	*pbeg;
	char	*pend;
	int		len;

	if (!str->buf)
		return NULL;

	if (!trim_pat)
		trim_pat = "\040\t\n\r";

	/* Skip leading characters ! */
	for (pbeg = &str->buf[0]; *pbeg && strchr(trim_pat,(int)*pbeg); pbeg++);
	
	pend = &str->buf[str->len - 1];
	if (pend < pbeg)
	{
		len = 0;
	}
	else
	{
		/* Skip traling characters ! */
		for (; pend > pbeg && strchr(trim_pat,(int)*pend); pend--);
		len = pend - pbeg + 1;
	}

	if (pbeg > str->buf)
	{
		memmove(str->buf,pbeg,len);
	}
	str->buf[len] = '\0';

	str->len = len;

	return str->buf;
}

/* Reads a line into a dynamically allocated buffer. */
static   char *
LongStringFgets(LongString *str,FILE *fp)
{
   char  *bf;
   char  *po;
   int   c;
   int   s;

   if (!fp)
      return NULL;

   if (!str->buf)
   {
      str->allocated = str->slot_size;
      str->buf = (char*)ckalloc(str->allocated);
      if (!str->buf)
      {
         str->allocated = 0;
         return NULL;
      }
   }

   for (c = EOF, po = str->buf, s = str->allocated - 1;;)
   {
      /* Read as much as possible! */
      while (s > 0 && (c = getc(fp)) != EOF && c != '\n')
      {
         if ((unsigned char)c != '\r')
         {
            *po++ = (char)c;
            s--;
         }
      }
      if (s != 0) /* We have read either '\n' or EOF. */
         break;

      /* The buffer must be reallocated! */
      str->allocated += str->slot_size;
      bf = (char*)ckrealloc(str->buf,str->allocated);
      if (!bf)
      {
         ckfree(str->buf);

         return NULL;
      }
      po = bf + (po - str->buf);
      str->buf = bf;
      s = str->allocated - (po - str->buf) - 1;
   }
   *po = '\0';
   str->len = po - str->buf;

   if (c == EOF && str->len == 0)
      return NULL;

   return str->buf;
}

void
LongStringInit(LongString *str,int slot_size)
{
   static   int   default_slot_size = 0;

   if (!default_slot_size)
   {
      char  *p;
      if ((p = getenv("LONGSTRING_SIZE")))
      {
         default_slot_size = atoi(p);
      }
      if (default_slot_size <= 0)
      {
         default_slot_size = STRING_SLOT_SIZE;
      }
   }

   memset((void *)str,0,sizeof(*str));

   str->slot_size = slot_size > 0 ? (unsigned short)slot_size : default_slot_size;
   str->append = (char * (*)(struct __LongString *,char *,unsigned int)) LongStringAppend;
   str->appendstrings = (char * (*)MX_VARARGS(struct __LongString *,str)) LongStringAppendStrings;
   str->copystrings = (char * (*)MX_VARARGS(struct __LongString *,str)) LongStringCopyStrings;
   str->copy = (char * (*)(struct __LongString *,char *string,unsigned int)) LongStringCopy;
   str->makespace = (char * (*)(struct __LongString *,unsigned int)) LongStringMakespace;
   str->min = (char *(*)(struct __LongString *)) LongStringMin;
   str->dup = (char *(*)(struct __LongString *,struct __LongString *)) LongStringDup;
   str->free = (void (*)(struct __LongString *)) LongStringFree;
   str->fgets = (char * (*)(struct __LongString *,FILE *)) LongStringFgets;
   str->trim = (char * (*)(struct __LongString *,char *)) LongStringTrim;
   str->readfile = (char * (*)(struct __LongString *,char *)) LongStringReadFile;
   str->split = (unsigned int (*)(struct __LongString *,void *,int,int,int,int)) LongStringSplit;
}

#undef TEST
#define  TEST 0
#if TEST
main()
{
   LongString str;
   LongString str2;
   int   cou,i;
   int   len;

   LongStringInit(&str,1);

   for (i = 0, cou = str.split(&str,"1 2",-1,0,' ',0); i < cou; i++)
   {
      printf("<%*.*s>\n",
         str.field_size[i],str.field_size[i],str.field_value[i]);
   };
   for (i = 0, cou = str.split(&str,"1 2 3def 4 5bcdef",-1,0,' ',0);
      i < cou; i++)
   {
      printf("<%*.*s>\n",
         str.field_size[i],str.field_size[i],str.field_value[i]);
   };
   for (i = 0, cou = str.split(&str,"AA\001BB",4,0,'\001',0); i < cou; i++)
   {
      printf("<%*.*s>\n",
         str.field_size[i],str.field_size[i],str.field_value[i]);
   };
   str.dup(&str2,&str);
   for (i = 0; i < cou; i++)
   {
      printf("<%*.*s>\n",
         str2.field_size[i],str2.field_size[i],str2.field_value[i]);
      printf("len: %d\n",str2.field_size[i]);
   };
#if 1
   str.appendstrings(&str,"1skfbjrhvbfr"," " "44442d,mn9"," " "3**",NULL);
   printf("<%s>\n",str.buf);
   str.copy(&str,"BAC",-1);
   str.append(&str,"Zsolt",-1);
   str.append(&str,"Hello",3);

   printf("<%s>\n",str.buf);

   str2.free(&str2);
   str.dup(&str2,&str);
   str2.min(&str2);
   printf("<%s>\n",str2.buf);

   str.copy(&str,"zk",-1);

   printf("<%s>\n",str.buf);
   str2.free(&str2);
   str.free(&str);
/*
   printf("%s",str.readfile(&str,"longstr.h"));
   printf("%s",str.readfile(&str,"longstr.c"));
*/
#endif
   str.free(&str);

   exit(0);
}
#endif

