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

#include "srchtbl.h"
#include "tcl.h"

typedef enum {Find, Add, Delete} Search_Action;

static SearchEntry *BtreeTableSearchFunc(SearchTable **htable,SearchEntry item,Search_Action action);
static SearchEntry *BtreeTableSeq(SearchTable **hhtable,int pos);

static SearchEntry *HashTableSearchFunc(SearchTable **htable,SearchEntry item,Search_Action action,int flag);
static SearchEntry *HashTableSearch(SearchTable **htable,SearchEntry item);
static SearchEntry *HashTableSearch(SearchTable **htable,SearchEntry item);
static SearchEntry *HashTableAdd(SearchTable **htable,SearchEntry item);
static SearchEntry *HashTableDelete(SearchTable **htable,SearchEntry item);
static SearchEntry *HashTableSeq(SearchTable **hhtable,int first);

/*
 * [Aho,Sethi,Ullman] Compilers: Principles, Techniques and Tools, 1986
 * [Knuth] The Art of Computer Programming, part 3 (6.4)
 */


/*
 * For the double hash method used here, the table size has to be a
 * prime. To correct the user given table size we need a prime test.
 * This trivial algorithm is adequate because
 * a) the code is (most probably) only called once per program run and
 * b) the number is small because the table must fit in the core
 */

static int
isprime(unsigned int number)
{
	/* no even number will be passed */
	register unsigned int div = 3;

	while (div*div < number && number%div != 0)
	{
		div += 2;
	}

	return number%div != 0;
}

static void
remove_entry(SearchTable *htable,SearchEntry *s)
{
	/* Call the user hook function if available ! */
	if (htable->_delete_entry)
		(*(htable->_delete_entry))(s);

	if (s->flag & SEARCH_DUP_KEY)
		ckfree(s->key);

	if (s->flag & SEARCH_DUP_DATA)
		ckfree(s->data);
}

/*
 * After use, the table can be destroyed. The memory used can
 * be freed and the local static variable can be marked as not used.
 */

static void
TableDestroy(SearchTable **hhtable)
{
	SearchTable *htable = *hhtable;
	SearchEntry *s;

	for (s = htable->seq(hhtable,0); s; s = htable->seq(hhtable,1))
	{
		remove_entry(htable,s);
	}
	ckfree((void *)htable->data);
	ckfree((void *)htable);

	*hhtable = NULL;
}

static SearchEntry *
BtreeTableAdd(SearchTable **htable,SearchEntry item)
{
	return BtreeTableSearchFunc(htable,item,Add);
}

static SearchEntry *
BtreeTableDelete(SearchTable **htable,SearchEntry item)
{
	return BtreeTableSearchFunc(htable,item,Delete);
}

static SearchEntry *
BtreeTableSearch(SearchTable **htable,SearchEntry item)
{
	return BtreeTableSearchFunc(htable,item,Find);
}

static SearchEntry *
BtreeTableSeq(SearchTable **hhtable,int pos)
{
	SearchTable *htable = *hhtable;
	int hsize = htable->filled;
	int	cou;

	if (!pos)
		cou = 0;
	else
		cou = htable->cursor + 1;

	if (cou >= hsize)
		return NULL;

	htable->cursor = cou;

	return &htable->data[cou].entry;
}

static SearchEntry *
BtreeTableSearchFunc(SearchTable **hhtable,SearchEntry item,Search_Action action)
{
	SearchTable *htable = *hhtable;
	register int l, u, idx;
	register SearchEntry *p = NULL;
	char  *key = item.key;
	int		key_len;
	int		comparison = -1;
	int		move_len;
	int		found;

	if (item.key_len == -1)
	{
		key_len = strlen(key);
		item.key_len = key_len;
	}
	else
		key_len = item.key_len;

	idx = 0;
	l = 0;
	u = htable->filled;
	found = 0;
	while (l < u)
	{
		idx = (l + u) / 2;
		p = &htable->data[idx].entry;

		comparison = *key - p->key[0];
		if (comparison == 0)
		{
			int		cmp = key_len - p->key_len;
			int		len;

			if (cmp < 0)
				len = key_len;
			else
				len = p->key_len;
			comparison = memcmp(key,p->key,len);

			if (comparison == 0)
				comparison = cmp;
		}

		if (comparison < 0)
			u = idx;
		else if (comparison > 0)
			l = idx + 1;
		else
		{
			found++;
			break;
		}
	}

	switch (action)
	{
	case Find:
		if (!found)
			return NULL;

		htable->cursor = idx;

		return p;

		break;

	case Delete:
		if (!found)
			return NULL;

		remove_entry(htable,p);

		move_len = htable->filled - idx + 1;

		memmove((void *)&htable->data[idx],
			(void *)&htable->data[idx+1],move_len * sizeof(DATA));

		htable->filled--;

		return p;

		break;

	case Add:
		if (found)
			return p;

		if (htable->filled + 1 >= htable->hsize)
		{
			htable->hsize *= 2;
			htable->data = (DATA*)ckrealloc((char*)htable->data,
				htable->hsize * sizeof(DATA));
		}

		if (comparison > 0)
		{
			idx++;
		}
		htable->cursor = idx;

		move_len = htable->filled - idx + 1;

		memmove((void *)&htable->data[idx+1],
			(void *)&htable->data[idx],move_len * sizeof(DATA));

		if (item.flag & ~(SEARCH_DUP_KEY | SEARCH_DUP_DATA |SEARCH_FIRST))
		{
			fprintf(stderr,"Bad flag value, file: %s line: %d\n",
				__FILE__,__LINE__);

			return NULL;
		}
		if (item.flag & SEARCH_DUP_KEY)
		{
			char	 *new_key = (char*)ckalloc(key_len + 1);

			memcpy(new_key,item.key,key_len);
			new_key[key_len] = '\0';
			item.key = new_key;
		}
		if (item.flag & SEARCH_DUP_DATA)
		{
			char  *data = NULL;

			if (item.data_len == -1)
			{
				item.data_len = strlen(item.data);
				data = (char*)ckalloc(item.data_len + 1);
				memcpy(data,item.data,item.data_len + 1);
			}
			else
			{
				data = (char*)ckalloc(item.data_len + 1);
				memcpy(data,item.data,item.data_len);
				data[item.data_len] = '\0';
			}
			item.data = data;
		}

/*	 printf("<%s> idx: %d\n",key,idx); */

		htable->data[idx].entry = item;
		htable->data[idx].used = 1;
		htable->filled++;

		return &htable->data[idx].entry;

		break;
	}

	return NULL;
}

static SearchTable *
BtreeTableCreate(unsigned int nel,void (*delete_entry)(SearchEntry *entry))
{
	SearchTable *htable;

	if ((htable = (SearchTable *)ckalloc(sizeof(SearchTable))) == NULL)
		return NULL;
	memset (htable, 0, sizeof(SearchTable));

	if (nel == 0)
		nel = 1;

	if ((htable->data = (void*)ckalloc(nel*sizeof(DATA))) == NULL)
		return NULL;
	memset (htable->data, 0, nel*sizeof(DATA));

	htable->filled = 0;
	htable->hsize = nel;
	htable->add = (SearchEntry * (*)(struct __SearchTable **,SearchEntry)) BtreeTableAdd;
	htable->search = (SearchEntry * (*)(struct __SearchTable **,SearchEntry)) BtreeTableSearch;
	htable->delete = (SearchEntry * (*)(struct __SearchTable **,SearchEntry)) BtreeTableDelete;
	htable->seq = (SearchEntry * (*)(struct __SearchTable **,int)) BtreeTableSeq;
	htable->destroy = (void (*)(struct __SearchTable **)) TableDestroy;
	htable->_delete_entry = delete_entry;

	/* everything went ok */
	return htable;
}


/*
 * Before using the hash table we must allocate memory for it. Test
 * for an existing table. We allocate one element more than the found
 * prime number says. This is done for more effective indexing as
 * explained in the comment for the hsearch function.  The contents of
 * the table is zeroed. In particular, the field used becomes zero.
 */

static SearchTable *
HashTableCreate(unsigned int nel,void (*delete_entry)(SearchEntry *entry))
{
	SearchTable *htable;

	/* Ensure that the original table will hold the requested number
	 * of entries, with no need for enlargement.
	 */
	nel = (nel*10)/9+1;

	/* Change nel to the first prime number not smaller than nel. */
	nel |= 1;		/* make sn */
	while (!isprime(nel))
		nel += 2;

	/* allocate memory and zero out */
	if ((htable = (SearchTable *)ckalloc(sizeof(SearchTable))) == NULL)
		return NULL;
	memset (htable, 0, sizeof(SearchTable));
	if ((htable->data = (void*)ckalloc((nel+1)*sizeof(DATA))) == NULL)
	{
		ckfree((void *)htable);
		return NULL;
	}
	memset (htable->data, 0, (nel+1)*sizeof(DATA));
	htable->hsize  = nel;
	htable->filled = 0;
	htable->cursor = 0;

	htable->add = (SearchEntry * (*)(struct __SearchTable **,SearchEntry)) HashTableAdd;
	htable->search = (SearchEntry * (*)(struct __SearchTable **,SearchEntry)) HashTableSearch;
	htable->delete = (SearchEntry * (*)(struct __SearchTable **,SearchEntry)) HashTableDelete;
	htable->seq = (SearchEntry * (*)(struct __SearchTable **,int)) HashTableSeq;
	htable->destroy = (void (*)(struct __SearchTable **)) TableDestroy;
	htable->_delete_entry = delete_entry;

	/* everything went ok */
	return htable;
}

SearchTable *
SearchTableCreate(unsigned int nel,int type,void (*delete_entry)(SearchEntry *entry))
{
	if (type == SEARCH_BTREE_TABLE)
		return BtreeTableCreate(nel,delete_entry);

	return HashTableCreate(nel,delete_entry);
}

static SearchEntry *
HashTableSeq(SearchTable **hhtable,int pos)
{
	SearchTable *htable = *hhtable;
	int hsize = htable->hsize;
	int	cou;

	if (!pos)
		cou = 0;
	else
		cou = htable->cursor + 1;

	if (cou >= hsize)
		return NULL;

	for (;!htable->data[cou].used && cou < hsize; cou++);

	if (cou >= hsize)
		return NULL;

	htable->cursor = cou;

	return &htable->data[cou].entry;
}

static SearchEntry *
HashTableDelete(SearchTable **hhtable,SearchEntry item)
{
	return HashTableSearchFunc(hhtable,item,Delete,0);
}

static SearchEntry *
HashTableAdd(SearchTable **hhtable,SearchEntry item)
{
	return HashTableSearchFunc(hhtable,item,Add,1);
}

static SearchEntry *
HashTableSearch(SearchTable **hhtable,SearchEntry item)
{
	return HashTableSearchFunc(hhtable,item,Find,0);
}

/*
 * This is the search function. It uses double hashing with open
 * adressing.  The argument item.key has to be a pointer to a zero
 * terminated array of char, most probably ASCII characters. The
 * function for generating a hash number for a string is simple but
 * fast. It can be replaced by a more complex function like ajw (see
 * [Aho,Sethi,Ullman]) if necessary.
 *
 * We use a trick to speed up the lookup. The table is created by
 * hcreate with one extra element available. This enables us to use the
 * index zero specially. This index will never be used because we
 * store the hash index in the field `used' where zero means not
 * used. Every other value means used. The `used' field can be used as
 * a first fast comparison for equality of the stored and the
 * parameter value. This helps to prevent unnecessary expensive calls
 * of strcmp.
 */

static SearchEntry *
HashTableSearchFunc(SearchTable **hhtable,SearchEntry item,Search_Action action,
	int dup_enabled)
{
	register unsigned int hval;
	register unsigned int hval2;
	register unsigned int count;
	register unsigned int idx;
	register SearchTable *htable = *hhtable;

	/*
	 * If table is near full and another entry should be entered then
	 * enlarge the table.
	 */
	if (action == Add && htable->filled*10 > htable->hsize*9)
	{
		SearchTable *oldtable = htable;
		unsigned int i, oldsize = htable->hsize;

		if (!(htable = HashTableCreate(oldsize*2,htable->_delete_entry)))
			return NULL;

		*hhtable = htable;
		for (i = 1; i <= oldsize; i++)
		{
			if (oldtable->data[i].used)
			{
				HashTableSearchFunc(hhtable,
					oldtable->data[i].entry,Add,0);
			}
		}
		ckfree((void *)oldtable->data);
		ckfree((void *)oldtable);
	}

	/* Compute a value for the given string. Could perhaps use a
		better method. */
	if (item.key_len <= 0)
	{
		count = strlen(item.key);
		item.key_len = count;
	}
	else
	{
		count = item.key_len;
	}
	hval = count;
	while (count-- > 0)
	{
		hval <<= 4;
		hval +=  item.key[count];
	}

	/* First hash function: simply take the	modulus  but prevent zero. */
	hval %=  htable->hsize;
	if (hval == 0)
		hval++;

	/* The first index tried. */
	idx = hval;

	if (htable->data[idx].used)
	{
		if (htable->data[idx].used == hval &&
			item.key_len == htable->data[idx].entry.key_len &&
			memcmp(item.key, htable->data[idx].entry.key,item.key_len) == 0)
		{
			if (action == Delete)
			{
				remove_entry(htable,&htable->data[idx].entry);

				htable->data[idx].used = 0;
				memset(&htable->data[idx].entry,0,
					sizeof(htable->data[idx].entry));
				htable->filled--;
			}
			return &htable->data[idx].entry;
		}

		/* Second hash function, as suggested in [Knuth] */
		hval2 = 1 + hval % (htable->hsize-2);
		do
		{
			/*
			 * Because hsize is prime this is guaranteed to step
			 * through all available indices.
			 */
			if (idx <= hval2)
				idx = htable->hsize+idx-hval2;
			else
				idx -= hval2;

			/* If entry is found use it. */
			if (htable->data[idx].used == hval &&
				item.key_len == htable->data[idx].entry.key_len &&
				memcmp(item.key, htable->data[idx].entry.key,item.key_len) == 0)
			{
				if (action == Delete)
				{
					remove_entry(htable,&htable->data[idx].entry);

					htable->data[idx].used = 0;
					memset(&htable->data[idx].entry,0,
						sizeof(htable->data[idx].entry));
					htable->filled--;
				}
				return &htable->data[idx].entry;
			}
		} while  (htable->data[idx].used);
	}

	/* An empty bucket has been found. */
	if (action == Add)
	{
		if (dup_enabled)
		{
			if (item.flag & ~(SEARCH_DUP_KEY | SEARCH_DUP_DATA | SEARCH_FIRST))
			{
				fprintf(stderr,"Bad flag value, file: %s line: %d\n",
					__FILE__,__LINE__);
				return NULL;
			}
			if (item.flag & SEARCH_DUP_KEY)
			{
				char  *key = (char*)ckalloc(item.key_len + 1);

				memcpy(key,item.key,item.key_len);
				key[item.key_len] = '\0';
				item.key = key;
			}
			if (item.flag & SEARCH_DUP_DATA)
			{
				char  *data = NULL;

				if (item.data_len == -1)
				{
					item.data_len = strlen(item.data);
					data = (char*)ckalloc(item.data_len + 1);
					memcpy(data,item.data,item.data_len + 1);
				}
				else
				{
					data = (char*)ckalloc(item.data_len + 1);
					memcpy(data,item.data,item.data_len);
					data[item.data_len] = '\0';
				}
				item.data = data;
			}
		}
		htable->data[idx].used  = hval;
		htable->data[idx].entry = item;

		htable->filled++;

		return &htable->data[idx].entry;
	}
	return NULL;
}

