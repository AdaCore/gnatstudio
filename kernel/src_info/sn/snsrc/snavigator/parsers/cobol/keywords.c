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

/* Ich, Doktor Josef Grosch, Informatiker, Nov. 1994 */

#include "keywords.h"
#include "keywdef.h"
#include "statdef.h"

unsigned long	dialect	= all;
tkeyword	keywords [num_keyword + 5];
tIdent		max_keyword;
char		keyword2stat [num_keyword + 5];

toption		options [] = {
   { "ans74"	, ans74	},
   { "ans85"	, ans85	},
   { "os"	, os	},
   { "vs2"	, vs2	},
   { "vs3"	, vs3	},
   { "vs4"	, vs4	},
   { "sa1"	, sa1	},
   { "sa2"	, sa2	},
   { "x"	, x	},
   { "mf1"	, mf1	},
   { "mf2"	, mf2	},
   { "mf3"	, mf3	},
   { "mf4"	, mf4	},
   { "mf5"	, mf5	},
   { "mf6"	, mf6	},
   { "mf7"	, mf7	},
   { "mf8"	, mf8	},
   { "ms"	, ms	},
   { "rm"	, rm	},
   { "c370"	, c370	},
   { "dvs"	, dvs	},
   { "er"	, er	},
   { "vs"	, vs	},
   { "sa"	, sa	},
   { "mf"	, mf	},
   { "all"	, all	},
   { 0		, 0	}
};

static void keyword
#if defined __STDC__ | defined __cplusplus
   (char * word, unsigned long mask, short code)
#else
   (word, mask, code) char * word; unsigned long mask; short code;
#endif
{
   tIdent ident = MakeIdent (word, strlen (word));
   keywords [ident].mask = mask;
   keywords [ident].code = code;
}

static void synonym
#if defined __STDC__ | defined __cplusplus
   (char * word, short code)
#else
   (word, code) char * word; short code;
#endif
{
   tIdent ident = MakeIdent (word, strlen (word));
   keywords [ident].code = code;
}

void init_keywords ARGS ((void))
{
   register int i;

#include "keywini.c"

   keyword ("NOTE"	, os | vs2 | dvs, 1);
   keyword ("EJECT"	, os | vs | sa | mf7 | ms | dvs, 1);
   keyword ("SKIP1"	, os | vs | sa | mf7 | dvs, 1);
   keyword ("SKIP2"	, os | vs | sa | mf7 | dvs, 1);
   keyword ("SKIP3"	, os | vs | sa | mf7 | dvs, 1);

   max_keyword = MaxIdent ();

   synonym ("BACKGROUND-COLOUR"	, kBACKGROUND_COLOR	);
   synonym ("COMP"		, kCOMPUTATIONAL	);
   synonym ("COMP-0"		, kCOMPUTATIONAL_0	);
   synonym ("COMP-1"		, kCOMPUTATIONAL_1	);
   synonym ("COMP-2"		, kCOMPUTATIONAL_2	);
   synonym ("COMP-3"		, kCOMPUTATIONAL_3	);
   synonym ("COMP-4"		, kCOMPUTATIONAL_4	);
   synonym ("COMP-5"		, kCOMPUTATIONAL_5	);
   synonym ("COMP-6"		, kCOMPUTATIONAL_6	);
   synonym ("COMP-X"		, kCOMPUTATIONAL_X	);
   synonym ("CORR"		, kCORRESPONDING	);
   synonym ("DE"		, kDETAIL		);
   synonym ("EOP"		, kEND_OF_PAGE		);
   synonym ("EXEC"		, kEXECUTE		);
   synonym ("FOREGROUND-COLOUR"	, kFOREGROUND_COLOR	);
   synonym ("HIGH-VALUES"	, kHIGH_VALUE		);
   synonym ("ID"		, kIDENTIFICATION	);
   synonym ("JUST"		, kJUSTIFIED		);
   synonym ("LOW-VALUES"	, kLOW_VALUE		);
   synonym ("OTHERWISE"		, kELSE			);
   synonym ("PIC"		, kPICTURE		);
   synonym ("QUOTES"		, kQUOTE		);
   synonym ("SPACES"		, kSPACE		);
   synonym ("SYNC"		, kSYNCHRONIZED		);
   synonym ("THRU"		, kTHROUGH		);
   synonym ("TIMEOUT"		, kTIME_OUT		);
   synonym ("UNIT"		, kREEL			);
   synonym ("ZEROES"		, kZEROS		);

   for (i = 1; i <= num_keyword; i ++) keyword2stat [i] = 0;
#include "key2stat.c"
}

