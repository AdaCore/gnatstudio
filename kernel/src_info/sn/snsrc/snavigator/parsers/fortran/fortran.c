
/*  A Bison parser, made from fortran.y
    by GNU Bison version 1.28  */

#define YYBISON 1  /* Identify Bison output.  */

#define	tok_identifier	257
#define	tok_array_identifier	258
#define	tok_label	259
#define	tok_integer_const	260
#define	tok_real_const	261
#define	tok_dp_const	262
#define	tok_complex_const	263
#define	tok_dcomplex_const	264
#define	tok_logical_const	265
#define	tok_string	266
#define	tok_hollerith	267
#define	tok_edit_descriptor	268
#define	tok_letter	269
#define	tok_relop	270
#define	tok_AND	271
#define	tok_OR	272
#define	tok_EQV	273
#define	tok_NEQV	274
#define	tok_NOT	275
#define	tok_power	276
#define	tok_concat	277
#define	tok_ACCEPT	278
#define	tok_ASSIGN	279
#define	tok_BACKSPACE	280
#define	tok_BLOCK	281
#define	tok_BLOCKDATA	282
#define	tok_BYTE	283
#define	tok_CALL	284
#define	tok_CHARACTER	285
#define	tok_CLOSE	286
#define	tok_COMMON	287
#define	tok_COMPLEX	288
#define	tok_CONTINUE	289
#define	tok_DATA	290
#define	tok_DIMENSION	291
#define	tok_DO	292
#define	tok_DOUBLE	293
#define	tok_DOUBLECOMPLEX	294
#define	tok_DOUBLEPRECISION	295
#define	tok_DOWHILE	296
#define	tok_ELSE	297
#define	tok_ELSEIF	298
#define	tok_END	299
#define	tok_ENDDO	300
#define	tok_ENDFILE	301
#define	tok_ENDIF	302
#define	tok_ENDMAP	303
#define	tok_ENDSTRUCTURE	304
#define	tok_ENDUNION	305
#define	tok_ENTRY	306
#define	tok_EQUIVALENCE	307
#define	tok_EXTERNAL	308
#define	tok_FILE	309
#define	tok_FORMAT	310
#define	tok_FUNCTION	311
#define	tok_GO	312
#define	tok_GOTO	313
#define	tok_IF	314
#define	tok_IMPLICIT	315
#define	tok_INCLUDE	316
#define	tok_INQUIRE	317
#define	tok_INTEGER	318
#define	tok_INTRINSIC	319
#define	tok_LOGICAL	320
#define	tok_MAP	321
#define	tok_NAMELIST	322
#define	tok_NONE	323
#define	tok_OPEN	324
#define	tok_PARAMETER	325
#define	tok_PAUSE	326
#define	tok_POINTER	327
#define	tok_PRECISION	328
#define	tok_PRINT	329
#define	tok_PROGRAM	330
#define	tok_READ	331
#define	tok_REAL	332
#define	tok_RECORD	333
#define	tok_RETURN	334
#define	tok_REWIND	335
#define	tok_SAVE	336
#define	tok_STOP	337
#define	tok_STRUCTURE	338
#define	tok_SUBROUTINE	339
#define	tok_UNION	340
#define	tok_THEN	341
#define	tok_TO	342
#define	tok_TYPE	343
#define	tok_WHILE	344
#define	tok_WRITE	345
#define	tok_illegal	346
#define	EOS	127
#define	REDUCE	347

#line 26 "fortran.y"


/*
    fortran.y

    Copyright (C) 1992 by Robert K. Moniot.
    This program is free software.  Permission is granted to
    modify it and/or redistribute it.  There is no warranty
    for this program.


	    This grammar is ANSI standard-conforming, except for:
		-- complex constant and a few other ambiguities needing
		   significant lookahead cannot be split across lines.

	    Extensions supported:
	        -- Case insensitive.
	 	-- Hollerith constants.
		-- Variable names may be longer than 6 characters.  Also
		   allows underscores and dollar signs in names.
		-- DO ... ENDDO and DO WHILE loop forms allowed.
		-- NAMELIST supported.
		-- TYPE and ACCEPT I/O statements allowed.
		-- Tabs are permitted in input, and (except in character data)
		   expand into blanks up to the next column equal to 1 mod 8.
		-- Type declarations INTEGER*2, REAL*8, etc. are allowed.
		-- IMPLICIT NONE allowed.
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "ftnchek.h"
#include "symtab.h"
#include <tcl.h>
#include "sn.h"

	/* The following section is for use with bison-derived
	   parser.  Define alloca to be ckalloc for those cases
	   not covered by the cases covered there.  The ifdefs
	   are those in the skeleton parser with includes removed */
#ifdef AIXC	/* IBM RS/6000 xlc compiler does it this way */
#pragma alloca
#endif
#ifndef alloca
#ifdef __GNUC__
#else /* Not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__)
#else /* Not sparc */
#ifdef MSDOS
#endif /* MSDOS */
#endif /* Not sparc.  */
#endif /* Not GNU C.  */
#define alloca ckalloc
#endif /* alloca now defined.  */

extern	int	highlight;
void exit();

#ifndef YYDEBUG	/* If not declared otherwise... */
int yydebug;	/* declare yydebug to satisfy extern in ftnchek.c */
#ifdef DEVELOPMENT
#define YYDEBUG 1		/* For development it is handy */
#else
#define YYDEBUG 0
#endif
#endif

#ifdef DEVELOPMENT
#define DEBUG_PARSER
#endif

int current_datatype,	/* set when parse type_name or type_stmt */
    stmt_sequence_no,   /* set when parsing, reset to 0 at end_stmt */
    control_item_count;	/* count of items in control_info_list */
long current_typesize;	/* for type*len declarations */

extern unsigned prev_stmt_line_num; /* shared with advance */

unsigned true_prev_stmt_line_num;	/* shared with symtab.c */

int current_struct_hash = -1;
int current_common_hash = -1;
int current_record_hash = -1;

int current_module_hash = -1,	/* hashtable index of current module name */
    current_module_type,
    executable_stmt=FALSE,
    prev_stmt_class=0,
		 /* flags for lexer */
    complex_const_allowed=FALSE, /* for help in lookahead for these */
    in_assignment_stmt=FALSE,
    inside_format=FALSE,	/* when inside parens of FORMAT  */
    integer_context=FALSE;	/* says integers-only are to follow */

long exec_stmt_count=0;	/* count of executable stmts in program */
int	cross_scope_type = PAF_SUBR_DEF;

#ifdef DEBUG_PARSER
PRIVATE void
print_comlist(), print_exprlist();
#endif

PRIVATE void	END_processing();

PRIVATE Token *
  append_token();
#if 0
PRIVATE Token *
  append_dot_token();
#endif
PRIVATE int
  do_bounds_type();

static Token *token_dup( Token *t );

		/* Uses of Token fields for nonterminals: */
/*
  1. dim_bound_lists: dimensioning info for arrays:
       token.class = no. of dimensions,
       token.subclass = no. of elements
  2. expressions
       token.value.integer = hash index (of identifier)
       token.class = type_byte = storage_class << 4 + datatype
       token.subclass = flags: CONST_EXPR, LVALUE_EXPR, etc.
  3. common variable lists
       token.subclass = flag: COMMA_FLAG used to handle extra/missing commas
*/

enum {
	SEQ_HEADER = 1,
	SEQ_IMPLICIT,
	SEQ_SPECIF,
	SEQ_STMT_FUN,
	SEQ_EXEC,
	SEQ_END
};

#ifndef YYSTYPE
#define YYSTYPE int
#endif
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		802
#define	YYFLAG		-32768
#define	YYNTBASE	105

#define YYTRANSLATE(x) ((unsigned)(x) <= 347 ? yytranslate[x] : 332)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    97,
    95,    99,   104,    98,   101,   103,    96,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,   100,     2,     2,
   102,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,    93,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     3,     4,     5,     6,
     7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
    17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
    27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
    37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
    47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
    57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
    67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
    77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
    87,    88,    89,    90,    91,    92,    94
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     3,     5,     8,    10,    12,    14,    16,    18,
    21,    23,    25,    27,    29,    31,    34,    36,    38,    40,
    42,    44,    47,    50,    54,    56,    58,    60,    62,    64,
    66,    68,    70,    72,    74,    76,    78,    80,    82,    84,
    86,    87,    97,    99,   102,   104,   106,   108,   110,   112,
   114,   116,   118,   120,   122,   124,   126,   128,   130,   132,
   134,   136,   138,   140,   142,   144,   146,   148,   150,   152,
   154,   156,   158,   160,   162,   164,   166,   168,   170,   171,
   176,   180,   187,   191,   198,   202,   209,   212,   214,   216,
   218,   220,   224,   231,   233,   234,   236,   238,   242,   244,
   246,   249,   253,   256,   258,   262,   264,   268,   273,   275,
   279,   281,   285,   287,   291,   292,   297,   301,   307,   311,
   315,   317,   319,   321,   326,   329,   332,   336,   340,   345,
   346,   349,   351,   354,   355,   359,   363,   366,   368,   370,
   373,   375,   378,   380,   382,   386,   388,   391,   394,   398,
   400,   403,   405,   408,   412,   416,   420,   425,   429,   431,
   435,   437,   439,   441,   443,   445,   448,   450,   453,   455,
   457,   462,   464,   468,   470,   474,   476,   478,   480,   484,
   486,   490,   492,   496,   498,   500,   504,   510,   512,   516,
   520,   522,   523,   528,   529,   535,   537,   541,   543,   547,
   551,   553,   557,   563,   565,   569,   570,   575,   579,   581,
   585,   589,   591,   595,   598,   602,   604,   608,   610,   614,
   618,   620,   623,   627,   628,   629,   636,   638,   642,   644,
   646,   648,   652,   654,   658,   660,   662,   664,   666,   668,
   672,   674,   676,   684,   688,   694,   695,   696,   703,   705,
   709,   711,   713,   715,   722,   727,   734,   742,   746,   753,
   761,   763,   766,   769,   774,   785,   788,   792,   793,   799,
   802,   803,   804,   813,   816,   819,   823,   829,   830,   838,
   839,   846,   850,   855,   858,   862,   868,   872,   875,   878,
   882,   886,   887,   889,   891,   893,   894,   898,   899,   904,
   905,   911,   917,   924,   932,   936,   942,   944,   948,   954,
   958,   959,   960,   968,   972,   973,   974,   982,   984,   988,
   992,   994,   996,  1000,  1004,  1008,  1010,  1012,  1016,  1018,
  1020,  1028,  1029,  1036,  1037,  1044,  1045,  1052,  1056,  1062,
  1064,  1068,  1074,  1076,  1079,  1083,  1089,  1091,  1093,  1095,
  1097,  1099,  1100,  1104,  1109,  1110,  1112,  1114,  1118,  1120,
  1121,  1125,  1126,  1132,  1133,  1140,  1143,  1145,  1149,  1153,
  1159,  1162,  1166,  1171,  1173,  1174,  1176,  1178,  1182,  1184,
  1188,  1192,  1194,  1198,  1200,  1204,  1206,  1209,  1211,  1215,
  1217,  1220,  1223,  1227,  1231,  1233,  1237,  1241,  1243,  1247,
  1249,  1253,  1257,  1259,  1261,  1263,  1265,  1267,  1271,  1273,
  1275,  1277,  1279,  1281,  1283,  1285,  1287,  1289,  1291,  1293,
  1295,  1297,  1302,  1307,  1309,  1313,  1315,  1318,  1321,  1324,
  1327,  1331,  1336,  1341,  1347,  1349,  1351,  1353,  1355,  1357,
  1359,  1361,  1363,  1366,  1369,  1371,  1373,  1375,  1377,  1378
};

static const short yyrhs[] = {   106,
     0,     0,   107,     0,   106,   107,     0,   108,     0,   114,
     0,    93,     0,   109,     0,   112,     0,     5,   110,     0,
   110,     0,   111,     0,   115,     0,   122,     0,   125,     0,
     1,    93,     0,   128,     0,   131,     0,   135,     0,   140,
     0,   113,     0,     5,   113,     0,    45,    93,     0,    62,
    12,    93,     0,   116,     0,   192,     0,   184,     0,   203,
     0,   117,     0,   130,     0,   283,     0,   142,     0,   147,
     0,   154,     0,   164,     0,   170,     0,   196,     0,   198,
     0,   200,     0,   118,     0,     0,    84,    96,   327,    96,
    93,   119,   120,    50,    93,     0,   121,     0,   120,   121,
     0,   170,     0,   123,     0,   124,     0,   223,     0,   225,
     0,   228,     0,   245,     0,   295,     0,   218,     0,   222,
     0,   224,     0,   244,     0,   246,     0,   253,     0,   255,
     0,   248,     0,   256,     0,   259,     0,   279,     0,   275,
     0,   277,     0,   269,     0,   271,     0,   273,     0,   289,
     0,   126,     0,   127,     0,   229,     0,   230,     0,   238,
     0,   243,     0,   233,     0,   236,     0,   237,     0,     0,
    76,   129,   327,    93,     0,    52,   327,    93,     0,    52,
   327,    97,   137,    95,    93,     0,   132,   327,    93,     0,
   132,   327,    97,   137,    95,    93,     0,   133,   327,    93,
     0,   133,   327,    97,   137,    95,    93,     0,   134,    57,
     0,    57,     0,   171,     0,   174,     0,   175,     0,   136,
   327,    93,     0,   136,   327,    97,   137,    95,    93,     0,
    85,     0,     0,   138,     0,   139,     0,   138,    98,   139,
     0,   327,     0,    99,     0,   141,    93,     0,   141,   327,
    93,     0,    27,    36,     0,    28,     0,    37,   143,    93,
     0,   144,     0,   143,    98,   144,     0,   327,    97,   145,
    95,     0,   146,     0,   145,    98,   146,     0,   315,     0,
   315,   100,   315,     0,    99,     0,   315,   100,    99,     0,
     0,    53,   148,   149,    93,     0,    97,   150,    95,     0,
   149,    98,    97,   150,    95,     0,   151,    98,   151,     0,
   150,    98,   151,     0,   327,     0,   152,     0,   153,     0,
   327,    97,   318,    95,     0,   327,   322,     0,   152,   322,
     0,    33,   155,    93,     0,    33,   157,    93,     0,    33,
   155,   157,    93,     0,     0,   156,   161,     0,   158,     0,
   157,   158,     0,     0,   160,   159,   161,     0,    96,   327,
    96,     0,    96,    96,     0,    23,     0,   162,     0,   161,
   162,     0,   163,     0,   163,    98,     0,   327,     0,   144,
     0,    68,   165,    93,     0,   166,     0,   165,   166,     0,
   167,   168,     0,    96,   327,    96,     0,   169,     0,   168,
   169,     0,   327,     0,   327,    98,     0,   171,   176,    93,
     0,   174,   178,    93,     0,   175,   178,    93,     0,   175,
    98,   178,    93,     0,   180,   181,    93,     0,   172,     0,
   172,    99,   329,     0,   173,     0,    64,     0,    78,     0,
    34,     0,    66,     0,    39,    74,     0,    41,     0,    39,
    34,     0,    40,     0,    29,     0,    79,    96,   327,    96,
     0,    31,     0,   174,    99,   191,     0,   177,     0,   176,
    98,   177,     0,   327,     0,   144,     0,   179,     0,   178,
    98,   179,     0,   327,     0,   327,    99,   191,     0,   144,
     0,   144,    99,   191,     0,    73,     0,   182,     0,   181,
    98,   182,     0,    97,   327,    98,   327,    95,     0,    61,
     0,   183,   185,    93,     0,   183,    69,    93,     0,   187,
     0,     0,   185,    98,   186,   187,     0,     0,   134,    97,
   188,   189,    95,     0,   190,     0,   189,    98,   190,     0,
    15,     0,    15,   101,    15,     0,    97,    99,    95,     0,
   329,     0,    97,   314,    95,     0,    71,    97,   193,    95,
    93,     0,   194,     0,   193,    98,   194,     0,     0,   327,
   195,   102,   300,     0,    54,   197,    93,     0,   327,     0,
   197,    98,   327,     0,    65,   199,    93,     0,   327,     0,
   199,    98,   327,     0,    82,    93,     0,    82,   201,    93,
     0,   202,     0,   201,    98,   202,     0,   327,     0,    96,
   327,    96,     0,    36,   204,    93,     0,   205,     0,   204,
   205,     0,   204,    98,   205,     0,     0,     0,   208,    96,
   206,   210,   207,    96,     0,   209,     0,   208,    98,   209,
     0,   221,     0,   216,     0,   211,     0,   210,    98,   211,
     0,   213,     0,   212,    99,   213,     0,   329,     0,   327,
     0,   328,     0,   327,     0,   215,     0,   214,    98,   215,
     0,   316,     0,   216,     0,    97,   214,    98,   327,   102,
   217,    95,     0,   314,    98,   314,     0,   314,    98,   314,
    98,   314,     0,     0,     0,   221,   102,   219,   300,   220,
    93,     0,   324,     0,   221,   103,   221,     0,   316,     0,
   321,     0,   285,     0,    25,   330,   331,    88,   324,    93,
     0,   226,   330,   331,    93,     0,   226,    97,   227,    95,
   312,    93,     0,   226,    97,   227,    95,    98,   312,    93,
     0,   226,   327,    93,     0,   226,   327,    97,   227,    95,
    93,     0,   226,   327,    98,    97,   227,    95,    93,     0,
    59,     0,    58,    88,     0,   330,   331,     0,   227,    98,
   330,   331,     0,   231,   330,   331,    98,   330,   331,    98,
   330,   331,    93,     0,   231,   122,     0,   231,    87,    93,
     0,     0,    60,    97,   232,   300,    95,     0,    43,   230,
     0,     0,     0,    44,    97,   234,   300,    95,   235,    87,
    93,     0,    43,    93,     0,    48,    93,     0,    45,    60,
    93,     0,   241,   324,   102,   242,    93,     0,     0,   241,
    90,    97,   239,   300,    95,    93,     0,     0,    42,    97,
   240,   300,    95,    93,     0,    38,   330,   331,     0,    38,
   330,   331,    98,     0,    38,   330,     0,   313,    98,   313,
     0,   313,    98,   313,    98,   313,     0,    45,    38,    93,
     0,    46,    93,     0,    35,    93,     0,    83,   247,    93,
     0,    72,   247,    93,     0,     0,     6,     0,   327,     0,
    12,     0,     0,   251,   249,    93,     0,     0,   251,   266,
   250,    93,     0,     0,    91,   252,    97,   262,    95,     0,
   254,    97,   262,    95,    93,     0,   254,    97,   262,    95,
   266,    93,     0,   254,    97,   262,    95,    98,   266,    93,
     0,   254,   282,    93,     0,   254,   282,    98,   266,    93,
     0,    77,     0,    24,   282,    93,     0,    24,   282,    98,
   266,    93,     0,    75,   282,    93,     0,     0,     0,    75,
   282,    98,   257,   266,   258,    93,     0,    89,   282,    93,
     0,     0,     0,    89,   282,    98,   260,   266,   261,    93,
     0,   263,     0,   262,    98,   263,     0,   327,   102,   281,
     0,   281,     0,   281,     0,   327,   102,   281,     0,   264,
    98,   265,     0,   327,   102,   281,     0,   327,     0,   267,
     0,   266,    98,   267,     0,   300,     0,   268,     0,    97,
   266,    98,   324,   102,   242,    95,     0,     0,    70,   270,
    97,   264,    95,    93,     0,     0,    32,   272,    97,   262,
    95,    93,     0,     0,    63,   274,    97,   262,    95,    93,
     0,   276,   281,    93,     0,   276,    97,   262,    95,    93,
     0,    26,     0,   278,   281,    93,     0,   278,    97,   262,
    95,    93,     0,    47,     0,    45,    55,     0,   280,   281,
    93,     0,   280,    97,   262,    95,    93,     0,    81,     0,
   300,     0,    99,     0,   308,     0,    99,     0,     0,    56,
   284,    93,     0,   325,    97,   286,    95,     0,     0,   287,
     0,   288,     0,   287,    98,   288,     0,   324,     0,     0,
   293,   290,    93,     0,     0,   293,    97,    95,   291,    93,
     0,     0,   293,    97,   294,    95,   292,    93,     0,    30,
   327,     0,   300,     0,    99,   330,   331,     0,   294,    98,
   300,     0,   294,    98,    99,   330,   331,     0,    80,    93,
     0,    80,   312,    93,     0,   297,    97,   298,    95,     0,
   325,     0,     0,   299,     0,   300,     0,   299,    98,   300,
     0,   301,     0,   300,    19,   301,     0,   300,    20,   301,
     0,   302,     0,   301,    18,   302,     0,   303,     0,   302,
    17,   303,     0,   304,     0,    21,   304,     0,   305,     0,
   304,    16,   304,     0,   306,     0,   101,   306,     0,   104,
   306,     0,   305,   104,   306,     0,   305,   101,   306,     0,
   307,     0,   306,    96,   307,     0,   306,    99,   307,     0,
   308,     0,   308,    22,   307,     0,   309,     0,   309,   103,
   308,     0,   308,    23,   309,     0,   324,     0,   317,     0,
   296,     0,   320,     0,   310,     0,    97,   300,    95,     0,
   311,     0,    12,     0,    13,     0,    11,     0,     6,     0,
     7,     0,     8,     0,     9,     0,    10,     0,   305,     0,
   305,     0,   305,     0,   305,     0,   326,    97,   318,    95,
     0,   326,    97,   318,    95,     0,   319,     0,   318,    98,
   319,     0,   300,     0,   297,   322,     0,   317,   322,     0,
   325,   322,     0,   316,   322,     0,    97,   100,    95,     0,
    97,   323,   100,    95,     0,    97,   100,   323,    95,     0,
    97,   323,   100,   323,    95,     0,   305,     0,   325,     0,
   326,     0,     3,     0,     4,     0,     3,     0,     4,     0,
   311,     0,   101,   311,     0,   104,   311,     0,    11,     0,
    12,     0,    13,     0,     6,     0,     0,     6,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   274,   275,   278,   279,   283,   305,   306,   314,   315,   318,
   324,   327,   332,   336,   349,   355,   369,   373,   377,   381,
   387,   388,   391,   394,   407,   413,   422,   426,   432,   438,
   439,   442,   443,   444,   445,   446,   447,   448,   449,   450,
   453,   469,   475,   476,   479,   484,   486,   489,   490,   491,
   492,   493,   496,   497,   498,   499,   500,   501,   502,   503,
   504,   505,   506,   507,   508,   509,   510,   511,   512,   515,
   517,   520,   522,   523,   524,   527,   528,   529,   533,   534,
   547,   552,   566,   574,   587,   594,   608,   614,   620,   624,
   625,   634,   642,   656,   662,   666,   669,   673,   679,   684,
   694,   708,   718,   722,   729,   732,   733,   737,   743,   749,
   756,   764,   774,   778,   785,   786,   789,   790,   793,   797,
   804,   808,   812,   818,   822,   823,   827,   837,   838,   849,
   865,   871,   875,   883,   900,   913,   918,   922,   928,   933,
   942,   947,   954,   959,   972,   975,   976,   982,   989,   995,
   999,  1008,  1014,  1023,  1027,  1028,  1029,  1030,  1033,  1038,
  1055,  1058,  1063,  1068,  1073,  1080,  1085,  1090,  1095,  1100,
  1105,  1113,  1121,  1127,  1128,  1131,  1136,  1143,  1144,  1147,
  1152,  1157,  1162,  1169,  1176,  1177,  1180,  1185,  1188,  1198,
  1209,  1210,  1212,  1216,  1217,  1220,  1221,  1224,  1230,  1241,
  1243,  1245,  1257,  1260,  1261,  1264,  1265,  1275,  1278,  1282,
  1289,  1292,  1296,  1303,  1307,  1310,  1311,  1314,  1318,  1325,
  1328,  1329,  1330,  1333,  1336,  1337,  1340,  1341,  1344,  1348,
  1351,  1352,  1355,  1356,  1359,  1360,  1366,  1367,  1374,  1375,
  1378,  1382,  1385,  1392,  1393,  1398,  1400,  1407,  1415,  1416,
  1420,  1421,  1422,  1428,  1436,  1440,  1441,  1445,  1449,  1453,
  1459,  1463,  1469,  1470,  1474,  1479,  1483,  1486,  1486,  1499,
  1500,  1500,  1509,  1513,  1517,  1518,  1529,  1553,  1554,  1562,
  1563,  1573,  1574,  1575,  1582,  1586,  1592,  1593,  1597,  1601,
  1605,  1608,  1609,  1610,  1614,  1618,  1620,  1620,  1622,  1624,
  1625,  1633,  1634,  1635,  1636,  1637,  1639,  1642,  1643,  1647,
  1648,  1650,  1651,  1653,  1654,  1656,  1657,  1660,  1664,  1673,
  1677,  1703,  1711,  1716,  1722,  1726,  1733,  1734,  1737,  1747,
  1751,  1758,  1759,  1763,  1764,  1768,  1769,  1773,  1774,  1776,
  1780,  1781,  1783,  1784,  1788,  1789,  1791,  1799,  1800,  1804,
  1810,  1814,  1815,  1820,  1834,  1838,  1841,  1845,  1852,  1856,
  1862,  1862,  1868,  1868,  1877,  1879,  1904,  1912,  1916,  1924,
  1931,  1935,  1942,  1979,  1990,  1995,  1998,  2006,  2017,  2019,
  2024,  2031,  2033,  2040,  2042,  2049,  2051,  2057,  2059,  2067,
  2069,  2073,  2077,  2082,  2089,  2091,  2101,  2108,  2110,  2117,
  2119,  2124,  2131,  2133,  2135,  2137,  2139,  2147,  2162,  2167,
  2172,  2181,  2188,  2192,  2196,  2200,  2204,  2211,  2220,  2231,
  2245,  2268,  2280,  2292,  2296,  2302,  2316,  2324,  2330,  2334,
  2340,  2346,  2351,  2356,  2363,  2381,  2382,  2385,  2411,  2434,
  2449,  2467,  2468,  2469,  2470,  2471,  2472,  2478,  2494,  2501
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","tok_identifier",
"tok_array_identifier","tok_label","tok_integer_const","tok_real_const","tok_dp_const",
"tok_complex_const","tok_dcomplex_const","tok_logical_const","tok_string","tok_hollerith",
"tok_edit_descriptor","tok_letter","tok_relop","tok_AND","tok_OR","tok_EQV",
"tok_NEQV","tok_NOT","tok_power","tok_concat","tok_ACCEPT","tok_ASSIGN","tok_BACKSPACE",
"tok_BLOCK","tok_BLOCKDATA","tok_BYTE","tok_CALL","tok_CHARACTER","tok_CLOSE",
"tok_COMMON","tok_COMPLEX","tok_CONTINUE","tok_DATA","tok_DIMENSION","tok_DO",
"tok_DOUBLE","tok_DOUBLECOMPLEX","tok_DOUBLEPRECISION","tok_DOWHILE","tok_ELSE",
"tok_ELSEIF","tok_END","tok_ENDDO","tok_ENDFILE","tok_ENDIF","tok_ENDMAP","tok_ENDSTRUCTURE",
"tok_ENDUNION","tok_ENTRY","tok_EQUIVALENCE","tok_EXTERNAL","tok_FILE","tok_FORMAT",
"tok_FUNCTION","tok_GO","tok_GOTO","tok_IF","tok_IMPLICIT","tok_INCLUDE","tok_INQUIRE",
"tok_INTEGER","tok_INTRINSIC","tok_LOGICAL","tok_MAP","tok_NAMELIST","tok_NONE",
"tok_OPEN","tok_PARAMETER","tok_PAUSE","tok_POINTER","tok_PRECISION","tok_PRINT",
"tok_PROGRAM","tok_READ","tok_REAL","tok_RECORD","tok_RETURN","tok_REWIND","tok_SAVE",
"tok_STOP","tok_STRUCTURE","tok_SUBROUTINE","tok_UNION","tok_THEN","tok_TO",
"tok_TYPE","tok_WHILE","tok_WRITE","tok_illegal","EOS","REDUCE","')'","'/'",
"'('","','","'*'","':'","'-'","'='","'.'","'+'","prog_body","stmt_list","stmt_list_item",
"ordinary_stmt","stmt","unlabeled_stmt","subprogram_header","end_stmt","unlabeled_end_stmt",
"include_stmt","specification_stmt","anywhere_stmt","specif_stmt","struct_stmt",
"@1","struct_list","struct_item","executable_stmt","transfer_stmt","nontransfer_stmt",
"restricted_stmt","restricted_nontransfer_stmt","else_or_endif_stmt","prog_stmt",
"@2","entry_stmt","function_stmt","typed_function_handle","plain_function_handle",
"type_name","subroutine_stmt","subroutine_handle","dummy_argument_list","non_empty_arg_list",
"dummy_argument","block_data_stmt","block_data_handle","dimension_stmt","array_declarator_list",
"array_declarator","dim_bound_list","dim_bound_item","equivalence_stmt","@3",
"equivalence_list","equivalence_list_item","equiv_entity","array_equiv_name",
"substring_equiv_name","common_stmt","blank_common_block","@4","common_block_list",
"labeled_common_block","@5","common_block_name","common_variable_list","common_list_item",
"common_entity","namelist_stmt","namelist_list","namelist_decl","namelist_name",
"namelist_var_list","namelist_item","type_stmt","arith_type_name","sizeable_type_name",
"unsizeable_type_name","plain_char_type_name","char_type_name","arith_type_decl_list",
"arith_type_decl_item","char_type_decl_list","char_type_decl_item","pointer_type_name",
"pointer_type_decl_list","pointer_type_decl_item","implicit_handle","implicit_stmt",
"implicit_decl_list","@6","implicit_decl_item","@7","letter_list","letter_list_item",
"len_specification","parameter_stmt","parameter_defn_list","parameter_defn_item",
"@8","external_stmt","external_name_list","intrinsic_stmt","intrinsic_name_list",
"save_stmt","save_list","save_item","data_stmt","data_defn_list","data_defn_item",
"@9","@10","data_defn_assignee_list","data_defn_assignee","data_value_list",
"data_value","data_repeat_factor","data_constant_value","data_dlist","data_dlist_item",
"data_implied_do_list","data_do_loop_bounds","assignment_stmt","@11","@12","lvalue",
"assign_stmt","unconditional_goto","computed_goto","assigned_goto","goto","goto_list",
"arithmetic_if_stmt","logical_if_stmt","block_if_stmt","if_handle","@13","else_if_stmt",
"@14","@15","else_stmt","end_if_stmt","do_stmt","@16","@17","do_handle","do_loop_bounds",
"enddo_stmt","continue_stmt","stop_stmt","pause_stmt","stop_info","write_stmt",
"@18","@19","write_handle","@20","read_stmt","read_handle","accept_stmt","print_stmt",
"@21","@22","type_output_stmt","@23","@24","control_info_list","control_info_item",
"open_info_list","open_info_item","io_list","io_item","io_implied_do_list","open_stmt",
"@25","close_stmt","@26","inquire_stmt","@27","backspace_stmt","backspace_handle",
"endfile_stmt","endfile_handle","rewind_stmt","rewind_handle","unit_id","format_id",
"format_stmt","@28","stmt_function_handle","stmt_function_dummy_list","nonempty_stmt_fun_dummy_list",
"stmt_function_dummy_arg","call_stmt","@29","@30","@31","call_handle","expr_list",
"return_stmt","function_reference","fun_or_substr_handle","fun_arg_list","nonempty_fun_arg_list",
"expr","log_disjunct","log_term","log_factor","log_primary","arith_expr","term",
"factor","char_expr","primary","literal_const","numeric_const","integer_expr",
"int_real_dp_expr","int_constant_expr","dim_bound_expr","array_element_lvalue",
"array_element_name","subscript_list","subscript","substring_name","substring_lvalue",
"substring_interval","substr_index_expr","variable_name","scalar_name","array_name",
"symbolic_name","data_constant","nonzero_unsigned_int_const","pre_label","label", NULL
};
#endif

static const short yyr1[] = {     0,
   105,   105,   106,   106,   107,   107,   107,   108,   108,   109,
   109,   110,   110,   110,   110,   110,   111,   111,   111,   111,
   112,   112,   113,   114,   115,   115,   115,   115,   115,   116,
   116,   117,   117,   117,   117,   117,   117,   117,   117,   117,
   119,   118,   120,   120,   121,   122,   122,   123,   123,   123,
   123,   123,   124,   124,   124,   124,   124,   124,   124,   124,
   124,   124,   124,   124,   124,   124,   124,   124,   124,   125,
   125,   126,   126,   126,   126,   127,   127,   127,   129,   128,
   130,   130,   131,   131,   131,   131,   132,   133,   134,   134,
   134,   135,   135,   136,   137,   137,   138,   138,   139,   139,
   140,   140,   141,   141,   142,   143,   143,   144,   145,   145,
   146,   146,   146,   146,   148,   147,   149,   149,   150,   150,
   151,   151,   151,   152,   153,   153,   154,   154,   154,   156,
   155,   157,   157,   159,   158,   160,   160,   160,   161,   161,
   162,   162,   163,   163,   164,   165,   165,   166,   167,   168,
   168,   169,   169,   170,   170,   170,   170,   170,   171,   171,
   171,   172,   172,   172,   172,   173,   173,   173,   173,   173,
   173,   174,   175,   176,   176,   177,   177,   178,   178,   179,
   179,   179,   179,   180,   181,   181,   182,   183,   184,   184,
   185,   186,   185,   188,   187,   189,   189,   190,   190,   191,
   191,   191,   192,   193,   193,   195,   194,   196,   197,   197,
   198,   199,   199,   200,   200,   201,   201,   202,   202,   203,
   204,   204,   204,   206,   207,   205,   208,   208,   209,   209,
   210,   210,   211,   211,   212,   212,   213,   213,   214,   214,
   215,   215,   216,   217,   217,   219,   220,   218,   221,   221,
   221,   221,   221,   222,   223,   224,   224,   225,   225,   225,
   226,   226,   227,   227,   228,   229,   230,   232,   231,   233,
   234,   235,   233,   236,   237,   237,   238,   239,   238,   240,
   238,   241,   241,   241,   242,   242,   243,   243,   244,   245,
   246,   247,   247,   247,   247,   249,   248,   250,   248,   252,
   251,   253,   253,   253,   253,   253,   254,   255,   255,   256,
   257,   258,   256,   259,   260,   261,   259,   262,   262,   263,
   263,   264,   264,   264,   265,   265,   266,   266,   267,   267,
   268,   270,   269,   272,   271,   274,   273,   275,   275,   276,
   277,   277,   278,   278,   279,   279,   280,   281,   281,   282,
   282,   284,   283,   285,   286,   286,   287,   287,   288,   290,
   289,   291,   289,   292,   289,   293,   294,   294,   294,   294,
   295,   295,   296,   297,   298,   298,   299,   299,   300,   300,
   300,   301,   301,   302,   302,   303,   303,   304,   304,   305,
   305,   305,   305,   305,   306,   306,   306,   307,   307,   308,
   308,   308,   309,   309,   309,   309,   309,   309,   310,   310,
   310,   310,   311,   311,   311,   311,   311,   312,   313,   314,
   315,   316,   317,   318,   318,   319,   320,   320,   321,   321,
   322,   322,   322,   322,   323,   324,   324,   325,   326,   327,
   327,   328,   328,   328,   328,   328,   328,   329,   330,   331
};

static const short yyr2[] = {     0,
     1,     0,     1,     2,     1,     1,     1,     1,     1,     2,
     1,     1,     1,     1,     1,     2,     1,     1,     1,     1,
     1,     2,     2,     3,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     0,     9,     1,     2,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     0,     4,
     3,     6,     3,     6,     3,     6,     2,     1,     1,     1,
     1,     3,     6,     1,     0,     1,     1,     3,     1,     1,
     2,     3,     2,     1,     3,     1,     3,     4,     1,     3,
     1,     3,     1,     3,     0,     4,     3,     5,     3,     3,
     1,     1,     1,     4,     2,     2,     3,     3,     4,     0,
     2,     1,     2,     0,     3,     3,     2,     1,     1,     2,
     1,     2,     1,     1,     3,     1,     2,     2,     3,     1,
     2,     1,     2,     3,     3,     3,     4,     3,     1,     3,
     1,     1,     1,     1,     1,     2,     1,     2,     1,     1,
     4,     1,     3,     1,     3,     1,     1,     1,     3,     1,
     3,     1,     3,     1,     1,     3,     5,     1,     3,     3,
     1,     0,     4,     0,     5,     1,     3,     1,     3,     3,
     1,     3,     5,     1,     3,     0,     4,     3,     1,     3,
     3,     1,     3,     2,     3,     1,     3,     1,     3,     3,
     1,     2,     3,     0,     0,     6,     1,     3,     1,     1,
     1,     3,     1,     3,     1,     1,     1,     1,     1,     3,
     1,     1,     7,     3,     5,     0,     0,     6,     1,     3,
     1,     1,     1,     6,     4,     6,     7,     3,     6,     7,
     1,     2,     2,     4,    10,     2,     3,     0,     5,     2,
     0,     0,     8,     2,     2,     3,     5,     0,     7,     0,
     6,     3,     4,     2,     3,     5,     3,     2,     2,     3,
     3,     0,     1,     1,     1,     0,     3,     0,     4,     0,
     5,     5,     6,     7,     3,     5,     1,     3,     5,     3,
     0,     0,     7,     3,     0,     0,     7,     1,     3,     3,
     1,     1,     3,     3,     3,     1,     1,     3,     1,     1,
     7,     0,     6,     0,     6,     0,     6,     3,     5,     1,
     3,     5,     1,     2,     3,     5,     1,     1,     1,     1,
     1,     0,     3,     4,     0,     1,     1,     3,     1,     0,
     3,     0,     5,     0,     6,     2,     1,     3,     3,     5,
     2,     3,     4,     1,     0,     1,     1,     3,     1,     3,
     3,     1,     3,     1,     3,     1,     2,     1,     3,     1,
     2,     2,     3,     3,     1,     3,     3,     1,     3,     1,
     3,     3,     1,     1,     1,     1,     1,     3,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     4,     4,     1,     3,     1,     2,     2,     2,     2,
     3,     4,     4,     5,     1,     1,     1,     1,     1,     1,
     1,     1,     2,     2,     1,     1,     1,     1,     0,     1
};

static const short yydefact[] = {     0,
     0,   438,   439,     0,     0,   449,   340,     0,   104,   170,
     0,   172,   334,   130,   164,     0,     0,     0,   449,     0,
   169,   167,     0,     0,     0,     0,     0,   343,     0,     0,
   115,     0,   352,    88,     0,   261,     0,   188,     0,   336,
   162,     0,   165,     0,   332,     0,   292,   184,     0,    79,
   307,   163,     0,     0,   347,     0,   292,     0,    94,     0,
   300,     7,     0,     3,     5,     8,    11,    12,     9,    21,
     6,    13,    25,    29,    40,    14,    46,    47,    15,    70,
    71,    17,    30,    18,     0,     0,     0,    19,     0,    20,
     0,    32,    33,    34,    35,    36,    89,   159,   161,    90,
    91,     0,     0,    27,    26,    37,    38,    39,    28,    53,
     0,    54,    48,    55,    49,   449,    50,    72,    73,   449,
    76,    77,    78,    74,     0,    75,    56,    51,    57,    60,
   296,    58,     0,    59,    61,    62,    66,    67,    68,    64,
     0,    65,     0,    63,     0,    31,   253,    69,   360,    52,
   251,   252,   249,   436,   437,    16,    10,    22,   413,   414,
   415,   416,   417,   412,   410,   411,     0,   351,     0,   405,
     0,   350,   400,   407,   409,   404,   406,   403,   436,   437,
     0,   103,   440,   441,   366,     0,   138,     0,     0,     0,
     0,   132,   134,   289,     0,     0,   221,     0,   227,   230,
   229,     0,   106,     0,   284,   168,   166,   280,   274,   270,
     0,   271,     0,   344,     0,    23,   288,   275,     0,     0,
     0,   209,     0,   262,   268,     0,     0,     0,   212,     0,
     0,   146,     0,     0,     0,   293,   295,     0,   294,     0,
     0,     0,   371,     0,     0,   418,   390,   395,   398,     0,
   214,     0,     0,   216,   218,     0,     0,     0,     0,     4,
     0,     0,    87,     0,   101,     0,   177,     0,   174,   176,
     0,     0,   182,     0,   178,   180,     0,     0,     0,     0,
   185,     0,     0,    89,    90,    91,     0,   191,   246,     0,
   449,     0,     0,     0,     0,   266,   449,     0,     0,     0,
   436,   437,     0,     0,     0,   298,   327,   330,   329,   379,
   382,   384,   386,   388,     0,     0,     0,   349,     0,   348,
     0,     0,     0,     0,     0,     0,     0,   430,   355,   429,
     0,     0,   308,     0,   375,   427,     0,     0,   428,     0,
   450,     0,     0,   137,     0,   127,     0,   144,   131,   139,
   141,   143,   128,   133,     0,     0,   239,   242,   241,     0,
   220,     0,   222,   224,     0,   105,     0,     0,   282,     0,
     0,   287,   276,    81,    95,     0,     0,   208,     0,   353,
     0,    24,     0,   211,     0,     0,   145,   147,   148,   150,
   152,     0,     0,   204,   206,   291,   310,   311,     0,     0,
   391,   392,     0,     0,     0,     0,     0,   372,     0,   215,
     0,   290,     0,   314,   315,     0,    83,    95,    85,    95,
    92,    95,   102,   154,     0,   448,   160,     0,   173,   201,
     0,   155,     0,     0,     0,   156,     0,   158,     0,   190,
   194,   189,   192,     0,   250,     0,     0,   258,   449,     0,
     0,   267,     0,   278,     0,   387,     0,   329,   297,     0,
     0,     0,     0,     0,     0,     0,   438,   439,     0,   318,
   321,   348,     0,   305,     0,     0,   338,     0,   341,     0,
   345,   362,   449,     0,   367,   361,     0,   435,     0,     0,
   356,   357,   403,   426,     0,   424,   408,     0,     0,   376,
   377,   388,   402,   401,     0,     0,     0,   136,   129,   140,
   142,   135,     0,   223,     0,   228,   107,   113,     0,   109,
   421,   111,   283,     0,     0,   100,     0,    96,    97,    99,
     0,     0,   122,   123,   121,   116,     0,   210,     0,     0,
   213,   149,   151,   153,     0,   322,     0,     0,     0,     0,
     0,    80,   171,   394,   393,   396,   397,   399,   219,   217,
     0,     0,     0,     0,     0,     0,   175,     0,   420,     0,
   183,   179,   181,   157,     0,   186,     0,     0,   247,     0,
   449,   263,     0,   449,   255,   449,     0,     0,   419,     0,
     0,   328,   299,   380,   381,   383,   385,   389,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   364,     0,   431,
     0,     0,   354,     0,   422,     0,   309,   373,     0,   423,
     0,     0,   240,     0,   413,   445,   446,   447,     0,     0,
   225,   231,     0,   233,   442,   238,   237,   235,   108,     0,
     0,     0,   272,     0,     0,   117,     0,     0,   126,     0,
   125,     0,   269,     0,     0,     0,     0,   203,   205,     0,
   312,    41,   316,   301,     0,     0,     0,   200,   202,     0,
   198,     0,   196,   193,     0,     0,     0,     0,     0,     0,
     0,     0,   277,     0,   403,   302,     0,     0,   319,   320,
   306,   339,   342,   346,   363,   368,     0,   449,   369,   433,
   432,     0,   358,   359,   425,   378,   254,   335,     0,   443,
   444,     0,     0,     0,   110,   114,   112,   281,     0,    82,
    98,   120,   119,     0,     0,   337,   333,   324,   326,   323,
   207,     0,     0,     0,    84,    86,    93,   187,     0,   195,
     0,   248,     0,   256,   264,   259,     0,     0,     0,   285,
     0,     0,   303,   365,     0,   434,     0,     0,   232,   226,
   234,   238,     0,   124,   118,     0,   313,     0,    43,    45,
     0,     0,     0,   317,   199,   197,   257,   260,   449,   279,
     0,     0,   304,   370,   243,     0,   273,   325,     0,    44,
     0,   286,   331,   244,    42,     0,     0,   265,   245,     0,
     0,     0
};

static const short yydefgoto[] = {   800,
    63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
    73,    74,    75,   733,   768,   769,    76,    77,    78,    79,
    80,    81,    82,   241,    83,    84,    85,    86,    87,    88,
    89,   527,   528,   529,    90,    91,    92,   202,   273,   519,
   520,    93,   220,   377,   531,   532,   533,   534,    94,   189,
   190,   191,   192,   355,   193,   349,   350,   351,    95,   231,
   232,   233,   389,   390,    96,    97,    98,    99,   100,   101,
   268,   269,   274,   275,   102,   280,   281,   103,   104,   287,
   578,   288,   577,   672,   673,   429,   105,   393,   394,   550,
   106,   221,   107,   228,   108,   253,   254,   109,   196,   197,
   515,   713,   198,   199,   631,   632,   633,   634,   356,   357,
   200,   757,   110,   444,   675,   111,   112,   113,   114,   115,
   116,   446,   117,   118,   119,   120,   381,   121,   371,   719,
   122,   123,   124,   587,   370,   125,   588,   126,   127,   128,
   129,   238,   130,   305,   461,   131,   259,   132,   133,   134,
   135,   551,   732,   136,   562,   734,   469,   470,   545,   728,
   306,   307,   308,   137,   234,   138,   186,   139,   227,   140,
   141,   142,   143,   144,   145,   471,   169,   146,   223,   147,
   490,   491,   492,   148,   326,   606,   697,   149,   484,   150,
   170,   171,   499,   500,   320,   310,   311,   312,   313,   314,
   247,   248,   249,   173,   174,   175,   250,   590,   570,   522,
   151,   176,   495,   496,   177,   152,   328,   489,   178,   179,
   180,   473,   637,   430,   447,   342
};

static const short yypact[] = {  1127,
   -38,-32768,-32768,  1312,    55,-32768,-32768,    36,-32768,-32768,
   182,-32768,-32768,    88,-32768,    -4,    74,   182,-32768,    73,
-32768,-32768,    21,   123,    30,   263,    16,-32768,    44,   182,
-32768,   182,-32768,-32768,   119,-32768,    58,-32768,   185,-32768,
-32768,   182,-32768,   134,-32768,   168,   234,-32768,    55,-32768,
-32768,-32768,   217,   802,-32768,   109,   234,   238,-32768,    55,
-32768,-32768,  1221,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,   182,   182,   283,-32768,   182,-32768,
    95,-32768,-32768,-32768,-32768,-32768,   182,   248,-32768,    47,
    89,   281,  1440,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
   223,-32768,-32768,-32768,-32768,    93,-32768,-32768,-32768,  1395,
-32768,-32768,-32768,-32768,   132,-32768,-32768,-32768,-32768,-32768,
   821,-32768,  1051,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
   286,-32768,   418,-32768,   599,-32768,-32768,-32768,   294,-32768,
   309,-32768,-32768,   323,   335,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,   837,-32768,   141,-32768,
   341,   375,   337,-32768,-32768,   309,-32768,-32768,   360,   362,
   457,-32768,-32768,-32768,-32768,   379,-32768,    98,    57,   182,
   122,-32768,-32768,-32768,    41,    70,-32768,   190,-32768,-32768,
   409,   152,-32768,   381,   457,-32768,-32768,-32768,-32768,-32768,
   395,-32768,   402,-32768,   421,-32768,-32768,-32768,   116,   437,
   175,-32768,   432,-32768,-32768,   456,   439,   187,-32768,   182,
   103,-32768,   182,   453,   182,-32768,-32768,   458,-32768,   210,
   182,   182,-32768,   438,   438,   140,   215,-32768,   449,   461,
-32768,   182,   219,-32768,-32768,   462,   182,   222,   455,-32768,
   257,   295,-32768,   307,-32768,   463,-32768,   229,-32768,   381,
   536,    78,   460,   231,-32768,   203,   182,   235,   182,   239,
-32768,   468,   465,-32768,   464,-32768,   253,-32768,-32768,   496,
-32768,   212,   457,   509,   473,-32768,-32768,   457,   470,   466,
-32768,-32768,   982,   821,   476,   472,-32768,-32768,   487,   553,
   555,-32768,   557,   140,   652,   265,   652,-32768,   481,   487,
   652,   483,   652,   484,   113,   485,   853,-32768,   853,-32768,
   837,   111,-32768,   821,   674,-32768,   438,   438,-32768,   837,
-32768,   491,   652,-32768,   486,-32768,   188,-32768,   182,-32768,
   488,   381,-32768,-32768,   182,   489,-32768,-32768,-32768,   335,
-32768,    74,-32768,-32768,    74,-32768,   182,   872,   492,   837,
   837,-32768,-32768,-32768,    67,   182,   274,-32768,   182,-32768,
   837,-32768,   652,-32768,   182,   495,-32768,-32768,   182,-32768,
   494,   652,   266,-32768,-32768,-32768,-32768,-32768,   501,   499,
   215,   215,   438,   438,   438,   438,   438,-32768,   500,-32768,
   125,-32768,   504,-32768,-32768,   652,-32768,    67,-32768,    67,
-32768,    67,-32768,-32768,   182,-32768,-32768,   901,-32768,-32768,
    78,-32768,   182,    78,   275,-32768,   503,-32768,   281,-32768,
-32768,-32768,-32768,   837,   409,   282,   457,-32768,-32768,   507,
   520,-32768,   517,-32768,   982,   557,   518,   111,-32768,   821,
   524,   837,   837,   837,   837,   982,   479,   482,   321,-32768,
-32768,   487,   516,-32768,   821,   338,-32768,   339,-32768,   357,
-32768,-32768,-32768,   363,   487,-32768,   920,   140,   497,   490,
   521,-32768,   392,   487,   393,-32768,-32768,   288,   526,   525,
   487,   270,-32768,   375,   394,   496,   398,-32768,-32768,-32768,
-32768,   182,   101,-32768,  1000,-32768,-32768,-32768,   399,-32768,
   140,   527,-32768,   124,   138,-32768,   530,   528,-32768,-32768,
   406,   531,   309,-32768,   533,-32768,   534,-32768,   184,   413,
-32768,-32768,-32768,-32768,   423,-32768,   532,   535,   182,   541,
   821,-32768,-32768,   215,   215,-32768,-32768,-32768,-32768,-32768,
   540,   821,   428,   549,   552,   554,-32768,   556,   140,   558,
-32768,-32768,-32768,-32768,   182,-32768,   633,  1058,   487,   936,
-32768,-32768,   429,-32768,-32768,-32768,   837,   559,   140,   569,
   821,-32768,-32768,   553,   553,   555,-32768,   634,   251,   652,
   703,   291,   575,   576,   577,   579,   457,-32768,   773,-32768,
   584,   952,-32768,   496,-32768,   837,-32768,-32768,   837,-32768,
   595,   596,-32768,   589,   594,-32768,-32768,-32768,   459,   459,
   607,-32768,   598,-32768,-32768,   609,-32768,-32768,-32768,   872,
   971,   601,-32768,   624,    67,-32768,   182,   182,-32768,   674,
-32768,   182,-32768,   625,   626,   182,   703,-32768,-32768,   837,
   472,-32768,   472,-32768,   627,   628,   629,-32768,-32768,   630,
   622,   434,-32768,-32768,   635,   982,   638,   457,   639,   435,
   457,   209,-32768,   982,   631,-32768,   821,   301,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,   642,-32768,   487,-32768,
-32768,   632,-32768,-32768,-32768,   487,-32768,-32768,   982,-32768,
-32768,  1000,   640,  1019,-32768,-32768,-32768,-32768,   650,-32768,
-32768,-32768,-32768,   443,   445,-32768,-32768,-32768,   637,-32768,
   487,   647,  1456,   648,-32768,-32768,-32768,-32768,   727,-32768,
   633,-32768,   651,-32768,-32768,-32768,   653,   645,   654,   656,
   982,   320,-32768,-32768,   457,-32768,   655,   661,-32768,-32768,
-32768,-32768,   659,-32768,-32768,   703,-32768,  1373,-32768,-32768,
   182,    47,    89,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
   982,   665,-32768,-32768,-32768,   982,-32768,-32768,   671,-32768,
   457,-32768,-32768,   667,-32768,   673,   982,-32768,-32768,   767,
   768,-32768
};

static const short yypgoto[] = {-32768,
-32768,   706,-32768,-32768,   766,-32768,-32768,   783,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,     4,   668,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   -93,-32768,
-32768,  -144,-32768,   145,-32768,-32768,-32768,-32768,   -14,-32768,
   151,-32768,-32768,-32768,   143,  -103,-32768,-32768,-32768,-32768,
-32768,   603,  -165,-32768,-32768,   441,  -321,-32768,-32768,-32768,
   562,-32768,-32768,   408,  -594,   -97,-32768,-32768,   -94,   -92,
-32768,   373,   -98,   368,-32768,-32768,   364,-32768,-32768,-32768,
-32768,   240,-32768,-32768,    75,    82,-32768,-32768,   268,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,   410,-32768,-32768,  -173,
-32768,-32768,-32768,   454,-32768,   108,-32768,   121,-32768,   310,
  -177,-32768,-32768,-32768,-32768,    -3,-32768,-32768,-32768,-32768,
-32768,  -415,-32768,-32768,   798,    12,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,    86,-32768,-32768,-32768,
-32768,   779,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,  -235,   252,-32768,-32768,
  -280,  -430,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,  -126,   110,-32768,-32768,-32768,
-32768,-32768,   224,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,   139,    85,   387,   374,  -274,   -52,
  -203,  -154,     0,   550,-32768,  -488,  -527,  -641,  -624,   213,
  -170,-32768,  -319,   237,-32768,-32768,  -138,  -448,    31,    52,
    40,   -10,-32768,  -259,    -6,  -198
};


#define	YYLAST		1535


static const short yytable[] = {   181,
   185,   246,   278,   203,   172,   284,   369,   204,   285,   283,
   286,   427,   205,   201,   319,   330,   322,   358,   324,   219,
   505,   222,   363,   457,   359,   354,   635,   510,   456,   592,
   153,   229,   336,   583,   153,   211,   239,   339,   611,   155,
   401,   402,   750,   155,     3,   255,   239,   153,   172,   183,
   184,   154,   677,   498,   156,   154,   155,     2,     3,   172,
   159,   160,   161,   162,   163,   164,   165,   166,   154,   183,
   184,   182,     2,     3,   261,   262,     2,     3,   264,   187,
   266,   476,   267,   426,   758,   478,   270,   480,   194,   276,
   276,   183,   184,   153,   451,   183,   184,   183,   184,   453,
   183,   184,   155,   183,   468,   292,   206,   507,   217,   293,
   187,   183,   184,   298,   154,     2,     3,   208,   159,   160,
   161,   162,   163,   164,   165,   166,   212,   183,   184,   462,
   463,   297,   172,   303,     2,     3,   218,   195,   770,   792,
   710,   711,   462,   463,   187,   272,   207,   540,   743,   346,
   153,   167,   188,   168,   225,   300,   462,   463,   240,   155,
   592,   794,   361,   702,   302,   526,   195,   362,   680,   258,
   195,   154,   799,   770,   428,   348,   301,   345,   435,   352,
   563,   354,    37,   188,   183,   184,   277,   265,   514,   291,
   510,   598,   201,   344,   602,   387,   226,   195,   230,   554,
   555,   251,   462,   463,   252,   497,   224,   482,   374,   167,
   187,   483,   375,   244,   353,   209,   245,   188,   642,   386,
   252,   299,   391,   635,   395,   635,   153,   462,   463,   230,
   399,   400,   643,   333,   360,   155,   183,   184,   334,   236,
   403,   409,   316,   404,   366,   237,   413,   154,   582,   367,
   556,   557,   558,     2,     3,   638,   159,   160,   161,   162,
   163,   164,   165,   166,   235,   546,   276,   378,   437,   309,
   661,   303,   379,   564,   488,   565,   488,   566,   653,   384,
   509,   663,   502,   188,   385,   364,   445,   365,     2,     3,
   298,   159,   160,   161,   162,   163,   164,   165,   166,   368,
   213,   434,   397,   749,   448,   332,   303,   398,   449,   450,
   405,   410,   242,   406,   414,   521,   411,   214,   688,   415,
   153,   424,   215,   432,   289,   290,   425,   436,   433,   155,
   724,   438,   433,   257,   348,   358,   439,   504,   352,   263,
   348,   154,   359,   686,   352,   442,   271,   304,   687,   417,
   443,   244,   517,   418,   245,   216,   204,   474,   201,   493,
   548,   201,   475,   549,   530,   535,   536,   574,   538,  -435,
   403,   537,   433,   404,   541,   569,   580,   279,   391,   581,
   617,   547,   317,   691,   318,   460,   244,   419,   460,   245,
   325,   420,   153,   753,   649,   153,   651,   337,   460,   421,
   255,   155,   589,   422,   155,   327,   752,   530,   696,   530,
   267,   530,   783,   154,   270,   599,   154,   460,   600,   329,
     2,     3,   276,   159,   160,   161,   162,   163,   164,   165,
   166,   331,   603,   604,   488,   600,   600,   335,   303,   338,
     2,     3,   458,   159,   160,   161,   162,   163,   164,   165,
   166,   605,   638,   472,   600,   472,  -374,   608,   340,   472,
   609,   472,   341,   485,   159,   160,   161,   162,   163,   494,
   407,   337,   309,   501,   690,   343,   607,   368,   494,   745,
   284,   295,   748,   285,   283,   286,  -359,   615,   620,  -359,
   616,   616,   622,   639,   372,   600,   640,   348,     2,     3,
   646,   352,   624,   647,   636,   462,   463,   654,   524,   525,
   600,   290,   571,   373,   321,   573,   318,   655,   244,   539,
   656,   245,   664,   679,   380,   600,   581,   246,   740,   747,
   730,   741,   581,   376,   167,   383,   621,   764,   395,   765,
   616,   426,   647,   722,   723,   302,   594,   595,   382,   392,
   396,   416,   360,   408,   412,   423,   784,   301,   431,   488,
   440,   441,   272,   214,   670,   452,   454,   455,   459,   460,
   464,   465,   466,   477,   678,   479,   481,   486,   506,   681,
  -440,   508,   579,  -441,   613,   511,   513,   521,   521,   523,
   542,   544,   796,   552,   553,   559,   612,   502,   309,   561,
   575,     2,     3,   584,   159,   160,   161,   162,   163,   164,
   165,   166,   585,   309,   586,   591,   593,   601,   614,   303,
   618,   685,   619,   246,   644,   645,   641,   658,   648,   650,
   652,   589,   662,   657,   530,   771,   535,   535,   772,   788,
   773,   535,   660,   665,   704,   729,   666,   671,   667,-32768,
   668,   683,   669,   302,   467,   468,   569,   159,   160,   161,
   162,   163,   164,   165,   166,   301,   684,   692,   693,   694,
   771,   695,   303,   772,   278,   773,     2,     3,   700,   159,
   160,   161,   162,   163,   164,   165,   166,   707,   708,   309,
   709,   755,  -448,   718,   303,   323,   714,   318,   589,   244,
   309,   636,   245,   762,   712,     2,     3,  -236,   159,   160,
   161,   162,   163,   164,   165,   166,   720,   726,   727,   735,
   736,   737,   739,   303,   738,   682,   756,   742,   589,   309,
   744,   746,   751,   569,   754,   760,   763,   309,   766,   767,
   774,   775,   779,   777,   569,   778,   780,   699,   167,   785,
   318,   787,   244,   781,   494,   245,   267,   706,   786,   793,
   270,   276,   276,   795,   797,   798,   801,   802,   260,   157,
   167,   790,   791,   487,   244,     2,     3,   245,   159,   160,
   161,   162,   163,   164,   165,   166,   158,   296,   494,   721,
   715,   347,   388,   303,   725,   512,   543,   567,   731,   167,
   572,   318,   576,   244,     2,     3,   245,   159,   160,   161,
   162,   163,   164,   165,   166,   776,   659,   674,   516,   759,
   560,   210,   623,     2,     3,   309,   159,   160,   161,   162,
   163,   164,   165,   166,   761,   256,   782,   703,   597,     2,
     3,   303,   159,   160,   161,   162,   163,   164,   165,   166,
   596,   689,   705,   717,     0,     2,     3,   303,   159,   160,
   161,   162,   163,   164,   165,   166,     0,     0,     0,   167,
     0,   698,     0,   244,     2,     3,   245,   159,   160,   161,
   162,   163,   164,   165,   166,     0,   503,     0,     0,     0,
     0,     0,     0,     0,   243,     0,     0,     0,   167,     0,
     0,     0,   244,     2,     3,   245,   159,   160,   161,   162,
   163,   164,   165,   166,     0,     0,     0,   304,     0,     0,
     0,   244,     2,     3,   245,   159,   160,   161,   162,   163,
   164,   165,   166,   167,     0,     0,     0,   244,     2,     3,
   245,   159,   160,   161,   162,   163,   164,   165,   166,   167,
     0,     0,   487,   244,     2,     3,   245,   159,   160,   161,
   162,   163,   164,   165,   166,     0,     0,     0,   167,     0,
   518,     0,   244,     2,     3,   245,   159,   160,   161,   162,
   163,   164,   165,   166,     2,     3,     0,   159,   160,   161,
   162,   163,   164,   165,   166,     0,     0,   167,     0,   568,
     0,   244,   183,   184,   245,   625,   160,   161,   162,   163,
   626,   627,   628,     0,   610,     0,   167,     0,     0,     0,
   244,   183,   184,   245,   159,   160,   161,   162,   163,   626,
   627,   628,   167,   676,     0,     0,   244,     0,     0,   245,
     0,     0,     0,     0,     0,     0,   701,     0,   167,     0,
     0,     0,   244,     2,     3,   245,   159,   160,   161,   162,
   163,   164,   165,   166,     0,     0,     0,   167,     0,   716,
     0,   244,     0,     0,   245,     0,     0,     0,   167,     0,
     0,     0,   244,     0,     0,   245,    10,     0,    12,     0,
     0,    15,     0,     0,     0,     0,    20,    21,    22,     0,
   629,     0,     0,   630,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   629,
     0,    41,   630,    43,     0,     0,    -2,     1,     0,     2,
     3,     4,     0,     0,     0,    52,    53,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   315,     0,   168,
     5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
    25,    26,    27,    28,    29,     0,     0,     0,    30,    31,
    32,     0,    33,    34,    35,    36,    37,    38,    39,    40,
    41,    42,    43,     0,    44,     0,    45,    46,    47,    48,
     0,    49,    50,    51,    52,    53,    54,    55,    56,    57,
    58,    59,     0,     0,     0,    60,     0,    61,     0,    62,
    -1,     1,     0,     2,     3,     4,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     5,     6,     7,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,    23,    24,    25,    26,    27,    28,    29,     0,
     0,     0,    30,    31,    32,     0,    33,    34,    35,    36,
    37,    38,    39,    40,    41,    42,    43,     0,    44,     0,
    45,    46,    47,    48,     0,    49,    50,    51,    52,    53,
    54,    55,    56,    57,    58,    59,     0,     0,     0,    60,
     0,    61,     1,    62,     2,     3,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     5,     6,     7,     8,     9,
    10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
    20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
     0,     0,     0,    30,    31,    32,     0,    33,    34,    35,
    36,    37,    38,     0,    40,    41,    42,    43,     0,    44,
     0,    45,    46,    47,    48,     0,    49,    50,    51,    52,
    53,    54,    55,    56,    57,    58,    59,     2,     3,     0,
    60,    10,    61,    12,     0,     0,    15,     0,     0,     0,
     0,    20,    21,    22,     0,     0,     0,     0,     5,     6,
     7,     0,   789,     0,    11,     0,    13,     0,     0,    16,
     0,     0,     0,     0,     0,     0,    41,     0,    43,   294,
     0,    28,     0,     0,     0,    48,     0,     0,     0,     0,
    52,    53,    35,    36,    37,     0,     0,    40,     0,     0,
     0,     0,     0,     0,    45,     0,    47,     0,    10,    49,
    12,    51,     0,    15,    54,    55,     0,    57,    20,    21,
    22,   295,     0,    60,    10,    61,    12,     0,     0,    15,
     0,     0,     0,     0,    20,    21,    22,     0,     0,     0,
     0,     0,     0,    41,     0,    43,     0,     0,   282,     0,
     0,     0,     0,     0,     0,     0,     0,    52,    53,    41,
     0,    43,     0,     0,     0,     0,     0,     0,    48,     0,
     0,     0,     0,    52,    53
};

static const short yycheck[] = {     6,
    11,    54,   101,    18,     5,   103,   205,    18,   103,   103,
   103,   271,    19,    17,   141,   154,   143,   195,   145,    30,
   340,    32,   196,   304,   195,   191,   515,   349,   303,   460,
     0,    42,   171,   449,     4,    24,    47,   176,   487,     0,
   244,   245,   684,     4,     4,    56,    57,    17,    49,     3,
     4,     0,   580,   334,    93,     4,    17,     3,     4,    60,
     6,     7,     8,     9,    10,    11,    12,    13,    17,     3,
     4,    36,     3,     4,    85,    86,     3,     4,    89,    23,
    91,   317,    97,     6,   709,   321,    97,   323,    93,   100,
   101,     3,     4,    63,   293,     3,     4,     3,     4,   298,
     3,     4,    63,     3,     4,   116,    34,   343,    93,   116,
    23,     3,     4,   120,    63,     3,     4,    97,     6,     7,
     8,     9,    10,    11,    12,    13,    97,     3,     4,    19,
    20,   120,   133,    21,     3,     4,    93,    97,   733,   781,
   629,   630,    19,    20,    23,    99,    74,   383,   676,    93,
   120,    97,    96,    99,    97,   125,    19,    20,    49,   120,
   591,   786,    93,   612,   125,    99,    97,    98,   584,    60,
    97,   120,   797,   768,    97,   190,   125,   188,   277,   190,
   416,   347,    60,    96,     3,     4,    98,    93,   362,    97,
   512,   466,   196,    96,   475,    93,    12,    97,    96,   403,
   404,    93,    19,    20,    96,    95,    88,    95,    93,    97,
    23,    99,    97,   101,    93,    93,   104,    96,    95,   230,
    96,    90,   233,   712,   235,   714,   196,    19,    20,    96,
   241,   242,    95,    93,   195,   196,     3,     4,    98,     6,
   101,   252,   133,   104,    93,    12,   257,   196,   447,    98,
   405,   406,   407,     3,     4,   515,     6,     7,     8,     9,
    10,    11,    12,    13,    97,   392,   277,    93,   279,   131,
   551,    21,    98,   418,   327,   420,   329,   422,    95,    93,
    93,   562,   335,    96,    98,    96,   290,    98,     3,     4,
   297,     6,     7,     8,     9,    10,    11,    12,    13,    97,
    38,    99,    93,    95,    93,   167,    21,    98,    97,    98,
    96,    93,    96,    99,    93,   368,    98,    55,   599,    98,
   290,    93,    60,    93,   102,   103,    98,    93,    98,   290,
   650,    93,    98,    96,   349,   513,    98,   338,   349,    57,
   355,   290,   513,    93,   355,    93,    99,    97,    98,    93,
    98,   101,   367,    97,   104,    93,   367,    93,   362,   329,
    95,   365,    98,    98,   375,   376,    93,    93,   379,   100,
   101,    98,    98,   104,   385,   428,    95,    97,   389,    98,
    93,   392,    97,    93,    99,    98,   101,    93,    98,   104,
    97,    97,   362,    93,   533,   365,   535,    23,    98,    93,
   411,   362,   455,    97,   365,    97,   687,   418,   607,   420,
   425,   422,    93,   362,   425,    95,   365,    98,    98,    97,
     3,     4,   433,     6,     7,     8,     9,    10,    11,    12,
    13,    97,    95,    95,   487,    98,    98,    97,    21,   103,
     3,     4,   304,     6,     7,     8,     9,    10,    11,    12,
    13,    95,   712,   315,    98,   317,    97,    95,    97,   321,
    98,   323,     6,   325,     6,     7,     8,     9,    10,   331,
    22,    23,   334,   335,   601,    97,   483,    97,   340,   678,
   578,    87,   681,   578,   578,   578,    95,    95,    95,    98,
    98,    98,    95,    95,    93,    98,    98,   512,     3,     4,
    95,   512,   513,    98,   515,    19,    20,    95,   370,   371,
    98,   103,   431,    93,    97,   434,    99,    95,   101,   381,
    98,   104,    95,    95,    93,    98,    98,   580,    95,    95,
   657,    98,    98,    97,    97,    97,   506,    95,   549,    95,
    98,     6,    98,   647,   648,   506,   462,   463,    93,    97,
    93,    97,   513,    93,    93,    93,   755,   506,    99,   612,
    93,    97,    99,    55,   575,    93,    97,   102,    93,    98,
    18,    17,    16,    93,   581,    93,    93,    93,    88,   586,
   102,    96,   444,   102,    95,    98,    98,   640,   641,    98,
    96,    98,   791,    93,    96,    96,   100,   650,   460,    96,
    98,     3,     4,    97,     6,     7,     8,     9,    10,    11,
    12,    13,    93,   475,    98,    98,    93,   102,    98,    21,
    95,   591,    98,   676,    95,    98,   100,    93,    98,    97,
    97,   684,    93,   102,   645,   733,   647,   648,   733,   766,
   733,   652,   102,    95,   614,   656,    95,    15,    95,    16,
    95,    93,    95,   614,     3,     4,   709,     6,     7,     8,
     9,    10,    11,    12,    13,   614,    98,    93,    93,    93,
   768,    93,    21,   768,   773,   768,     3,     4,    95,     6,
     7,     8,     9,    10,    11,    12,    13,    93,    93,   551,
   102,   698,    99,    93,    21,    97,    99,    99,   751,   101,
   562,   712,   104,   714,    98,     3,     4,    99,     6,     7,
     8,     9,    10,    11,    12,    13,    93,    93,    93,    93,
    93,    93,   101,    21,    95,   587,    95,    93,   781,   591,
    93,    93,   102,   786,    93,    96,    87,   599,   102,    93,
    93,    15,    98,    93,   797,    93,    93,   609,    97,    95,
    99,    93,   101,    98,   616,   104,   771,   619,    98,    95,
   771,   772,   773,    93,    98,    93,     0,     0,    63,     4,
    97,   768,   779,   100,   101,     3,     4,   104,     6,     7,
     8,     9,    10,    11,    12,    13,     4,   120,   650,   645,
   640,   189,   231,    21,   652,   355,   389,   425,   660,    97,
   433,    99,   439,   101,     3,     4,   104,     6,     7,     8,
     9,    10,    11,    12,    13,   741,   549,   578,   365,   712,
   411,    24,   513,     3,     4,   687,     6,     7,     8,     9,
    10,    11,    12,    13,   714,    57,   751,   614,   465,     3,
     4,    21,     6,     7,     8,     9,    10,    11,    12,    13,
   464,   600,   616,   641,    -1,     3,     4,    21,     6,     7,
     8,     9,    10,    11,    12,    13,    -1,    -1,    -1,    97,
    -1,    99,    -1,   101,     3,     4,   104,     6,     7,     8,
     9,    10,    11,    12,    13,    -1,   337,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    93,    -1,    -1,    -1,    97,    -1,
    -1,    -1,   101,     3,     4,   104,     6,     7,     8,     9,
    10,    11,    12,    13,    -1,    -1,    -1,    97,    -1,    -1,
    -1,   101,     3,     4,   104,     6,     7,     8,     9,    10,
    11,    12,    13,    97,    -1,    -1,    -1,   101,     3,     4,
   104,     6,     7,     8,     9,    10,    11,    12,    13,    97,
    -1,    -1,   100,   101,     3,     4,   104,     6,     7,     8,
     9,    10,    11,    12,    13,    -1,    -1,    -1,    97,    -1,
    99,    -1,   101,     3,     4,   104,     6,     7,     8,     9,
    10,    11,    12,    13,     3,     4,    -1,     6,     7,     8,
     9,    10,    11,    12,    13,    -1,    -1,    97,    -1,    99,
    -1,   101,     3,     4,   104,     6,     7,     8,     9,    10,
    11,    12,    13,    -1,    95,    -1,    97,    -1,    -1,    -1,
   101,     3,     4,   104,     6,     7,     8,     9,    10,    11,
    12,    13,    97,    98,    -1,    -1,   101,    -1,    -1,   104,
    -1,    -1,    -1,    -1,    -1,    -1,    95,    -1,    97,    -1,
    -1,    -1,   101,     3,     4,   104,     6,     7,     8,     9,
    10,    11,    12,    13,    -1,    -1,    -1,    97,    -1,    99,
    -1,   101,    -1,    -1,   104,    -1,    -1,    -1,    97,    -1,
    -1,    -1,   101,    -1,    -1,   104,    29,    -1,    31,    -1,
    -1,    34,    -1,    -1,    -1,    -1,    39,    40,    41,    -1,
   101,    -1,    -1,   104,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
    -1,    64,   104,    66,    -1,    -1,     0,     1,    -1,     3,
     4,     5,    -1,    -1,    -1,    78,    79,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    97,    -1,    99,
    24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
    34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
    44,    45,    46,    47,    48,    -1,    -1,    -1,    52,    53,
    54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
    64,    65,    66,    -1,    68,    -1,    70,    71,    72,    73,
    -1,    75,    76,    77,    78,    79,    80,    81,    82,    83,
    84,    85,    -1,    -1,    -1,    89,    -1,    91,    -1,    93,
     0,     1,    -1,     3,     4,     5,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    24,    25,    26,    27,    28,    29,
    30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
    40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
    -1,    -1,    52,    53,    54,    -1,    56,    57,    58,    59,
    60,    61,    62,    63,    64,    65,    66,    -1,    68,    -1,
    70,    71,    72,    73,    -1,    75,    76,    77,    78,    79,
    80,    81,    82,    83,    84,    85,    -1,    -1,    -1,    89,
    -1,    91,     1,    93,     3,     4,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    24,    25,    26,    27,    28,
    29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
    39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
    -1,    -1,    -1,    52,    53,    54,    -1,    56,    57,    58,
    59,    60,    61,    -1,    63,    64,    65,    66,    -1,    68,
    -1,    70,    71,    72,    73,    -1,    75,    76,    77,    78,
    79,    80,    81,    82,    83,    84,    85,     3,     4,    -1,
    89,    29,    91,    31,    -1,    -1,    34,    -1,    -1,    -1,
    -1,    39,    40,    41,    -1,    -1,    -1,    -1,    24,    25,
    26,    -1,    50,    -1,    30,    -1,    32,    -1,    -1,    35,
    -1,    -1,    -1,    -1,    -1,    -1,    64,    -1,    66,    45,
    -1,    47,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
    78,    79,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
    -1,    -1,    -1,    -1,    70,    -1,    72,    -1,    29,    75,
    31,    77,    -1,    34,    80,    81,    -1,    83,    39,    40,
    41,    87,    -1,    89,    29,    91,    31,    -1,    -1,    34,
    -1,    -1,    -1,    -1,    39,    40,    41,    -1,    -1,    -1,
    -1,    -1,    -1,    64,    -1,    66,    -1,    -1,    69,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    79,    64,
    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
    -1,    -1,    -1,    78,    79
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/lib/bison.simple"
/* This file comes from bison-1.28.  */

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

#ifndef YYSTACK_USE_ALLOCA
#ifdef alloca
#define YYSTACK_USE_ALLOCA
#else /* alloca not defined */
#ifdef __GNUC__
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi) || (defined (__sun) && defined (__i386))
#define YYSTACK_USE_ALLOCA
#include <alloca.h>
#else /* not sparc */
/* We think this test detects Watcom and Microsoft C.  */
/* This used to test MSDOS, but that is a bad idea
   since that symbol is in the user namespace.  */
#if (defined (_MSDOS) || defined (_MSDOS_)) && !defined (__TURBOC__)
#if 0 /* No need for malloc.h, which pollutes the namespace;
	 instead, just don't use alloca.  */
#include <malloc.h>
#endif
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
/* I don't know what this was needed for, but it pollutes the namespace.
   So I turned it off.   rms, 2 May 1997.  */
/* #include <malloc.h>  */
 #pragma alloca
#define YYSTACK_USE_ALLOCA
#else /* not MSDOS, or __TURBOC__, or _AIX */
#if 0
#ifdef __hpux /* haible@ilog.fr says this works for HPUX 9.05 and up,
		 and on HPUX 10.  Eventually we can turn this on.  */
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#endif /* __hpux */
#endif
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc */
#endif /* not GNU C */
#endif /* alloca not defined */
#endif /* YYSTACK_USE_ALLOCA not defined */

#ifdef YYSTACK_USE_ALLOCA
#define YYSTACK_ALLOC alloca
#else
#define YYSTACK_ALLOC malloc
#endif

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto yyacceptlab
#define YYABORT 	goto yyabortlab
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, &yylloc, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval, &yylloc)
#endif
#else /* not YYLSP_NEEDED */
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif /* not YYLSP_NEEDED */
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Define __yy_memcpy.  Note that the size argument
   should be passed with type unsigned int, because that is what the non-GCC
   definitions require.  With GCC, __builtin_memcpy takes an arg
   of type size_t, but it can handle unsigned int.  */

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_memcpy(TO,FROM,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (to, from, count)
     char *to;
     char *from;
     unsigned int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (char *to, char *from, unsigned int count)
{
  register char *t = to;
  register char *f = from;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 217 "/usr/lib/bison.simple"

/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#ifdef __cplusplus
#define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else /* not __cplusplus */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#endif /* not __cplusplus */
#else /* not YYPARSE_PARAM */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif /* not YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
#ifdef YYPARSE_PARAM
int yyparse (void *);
#else
int yyparse (void);
#endif
#endif

int
yyparse(YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;
  int yyfree_stacks = 0;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  if (yyfree_stacks)
	    {
	      free (yyss);
	      free (yyvs);
#ifdef YYLSP_NEEDED
	      free (yyls);
#endif
	    }
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
#ifndef YYSTACK_USE_ALLOCA
      yyfree_stacks = 1;
#endif
      yyss = (short *) YYSTACK_ALLOC (yystacksize * sizeof (*yyssp));
      __yy_memcpy ((char *)yyss, (char *)yyss1,
		   size * (unsigned int) sizeof (*yyssp));
      yyvs = (YYSTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yyvsp));
      __yy_memcpy ((char *)yyvs, (char *)yyvs1,
		   size * (unsigned int) sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *)yyls, (char *)yyls1,
		   size * (unsigned int) sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 5:
#line 284 "fortran.y"
{
				/* Create id token for prog if unnamed. */
			  if(current_module_hash == -1) {
			    implied_id_token(&(yyvsp[0]),unnamed_prog);
			    def_function(
				type_PROGRAM,size_DEFAULT,&(yyvsp[0]),(Token*)NULL);
			    current_module_hash =
			      def_curr_module(&(yyvsp[0]));
			    current_module_type = type_PROGRAM;
			  }

					/* Handle END statement */
			  if(curr_stmt_class == tok_END) {
			    if(prev_stmt_class != tok_RETURN)
			      do_RETURN(current_module_hash,&(yyvsp[0]));
			    END_processing(&(yyval));
			  }
			  prev_stmt_class = curr_stmt_class;
			  integer_context = FALSE;
			  true_prev_stmt_line_num = yyval.line_num;
			;
    break;}
case 10:
#line 319 "fortran.y"
{
#ifdef CHECK_LABELS
			  def_label(&(yyvsp[-1]));
#endif
			;
    break;}
case 12:
#line 328 "fortran.y"
{
			    exec_stmt_count = 0;
			    executable_stmt = FALSE;
			;
    break;}
case 13:
#line 333 "fortran.y"
{
			    executable_stmt = FALSE;
			;
    break;}
case 14:
#line 337 "fortran.y"
{	/* handle statement functions correctly */
			  if(is_true(STMT_FUNCTION_EXPR, yyvsp[0].subclass)
				     && stmt_sequence_no <= SEQ_STMT_FUN) {
			    stmt_sequence_no = SEQ_STMT_FUN;
			    executable_stmt = FALSE;
			  }
			  else {
			    stmt_sequence_no = SEQ_EXEC;
			    ++exec_stmt_count;
			    executable_stmt = TRUE;
			  }
			;
    break;}
case 15:
#line 350 "fortran.y"
{
			    stmt_sequence_no = SEQ_EXEC;
			    ++exec_stmt_count;
			    executable_stmt = TRUE;
			;
    break;}
case 16:
#line 356 "fortran.y"
{
			    executable_stmt = TRUE;
			    if(stmt_sequence_no == 0)
			      stmt_sequence_no = SEQ_HEADER;
			    complex_const_allowed = FALSE; /* turn off flags */
			    inside_format=FALSE;
			    integer_context = FALSE;
			    in_assignment_stmt = FALSE;
			    yyval.line_num = prev_stmt_line_num; /* best guess */
			    yyerrok; /* (error message already given) */
			;
    break;}
case 17:
#line 370 "fortran.y"
{
			    current_module_type = type_PROGRAM;
			;
    break;}
case 18:
#line 374 "fortran.y"
{
			    current_module_type = type_SUBROUTINE;
			;
    break;}
case 19:
#line 378 "fortran.y"
{
			    current_module_type = type_SUBROUTINE;
			;
    break;}
case 20:
#line 382 "fortran.y"
{
			    current_module_type = type_BLOCK_DATA;
			;
    break;}
case 24:
#line 395 "fortran.y"
{
#ifdef ALLOW_INCLUDE
 			  open_include_file(yyvsp[-1].value.string);
#endif
 			;
    break;}
case 25:
#line 408 "fortran.y"
{
			     if(stmt_sequence_no < SEQ_IMPLICIT) {
				stmt_sequence_no = SEQ_IMPLICIT;
			     }
			;
    break;}
case 26:
#line 414 "fortran.y"
{
			     if(stmt_sequence_no < SEQ_IMPLICIT) {
				   stmt_sequence_no = SEQ_IMPLICIT;
			     }
			     else if(stmt_sequence_no > SEQ_SPECIF) {
			       check_stmt_sequence(&(yyvsp[0]),SEQ_SPECIF);
			     }
			;
    break;}
case 27:
#line 423 "fortran.y"
{
			  check_stmt_sequence(&(yyvsp[0]),SEQ_IMPLICIT);
			;
    break;}
case 28:
#line 427 "fortran.y"
{
			     if(stmt_sequence_no < SEQ_STMT_FUN) {
				stmt_sequence_no = SEQ_STMT_FUN;
		 	     }
			;
    break;}
case 29:
#line 433 "fortran.y"
{
			  check_stmt_sequence(&(yyvsp[0]),SEQ_SPECIF);
			;
    break;}
case 41:
#line 454 "fortran.y"
{
			if( highlight != -1 )
			{
				put_symbol(PAF_CLASS_DEF,NULL,
					hashtab[yyvsp[-2].value.integer].name,
					current_filename,
					yyvsp[-2].line_num,
					yyvsp[-2].curr_index,
					0,0,
					(long)0,NULL,NULL,NULL,
					get_comment(current_filename,yyvsp[-2].line_num),
					0,0,0,0);
			}
            current_struct_hash = yyvsp[-2].value.integer;
        ;
    break;}
case 42:
#line 470 "fortran.y"
{
            current_struct_hash = -1;
        ;
    break;}
case 79:
#line 533 "fortran.y"
{check_seq_header(&(yyvsp[0]));;
    break;}
case 80:
#line 535 "fortran.y"
{
			     def_function(
				type_PROGRAM,size_DEFAULT,&(yyvsp[-1]),(Token*)NULL);
			     current_module_hash =
			       def_curr_module(&(yyvsp[-1]));
			;
    break;}
case 81:
#line 548 "fortran.y"
{
			  do_ENTRY(&(yyvsp[-1]),(Token*)NULL
				   ,current_module_hash);
			;
    break;}
case 82:
#line 553 "fortran.y"
{
			  do_ENTRY(&(yyvsp[-4]),&(yyvsp[-2])
				   ,current_module_hash);
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_exprlist("entry stmt",&(yyvsp[-2]));
#endif
			;
    break;}
case 83:
#line 567 "fortran.y"
{
			 def_function(
				current_datatype,current_typesize,
				      &(yyvsp[-1]),(Token*)NULL);
			 current_module_hash=
			   def_curr_module(&(yyvsp[-1]));
			;
    break;}
case 84:
#line 576 "fortran.y"
{
			 def_function(
				current_datatype,current_typesize,
				      &(yyvsp[-4]),&(yyvsp[-2]));
			 current_module_hash=
			   def_curr_module(&(yyvsp[-4]));
#ifdef DEBUG_PARSER
			 if(debug_parser)
			   print_exprlist("function stmt",&(yyvsp[-2]));
#endif
			;
    break;}
case 85:
#line 588 "fortran.y"
{
			 def_function(
				type_UNDECL,size_DEFAULT,&(yyvsp[-1]),(Token*)NULL);
			 current_module_hash=
			   def_curr_module(&(yyvsp[-1]));
			;
    break;}
case 86:
#line 596 "fortran.y"
{
			 def_function(
				type_UNDECL,size_DEFAULT,&(yyvsp[-4]),&(yyvsp[-2]));
			 current_module_hash=
			   def_curr_module(&(yyvsp[-4]));
#ifdef DEBUG_PARSER
			 if(debug_parser)
			   print_exprlist("function stmt",&(yyvsp[-2]));
#endif
			;
    break;}
case 87:
#line 609 "fortran.y"
{
			  check_seq_header(&(yyvsp[0]));
			;
    break;}
case 88:
#line 615 "fortran.y"
{
			  check_seq_header(&(yyvsp[0]));
			;
    break;}
case 89:
#line 621 "fortran.y"
{
				current_record_hash = -1;
			;
    break;}
case 92:
#line 635 "fortran.y"
{
			  def_function(
				 type_SUBROUTINE,size_DEFAULT,
				       &(yyvsp[-1]),(Token*)NULL);
			  current_module_hash=
			    def_curr_module(&(yyvsp[-1]));
			;
    break;}
case 93:
#line 644 "fortran.y"
{
			  def_function(
				 type_SUBROUTINE,size_DEFAULT,&(yyvsp[-4]),&(yyvsp[-2]));
			  current_module_hash=
			    def_curr_module(&(yyvsp[-4]));
#ifdef DEBUG_PARSER
			  if(debug_parser)
			    print_exprlist("subroutine stmt",&(yyvsp[-2]));
#endif
			;
    break;}
case 94:
#line 657 "fortran.y"
{
			  check_seq_header(&(yyvsp[0]));
			;
    break;}
case 95:
#line 663 "fortran.y"
{
			    yyval.next_token = (Token*)NULL;
			;
    break;}
case 97:
#line 670 "fortran.y"
{
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			;
    break;}
case 98:
#line 674 "fortran.y"
{
			    yyval.next_token = append_token(yyvsp[-2].next_token,&(yyvsp[0]));
			;
    break;}
case 99:
#line 680 "fortran.y"
{
			     def_arg_name(&(yyvsp[0]));
			     primary_id_expr(&(yyvsp[0]),&(yyval));
			;
    break;}
case 100:
#line 685 "fortran.y"
{
			     yyval.class = type_byte(class_LABEL,type_LABEL);
			     yyval.subclass = 0;
			;
    break;}
case 101:
#line 695 "fortran.y"
{
				  /* form name %DATnn */
			  ++block_data_number;
			  sprintf(unnamed_block_data+4,"%02d"
				  ,block_data_number%100);
			  implied_id_token(&(yyval),unnamed_block_data);

			  def_function(
				 type_BLOCK_DATA,size_DEFAULT,
				       &(yyval),(Token*)NULL);
			  current_module_hash=
			    def_curr_module(&(yyval));
			;
    break;}
case 102:
#line 709 "fortran.y"
{
			  def_function(
				 type_BLOCK_DATA,size_DEFAULT,
				       &(yyvsp[-1]),(Token*)NULL);
			  current_module_hash=
			    def_curr_module(&(yyvsp[-1]));
			;
    break;}
case 103:
#line 719 "fortran.y"
{
			  check_seq_header(&(yyvsp[0]));
			;
    break;}
case 104:
#line 723 "fortran.y"
{
			  check_seq_header(&(yyvsp[0]));
			;
    break;}
case 108:
#line 738 "fortran.y"
{
			     def_array_dim(&(yyvsp[-3]),&(yyvsp[-1]));
			;
    break;}
case 109:
#line 745 "fortran.y"
{
			     yyval.class = 1;
			     yyval.subclass = yyvsp[0].subclass;
			;
    break;}
case 110:
#line 750 "fortran.y"
{
			     yyval.class = yyvsp[-2].class + 1; /* one more dimension */
			     yyval.subclass = yyvsp[-2].subclass * yyvsp[0].subclass;
			;
    break;}
case 111:
#line 757 "fortran.y"
{
			      if( datatype_of(yyvsp[0].class) == type_INTEGER
				 && is_true(EVALUATED_EXPR,yyvsp[0].subclass) )
				yyval.subclass = yyvsp[0].value.integer;
			      else
				yyval.subclass = 0;
			;
    break;}
case 112:
#line 765 "fortran.y"
{	/* avoid getting 0 - 0 + 1 = 1 if bounds nonconstant */
			      if( datatype_of(yyvsp[-2].class) == type_INTEGER
				 && is_true(EVALUATED_EXPR,yyvsp[-2].subclass)
				 && datatype_of(yyvsp[0].class) == type_INTEGER
				 && is_true(EVALUATED_EXPR,yyvsp[0].subclass) )
				yyval.subclass = yyvsp[0].value.integer - yyvsp[-2].value.integer + 1;
			      else
				yyval.subclass = 0;
			;
    break;}
case 113:
#line 775 "fortran.y"
{
			     yyval.subclass = 0;
			;
    break;}
case 114:
#line 779 "fortran.y"
{
			     yyval.subclass = 0;
			;
    break;}
case 115:
#line 785 "fortran.y"
{equivalence_flag = TRUE;;
    break;}
case 116:
#line 786 "fortran.y"
{equivalence_flag = FALSE;;
    break;}
case 119:
#line 794 "fortran.y"
{
			  equivalence(&(yyvsp[-2]), &(yyvsp[0]));
			;
    break;}
case 120:
#line 798 "fortran.y"
{
			  equivalence(&(yyvsp[-2]), &(yyvsp[0]));
			;
    break;}
case 121:
#line 805 "fortran.y"
{
			     def_equiv_name(&(yyvsp[0]));
			;
    break;}
case 122:
#line 809 "fortran.y"
{
			     def_equiv_name(&(yyvsp[0]));
			;
    break;}
case 123:
#line 813 "fortran.y"
{
			     def_equiv_name(&(yyvsp[0]));
			;
    break;}
case 127:
#line 828 "fortran.y"
{
			     implied_id_token(&(yyval),blank_com_name);
			     def_com_block(&(yyval), &(yyvsp[-1]));
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("blank common",&(yyvsp[-1]));
#endif

			;
    break;}
case 129:
#line 839 "fortran.y"
{
			     implied_id_token(&(yyval),blank_com_name);
			     def_com_block(&(yyval),&(yyvsp[-2]));
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("blank common",&(yyvsp[-2]));
#endif
			;
    break;}
case 130:
#line 850 "fortran.y"
{
		;
			if( highlight != -1 )
			{
				put_symbol(PAF_COMMON_DEF,NULL,blank_com_name,
					current_filename,
					line_num,
					curr_index,
					0,0,
					(long)0,NULL,NULL,NULL,
					get_comment(current_filename,line_num),
					0,0,0,0);
			}
		;
    break;}
case 131:
#line 865 "fortran.y"
{ yyval = yyvsp[0]; ;
    break;}
case 132:
#line 872 "fortran.y"
{
			     yyval.subclass = yyvsp[0].subclass;
			;
    break;}
case 133:
#line 876 "fortran.y"
{
			     yyval.subclass = yyvsp[0].subclass;
			     yyval.line_num = yyvsp[0].line_num;
			     yyval.col_num = yyvsp[0].col_num;
			;
    break;}
case 134:
#line 884 "fortran.y"
{
				if( highlight != -1 )
				{
					put_symbol(PAF_COMMON_DEF,NULL,
						hashtab[yyvsp[0].value.integer].name,
						current_filename,
						yyvsp[0].line_num,
						yyvsp[0].curr_index,
						0,0,
						(long)0,NULL,NULL,NULL,
						get_comment(current_filename,yyvsp[0].line_num),
						0,0,0,0);
				}
            	current_common_hash = yyvsp[0].value.integer;
			;
    break;}
case 135:
#line 900 "fortran.y"
{
            	current_common_hash = -1;
			     def_com_block(&(yyvsp[-2]),&(yyvsp[0]));
			     yyval.subclass = yyvsp[0].subclass;
			     yyval.line_num = yyvsp[0].line_num;
			     yyval.col_num = yyvsp[0].col_num;
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("labeled common",&(yyvsp[0]));
#endif
			;
    break;}
case 136:
#line 914 "fortran.y"
{
			     yyval = yyvsp[-1];
			;
    break;}
case 137:
#line 919 "fortran.y"
{
			     implied_id_token(&(yyval),blank_com_name);
			;
    break;}
case 138:
#line 923 "fortran.y"
{
			     implied_id_token(&(yyval),blank_com_name);
			;
    break;}
case 139:
#line 929 "fortran.y"
{
			    yyval.subclass = yyvsp[0].subclass;
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			;
    break;}
case 140:
#line 934 "fortran.y"
{
			    yyval.subclass = yyvsp[0].subclass;
			    yyval.line_num = yyvsp[0].line_num;
			    yyval.col_num = yyvsp[0].col_num;
			    yyval.next_token = append_token(yyvsp[-1].next_token,&(yyvsp[0]));
			;
    break;}
case 141:
#line 943 "fortran.y"
{			   /* no comma */
			     yyval.subclass = yyvsp[0].subclass;
			     make_false(COMMA_FLAG,yyval.subclass);
			;
    break;}
case 142:
#line 948 "fortran.y"
{			   /* has comma */
			     yyval.subclass = yyvsp[-1].subclass;
			     make_true(COMMA_FLAG,yyval.subclass);
   			;
    break;}
case 143:
#line 955 "fortran.y"
{
			     def_com_variable(&(yyvsp[0]));
			     primary_id_expr(&(yyvsp[0]),&(yyval));
			;
    break;}
case 144:
#line 960 "fortran.y"
{
			     def_com_variable(&(yyvsp[0]));
			     primary_id_expr(&(yyvsp[0]),&(yyval));
			;
    break;}
case 147:
#line 977 "fortran.y"
{
			    yyval = yyvsp[0];
			;
    break;}
case 148:
#line 983 "fortran.y"
{
			     def_namelist(&(yyvsp[-1]),&(yyvsp[0]));
			     yyval = yyvsp[0];
			;
    break;}
case 149:
#line 990 "fortran.y"
{
			    yyval = yyvsp[-1];
			;
    break;}
case 150:
#line 996 "fortran.y"
{
			     yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			;
    break;}
case 151:
#line 1000 "fortran.y"
{
			    yyval.subclass = yyvsp[0].subclass;
			    yyval.line_num = yyvsp[0].line_num;
			    yyval.col_num = yyvsp[0].col_num;
			    yyval.next_token = append_token(yyvsp[-1].next_token,&(yyvsp[0]));
			;
    break;}
case 152:
#line 1009 "fortran.y"
{			   /* no comma */
			     def_namelist_item(&(yyvsp[0]));
			     primary_id_expr(&(yyvsp[0]),&(yyval));
			     make_false(COMMA_FLAG,yyval.subclass);
			;
    break;}
case 153:
#line 1015 "fortran.y"
{			   /* has comma */
			     def_namelist_item(&(yyvsp[-1]));
			     primary_id_expr(&(yyvsp[-1]),&(yyval));
			     make_true(COMMA_FLAG,yyval.subclass);
			;
    break;}
case 154:
#line 1024 "fortran.y"
{
				current_record_hash = -1;
			;
    break;}
case 159:
#line 1034 "fortran.y"
{
			  current_typesize = size_DEFAULT;
			;
    break;}
case 160:
#line 1039 "fortran.y"
{
			    current_typesize = yyvsp[0].value.integer;
#if 0 /* defunct feature */
			    if(local_wordsize > 0) {
			      /*  recognize REAL*2w as DOUBLE PRECISION */
			      if(current_datatype == type_REAL
				 && yyvsp[0].value.integer == type_size[type_DP])
				current_datatype = type_DP;
			      /*  recognize COMPLEX*4w as DOUBLE COMPLEX */
			      if(current_datatype == type_COMPLEX
				 && yyvsp[0].value.integer==type_size[type_DCOMPLEX])
				current_datatype = type_DCOMPLEX;
			    }
#endif
			;
    break;}
case 162:
#line 1059 "fortran.y"
{
			     current_datatype = type_INTEGER;
			     integer_context = TRUE;
			;
    break;}
case 163:
#line 1064 "fortran.y"
{
			     current_datatype = type_REAL;
			     integer_context = TRUE;
			;
    break;}
case 164:
#line 1069 "fortran.y"
{
			     current_datatype = type_COMPLEX;
			     integer_context = TRUE;
			;
    break;}
case 165:
#line 1074 "fortran.y"
{
			     current_datatype = type_LOGICAL;
			     integer_context = TRUE;
			;
    break;}
case 166:
#line 1081 "fortran.y"
{
			     current_datatype = type_DP;
			     current_typesize = size_DEFAULT;
			;
    break;}
case 167:
#line 1086 "fortran.y"
{
			     current_datatype = type_DP;
			     current_typesize = size_DEFAULT;
			;
    break;}
case 168:
#line 1091 "fortran.y"
{
			     current_datatype = type_DCOMPLEX;
			     current_typesize = size_DEFAULT;
			;
    break;}
case 169:
#line 1096 "fortran.y"
{
			     current_datatype = type_DCOMPLEX;
			     current_typesize = size_DEFAULT;
			;
    break;}
case 170:
#line 1101 "fortran.y"
{
			     current_datatype = type_INTEGER;
			     current_typesize = 1;
			;
    break;}
case 171:
#line 1106 "fortran.y"
{
                current_datatype = type_RECORD;
                current_typesize = size_DEFAULT;
				current_record_hash = yyvsp[-1].value.integer;
            ;
    break;}
case 172:
#line 1114 "fortran.y"
{
			     current_datatype = type_STRING;
			     current_typesize = 1;
			     integer_context = TRUE;
			;
    break;}
case 173:
#line 1122 "fortran.y"
{
			     current_typesize = yyvsp[0].value.integer;
			;
    break;}
case 176:
#line 1132 "fortran.y"
{
			     declare_type(&(yyvsp[0]),
					  current_datatype,current_typesize);
			;
    break;}
case 177:
#line 1137 "fortran.y"
{
			     declare_type(&(yyvsp[0]),
					  current_datatype,current_typesize);
			;
    break;}
case 180:
#line 1148 "fortran.y"
{
			     declare_type(&(yyvsp[0]),
					  current_datatype,current_typesize);
			;
    break;}
case 181:
#line 1153 "fortran.y"
{
			     declare_type(&(yyvsp[-2]),
					  current_datatype,yyvsp[0].value.integer);
			;
    break;}
case 182:
#line 1158 "fortran.y"
{
			     declare_type(&(yyvsp[0]),
					  current_datatype,current_typesize);
			;
    break;}
case 183:
#line 1163 "fortran.y"
{
			     declare_type(&(yyvsp[-2]),
					  current_datatype,yyvsp[0].value.integer);
			;
    break;}
case 184:
#line 1170 "fortran.y"
{
                             current_datatype = type_POINTER;
                             current_typesize = size_DEFAULT;
                        ;
    break;}
case 188:
#line 1185 "fortran.y"
{implicit_flag=TRUE;;
    break;}
case 189:
#line 1189 "fortran.y"
{
			    implicit_flag=FALSE;
			    if(implicit_none) {
			    }
			    else {
				implicit_type_given = TRUE;
			    }
			;
    break;}
case 190:
#line 1199 "fortran.y"
{
			    implicit_flag=FALSE;
				if(implicit_type_given) {
				}
				else {
				    implicit_none = TRUE;
				}
			;
    break;}
case 192:
#line 1210 "fortran.y"
{initial_flag = TRUE;;
    break;}
case 194:
#line 1216 "fortran.y"
{implicit_letter_flag = TRUE;;
    break;}
case 195:
#line 1217 "fortran.y"
{implicit_letter_flag = FALSE;;
    break;}
case 198:
#line 1225 "fortran.y"
{
			  int c1 = (int)yyvsp[0].subclass;
			   set_implicit_type(current_datatype,current_typesize,
			     		c1,c1);
			;
    break;}
case 199:
#line 1231 "fortran.y"
{
			  int c1 = (int)yyvsp[-2].subclass,
			      c2 = (int)yyvsp[0].subclass;
			   set_implicit_type(current_datatype,current_typesize,
					c1,c2);
			;
    break;}
case 200:
#line 1242 "fortran.y"
{yyval.value.integer = size_ADJUSTABLE;;
    break;}
case 201:
#line 1244 "fortran.y"
{yyval.value.integer = yyvsp[0].value.integer;;
    break;}
case 202:
#line 1246 "fortran.y"
{
			      if((yyval.value.integer = yyvsp[-1].value.integer) <= 0 ){
				warning(yyvsp[-1].line_num,yyvsp[-1].col_num,
					"invalid length specification");
				msg_tail(": substituting 1");
				yyval.value.integer = 1;
			      }
			    ;
    break;}
case 206:
#line 1264 "fortran.y"
{complex_const_allowed = TRUE;;
    break;}
case 207:
#line 1266 "fortran.y"
{
			     def_parameter(&(yyvsp[-3]),&(yyvsp[0]));
			     primary_id_expr(&(yyvsp[-3]),&(yyvsp[-3]));
			     assignment_stmt_type(&(yyvsp[-3]),&(yyvsp[-1]),&(yyvsp[0]));
			     complex_const_allowed = FALSE;
			;
    break;}
case 209:
#line 1279 "fortran.y"
{
			     def_ext_name(&(yyvsp[0]));
			;
    break;}
case 210:
#line 1283 "fortran.y"
{
			     def_ext_name(&(yyvsp[0]));
			;
    break;}
case 212:
#line 1293 "fortran.y"
{
			     def_intrins_name(&(yyvsp[0]));
			;
    break;}
case 213:
#line 1297 "fortran.y"
{
			     def_intrins_name(&(yyvsp[0]));
			;
    break;}
case 214:
#line 1304 "fortran.y"
{
			  global_save = TRUE;
			;
    break;}
case 218:
#line 1315 "fortran.y"
{
			     save_variable(&(yyvsp[0]));
			;
    break;}
case 219:
#line 1319 "fortran.y"
{
			     save_com_block(&(yyvsp[-1]));
			;
    break;}
case 224:
#line 1334 "fortran.y"
{complex_const_allowed=TRUE;;
    break;}
case 225:
#line 1336 "fortran.y"
{complex_const_allowed=FALSE;;
    break;}
case 229:
#line 1345 "fortran.y"
{
			     use_lvalue(&(yyvsp[0]));
			;
    break;}
case 236:
#line 1361 "fortran.y"
{
			     use_parameter(&(yyvsp[0]));
			;
    break;}
case 238:
#line 1368 "fortran.y"
{
			     use_parameter(&(yyvsp[0]));
			;
    break;}
case 241:
#line 1379 "fortran.y"
{
			     use_lvalue(&(yyvsp[0]));
			;
    break;}
case 243:
#line 1387 "fortran.y"
{
			    use_implied_do_index(&(yyvsp[-3]));
			;
    break;}
case 246:
#line 1398 "fortran.y"
{complex_const_allowed = TRUE;
				    in_assignment_stmt = TRUE;;
    break;}
case 247:
#line 1400 "fortran.y"
{
			  assignment_stmt_type(&(yyvsp[-3]),&(yyvsp[-2]),
					&(yyvsp[0]));
			  complex_const_allowed = FALSE;
			  in_assignment_stmt = FALSE;
			;
    break;}
case 248:
#line 1407 "fortran.y"
{
				/* Clear u-b-s flags spuriously set */
			  if(is_true(STMT_FUNCTION_EXPR, yyvsp[-5].subclass)
				     && stmt_sequence_no <= SEQ_STMT_FUN)
			     stmt_function_stmt(&(yyvsp[-5]));
		        ;
    break;}
case 250:
#line 1417 "fortran.y"
{
			    yyval.dot_token = token_dup(&(yyvsp[0])); /* rigo */
			;
    break;}
case 254:
#line 1429 "fortran.y"
{
			    do_ASSIGN(&(yyvsp[-1]));
			;
    break;}
case 258:
#line 1446 "fortran.y"
{
			     do_assigned_GOTO(&(yyvsp[-1]));
			;
    break;}
case 259:
#line 1450 "fortran.y"
{
			     do_assigned_GOTO(&(yyvsp[-4]));
			;
    break;}
case 260:
#line 1454 "fortran.y"
{
			     do_assigned_GOTO(&(yyvsp[-5]));
			;
    break;}
case 261:
#line 1460 "fortran.y"
{
			    integer_context=TRUE;
			;
    break;}
case 262:
#line 1464 "fortran.y"
{
			    integer_context=TRUE;
			;
    break;}
case 268:
#line 1486 "fortran.y"
{complex_const_allowed = TRUE;;
    break;}
case 269:
#line 1487 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[-1].subclass)){
				use_variable(&(yyvsp[-1]));
			    }
			    complex_const_allowed = FALSE;

			    initial_flag = TRUE;	/* for is_keyword */
			    yyval = yyvsp[-1]; /* Inherit expr for type checking above */
			;
    break;}
case 271:
#line 1500 "fortran.y"
{complex_const_allowed = TRUE;;
    break;}
case 272:
#line 1501 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[-1].subclass)){
				use_variable(&(yyvsp[-1]));
			    }
			    complex_const_allowed = FALSE;

			    initial_flag = TRUE;
			;
    break;}
case 277:
#line 1531 "fortran.y"
{
			     use_lvalue(&(yyvsp[-3]));
			     use_variable(&(yyvsp[-3]));

				/* Check for non-integer DO index or bounds */
			     if(datatype_of(yyvsp[-3].class) == type_INTEGER
				&& datatype_of(yyvsp[-1].class) != type_INTEGER)
			       warning(yyvsp[-2].line_num,yyvsp[-2].col_num,
				  "type mismatch between DO index and bounds");

			     else if(datatype_of(yyvsp[-3].class) != type_INTEGER)
			       if(datatype_of(yyvsp[-1].class) != type_INTEGER) {
				 if(port_check)
				   nonportable(yyvsp[-1].line_num,yyvsp[-1].col_num,
					       "non-integer DO loop bounds");
			       }
			       else {
				 if(trunc_check)
				   warning(yyvsp[-3].line_num,yyvsp[-3].col_num,
					   "DO index is not integer");
			       }
			;
    break;}
case 278:
#line 1554 "fortran.y"
{complex_const_allowed=TRUE;;
    break;}
case 279:
#line 1555 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[-2].subclass)){
				use_variable(&(yyvsp[-2]));
			    }
			    complex_const_allowed=FALSE;
			    make_true(NONSTD_USAGE_FLAG,yyval.subclass);
			;
    break;}
case 280:
#line 1563 "fortran.y"
{complex_const_allowed=TRUE;;
    break;}
case 281:
#line 1564 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[-2].subclass)){
				use_variable(&(yyvsp[-2]));
			    }
			    complex_const_allowed=FALSE;
			    make_true(NONSTD_USAGE_FLAG,yyval.subclass);
			;
    break;}
case 284:
#line 1576 "fortran.y"
{
			    make_true(NONSTD_USAGE_FLAG,yyval.subclass);
			    integer_context=FALSE;
			;
    break;}
case 285:
#line 1583 "fortran.y"
{
			    yyval.class=do_bounds_type(&(yyvsp[-2]),&(yyvsp[0]),&(yyvsp[0]));
			;
    break;}
case 286:
#line 1587 "fortran.y"
{
			    yyval.class=do_bounds_type(&(yyvsp[-4]),&(yyvsp[-2]),&(yyvsp[0]));
			;
    break;}
case 294:
#line 1611 "fortran.y"
{
			     use_variable(&(yyvsp[0]));
			;
    break;}
case 296:
#line 1619 "fortran.y"
{complex_const_allowed = FALSE;;
    break;}
case 298:
#line 1621 "fortran.y"
{complex_const_allowed = FALSE;;
    break;}
case 300:
#line 1624 "fortran.y"
{control_item_count = 0;;
    break;}
case 301:
#line 1626 "fortran.y"
{complex_const_allowed = TRUE;;
    break;}
case 307:
#line 1639 "fortran.y"
{control_item_count = 0;;
    break;}
case 311:
#line 1649 "fortran.y"
{complex_const_allowed = TRUE;;
    break;}
case 312:
#line 1650 "fortran.y"
{complex_const_allowed = FALSE;;
    break;}
case 315:
#line 1655 "fortran.y"
{complex_const_allowed = TRUE;;
    break;}
case 316:
#line 1656 "fortran.y"
{complex_const_allowed = FALSE;;
    break;}
case 318:
#line 1661 "fortran.y"
{
			    ++control_item_count;
			;
    break;}
case 319:
#line 1665 "fortran.y"
{
			    ++control_item_count;
			;
    break;}
case 320:
#line 1674 "fortran.y"
{
			    use_io_keyword(&(yyvsp[-2]),&(yyvsp[0]),curr_stmt_class);
			;
    break;}
case 321:
#line 1678 "fortran.y"
{
			    if( yyvsp[0].class != '*'
			       && is_true(ID_EXPR,yyvsp[0].subclass)){
					/* WRITE(string,...) means store
					   output in the string */
				if(curr_stmt_class == tok_WRITE
				 && control_item_count == 0
				 && datatype_of(yyvsp[0].class) == type_STRING)
				    use_lvalue(&(yyvsp[0]));
					/* READ/WRITE(..,namelist) means
					   I/O with variables of namelist. */
				else if( control_item_count == 1
				    && datatype_of(yyvsp[0].class) == type_NAMELIST)
				    ref_namelist(&(yyvsp[0]),curr_stmt_class);

				use_variable(&(yyvsp[0]));
			    }
			;
    break;}
case 322:
#line 1704 "fortran.y"
{
			    if( yyvsp[0].class != '*'
			       && is_true(ID_EXPR,yyvsp[0].subclass)){
				use_variable(&(yyvsp[0]));
			    }
			    ++control_item_count;
			;
    break;}
case 323:
#line 1712 "fortran.y"
{
			    use_io_keyword(&(yyvsp[-2]),&(yyvsp[0]),curr_stmt_class);
			    ++control_item_count;
			;
    break;}
case 324:
#line 1717 "fortran.y"
{
			    ++control_item_count;
			;
    break;}
case 325:
#line 1723 "fortran.y"
{
			    use_io_keyword(&(yyvsp[-2]),&(yyvsp[0]),curr_stmt_class);
			;
    break;}
case 326:
#line 1727 "fortran.y"
{
			    use_special_open_keywd(&(yyvsp[0]));
			;
    break;}
case 329:
#line 1738 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].subclass)){
				if( curr_stmt_class == tok_READ ||
				    curr_stmt_class == tok_ACCEPT )
				    use_lvalue(&(yyvsp[0]));
				else
				    use_variable(&(yyvsp[0]));
			    }
			;
    break;}
case 331:
#line 1752 "fortran.y"
{
			     use_implied_do_index(&(yyvsp[-3]));
			;
    break;}
case 332:
#line 1758 "fortran.y"
{control_item_count = 0;;
    break;}
case 334:
#line 1763 "fortran.y"
{control_item_count = 0;;
    break;}
case 336:
#line 1768 "fortran.y"
{control_item_count = 0;;
    break;}
case 340:
#line 1776 "fortran.y"
{control_item_count = 0;;
    break;}
case 343:
#line 1783 "fortran.y"
{control_item_count = 0;;
    break;}
case 344:
#line 1784 "fortran.y"
{control_item_count = 0;;
    break;}
case 347:
#line 1791 "fortran.y"
{control_item_count = 0;;
    break;}
case 350:
#line 1805 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].subclass)){
				 use_variable(&(yyvsp[0]));
			    }
			;
    break;}
case 352:
#line 1814 "fortran.y"
{ inside_format=TRUE; ;
    break;}
case 353:
#line 1815 "fortran.y"
{ inside_format=FALSE; ;
    break;}
case 354:
#line 1821 "fortran.y"
{
			  check_stmt_sequence(&(yyvsp[-3]),SEQ_STMT_FUN);

				def_stmt_function(&(yyvsp[-3]),&(yyvsp[-1]));
					/* make token info */
				primary_id_expr(&(yyvsp[-3]),&(yyval));
#ifdef DEBUG_PARSER
				if(debug_parser)
				  print_exprlist("stmt function",&(yyvsp[-1]));
#endif
			;
    break;}
case 355:
#line 1835 "fortran.y"
{
			    yyval.next_token = (Token*)NULL;
			;
    break;}
case 357:
#line 1842 "fortran.y"
{
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			;
    break;}
case 358:
#line 1847 "fortran.y"
{
			    yyval.next_token = append_token(yyvsp[-2].next_token,&(yyvsp[0]));
			;
    break;}
case 360:
#line 1857 "fortran.y"
{
			     call_subr(&(yyvsp[0]),(Token*)NULL);
			     complex_const_allowed = FALSE;
			;
    break;}
case 362:
#line 1863 "fortran.y"
{
			     call_subr(&(yyvsp[-2]),(Token*)NULL);
			     complex_const_allowed = FALSE;
			;
    break;}
case 364:
#line 1869 "fortran.y"
{
			     call_subr(&(yyvsp[-3]),&(yyvsp[-1]));
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_exprlist("call stmt",&(yyvsp[-1]));
#endif
			     complex_const_allowed = FALSE;
			;
    break;}
case 366:
#line 1880 "fortran.y"
{
				if( current_module_hash != -1 )
				{
					if( highlight != -1 )
					{
						put_cross_ref(PAF_REF_TO_SUBROUTINE,
							cross_scope_type,
							PAF_REF_SCOPE_GLOBAL,
							NULL,
							hashtab[current_module_hash].name,
							NULL,
							NULL,
							hashtab[yyvsp[0].value.integer].name,
							NULL,
							current_filename,
							yyvsp[0].line_num,
							PAF_REF_PASS);
					}
				}
			    complex_const_allowed = TRUE;
			    yyval = yyvsp[0];
			;
    break;}
case 367:
#line 1905 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].subclass)){
				 use_actual_arg(&(yyvsp[0]));
				 use_variable(&(yyvsp[0]));
			    }
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			;
    break;}
case 368:
#line 1913 "fortran.y"
{
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			;
    break;}
case 369:
#line 1917 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].subclass)){
				 use_actual_arg(&(yyvsp[0]));
				 use_variable(&(yyvsp[0]));
			    }
			    yyval.next_token = append_token(yyvsp[-2].next_token,&(yyvsp[0]));
			;
    break;}
case 370:
#line 1925 "fortran.y"
{
			    yyval.next_token = append_token(yyvsp[-4].next_token,&(yyvsp[0]));
			;
    break;}
case 371:
#line 1932 "fortran.y"
{
			     do_RETURN(current_module_hash,&(yyvsp[-1]));
			;
    break;}
case 372:
#line 1936 "fortran.y"
{
			     do_RETURN(current_module_hash,&(yyvsp[-2]));
			;
    break;}
case 373:
#line 1943 "fortran.y"
{
				if( highlight != -1 )
				{
					put_cross_ref(PAF_REF_TO_FUNCTION,
						cross_scope_type,
						PAF_REF_SCOPE_GLOBAL,
						NULL,
						hashtab[current_module_hash].name,
						NULL,
						NULL,
						hashtab[yyvsp[-3].value.integer].name,
						NULL,
						current_filename,
						yyvsp[-3].line_num,
						PAF_REF_PASS);
				}
				   /* restore context */
				if(!is_true(COMPLEX_FLAG,yyvsp[-3].subclass))
				  complex_const_allowed=FALSE;
				if(is_true(IN_ASSIGN,yyvsp[-3].subclass))
				  in_assignment_stmt = TRUE;

				  /* Change empty arg list to no arg list */
				if(yyvsp[-1].next_token == NULL)
				  call_func(&(yyvsp[-3]),(Token *)NULL);
				else
				  call_func(&(yyvsp[-3]),&(yyvsp[-1]));
							/* make token info */
				func_ref_expr(&(yyvsp[-3]),&(yyvsp[-1]),&(yyval));
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("function",&(yyvsp[-1]));
#endif
			;
    break;}
case 374:
#line 1980 "fortran.y"
{
			    if(complex_const_allowed)/* save context */
			        make_true(COMPLEX_FLAG,yyval.subclass);
			    complex_const_allowed=TRUE;
			    if(in_assignment_stmt)
			        make_true(IN_ASSIGN,yyval.subclass);
			    in_assignment_stmt = FALSE;
			;
    break;}
case 375:
#line 1991 "fortran.y"
{
				yyval.class = 0;
				yyval.next_token = NULL;
			;
    break;}
case 377:
#line 1999 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].subclass)){
				 use_actual_arg(&(yyvsp[0]));
/* 				 use_variable(&($1)); */
			    }
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			;
    break;}
case 378:
#line 2007 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].subclass)){
				 use_actual_arg(&(yyvsp[0]));
/* 				 use_variable(&($3)); */
			    }
			    yyval.next_token = append_token(yyvsp[-2].next_token,&(yyvsp[0]));
			;
    break;}
case 380:
#line 2020 "fortran.y"
{
			    binexpr_type(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 381:
#line 2025 "fortran.y"
{
			    binexpr_type(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 383:
#line 2034 "fortran.y"
{
			    binexpr_type(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 385:
#line 2043 "fortran.y"
{
			    binexpr_type(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 387:
#line 2052 "fortran.y"
{
			    unexpr_type(&(yyvsp[-1]),&(yyvsp[0]),&(yyval));
			;
    break;}
case 389:
#line 2060 "fortran.y"
{
			    binexpr_type(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 391:
#line 2070 "fortran.y"
{
			    unexpr_type(&(yyvsp[-1]),&(yyvsp[0]),&(yyval));
			;
    break;}
case 392:
#line 2074 "fortran.y"
{
			    unexpr_type(&(yyvsp[-1]),&(yyvsp[0]),&(yyval));
			;
    break;}
case 393:
#line 2078 "fortran.y"
{
			    binexpr_type(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 394:
#line 2083 "fortran.y"
{
			    binexpr_type(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 396:
#line 2092 "fortran.y"
{
			    binexpr_type(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			    if(div_check &&
			       !is_true(CONST_EXPR,yyvsp[0].subclass)){
				warning(yyvsp[-1].line_num,yyvsp[-1].col_num,
					"Possible division by zero");
			    }
			;
    break;}
case 397:
#line 2102 "fortran.y"
{
			    binexpr_type(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 399:
#line 2111 "fortran.y"
{
			    binexpr_type(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 401:
#line 2120 "fortran.y"
{
			    yyval.dot_token = token_dup(&(yyvsp[0])); /* rigo */
/* 			    $$.next_token = append_token($1.next_token,&($3)); */
			;
    break;}
case 402:
#line 2125 "fortran.y"
{
			    binexpr_type(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 407:
#line 2140 "fortran.y"
{
			    yyval.subclass = 0;
			    make_true(CONST_EXPR,yyval.subclass);
			    make_true(PARAMETER_EXPR,yyval.subclass);
			    make_true(LIT_CONST,yyval.subclass);
			    make_true(EVALUATED_EXPR,yyval.subclass);
			;
    break;}
case 408:
#line 2148 "fortran.y"
{
			    yyval = yyvsp[-1];
				/* (identifier) becomes a non-identifier */
			    if(is_true(LVALUE_EXPR,yyvsp[-1].subclass)) {
				use_variable(&(yyvsp[-1]));
				make_false(LVALUE_EXPR,yyval.subclass);
				make_false(ARRAY_ID_EXPR,yyval.subclass);
				make_false(ID_EXPR,yyval.subclass);
			    }
			;
    break;}
case 409:
#line 2163 "fortran.y"
{
			    /* (class is set in numeric_const productions) */
			    yyval.size = size_DEFAULT;
			;
    break;}
case 410:
#line 2168 "fortran.y"
{
			    yyval.class = type_byte(class_VAR,type_STRING);
			    /* (size is set in get_string) */
			;
    break;}
case 411:
#line 2173 "fortran.y"
{
			    yyval.class = type_byte(class_VAR,type_HOLLERITH);
			    /* (size is set in get_hollerith) */
			    if(port_check && hollerith_check) {
				warning(yyvsp[0].line_num,yyvsp[0].col_num,
				"hollerith constant may not be portable");
			    }
			;
    break;}
case 412:
#line 2182 "fortran.y"
{
			    yyval.class = type_byte(class_VAR,type_LOGICAL);
			    yyval.size = size_DEFAULT;
			;
    break;}
case 413:
#line 2189 "fortran.y"
{
			    yyval.class = type_byte(class_VAR,type_INTEGER);
			;
    break;}
case 414:
#line 2193 "fortran.y"
{
			    yyval.class = type_byte(class_VAR,type_REAL);
			;
    break;}
case 415:
#line 2197 "fortran.y"
{
			    yyval.class = type_byte(class_VAR,type_DP);
			;
    break;}
case 416:
#line 2201 "fortran.y"
{
			    yyval.class = type_byte(class_VAR,type_COMPLEX);
			;
    break;}
case 417:
#line 2205 "fortran.y"
{
			    yyval.class = type_byte(class_VAR,type_DCOMPLEX);
			;
    break;}
case 418:
#line 2212 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].subclass)){
				use_variable(&(yyvsp[0]));
			    }
			;
    break;}
case 419:
#line 2221 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].subclass)){
				use_variable(&(yyvsp[0]));
			    }
			;
    break;}
case 420:
#line 2232 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].subclass)){
				use_variable(&(yyvsp[0]));
			    }
			    if( is_true(CONST_EXPR,yyvsp[0].subclass) ) {
			      if(datatype_of(yyvsp[0].class) == type_INTEGER){
				yyval.value.integer = int_expr_value(&(yyvsp[0]));
			      }
			    }
			;
    break;}
case 421:
#line 2246 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].subclass)){
				use_variable(&(yyvsp[0]));
			    }

			    if( datatype_of(yyvsp[0].class) != type_INTEGER ){
				yyval.value.integer = 0;
			    }
			    else {
			      if( is_true(EVALUATED_EXPR,yyvsp[0].subclass) )
				yyval.value.integer =
				  int_expr_value(&(yyvsp[0]));
			      else		/* must be dummy */
				yyval.value.integer = 0;
			    }
			;
    break;}
case 422:
#line 2269 "fortran.y"
{
				ref_array(&(yyvsp[-3]),&(yyvsp[-1]));
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("array lvalue",&(yyvsp[-1]));
#endif
					/* array now becomes scalar */
				make_false(ARRAY_ID_EXPR,yyval.subclass);
			;
    break;}
case 423:
#line 2281 "fortran.y"
{
				ref_array(&(yyvsp[-3]),&(yyvsp[-1]));
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("array",&(yyvsp[-1]));
#endif
					/* array now becomes scalar */
				make_false(ARRAY_ID_EXPR,yyval.subclass);
			;
    break;}
case 424:
#line 2293 "fortran.y"
{
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			;
    break;}
case 425:
#line 2297 "fortran.y"
{
			    yyval.next_token = append_token(yyvsp[-2].next_token,&(yyvsp[0]));
			;
    break;}
case 426:
#line 2303 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].subclass)){
				 use_variable(&(yyvsp[0]));
			    }
				/* check subscript exprs for integer type */
			    if(datatype_of(yyvsp[0].class) != type_INTEGER)
			      if(trunc_check)
			         warning(yyvsp[0].line_num,yyvsp[0].col_num,
					 "subscript is not integer");
			;
    break;}
case 427:
#line 2317 "fortran.y"
{
				   /* restore status of complex flag */
			    if(!is_true(COMPLEX_FLAG,yyvsp[-1].subclass))
				  complex_const_allowed=FALSE;
			    yyval.size=substring_size(&(yyvsp[-1]),&(yyvsp[0]));
			;
    break;}
case 428:
#line 2325 "fortran.y"
{
			    yyval.size=substring_size(&(yyvsp[-1]),&(yyvsp[0]));
			;
    break;}
case 429:
#line 2331 "fortran.y"
{
			    yyval.size=substring_size(&(yyvsp[-1]),&(yyvsp[0]));
			;
    break;}
case 430:
#line 2335 "fortran.y"
{
			    yyval.size=substring_size(&(yyvsp[-1]),&(yyvsp[0]));
			;
    break;}
case 431:
#line 2341 "fortran.y"
{
			    yyval.class=1;
			    yyval.subclass=0; /* 0 means LEN */
			;
    break;}
case 432:
#line 2347 "fortran.y"
{
			    yyval.class=yyvsp[-2].value.integer;
			    yyval.subclass=0; /* 0 means LEN */
			;
    break;}
case 433:
#line 2352 "fortran.y"
{
			    yyval.class=1;
			    yyval.subclass=yyvsp[-1].value.integer;
			;
    break;}
case 434:
#line 2357 "fortran.y"
{
			      yyval.class=yyvsp[-3].value.integer;
			      yyval.subclass=yyvsp[-1].value.integer;
			;
    break;}
case 435:
#line 2364 "fortran.y"
{
			  if(is_true(ID_EXPR,yyvsp[0].subclass)){
			    use_variable(&(yyvsp[0]));
			  }
				/* check validity and replace nonconst
				   value by size_UNKNOWN. */
			  if(is_true(CONST_EXPR,yyvsp[0].subclass)) {
			    yyval.value.integer=int_expr_value(&(yyvsp[0]));
			  }
			  else  /* (no longer need ID hash index) */
			    yyval.value.integer=size_UNKNOWN;
			;
    break;}
case 438:
#line 2386 "fortran.y"
{
/* 				printf( "Scalar name: %d <%s>\n" */
/* 					  , $1.value.integer */
/* 					  , hashtab[$1.value.integer].name ); */
			    ref_variable(&(yyvsp[0]));
			    primary_id_expr(&(yyvsp[0]),&(yyval));

				if( highlight != -1 && hashtab[yyvsp[0].value.integer].define )
				{
					put_cross_ref(PAF_REF_TO_CONSTANT,
						cross_scope_type,
						PAF_REF_SCOPE_GLOBAL,
						NULL,
						hashtab[current_module_hash].name,
						NULL,
						NULL,
						hashtab[yyvsp[0].value.integer].name,
						NULL,
						current_filename,
						yyvsp[0].line_num,
						PAF_REF_READ);
				}
			;
    break;}
case 439:
#line 2412 "fortran.y"
{
/* 				printf( "Array name: <%s>\n", hashtab[$1.value.integer].name ); */
			    ref_variable(&(yyvsp[0]));
			    primary_id_expr(&(yyvsp[0]),&(yyval));

				if( highlight != -1 && hashtab[yyvsp[0].value.integer].define )
				{
					put_symbol(PAF_CONS_DEF,NULL,
						hashtab[yyvsp[0].value.integer].name,
						current_filename,
						yyvsp[0].line_num,
						yyvsp[0].curr_index,
						0,0,
						(long)0,NULL,NULL,NULL,
						get_comment(current_filename,yyvsp[0].line_num),
						0,0,0,0);
				}
			;
    break;}
case 440:
#line 2435 "fortran.y"
{
				if( highlight != -1 && hashtab[yyvsp[0].value.integer].define )
				{
					put_symbol(PAF_CONS_DEF,NULL,
						hashtab[yyvsp[0].value.integer].name,
						current_filename,
						yyvsp[0].line_num,
						yyvsp[0].curr_index,
						0,0,
						(long)0,NULL,NULL,NULL,
						get_comment(current_filename,yyvsp[0].line_num),
						0,0,0,0);
				}
			;
    break;}
case 441:
#line 2450 "fortran.y"
{
				if( highlight != -1 && hashtab[yyvsp[0].value.integer].define )
				{
					put_symbol(PAF_CONS_DEF,NULL,
						hashtab[yyvsp[0].value.integer].name,
						current_filename,
						yyvsp[0].line_num,
						yyvsp[0].curr_index,
						0,0,
						(long)0,NULL,NULL,NULL,
						get_comment(current_filename,yyvsp[0].line_num),
						0,0,0,0);
				}
			;
    break;}
case 448:
#line 2480 "fortran.y"
{
			  if(yyvsp[0].value.integer == 0) {
			    warning(yyvsp[0].line_num,yyvsp[0].col_num,
				    "nonzero integer expected");
			    msg_tail(": substituting 1");
			    yyval.value.integer = 1;
			  }
			;
    break;}
case 449:
#line 2495 "fortran.y"
{
			    integer_context=TRUE;
			;
    break;}
case 450:
#line 2502 "fortran.y"
{
				integer_context=FALSE;
				yyval.class = type_byte(class_LABEL,type_LABEL);
				yyval.subclass = 0;
			;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 543 "/usr/lib/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;

 yyacceptlab:
  /* YYACCEPT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 0;

 yyabortlab:
  /* YYABORT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 1;
}
#line 2511 "fortran.y"

void
init_parser()			/* Initialize various flags & counters */
{
	initial_flag = TRUE;	/* set flag for keyword test */
	implicit_flag=FALSE;	/* clear flags for IMPLICIT stmt */
	implicit_letter_flag = FALSE;
	implicit_type_given = FALSE;
	implicit_none = FALSE;
	global_save = FALSE;
	prev_token_class = EOS;
	complex_const_allowed = FALSE;
	stmt_sequence_no = 0;
	true_prev_stmt_line_num = 0;
}

		/* Propagate non-integer type if any of DO loop
		   bounds are non-integer. */
PRIVATE int
do_bounds_type(t1,t2,t3)
     Token *t1, *t2, *t3;
{
  int result_class;
       if(datatype_of(t1->class) != type_INTEGER) result_class = t1->class;
  else if(datatype_of(t2->class) != type_INTEGER) result_class = t2->class;
  else if(datatype_of(t3->class) != type_INTEGER) result_class = t3->class;
  else result_class = t1->class;
  return result_class;
}


/* Debugging routine: prints the expression list of various productions */

#ifdef DEBUG_PARSER
PRIVATE void
print_exprlist(s,t)
	char *s;
	Token *t;
{

	fprintf(list_fd,"\n%s arglist: ",s);

	if(t == NULL)
		fprintf(list_fd,"(empty)");
	else {
  	    while( (t=t->next_token) != NULL) {
		  fprintf(list_fd,"%s ",type_name[datatype_of(t->class)]);
		  if( is_true(ID_EXPR,t->subclass) )
			fprintf(list_fd,"(%s) ",token_name(*t));
	    }
	}
}
#endif /* DEBUG_PARSER */

#ifdef DEBUG_PARSER
PRIVATE void
print_comlist(s,t)
	char *s;
	Token *t;
{

	fprintf(list_fd,"\n%s varlist: ",s);

	if(t == NULL)
		fprintf(list_fd,"(empty)");
	else {
  	    while( (t=t->next_token) != NULL) {
		  fprintf(list_fd,"%s ",type_name[datatype_of(t->class)]);
		  if( is_true(ID_EXPR,t->subclass) )
			fprintf(list_fd,"(%s) ",token_name(*t));
		}
	  }
}
#endif /* DEBUG_PARSER */

/* After having parsed prog_stmt, function_stmt, subroutine_stmt,
   block_data_stmt, the stmt_sequence_no is set to the value SEQ_HEADER.
*/

void
check_seq_header(t)
     Token *t;
{
	if(stmt_sequence_no >= SEQ_HEADER) {
	   END_processing(t);
	}
	stmt_sequence_no = SEQ_HEADER;
}

void
check_stmt_sequence(t,seq_num)
     Token *t;
     int seq_num;
{
    if(stmt_sequence_no <= seq_num) {
	stmt_sequence_no = seq_num;
    }
}

	/* After having parsed end_stmt, common block lists and
	   subprogram argument lists are copied over into global symbol
	   table, the local symbol table is printed out and then cleared,
	   and stmt_sequence_no is set to zero for start of next module.
	*/

PRIVATE void
END_processing(t)
	Token *t;
{
  if(current_module_hash != -1) {
	if(do_list && t != (Token *)NULL)
	    flush_line_out(t->line_num);
/* 	check_loose_ends(current_module_hash); */
	process_lists(current_module_hash);
	debug_symtabs();
#ifdef ERROR_MESS
#endif
	print_loc_symbols(current_module_hash);
	init_symtab();
  }
  exec_stmt_count = 0;
  stmt_sequence_no = 0;
  current_module_hash = -1;
  implicit_type_given = FALSE;
  implicit_none = FALSE;
  true_prev_stmt_line_num = 0;
  integer_context = FALSE;
  global_save = FALSE;
}

		/* Routine to add token t to the front of a token list. */
PRIVATE Token *
append_token(tlist,t)
     Token *tlist, *t;
{
	Token *tcopy;
	if((tcopy=new_token()) == (Token *)NULL){
	  oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
#ifdef LARGE_MACHINE
"Out of token space\nRecompile me with larger TOKENSPACESZ value"
#else
"Out of token space\nRecompile me with LARGE_MACHINE option"
#endif
		       );
	}

	*tcopy = *t;		/* make permanent copy of token */
	tcopy->next_token = tlist; /* link it onto front of list */
	return tcopy;		/* return it as new tlist */
}

		/* Routine to add token t to the front of a dot_token list. */
#if 0
PRIVATE Token *
append_dot_token(tlist,t)
     Token *tlist, *t;
{
	Token *tcopy;
	if((tcopy=new_token()) == (Token *)NULL){
	  oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
#ifdef LARGE_MACHINE
"Out of token space\nRecompile me with larger TOKENSPACESZ value"
#else
"Out of token space\nRecompile me with LARGE_MACHINE option"
#endif
		       );
	}

	*tcopy = *t;		/* make permanent copy of token */
	tcopy->dot_token = tlist; /* link it onto front of list */
	return tcopy;		/* return it as new tlist */
}
#endif

char *print_line_num( int line_num )
{
	static char ac[100];

	if( strcmp( current_filename, top_filename ))
	{
		sprintf( ac, "%d %s", line_num, current_filename );
	}
	else
	{
		sprintf( ac, "%d", line_num );
	}

	return ac;
}

static Token *token_dup( Token *t )
{
	Token *tcopy;

	if((tcopy=new_token()) == (Token *)NULL){
	  oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
#ifdef LARGE_MACHINE
"Out of token space\nRecompile me with larger TOKENSPACESZ value"
#else
"Out of token space\nRecompile me with LARGE_MACHINE option"
#endif
		       );
	}

	*tcopy = *t;		/* make permanent copy of token */

	return tcopy;
}

#ifdef NO_DATABASE

int put_cross_ref( int typ, int scope_type, int scope, char *name1, char *name2, char *name3, char *name4, char *filename, int lineno, int mode )
{
	FILE *sym_fp = stdout;

	fprintf( sym_fp,"%d", typ );
	fprintf( sym_fp," %d #", scope );
	if( name1 ) fprintf( sym_fp," %s", name1 );
	if( name2 ) fprintf( sym_fp," %s", name2 );
	if( name3 ) fprintf( sym_fp," %s", name3 );
	if( name4 ) fprintf( sym_fp," %s", name4 );
	if( filename ) fprintf( sym_fp," %s", filename );
	fprintf( sym_fp," %d", lineno );
	fprintf( sym_fp," %d", mode );
	fprintf( sym_fp, "\n" );
	return 0;
}

int put_symbol( int typ, char *function, char *name, char *filename, int lineno , int colpos, long l, void *x1, void *x2, void *x3)
{
	FILE *sym_fp = stdout;

	fprintf( sym_fp,"%d #", typ );
	if( function ) fprintf( sym_fp," %s", function );
	if( name ) fprintf( sym_fp," %s", name );
	if( filename ) fprintf( sym_fp," %s", filename );
	fprintf( sym_fp," %d.%d", lineno,colpos);
	fprintf( sym_fp, "\n" );
}

int put_symbol_comment( char *class, char *function, char *current_filename, char *comment, int start_line, int start_char, int end_line, int end_char )
{
	printf( "comment: <%s>\n", comment );
}

int put_comment( char *class_name, char *func_name, char *filename, char *comment, int line_num, int col_num )
{
}

#endif

