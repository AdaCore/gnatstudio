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

/* intrinsics.h:
   List of intrinsic functions for use by find_intrinsic
   and check_intrins_args.  You may add locally available intrinsics
   to this list (order not important).

    Copyright (C) 1993 by Robert K. Moniot.
    This program is free software.  Permission is granted to
    modify it and/or redistribute it, retaining this notice.
    No guarantees accompany this software.


*/


	/* Define positional flags to allow specifying more
	   than one allowed type of argument for generics.
	 */

#define I   (1 << type_INTEGER)
#define R   (1 << type_REAL)
#define D   (1 << type_DP)
#define C   (1 << type_COMPLEX)
#define Z   (1 << type_DCOMPLEX)
#define L   (1 << type_LOGICAL)
#define STR (1 << type_STRING)

	/* Table contains: name, num_args, arg_type, result_type, flags.
	   Special num_args values are defined in symtab.h.

	   Flags: I_F77 if it is in Table 5 p. 15-24, I_NONF77 otherwise
		  I_MIXED_ARGS if arguments are not all of same type.
		  I_NONPURE if arg need not have defined value (LEN).
	          I_C_TO_R indicates complex -> real in generic case (ABS).
	          I_NOTARG if it is a generic with no specific meaning,
		      or if it is a type conversion, lexical relationship,
		      or min or max (cf. p. 15-3, sec. 15.3.2)
	 */

{"INT", 	1,	I|R|D|C|Z,type_INTEGER,	I_F77|I_NOTARG},
{"IFIX",	1,	R,	type_INTEGER,	I_F77|I_NOTARG},
{"IDINT",	1,	D,	type_INTEGER,	I_F77|I_NOTARG},
{"REAL",	1,	I|R|D|C|Z,type_REAL,	I_F77|I_NOTARG},
{"FLOAT",	1,	I,	type_REAL,	I_F77|I_NOTARG},
{"SNGL",	1,	D,	type_REAL,	I_F77|I_NOTARG},
{"DBLE",	1,	I|R|D|C|Z,type_DP,	I_F77|I_NOTARG},
{"CMPLX",	I_1or2,	I|R|D|C|Z,type_COMPLEX,	I_F77|I_NOTARG},
{"ICHAR",	1,	STR,	type_INTEGER,	I_F77|I_NOTARG|I_ICHAR},
{"CHAR",	1,	I,	type_STRING,	I_F77|I_NOTARG},
{"AINT",	1,	R|D,	type_GENERIC,	I_F77},
{"DINT",	1,	D,	type_DP,	I_F77},
{"ANINT",	1,	R|D,	type_GENERIC,	I_F77},
{"DNINT",	1,	D,	type_DP,	I_F77},
{"NINT",	1,	R|D,	type_INTEGER,	I_F77},
{"IDNINT",	1,	D,	type_INTEGER,	I_F77},
{"ABS", 	1,	I|R|D|C|Z,type_GENERIC,	I_F77|I_C_TO_R|I_ABS},
{"IABS",	1,	I,	type_INTEGER,	I_F77|I_ABS},
{"DABS",	1,	D,	type_DP,	I_F77},
{"CABS",	1,	C,	type_REAL,	I_F77},
{"MOD", 	2,	I|R|D,	type_GENERIC,	I_F77|I_MOD},
{"AMOD",	2,	R,	type_REAL,	I_F77},
{"DMOD",	2,	D,	type_DP,	I_F77},
{"SIGN",	2,	I|R|D,	type_GENERIC,	I_F77|I_SIGN},
{"ISIGN",	2,	I,	type_INTEGER,	I_F77|I_SIGN},
{"DSIGN",	2,	D,	type_DP,	I_F77},
{"DIM",		2,	I|R|D,	type_GENERIC,	I_F77|I_DIM},
{"IDIM",	2,	I,	type_INTEGER,	I_F77|I_DIM},
{"DDIM",	2,	D,	type_DP,	I_F77},
{"DPROD",	2,	R,	type_DP,	I_F77},
{"MAX",		I_2up,	I|R|D,	type_GENERIC,	I_F77|I_NOTARG|I_MAX},
{"MAX0",	I_2up,	I,	type_INTEGER,	I_F77|I_NOTARG|I_MAX},
{"AMAX1",	I_2up,	R,	type_REAL,	I_F77|I_NOTARG},
{"DMAX1",	I_2up,	D,	type_DP,	I_F77|I_NOTARG},
{"AMAX0",	I_2up,	I,	type_REAL,	I_F77|I_NOTARG},
{"MAX1",	I_2up,	R,	type_INTEGER,	I_F77|I_NOTARG},
{"MIN", 	I_2up,	I|R|D,	type_GENERIC,	I_F77|I_NOTARG|I_MIN},
{"MIN0",	I_2up,	I,	type_INTEGER,	I_F77|I_NOTARG|I_MIN},
{"AMIN1",	I_2up,	R,	type_REAL,	I_F77|I_NOTARG},
{"DMIN1",	I_2up,	D,	type_DP,	I_F77|I_NOTARG},
{"AMIN0",	I_2up,	I,	type_REAL,	I_F77|I_NOTARG},
{"MIN1",	I_2up,	R,	type_INTEGER,	I_F77|I_NOTARG},
{"LEN", 	1,	STR,	type_INTEGER,	I_F77|I_NONPURE|I_LEN},
{"INDEX",	2,	STR,	type_INTEGER,	I_F77|I_INDEX},
{"AIMAG",	1,	C,	type_REAL,	I_F77},
{"CONJG",	1,	C,	type_COMPLEX,	I_F77},
{"SQRT",	1,	R|D|C|Z,type_GENERIC,	I_F77},
{"DSQRT",	1,	D,	type_DP,	I_F77},
{"CSQRT",	1,	C,	type_COMPLEX,	I_F77},
{"EXP",		1,	R|D|C|Z,type_GENERIC,	I_F77},
{"DEXP",	1,	D,	type_DP,	I_F77},
{"CEXP",	1,	C,	type_COMPLEX,	I_F77},
{"LOG", 	1,	R|D|C|Z,type_GENERIC,	I_F77|I_NOTARG},
{"ALOG",	1,	R,	type_REAL,	I_F77},
{"DLOG",	1,	D,	type_DP,	I_F77},
{"CLOG",	1,	C,	type_COMPLEX,	I_F77},
{"LOG10",	1,	R|D,	type_GENERIC,	I_F77|I_NOTARG},
{"ALOG10",	1,	R,	type_REAL,	I_F77},
{"DLOG10",	1,	D,	type_DP,	I_F77},
{"SIN", 	1,	R|D|C|Z,type_GENERIC,	I_F77},
{"DSIN",	1,	D,	type_DP,	I_F77},
{"CSIN",	1,	C,	type_COMPLEX,	I_F77},
{"COS", 	1,	R|D|C|Z,type_GENERIC,	I_F77},
{"DCOS",	1,	D,	type_DP,	I_F77},
{"CCOS",	1,	C,	type_COMPLEX,	I_F77},
{"TAN", 	1,	R|D,	type_GENERIC,	I_F77},
{"DTAN",	1,	D,	type_DP,	I_F77},
{"ASIN",	1,	R|D,	type_GENERIC,	I_F77},
{"DASIN",	1,	D,	type_DP,	I_F77},
{"ACOS",	1,	R|D,	type_GENERIC,	I_F77},
{"DACOS",	1,	D,	type_DP,	I_F77},
{"ATAN",	1,	R|D,	type_GENERIC,	I_F77},
{"DATAN",	1,	D,	type_DP,	I_F77},
{"ATAN2",	2,	R|D,	type_GENERIC,	I_F77},
{"DATAN2",	2,	D,	type_DP,	I_F77},
{"SINH",	1,	R|D,	type_GENERIC,	I_F77},
{"DSINH",	1,	D,	type_DP,	I_F77},
{"COSH",	1,	R|D,	type_GENERIC,	I_F77},
{"DCOSH",	1,	D,	type_DP,	I_F77},
{"TANH",	1,	R|D,	type_GENERIC,	I_F77},
{"DTANH",	1,	D,	type_DP,	I_F77},
{"LGE", 	2,	STR,	type_LOGICAL,	I_F77|I_NOTARG},
{"LGT", 	2,	STR,	type_LOGICAL,	I_F77|I_NOTARG},
{"LLE", 	2,	STR,	type_LOGICAL,	I_F77|I_NOTARG},
{"LLT", 	2,	STR,	type_LOGICAL,	I_F77|I_NOTARG},

		/* DOUBLE COMPLEX intrinsics are included regardless
		   of NONSTD_INTRINSICS option, since they are essential
		   to support of this datatype.
		 */
{"DCMPLX",	I_1or2,	I|R|D|C|Z,type_DCOMPLEX,I_NONF77|I_NOTARG},
{"DCONJG",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"DIMAG",	1,	Z,	type_DP,	I_NONF77},
{"DREAL",	1,	Z,	type_DP,	I_NONF77},
{"CDABS",	1,	Z,	type_DP,	I_NONF77},
{"ZABS",	1,	Z,	type_DP,	I_NONF77},
{"CDSQRT",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"ZSQRT",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"CDEXP",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"ZEXP",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"CDLOG",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"ZLOG",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"CDSIN",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"ZSIN",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"CDCOS",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"ZCOS",	1,	Z,	type_DCOMPLEX,	I_NONF77},


#ifdef NONSTD_INTRINSICS

	/* Nonstandard but widely used intrinsics.  These follow both
	   VMS and AIX defns, so they are probably de facto standard.
	   Not included: specifics covered by a generic.
	   N.B. Argument checking is not tight for these: some
	   take arrays, 0 or 1 arguments, etc. that are not
	   handled by check_intrins_args().  Remarks are placed by
	   these cases.
	 */


		/* Bit test & Shift operations */
{"BTEST",	2,	I,	type_INTEGER,	I_NONF77},
{"IAND",	2,	I,	type_INTEGER,	I_NONF77},
{"IOR",		2,	I,	type_INTEGER,	I_NONF77},
{"IBSET",	2,	I,	type_INTEGER,	I_NONF77},
{"IBCLR",	2,	I,	type_INTEGER,	I_NONF77},
{"IBITS",	3,	I,	type_INTEGER,	I_NONF77},
{"IEOR",	2,	I,	type_INTEGER,	I_NONF77},
{"ISHFT",	2,	I,	type_INTEGER,	I_NONF77},
{"ISHFTC",	3,	I,	type_INTEGER,	I_NONF77},
{"NOT",		1,	I,	type_INTEGER,	I_NONF77},

		/* Utility routines */
{"EXIT",       I_0or1,	I,	type_SUBROUTINE,I_NONF77},
#endif

		/* Unix only.  These are a selected subset of the I_F77
		   library routines listed in the USENIX manual section 3F.
		 */
#ifdef UNIX_INTRINSICS
{"ABORT",	1,	STR,	type_SUBROUTINE,I_NONF77},
{"AND",		2,	I,	type_INTEGER,	I_NONF77},
		/* I, then STR not enforced in GETARG. */
{"GETARG",	2,	I|STR,	type_SUBROUTINE,I_MIXED_ARGS|I_NONF77},
{"GETENV",	2,	STR,	type_SUBROUTINE,I_NONF77},
{"GMTIME",	2,	I,	type_SUBROUTINE,I_NONF77},/*2nd arg array(9)*/
{"IARGC",	0,	0,	type_INTEGER,	I_NONF77},
{"LSHIFT",	2,	I,	type_INTEGER,	I_NONF77},
{"LTIME",	2,	I,	type_SUBROUTINE,I_NONF77},/*2nd arg array(9)*/
{"OR",		2,	I,	type_INTEGER,	I_NONF77},
#ifdef RAND_NO_ARG	/*RAND() form*/
{"IRAND",	0,	0,	type_INTEGER,	I_NONF77},
{"RAND",	0,	0,	type_REAL,	I_NONF77},
#else
#ifdef RAND_ONE_ARG	/*RAND(ISEED) form*/
{"IRAND",	1,	I,	type_INTEGER,	I_NONF77|I_NONPURE},
{"RAND",	1,	I,	type_REAL,	I_NONF77|I_NONPURE},
#else				/* Allow either form */
{"IRAND",	I_0or1,	I,	type_INTEGER,	I_NONF77|I_NONPURE},
{"RAND",	I_0or1,	I,	type_REAL,	I_NONF77|I_NONPURE},
#endif
#endif
{"RSHIFT",	2,	I,	type_INTEGER,	I_NONF77},
{"SRAND",	1,	I|R,	type_SUBROUTINE,I_NONF77},/*AIX has this*/
{"SYSTEM",	1,	STR,	type_SUBROUTINE,I_NONF77},
{"TIME",	I_0or1,	I,	type_INTEGER,	I_NONF77},
{"XOR",		2,	I,	type_INTEGER,	I_NONF77},
#endif

#ifdef VMS_INTRINSICS		/* VMS only */
{"DATE",	1,	STR,	type_SUBROUTINE,I_NONF77},
{"ERRSNS",	5,	I,	type_SUBROUTINE,I_NONF77},
{"IDATE",	3,	I,	type_SUBROUTINE,I_NONF77},
{"RAN",		1,	I,	type_REAL,	I_NONF77|I_NONPURE},
{"SECNDS",	1,	R,	type_REAL,	I_NONF77},
{"SIZEOF",	1,	I|R|D|C|Z|L|STR,type_INTEGER,	I_NONF77},
{"TIME",	1,	STR,	type_SUBROUTINE,I_NONF77},
#endif

#undef I
#undef R
#undef D
#undef C
#undef Z
#undef L
#undef STR

