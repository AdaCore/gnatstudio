dnl $Id$

dnl Copyright (C) 1997 Cygnus Solutions, Inc.

dnl Description:
dnl A set of useful macro routines written in M4.

dnl This macro dumps out each of its arguments with a pipe character and a
dnl newline between each.  This is useful for lex alternative tokens (ie.
dnl "foo" | "bar". 

define(`oneof', `ifelse($#, 0,, $#, 1, "$1", "$1"|`oneof(shift($@))')')
