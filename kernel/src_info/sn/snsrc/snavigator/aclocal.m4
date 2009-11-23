dnl aclocal.m4 generated automatically by aclocal 1.4

dnl Copyright (C) 1994, 1995-8, 1999 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY, to the extent permitted by law; without
dnl even the implied warranty of MERCHANTABILITY or FITNESS FOR A
dnl PARTICULAR PURPOSE.

dnl written by Rob Savoye <rob@cygnus.com> for Cygnus Support
dnl major rewriting for Tcl 7.5 by Don Libes <libes@nist.gov>

"sinclude(../config/acinclude.m4)"

dnl Start of Tix stuff

AC_DEFUN(CY_AC_PATH_TIXCONFIG, [
#
# Ok, lets find the tix configuration
# First, look for one uninstalled.  
# the alternative search directory is invoked by --with-tixconfig
#

if test x"${no_tix}" = x ; then
  # we reset no_tix in case something fails here
  no_tix=true
  AC_ARG_WITH(tixconfig, [  --with-tixconfig           directory containing tix configuration (tixConfig.sh)],
         with_tixconfig=${withval})
  AC_MSG_CHECKING([for Tix configuration])
  AC_CACHE_VAL(ac_cv_c_tixconfig,[

  # First check to see if --with-tixconfig was specified.
  if test x"${with_tixconfig}" != x ; then
    if test -f "${with_tixconfig}/tixConfig.sh" ; then
      ac_cv_c_tixconfig=`(cd ${with_tixconfig}; pwd)`
    else
      AC_MSG_ERROR([${with_tixconfig} directory doesn't contain tixConfig.sh])
    fi
  fi

  # then check for a private Tix library
  if test x"${ac_cv_c_tixconfig}" = x ; then
    for i in \
		../tix \
		`ls -dr ../tix[[4]]* 2>/dev/null` \
		../../tix \
		`ls -dr ../../tix[[4]]* 2>/dev/null` \
		../../../tix \
		`ls -dr ../../../tix[[4]]* 2>/dev/null` ; do
      if test -f "$i/tixConfig.sh" ; then
        ac_cv_c_tixconfig=`(cd $i; pwd)`
	break
      fi
    done
  fi
  # check in a few common install locations
  if test x"${ac_cv_c_tixconfig}" = x ; then
    for i in `ls -d ${prefix}/lib /usr/local/lib 2>/dev/null` ; do
      if test -f "$i/tixConfig.sh" ; then
        ac_cv_c_tkconfig=`(cd $i; pwd)`
	break
      fi
    done
  fi
  # check in a few other private locations
  if test x"${ac_cv_c_tixconfig}" = x ; then
    for i in \
		${srcdir}/../tix \
		`ls -dr ${srcdir}/../tix[[4-9]]* 2>/dev/null` ; do
      if test -f "$i/tixConfig.sh" ; then
        ac_cv_c_tixconfig=`(cd $i; pwd)`
	break
      fi
    done
  fi
  ])
  if test x"${ac_cv_c_tixconfig}" = x ; then
    TIXCONFIG="# no Tix configs found"
    AC_MSG_WARN(Can't find Tix configuration definitions)
  else
    no_tix=
    TIXCONFIG=${ac_cv_c_tixconfig}/tixConfig.sh
    AC_MSG_RESULT(found $TIXCONFIG)
  fi
fi

])

# Defined as a separate macro so we don't have to cache the values
# from PATH_TIXCONFIG (because this can also be cached).
AC_DEFUN(CY_AC_LOAD_TIXCONFIG, [
    if test -f "$TIXCONFIG" ; then
      . $TIXCONFIG
    fi

    AC_SUBST(TIX_BUILD_LIB_SPEC)
])

# End of Tix stuff

dnl Use this macro if you want to include db.h.
AC_DEFUN(CY_AC_BERKELEY_DB,
[for type in int8_t u_int8_t int16_t u_int16_t int32_t u_int32_t int64_t u_int64_t ; do
  AC_MSG_CHECKING(for $type)
  AC_CACHE_VAL(db_cv_$type,
  AC_TRY_COMPILE([#include <sys/types.h>], $type foo;,
     eval db_cv_$type=yes, eval db_cv_$type=no))
  result=`eval echo \\${db_cv_\${type}}` 
  AC_MSG_RESULT($result)
  if test x$result = xyes
  then
    case $type in
	int8_t)		AC_DEFINE(HAVE_INT8_T)
			;;
	u_int8_t)	AC_DEFINE(HAVE_UINT8_T)
			;;
	int16_t)	AC_DEFINE(HAVE_INT16_T)
			;;
	u_int16_t)	AC_DEFINE(HAVE_UINT16_T)
			;;
	int32_t)	AC_DEFINE(HAVE_INT32_T)
			;;
	u_int32_t)	AC_DEFINE(HAVE_UINT32_T)
			;;
	int64_t)	AC_DEFINE(HAVE_INT64_T)
			;;
	u_int64_t)	AC_DEFINE(HAVE_UINT64_T)
			;;
	*)		AC_MSG_ERROR([this should never happen])
			;;
    esac
  fi
done])

# Like AC_CONFIG_HEADER, but automatically create stamp file.

AC_DEFUN(AM_CONFIG_HEADER,
[AC_PREREQ([2.12])
AC_CONFIG_HEADER([$1])
dnl When config.status generates a header, we must update the stamp-h file.
dnl This file resides in the same directory as the config header
dnl that is generated.  We must strip everything past the first ":",
dnl and everything past the last "/".
AC_OUTPUT_COMMANDS(changequote(<<,>>)dnl
ifelse(patsubst(<<$1>>, <<[^ ]>>, <<>>), <<>>,
<<test -z "<<$>>CONFIG_HEADERS" || echo timestamp > patsubst(<<$1>>, <<^\([^:]*/\)?.*>>, <<\1>>)stamp-h<<>>dnl>>,
<<am_indx=1
for am_file in <<$1>>; do
  case " <<$>>CONFIG_HEADERS " in
  *" <<$>>am_file "*<<)>>
    echo timestamp > `echo <<$>>am_file | sed -e 's%:.*%%' -e 's%[^/]*$%%'`stamp-h$am_indx
    ;;
  esac
  am_indx=`expr "<<$>>am_indx" + 1`
done<<>>dnl>>)
changequote([,]))])

# Do all the work for Automake.  This macro actually does too much --
# some checks are only needed if your package does certain things.
# But this isn't really a big deal.

# serial 1

dnl Usage:
dnl AM_INIT_AUTOMAKE(package,version, [no-define])

AC_DEFUN(AM_INIT_AUTOMAKE,
[AC_REQUIRE([AC_PROG_INSTALL])
PACKAGE=[$1]
AC_SUBST(PACKAGE)
VERSION=[$2]
AC_SUBST(VERSION)
dnl test to see if srcdir already configured
if test "`cd $srcdir && pwd`" != "`pwd`" && test -f $srcdir/config.status; then
  AC_MSG_ERROR([source directory already configured; run "make distclean" there first])
fi
ifelse([$3],,
AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE", [Name of package])
AC_DEFINE_UNQUOTED(VERSION, "$VERSION", [Version number of package]))
AC_REQUIRE([AM_SANITY_CHECK])
AC_REQUIRE([AC_ARG_PROGRAM])
dnl FIXME This is truly gross.
missing_dir=`cd $ac_aux_dir && pwd`
AM_MISSING_PROG(ACLOCAL, aclocal, $missing_dir)
AM_MISSING_PROG(AUTOCONF, autoconf, $missing_dir)
AM_MISSING_PROG(AUTOMAKE, automake, $missing_dir)
AM_MISSING_PROG(AUTOHEADER, autoheader, $missing_dir)
AM_MISSING_PROG(MAKEINFO, makeinfo, $missing_dir)
AC_REQUIRE([AC_PROG_MAKE_SET])])

#
# Check to make sure that the build environment is sane.
#

AC_DEFUN(AM_SANITY_CHECK,
[AC_MSG_CHECKING([whether build environment is sane])
# Just in case
sleep 1
echo timestamp > conftestfile
# Do `set' in a subshell so we don't clobber the current shell's
# arguments.  Must try -L first in case configure is actually a
# symlink; some systems play weird games with the mod time of symlinks
# (eg FreeBSD returns the mod time of the symlink's containing
# directory).
if (
   set X `ls -Lt $srcdir/configure conftestfile 2> /dev/null`
   if test "[$]*" = "X"; then
      # -L didn't work.
      set X `ls -t $srcdir/configure conftestfile`
   fi
   if test "[$]*" != "X $srcdir/configure conftestfile" \
      && test "[$]*" != "X conftestfile $srcdir/configure"; then

      # If neither matched, then we have a broken ls.  This can happen
      # if, for instance, CONFIG_SHELL is bash and it inherits a
      # broken ls alias from the environment.  This has actually
      # happened.  Such a system could not be considered "sane".
      AC_MSG_ERROR([ls -t appears to fail.  Make sure there is not a broken
alias in your environment])
   fi

   test "[$]2" = conftestfile
   )
then
   # Ok.
   :
else
   AC_MSG_ERROR([newly created file is older than distributed files!
Check your system clock])
fi
rm -f conftest*
AC_MSG_RESULT(yes)])

dnl AM_MISSING_PROG(NAME, PROGRAM, DIRECTORY)
dnl The program must properly implement --version.
AC_DEFUN(AM_MISSING_PROG,
[AC_MSG_CHECKING(for working $2)
# Run test in a subshell; some versions of sh will print an error if
# an executable is not found, even if stderr is redirected.
# Redirect stdin to placate older versions of autoconf.  Sigh.
if ($2 --version) < /dev/null > /dev/null 2>&1; then
   $1=$2
   AC_MSG_RESULT(found)
else
   $1="$3/missing $2"
   AC_MSG_RESULT(missing)
fi
AC_SUBST($1)])

# Add --enable-maintainer-mode option to configure.
# From Jim Meyering

# serial 1

AC_DEFUN(AM_MAINTAINER_MODE,
[AC_MSG_CHECKING([whether to enable maintainer-specific portions of Makefiles])
  dnl maintainer-mode is disabled by default
  AC_ARG_ENABLE(maintainer-mode,
[  --enable-maintainer-mode enable make rules and dependencies not useful
                          (and sometimes confusing) to the casual installer],
      USE_MAINTAINER_MODE=$enableval,
      USE_MAINTAINER_MODE=no)
  AC_MSG_RESULT($USE_MAINTAINER_MODE)
  AM_CONDITIONAL(MAINTAINER_MODE, test $USE_MAINTAINER_MODE = yes)
  MAINT=$MAINTAINER_MODE_TRUE
  AC_SUBST(MAINT)dnl
]
)

# Define a conditional.

AC_DEFUN(AM_CONDITIONAL,
[AC_SUBST($1_TRUE)
AC_SUBST($1_FALSE)
if $2; then
  $1_TRUE=
  $1_FALSE='#'
else
  $1_TRUE='#'
  $1_FALSE=
fi])


dnl AM_PROG_LEX
dnl Look for flex, lex or missing, then run AC_PROG_LEX and AC_DECL_YYTEXT
AC_DEFUN(AM_PROG_LEX,
[missing_dir=ifelse([$1],,`cd $ac_aux_dir && pwd`,$1)
AC_CHECK_PROGS(LEX, flex lex, "$missing_dir/missing flex")
AC_PROG_LEX
AC_DECL_YYTEXT])

