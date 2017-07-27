AC_DEFUN(AM_CONFIG_HEADER,
[AC_PREREQ([2.53])
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

##############################################################
# Usage: AM_HAS_GNAT_PROJECT(project)
# Check whether a given project file is available, and set
# HAVE_GNAT_PROJECT_<project> to "yes" or "no" accordingly.
# (from PolyORB ada.m4)
##############################################################

AC_DEFUN([AM_HAS_GNAT_PROJECT],
[
cat > conftest.gpr <<EOF
with "[$1]";
project Conftest is for Source_Files use (); end Conftest;
EOF
if AC_TRY_COMMAND([gprls -Pconftest.gpr system.ads > /dev/null 2>conftest.out])
then
  HAVE_GNAT_PROJECT_$1=yes
else
  # Try with "gnatls", in case gprls was not available
  if AC_TRY_COMMAND([gnat ls -Pconftest.gpr system.ads > /dev/null 2>conftest.out])
  then
    HAVE_GNAT_PROJECT_$1=yes
  else
    HAVE_GNAT_PROJECT_$1=no
  fi
fi
AC_MSG_RESULT($HAVE_GNAT_PROJECT_$1)
AC_SUBST(HAVE_GNAT_PROJECT_$1)
])

##########################################################################
## Detects GTK and GtkAda
## Input:
##   If CONFIGURE_SWITCH_WITH_GTK is set, it specifies the default value
##     for gtk. Otherwise, configure will choose the most recent version.
## This exports the following variables
##     @PKG_CONFIG@: path to pkg-config, or "no" if not found
##     @GTK_GCC_FLAGS@: cflags to pass to the compiler. It isn't call
##                      GTK_CFLAGS for compatibility reasons with GPS
##     @WITH_GTK@: Either "yes" or "no", depending on whether gtk+ was found
##     @GTK_VERSION@: one of 2.0, 3.0 or "no"
##########################################################################

AC_DEFUN(AM_PATH_GTK,
[
   AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
   if test "$PKG_CONFIG" = "no" ; then
      WITH_GTK=no
      GTK_VERSION=no
   else
      AC_ARG_WITH(gtk,
         AC_HELP_STRING(
       [--with-gtk=version],
       [Specify the version of GTK to support (3.0 or 2.0)])
AC_HELP_STRING(
       [--without-gtk],
       [Disable support for GTK]),
         [WITH_GTK=$withval],
         [
            AC_MSG_CHECKING(for default gtk+ version)
            # Detect the version we should use, from the system
            for WITH_GTK in "$CONFIGURE_SWITCH_WITH_GTK" "3.0" "2.0" "no"; do
                if test "$WITH_GTK" != ""; then
                   GTK_PREFIX=`$PKG_CONFIG gtk+-${WITH_GTK} --variable=prefix`
                   if test "$GTK_PREFIX" != ""; then
                      break
                   fi
                fi
            done
            AC_MSG_RESULT($WITH_GTK)
         ])

      if test "$WITH_GTK" != "no"; then
          AC_MSG_CHECKING(for gtk+ ${WITH_GTK})
          GTK_PREFIX=`$PKG_CONFIG gtk+-${WITH_GTK} --variable=prefix`
          AC_MSG_RESULT($GTK_PREFIX)
          GTK_GCC_FLAGS=`$PKG_CONFIG gtk+-${WITH_GTK} --cflags`
          GTK_GCC_LIBS=`$PKG_CONFIG gtk+-${WITH_GTK} --libs`
          if test x"$GTK_GCC_FLAGS" != x ; then
             AC_MSG_CHECKING(for gtkada.gpr)
             AM_HAS_GNAT_PROJECT(gtkada)
             HAVE_GTKADA=$HAVE_GNAT_PROJECT_gtkada
             GTK_VERSION=$WITH_GTK
             WITH_GTK=${HAVE_GTKADA}
          else
             GTK_VERSION=no
             WITH_GTK=no
          fi
      fi
   fi

   AC_SUBST(PKG_CONFIG)
   AC_SUBST(GTK_GCC_FLAGS)
   AC_SUBST(GTK_GCC_FLAGS_GPR)
   AC_SUBST(GTK_GCC_LIBS)
   AC_SUBST(WITH_GTK)
   AC_SUBST(GTK_VERSION)
])

##########################################################################
## Converts a list of space-separated words into a list suitable for
## inclusion in .gpr files
##   $1=the list
##   $2=exported name
##########################################################################

AC_DEFUN(AM_TO_GPR,
[
   value=[$1]

   # Special handling on darwin for gcc 4.5 and 4.7
   case "$build_os" in
      *darwin*)
         value=`echo $value | sed -e "s/-framework \([[^ ]]*\)/-Wl,-framework -Wl,\1/g"`
   esac

   output=$2
   result=""
   for v in $value; do
      if test "$result" != ""; then
         result="$result, "
      fi
      result="$result\"$v\""
   done
   $2=$result
   AC_SUBST($2)

])

#############################################################
#
#  Checking for Gnat
#
#############################################################

conftest_ok="conftest.ok"

AC_DEFUN(AM_PATH_GNAT,
[
   AC_PATH_PROG(GNATMAKE, gnatmake, no)

   if test x$GNATMAKE = xno ; then
      AC_MSG_ERROR(I could not find gnatmake. See the file 'INSTALL' for more details.)
   fi

   AC_MSG_CHECKING(that your gnat compiler works with a simple example)

   rm -f conftest.adb
   cat << EOF > conftest.adb
with Ada.Text_IO;

procedure Conftest is
   Conftest_Ok : Ada.Text_IO.File_Type;
begin
   Ada.Text_IO.Create (File => Conftest_Ok,
                       Name => "$conftest_ok");
   Ada.Text_IO.Close (Conftest_Ok);
end Conftest;
EOF

   $GNATMAKE conftest > /dev/null 2>&1

   if ( test ! -x conftest ) then
      AC_MSG_RESULT(no)
      AC_MSG_ERROR($GNATMAKE test failed at compile time! Check your configuration.)
   fi

   ./conftest

   if ( test ! -f $conftest_ok ) then
      AC_MSG_RESULT(no)
      AC_MSG_ERROR($GNATMAKE test failed at run time! Check your configuration.)
   fi

   AC_MSG_RESULT(yes)
])

#############################################################
#
# Configure for libclang
#
#############################################################

AC_DEFUN(AM_PATH_LIBCLANG,
[
   AC_MSG_CHECKING(for libclang)

   CLANG_LIBS="-lclang"

   AC_ARG_WITH(clang,
     [AC_HELP_STRING(
        [--with-clang=<path>],
        [Specify the directory that contains the libclang libary])],
     [CLANG_LIBS="-L$withval $CLANG_LIBS"])
   AC_LANG_CONFTEST(
     [AC_LANG_PROGRAM(
        [],
        [clang_getCursorSpelling(0)])])

   _save_LIBS="$LIBS"
   LIBS="$CLANG_LIBS $LIBS"

   AC_LINK_IFELSE([],
     [AC_MSG_RESULT(yes)],
     [AC_MSG_ERROR([libclang not found (see --with-clang)])])

   LIBS=$_save_LIBS
   AC_SUBST(CLANG_LIBS)
])

#############################################################
#
# Configure for gnatcoll build location
#
#############################################################

AC_DEFUN(AM_PATH_GNATCOLL,
[
   AC_MSG_CHECKING(for gnatcoll_build)

   AC_ARG_WITH(gnatcoll,
     [AC_HELP_STRING(
        [--with-gnatcoll_build=<path>],
        [Specify the directory that contains the gnatcoll install])],
      [GNATCOLL_INSTALL=$withval])

   AC_MSG_RESULT(${GNATCOLL_INSTALL:-not specified})
   AC_SUBST(GNATCOLL_INSTALL)
])


#############################################################
#
# Configure paths for GtkAda
#
#############################################################

dnl AM_PATH_GTK([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for GTK, and define GTK_CFLAGS and GTK_LIBS
dnl
AC_DEFUN(AM_PATH_GTKADA,
[dnl
dnl Get the cflags and libraries from the gtkada-config script
dnl
  AC_MSG_CHECKING(GTK GLIB ATK PANGO prefix)
  GTK="gtk+-3.0"
  GLIB="glib-2.0"
  PANGO="pango"
  ATK="atk"
  CAIRO="cairo"
  if test "$PKG_CONFIG" = "no" ; then
    GTK_PREFIX=unknown
    AC_MSG_RESULT(not found)
  else
    GTK_PREFIX=`$PKG_CONFIG $GTK --variable=prefix`
    GLIB_PREFIX=`$PKG_CONFIG $GLIB --variable=prefix`
    ATK_PREFIX=`$PKG_CONFIG $ATK --variable=prefix`
    PANGO_PREFIX=`$PKG_CONFIG $PANGO --variable=prefix`
    CAIRO_PREFIX=`$PKG_CONFIG $CAIRO --variable=prefix`
    AC_MSG_RESULT($GTK_PREFIX $GLIB_PREFIX $ATK_PREFIX $PANGO_PREFIX $CAIRO_PREFIX)
  fi

  AC_PATH_PROG(GNATDRV, gnat, no)
  min_gtk_version=ifelse([$1], ,2.0.0,$1)
  AC_MSG_CHECKING(for GtkAda - version >= $min_gtk_version)
  GTKADA_PRJ=`$GNATDRV ls -vP1 -Pgtkada 2>&1 | grep gtkada.gpr | grep Parsing | cut -d'"' -f2 | head -1`
  no_gtk=""
  if test "$GNATDRV" = "no" -o ! -f "$GTKADA_PRJ"; then
    no_gtk=yes
  else
    # Full version number
    version=`sed -n 's/version := \"\(.*\)\";/\1/p' $GTKADA_PRJ | sed -e 's/w//'`

    gtk_major_version=`echo $version | cut -d. -f1`
    gtk_minor_version=`echo $version.0 | cut -d. -f2`

dnl
dnl Now check if the installed GtkAda is sufficiently new.
dnl
    rm -f conf.gtktest
    AC_TRY_RUN([
#include <stdio.h>

int
main ()
{
  int major, minor;
  char *version = "$min_gtk_version";

  system ("touch conf.gtktest");

  if (sscanf(version, "%d.%d", &major, &minor) != 2) {
     printf("%s, bad version string\n", "$min_gtk_version");
     exit(1);
  }

  if (($gtk_major_version > major) ||
     (($gtk_major_version == major) && ($gtk_minor_version > minor)) ||
     (($gtk_major_version == major) && ($gtk_minor_version == minor)))
    {
      return 0;
     }
   else
    {
      printf("\n*** An old version of GtkAda (%d.%d) was found.\n",
             $gtk_major_version, $gtk_minor_version);
        printf("*** You need a version of GtkAda newer or equal to %d.%d. The latest version of\n",
               major, minor);
      printf("*** GtkAda is always available from http://libre.adacore.com\n");
      printf("***\n");
      printf("*** If you have already installed a sufficiently new version, this error\n");
      printf("*** probably means that the wrong copy of the gtkada.gpr project is\n");
      printf("*** being found. The easiest way to fix this is to remove the old version\n");
      printf("*** of GtkAda. You may may have to modify your LD_LIBRARY_PATH enviroment variable, \n");
      printf("*** or edit /etc/ld.so.conf so that the correct libraries are found at run-time))\n");
    }
  return 1;
}
],, no_gtk=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
       CFLAGS="$ac_save_CFLAGS"
       LIBS="$ac_save_LIBS"
  fi
  if test "x$no_gtk" = x ; then
     AC_MSG_RESULT(yes)
     ifelse([$2], , :, [$2])
  else
     AC_MSG_RESULT(no)
     if test -f conf.gtktest ; then
      :
     else
      echo "*** Could not run GtkAda test program"
     fi
     GTK_CFLAGS=""
     GTKADA_SRC=""
     GTKADA_OBJ=""
     GTK_LIBS=""
     GTK_STATIC_LIBS=""
     ifelse([$3], , :, [$3])
  fi
  AC_SUBST(GTK_PREFIX)
  AC_SUBST(GLIB_PREFIX)
  AC_SUBST(ATK_PREFIX)
  AC_SUBST(PANGO_PREFIX)
  AC_SUBST(CAIRO_PREFIX)
  AC_SUBST(GTKADA_SRC)
  AC_SUBST(GTKADA_OBJ)
  AC_SUBST(GTK_CFLAGS)
  AC_SUBST(GTK_LIBS)
  AC_SUBST(GTK_STATIC_LIBS)
  AC_SUBST(GTKADA_PREFIX)
  rm -f conf.gtktest
])

###########################################################################
## Checking for pygobject
##
###########################################################################

AC_DEFUN(AM_PATH_PYGOBJECT,
[
    AC_ARG_ENABLE(pygobject,
      AC_HELP_STRING(
        [--disable-pygobject],
        [Disable support for PyGobject [[default=enabled]]]),
      [WITH_PYGOBJECT=$enableval],
      [WITH_PYGOBJECT=$WITH_PYTHON])

    AC_MSG_CHECKING(for pygobject)

    if test "$PKG_CONFIG" = "" -o "$PKG_CONFIG" = "no" ; then
       AC_MSG_RESULT(no (pkg-config not found))
       WITH_PYGOBJECT=no

    elif test "$GTK_VERSION" = "no" ; then
       AC_MSG_RESULT(no (gtk+ not found))
       WITH_PYGOBJECT=no

    else
       for version in 3.0 2.0 ; do
           module="pygobject-$version"
           $PKG_CONFIG $module --exists
           if test $? = 0 ; then
               break;
           fi
           module=""
       done

       if test "$module" = "" ; then
          AC_MSG_RESULT(no)
          WITH_PYGOBJECT=no
       else
          PYGOBJECT_INCLUDE=`$PKG_CONFIG $module python --cflags`
          PYGOBJECT_LIB=`$PKG_CONFIG $module --libs`
          AC_MSG_RESULT(yes ($version))
          WITH_PYGOBJECT=yes
          PYGOBJECT_INCLUDE="$PYGOBJECT_INCLUDE -DPYGOBJECT"
       fi
    fi

    AC_SUBST(WITH_PYGOBJECT)
    AC_SUBST(PYGOBJECT_INCLUDE)
    AC_SUBST(PYGOBJECT_LIB)
])
