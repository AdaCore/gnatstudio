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

sinclude(gnatlib/aclocal.m4)

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
  GTKADA_PRJ=`$GNATDRV ls -vP1 -Pgtkada 2>&1 | grep gtkada.gpr | grep Parsing | cut -d'"' -f2`
  no_gtk=""
  if test "$GNATDRV" = "no" -o ! -f "$GTKADA_PRJ"; then
    no_gtk=yes
  else
    gtk_major_version=`sed -n 's/version := \"\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)\";/\1/p' $GTKADA_PRJ`
    gtk_minor_version=`sed -n 's/version := \"\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)\";/\2/p' $GTKADA_PRJ`
    gtk_micro_version=`sed -n 's/version := \"\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)\";/\3/p' $GTKADA_PRJ`

dnl for now we disable the version check to be compatible with GPL 2015
dnl so we set with a correct version if not found.
    if test "$gtk_major_version" = ""; then gtk_major_version=3; fi
    if test "$gtk_minor_version" = ""; then gtk_minor_version=14; fi
    if test "$gtk_micro_version" = ""; then gtk_micro_version=3; fi

dnl
dnl Now check if the installed GtkAda is sufficiently new.
dnl
    rm -f conf.gtktest
    AC_TRY_RUN([
#include <stdio.h>

int
main ()
{
  int major, minor, micro;
  char *version = "$min_gtk_version";

  system ("touch conf.gtktest");

  if (sscanf(version, "%d.%d.%d", &major, &minor, &micro) != 3) {
     printf("%s, bad version string\n", "$min_gtk_version");
     exit(1);
  }

  if (($gtk_major_version > major) ||
     (($gtk_major_version == major) && ($gtk_minor_version > minor)) ||
     (($gtk_major_version == major) && ($gtk_minor_version == minor) && ($gtk_micro_version >= micro)))
    {
      return 0;
     }
   else
    {
      printf("\n*** An old version of GtkAda (%d.%d.%d) was found.\n",
             $gtk_major_version, $gtk_minor_version, $gtk_micro_version);
        printf("*** You need a version of GtkAda newer or equal to %d.%d.%d. The latest version of\n",
               major, minor, micro);
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
