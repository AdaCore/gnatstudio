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
#  Checking for gnatprep
#
#############################################################


AC_DEFUN(AM_PATH_GNATPREP,
[
   AC_PATH_PROG(GNATPREP, gnatprep, no)

   if test x$GNATPREP = xno ; then
      AC_MSG_ERROR(I could not find gnatprep. See the file 'INSTALL' for more details.)
   fi

])

#############################################################
#
# Configure paths for GtkAda
#
#############################################################

dnl AM_PATH_GTK([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for GTK, and define GTK_CFLAGS and GTK_LIBS
dnl
AC_DEFUN(AM_PATH_GTK,
[dnl 
dnl Get the cflags and libraries from the gtkada-config script
dnl
  AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
  AC_MSG_CHECKING(GTK prefix)
  GTK="gtk+-2.0"
  if test "$PKG_CONFIG" = "no" ; then
    GTK_PREFIX=unknown
    AC_MSG_RESULT(not found)
  else
    GTK_PREFIX=`$PKG_CONFIG $GTK --variable=prefix`
    AC_MSG_RESULT($GTK_PREFIX)
  fi

  AC_PATH_PROG(GTK_CONFIG, gtkada-config, no)
  min_gtk_version=ifelse([$1], ,2.0.0,$1)
  AC_MSG_CHECKING(for GtkAda - version >= $min_gtk_version)
  no_gtk=""
  if test "$GTK_CONFIG" = "no" ; then
    no_gtk=yes
  else
    GTK_CFLAGS=`$GTK_CONFIG --cflags`
    GTK_LIBS=`$GTK_CONFIG --libs`
    GTK_STATIC_LIBS=`$GTK_CONFIG --libs --static`
    gtk_major_version=`$GTK_CONFIG --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    gtk_minor_version=`$GTK_CONFIG --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    gtk_micro_version=`$GTK_CONFIG --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
dnl
dnl Now check if the installed GtkAda is sufficiently new. (Also sanity
dnl checks the results of gtk-config to some extent
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
      printf("*** GtkAda is always available from http://gtkada.eu.org.\n");
      printf("***\n");
      printf("*** If you have already installed a sufficiently new version, this error\n");
      printf("*** probably means that the wrong copy of the gtkada-config shell script is\n");
      printf("*** being found. The easiest way to fix this is to remove the old version\n");
      printf("*** of GtkAda but you can also set the GTK_CONFIG environment to point to the\n");
      printf("*** correct copy of gtkada-config. (In this case, you will have to\n");
      printf("*** modify your LD_LIBRARY_PATH enviroment variable, or edit /etc/ld.so.conf\n");
      printf("*** so that the correct libraries are found at run-time))\n");
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
     if test "$GTK_CONFIG" = "no" ; then
       echo "*** The gtkada-config script could not be found."
       echo "*** If GtkAda was installed in PREFIX, make sure PREFIX/bin is in"
       echo "*** your path, or set the GTK_CONFIG environment variable to the"
       echo "*** full path to gtkada-config."
     else
       if test -f conf.gtktest ; then
        :
       else
	  echo "*** Could not run GtkAda test program"
       fi
     fi
     GTK_CFLAGS=""
     GTK_LIBS=""
     GTK_STATIC_LIBS=""
     ifelse([$3], , :, [$3])
  fi
  AC_SUBST(GTK_PREFIX)
  AC_SUBST(GTK_CFLAGS)
  AC_SUBST(GTK_LIBS)
  AC_SUBST(GTK_STATIC_LIBS)
  rm -f conf.gtktest
])

#############################################################
#
#  A small macro to create a file after preprocessing it using gnatprep
#
#############################################################


AC_DEFUN(AM_GNATPREP,
[   
   echo "creating $1"
   $GNATPREP $1.in $1 config.defs
])

#############################################################
#  Checking for working C++ compiler
#  As opposed to the standard AC_PROG_CXX, this one doesn't
#  fail if the C++ compiler doesn't work. Instead, it sets the
#  CXX variable to ""
#  Output: CXX is the name of the C++ compiler
#############################################################

AC_DEFUN(AM_CHECK_PROG_CXX,
[AC_BEFORE([$0], [AC_PROG_CXXCPP])dnl
AC_CHECK_PROGS(CXX, $CCC c++ g++ gcc CC cxx cc++ cl, gcc)

AM_CHECK_PROG_CXX_WORKS
AC_PROG_CXX_GNU

if test $ac_cv_prog_gxx = yes; then
  GXX=yes
else
  GXX=
fi


dnl Check whether -g works, even if CXXFLAGS is set, in case the package
dnl plays around with CXXFLAGS (such as to build both debugging and
dnl normal versions of a library), tasteless as that idea is.
ac_test_CXXFLAGS="${CXXFLAGS+set}"
ac_save_CXXFLAGS="$CXXFLAGS"
CXXFLAGS=
AC_PROG_CXX_G
if test "$ac_test_CXXFLAGS" = set; then
  CXXFLAGS="$ac_save_CXXFLAGS"
elif test $ac_cv_prog_cxx_g = yes; then
  if test "$GXX" = yes; then
    CXXFLAGS="-g -O2"
  else
    CXXFLAGS="-g"
  fi
else
  if test "$GXX" = yes; then
    CXXFLAGS="-O2"
  else
    CXXFLAGS=
  fi
fi
])


AC_DEFUN(AM_CHECK_PROG_CXX_WORKS,
[AC_MSG_CHECKING([whether the C++ compiler ($CXX $CXXFLAGS $LDFLAGS) works])
AC_LANG_SAVE
AC_LANG_CPLUSPLUS
AC_TRY_COMPILER([int main(){return(0);}], ac_cv_prog_cxx_works, ac_cv_prog_cxx_cross)
AC_LANG_RESTORE
AC_MSG_RESULT($ac_cv_prog_cxx_works)
if test $ac_cv_prog_cxx_works = no; then
  AC_MSG_RESULT([installation or configuration problem: C++ compiler cannot create executables.])
  CXX=""
fi
AC_MSG_CHECKING([whether the C++ compiler ($CXX $CXXFLAGS $LDFLAGS) is a cross-compiler])
AC_MSG_RESULT($ac_cv_prog_cxx_cross)
cross_compiling=$ac_cv_prog_cxx_cross
])
