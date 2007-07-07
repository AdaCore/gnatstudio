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
  GTK="gtk+-2.0"
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

  AC_PATH_PROG(GTK_CONFIG, gtkada-config, no)
  min_gtk_version=ifelse([$1], ,2.0.0,$1)
  AC_MSG_CHECKING(for GtkAda - version >= $min_gtk_version)
  no_gtk=""
  if test "$GTK_CONFIG" = "no" ; then
    no_gtk=yes
  else
    GTK_CFLAGS=`$GTK_CONFIG --cflags`
    GTKADA_PREFIX=`$GTK_CONFIG --prefix`
    GTKADA_SRC=`echo $GTK_CFLAGS | sed -e 's/ *-aO.*//' -e 's/-aI//'`
    GTKADA_OBJ=`echo $GTK_CFLAGS | sed -e 's/.*-aO//'`
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
      printf("*** GtkAda is always available from http://libre.adacore.com\n");
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

