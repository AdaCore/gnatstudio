## This file contains various m4 macros that can be included in your
## own projects to enable proper detection of the scripting languages
## In your own aclocal.m4 file, you can use syntax like
##   include(corelib/aclocal.m4)


#############################################################
# Checking for python
# This checks whether python is available on the system, and if yes
# what the paths are. The result can be forced by using the
#    --with-python=path
# command line switch
# The following variables are exported by configure on exit:
#    @PYTHON_BASE@:    Either "no" or the directory that contains python
#    @PYTHON_VERSION@: Version of python detected
#    @PYTHON_CFLAGS@:  Compiler flags to use for python code
#    @PYTHON_DIR@:     Directory for libpython.so
#    @PYTHON_LIBS@:    extra command line switches to pass to the linker
#                      In some cases, -lpthread should be added. We do not
#                      add this systematically, to allow you to recompile
#                      your own libpython and avoid dragging the tasking
#                      Ada runtime in your application if you do not use it
#                      otherwise
#    @PYTHON_ADA_SRC@: either "python" or "nopython" depending on whether
#                      python support is available. This can be used when you
#                      need to provide an interface even when python is not
#                      compiled in, but this interface does nothing.
#############################################################

AC_DEFUN(AM_PATH_PYTHON,
[
   AC_ARG_WITH(python,
               [ --with-python=<path>     Specify the full path to the Python installation],
               PYTHON_PATH_WITH=$withval,
               PYTHON_PATH_WITH=yes)

   PYTHON_ADA_SRC=python
   if test x"$PYTHON_PATH_WITH" = xno ; then
      AC_MSG_CHECKING(for python)
      AC_MSG_RESULT(no, use --with-python if needed)
      PYTHON_BASE=no
      PYTHON_ADA_SRC=nopython

   else
      AC_PATH_PROG(PYTHON, python, no, $PYTHON_PATH_WITH/bin:$PATH)
      if test x"$PYTHON" = xno ; then
         PYTHON_BASE=no
         PYTHON_ADA_SRC=nopython
      else
        AC_MSG_CHECKING(for python >= 2.0)
        if test x"$PYTHON_PATH_WITH" != xyes ; then
           PYTHON_BASE=$PYTHON_PATH_WITH
        else
           PYTHON_BASE=`$PYTHON -c 'import sys; print sys.prefix' `
        fi

        PYTHON_MAJOR_VERSION=`$PYTHON -c 'import sys; print sys.version_info[[0]]' 2>/dev/null`
        if test x$PYTHON_MAJOR_VERSION != x2 ; then
           AC_MSG_RESULT(no, need at least version 2.0)
           PYTHON_BASE=no
           PYTHON_ADA_SRC=nopython
        else
           PYTHON_VERSION=`$PYTHON -c 'import sys; print \`sys.version_info[[0]]\`+"."+\`sys.version_info[[1]]\`'`
           PYTHON_DIR=${PYTHON_BASE}/lib/python${PYTHON_VERSION}/config
           AC_MSG_RESULT(yes (version $PYTHON_VERSION))
        fi
      fi
   fi

   PYTHON_LIBS=""
   if test x"$PYTHON_BASE" != xno; then
      case "${host}" in
          hppa*-hp-hpux1[[0-9]]* )
             PYTHON_LIBS="-Wl,-E ${PYTHON_LIBS}"
             ;;
          powerpc-ibm-aix5.* ) 
             PYTHON_LIBS="-lld ${PYTHON_LIBS}"
             ;;
          powerpc-*-darwin* )
             PYTHON_LIBS="-ldl ${PYTHON_LIBS}"
             ;;
          *-sunos5.5* | *-solaris2.5* )
             PYTHON_LIBS="-lresolv -lsocket -lnsl -ldl -lm ${PYTHON_LIBS}"
             ;;
          *-sunos5* | *-solaris* )
             PYTHON_LIBS="-lresolv -lsocket -lnsl -ldl -lm ${PYTHON_LIBS}"
             ;;
          ia64-*-* )
             case "${host}" in
               *-linux-gnu* )
                  PYTHON_LIBS="-Wl,-export-dynamic -ldl ${PYTHON_LIBS}"
                  ;;
               *-hp-hpux11* )
                  PYTHON_LIBS="-ldld -ldl -Wl,-E ${PYTHON_LIBS}"
                  ;;
             esac
             ;;
          x86-64-*-* )
             PYTHON_LIBS="-Wl,-export-dynamic -ldl -lm ${PYTHON_LIBS}"
             ;;
          i[[3456]]86-*linux-gnu* )
             PYTHON_LIBS="-Wl,-export-dynamic -ldl -lm ${PYTHON_LIBS}"
             ;;
          i[[3456]]86-*win32* | i[[3456]]86-*mingw32* | i[[3456]]86-*cygwin* )
             ;;
          *-darwin* )
             PYTHON_LIBS="-ldl ${PYTHON_LIBS}"
             ;;
      esac

      PYTHON_LIBS="-L${PYTHON_DIR} -lpython${PYTHON_VERSION} ${PYTHON_LIBS}"
      PYTHON_CFLAGS="-I${PYTHON_BASE}/include/python${PYTHON_VERSION}"
   fi

   # Automatically check whether some libraries are needed to link with
   # the python libraries. If you are using the default system library, it is
   # generally the case that at least -lpthread will be needed. But you might
   # also have recompiled your own version, and if it doesn't depend on
   # pthreads, we shouldn't bring that in, since that also impacts the choice
   # of the GNAT runtime

   CFLAGS="${CFLAGS} ${PYTHON_CFLAGS}"
   LIBS="${LIBS} ${PYTHON_LIBS}"
   AC_LINK_IFELSE(
     [AC_LANG_PROGRAM([#include <Python.h>],[Py_Initialize();])],
     [],
     [LIBS="${LIBS} -lpthread -lutil"
      AC_LINK_IFELSE(
        [AC_LANG_PROGRAM([#include <Python.h>],[Py_Initialize();])],
        [PYTHON_LIBS="${PYTHON_LIBS} -lpthread -lutil"],
        [AC_MSG_FAILURE([Can't compile and link python example])])])

   AC_SUBST(PYTHON_BASE)
   AC_SUBST(PYTHON_VERSION)
   AC_SUBST(PYTHON_DIR)
   AC_SUBST(PYTHON_LIBS)
   AC_SUBST(PYTHON_CFLAGS)
   AC_SUBST(PYTHON_ADA_SRC)
])

###########################################################################
## Checking for pygtk
##   $1=minimum pygtk version required
## This function checks whether pygtk exists on the system, and has a recent
## enough version. It exports the following variables:
##    @PYGTK_PREFIX@: installation directory of pygtk
##    @PYGTK_INCLUDE@: cflags to use when compiling a pygtk application
## This function must be called after the variable PKG_CONFIG has been set,
## ie probably after gtk+ itself has been detected. Python must also have been
## detected first.
###########################################################################


AC_DEFUN(AM_PATH_PYGTK,
[
    AC_ARG_ENABLE(pygtk,
                  [  --disable-pygtk    do not try to build the special support
for PyGTK],
                  ,
                  enable_pygtk=yes)

    if test "$PKG_CONFIG" = "" -o "$PKG_CONFIG" = "no" ; then
       AC_MSG_CHECKING(for pygtk)
       AC_MSG_RESULT(no (pkg-config not found))
    else
       min_pygtk_version=ifelse([$1], ,2.8,$1)
       module=pygtk-2.0
       AC_MSG_CHECKING(for pygtk - version >= $min_pygtk_version)

       if test x"$enable_pygtk" = x -o x"$enable_pygtk" = xno ; then
          AC_MSG_RESULT(no)
          PYGTK_PREFIX=""
          PYGTK_INCLUDE=""

       elif test "$PYTHON_BASE" != "no" ; then
          pygtk_version=`$PKG_CONFIG $module --modversion`
          $PKG_CONFIG $module --atleast-version=$min_pygtk_version
          if test $? = 0 ; then
             PYGTK_INCLUDE="`$PKG_CONFIG $module --cflags` -DPYGTK"
             PYGTK_PREFIX=`$PKG_CONFIG $module --variable=prefix`
             AC_MSG_RESULT(yes (version $pygtk_version))
          else
             AC_MSG_RESULT(no (found $pygtk_version))
             PYGTK_PREFIX=""
             PYGTK_INCLUDE=""
          fi

       else
          AC_MSG_RESULT(no since python not found)
          PYGTK_PREFIX=""
          PYGTK_INCLUDE=""
       fi
    fi

    AC_SUBST(PYGTK_PREFIX)
    AC_SUBST(PYGTK_INCLUDE)
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
   output=$2
   result=""
   for v in $value; do
      if test "$result" != ""; then
         result="$result,
                      "
      fi
      result="$result\"$v\""
   done
   $2=$result
   AC_SUBST($2)

])

##########################################################################
## Detects GTK
## This exports the following variables
##     @PKG_CONFIG@: path to pkg-config, or "no" if not found
##     @GTK_CFLAGS@: cflags to pass to the compiler
##########################################################################

AC_DEFUN(AM_PATH_GTK,
[
   AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
   AC_MSG_CHECKING(gtk+)
   if test "$PKG_CONFIG" = "no" ; then
      AC_MSG_RESULT(not found)
   else
      GTK_PREFIX=`$PKG_CONFIG gtk+-2.0 --variable=prefix`
      AC_MSG_RESULT($GTK_PREFIX)
      GTK_CFLAGS=`$PKG_CONFIG gtk+-2.0 --cflags`
   fi
   AC_SUBST(PKG_CONFIG)
   AC_SUBST(GTK_CFLAGS)

])
