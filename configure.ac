AC_INIT([GNAT Studio],[0.0],[support@adacore.com])
AC_CONFIG_SRCDIR(gnatstudio/src/gps-main.adb)
AC_CONFIG_MACRO_DIRS([m4])
AC_CANONICAL_TARGET

if test -f VERSION.txt; then
    GPS_VERSION=`cat VERSION.txt`
else
    GPS_VERSION=AC_PACKAGE_VERSION()
fi

if test -f DATE.txt; then
    GPS_DATE=`cat DATE.txt`
else
    GPS_DATE="unknown date"
fi

YEAR=` date +%Y `

# Forces gtk+ 3.0
CONFIGURE_SWITCH_WITH_GTK=3.0
export CONFIGURE_SWITCH_WITH_GTK

STRIP_CR=False
CAN_OUTPUT=True
TARGET_CFLAGS=""
TARGET_LFLAGS='()'
EXEC_COMMAND="xterm -e"
PATH_STYLE=UNIX

# We need to double the quotes here, since this is inserted in an Ada file
# We pipe the output to "cat", since otherwise "ps" computes the size of its
# terminal and truncates the lines.
DEFAULT_PS='sh -c """"""(ps x 2> /dev/null || ps -u \$USER 2> /dev/null || ps) | cat""""""'

PRINT_CMD="lp"
OPAQUE_MDI=True
HOST_TYPE=Unix
DIFF_CMD="diff"
DIFF3_CMD="diff3"
PATCH_CMD="patch"
EXECUTION_WINDOW=True
CHARSET="ISO-8859-1"

TARGET=$host
EXEC_PREFIX="$prefix"

TARGET_IS_DARWIN="False"
case $TARGET in
   *darwin*) TARGET_IS_DARWIN="True" ;;
esac

CURRENT_YEAR=`date +%Y`

######## Check XML/Ada

AC_MSG_CHECKING(for xmlada.gpr)
AM_HAS_GNAT_PROJECT(xmlada)
if test x"${HAVE_GNAT_PROJECT_xmlada}" = x"yes" ; then
   XMLADA_GPR=with_xmlada
else
   XMLADA_GPR=no_xmlada
fi

######## Location of GNATcoll
AM_PATH_GNATCOLL

######## Check GtkAda

AC_MSG_CHECKING(for gtkada.gpr)
AM_HAS_GNAT_PROJECT(gtkada)
if test x"${HAVE_GNAT_PROJECT_gtkada}" != x"yes" ; then
   AC_MSG_ERROR(GTKADA project required but not found)
fi

######## checking for local tools ###
AC_PROG_CC
AC_SEARCH_LIBS([strerror],[cposix])
m4_warn([obsolete],
[The preprocessor macro `STDC_HEADERS' is obsolete.
  Except in unusual embedded environments, you can safely include all
  ISO C90 headers unconditionally.])dnl
# Autoupdate added the next two lines to ensure that your configure
# script's behavior did not change.  They are probably safe to remove.
AC_CHECK_INCLUDES_DEFAULT
AC_PROG_EGREP

AM_PATH_GNAT
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_RANLIB
AC_PROG_AWK(AWK)
AC_CHECK_PROGS(DVIPS, dvips)
AC_CHECK_PROGS(MAKEINFO, makeinfo)
AC_CHECK_PROGS(TEXI2DVI, texi2dvi)

######### checking for gtk
AM_PATH_GTK
AM_PATH_GTKADA(17.0,,AC_MSG_ERROR(Test for GtkAda failed. See the file 'INSTALL' for help.))
AM_TO_GPR($GTK_GCC_FLAGS,   GTK_GCC_FLAGS_GPR)

######### checking for python
AM_PATH_PYTHON
AX_PYTHON_DEVEL
AM_TO_GPR($PYTHON_CPPFLAGS, PYTHON_CFLAGS_GPR)
AM_TO_GPR("$PYTHON_LIBS $PYTHON_EXTRA_LIBS $PYTHON_EXTRA_LDFLAGS", PYTHON_SHARED_LIBS_GPR)

######### checking for pygobject
AM_PATH_PYGOBJECT
AM_TO_GPR($PYGOBJECT_INCLUDE, PYGOBJECT_INCLUDE_GPR)

######## GNU regex checking
AC_CHECK_FUNC(re_compile_pattern, AC_DEFINE(HAVE_GNU_REGEX))

######## Support for ptys ###
AC_CHECK_HEADERS(sys/stropts.h)

AC_CHECK_LIB(util,openpty,
 [AC_DEFINE(HAVE_OPENPTY) TARGET_LFLAGS=$TARGET_LFLAGS' & ("-lutil")'],
 [AC_CHECK_LIB(bsd,openpty,
  [AC_DEFINE(HAVE_OPENPTY) TARGET_LFLAGS=$TARGET_LFLAGS' & ("-lbsd")'],
  [AC_CHECK_FUNCS(openpty)]
 ])
)

case "${host}" in

  # ARM64 processors
  aarch64-*-* )
    case "${host}" in
      *-linux-gnu* )
        EXEC_COMMAND="xterm -hold -e"
        PRINT_CMD="a2ps" ;;
    esac
  ;;

  ## Alpha (DEC) machines.
  alpha*-dec-osf* ) EXECUTION_WINDOW=False ;;

  ## IBM, AIX machines
  powerpc-ibm-aix5.*  ) TARGET_LFLAGS=$TARGET_LFLAGS' & ("-Wl,-bbigtoc")' ;;

  ## PowerPC
  powerpc-*-linux-gnu )
    PRINT_CMD="a2ps"
    EXEC_COMMAND="xterm -hold -e"
    ;;

  powerpc-*-darwin* ) STRIP_CR=True ;;

  ## Suns
  *-sun-solaris* | i[[3456]]86-*-solaris2* | i[3456]86-*-sunos5*)
    TARGET_LFLAGS=$TARGET_LFLAGS' & ("-lXrender")' ;;

  # IA64 processors
  ia64-*-* )
    case "${host}" in
      *-linux-gnu* )
        EXEC_COMMAND="xterm -hold -e"
        PRINT_CMD="a2ps" ;;
    esac
  ;;

  ## Intel machines where we don't care about the manufacturer
  i[[3456]]86-*-* | x86_64-*-* )
    machine=intel386
    case "${host}" in
      *-linux-gnu* )
        EXEC_COMMAND="xterm -hold -e"
        PRINT_CMD="a2ps"
        ;;
      *win32* | *mingw32* | *cygwin* )
	STRIP_CR=True
	CAN_OUTPUT=False
	TARGET_LFLAGS='("-luser32", "-lcomdlg32", "-Wl,--stack=0x5000000")'
	COLOR_SELECTION="#BEBEBE"
	EXEC_COMMAND="cmd /c start gps_run.cmd"
	DEFAULT_PS="winps"
	PRINT_CMD=""
	DIFF_CMD="gnudiff"
	DIFF3_CMD="gnudiff3"
	PATCH_CMD="gnupatch --binary"
	OPAQUE_MDI=True
	HOST_TYPE=Windows
	PATH_STYLE=CYGWIN
	CHARSET="ISO-8859-1"
	;;

      *-darwin* )
	STRIP_CR=True
	;;
      # Otherwise we'll fall through to the generic opsys code at the bottom.
    esac
  ;;

  *) echo "Unknown system: ${host}"
     exit;;
esac

# Set the version number of GNAT Studio
AC_SUBST(GPS_VERSION)
AC_SUBST(GPS_DATE)

AC_SUBST(YEAR)

AC_SUBST(GNATCOLL_INSTALL)

AC_SUBST(TARGET)
AC_SUBST(EXEC_PREFIX)

AC_SUBST(STRIP_CR)
AC_SUBST(CAN_OUTPUT)
AC_SUBST(TARGET_CFLAGS)
AC_SUBST(TARGET_LFLAGS)
AC_SUBST(TARGET)
AC_SUBST(TARGET_IS_DARWIN)
AC_SUBST(CURRENT_YEAR)
AC_SUBST(EXEC_COMMAND)
AC_SUBST(DEFAULT_PS)
AC_SUBST(PRINT_CMD)
AC_SUBST(DIFF_CMD)
AC_SUBST(DIFF3_CMD)
AC_SUBST(PATCH_CMD)
AC_SUBST(OPAQUE_MDI)
AC_SUBST(HOST_TYPE)
AC_SUBST(EXECUTION_WINDOW)
AC_SUBST(CHARSET)
AC_SUBST(AWK)


AC_MSG_NOTICE([------------- Summary for GNAT Studio --------------------])
AC_MSG_NOTICE([ GNATcoll: $GNATCOLL_INSTALL])
AC_MSG_NOTICE([ Xml/Ada: $XMLADA_GPR])
AC_MSG_NOTICE([    use ADA_PROJECT_PATH to find xmlada.gpr])
AC_MSG_NOTICE([--------------------------------------------------])

AC_CONFIG_FILES([shared.gpr Makefile docs/Makefile common/core/src/config.ads common/common.gpr osx_bundle/Makefile osx_bundle/scripts/install.py docs/gnatstudio-welcome.html docs/gnatstudio-welcome-public.html])
AC_OUTPUT
