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
