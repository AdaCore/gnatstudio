"sinclude(../config/acinclude.m4)"
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

