"sinclude(snsrc/db/acinclude.m4)"

dnl ---------------------------------------------------------------------------
dnl Verify that a test program compiles and runs with GNAT
dnl $cf_ada_make is set to the program that compiles/links
AC_DEFUN([CF_GNAT_TRY_RUN],
[
rm -f conftest*
cat >>conftest.ads <<CF_EOF
$1
CF_EOF
cat >>conftest.adb <<CF_EOF
$2
CF_EOF
if ( $cf_ada_make conftest 1>&AC_FD_CC 2>&1 ) ; then
   if ( ./conftest 1>&AC_FD_CC 2>&1 ) ; then
ifelse($3,,      :,[      $3])
ifelse($4,,,[   else
      $4])
   fi
ifelse($4,,,[else
   $4])
fi
rm -f conftest*
])dnl
dnl ---------------------------------------------------------------------------
dnl Verify Version of GNAT.
AC_DEFUN([CF_GNAT_VERSION],
[
changequote(<<, >>)dnl
cf_cv_gnat_version=`$cf_ada_make -v 2>&1 | grep '[0-9].[0-9][0-9]*' |\
  sed -e 's/[^0-9 \.]//g' | $AWK '{print $<<1>>;}'`
case $cf_cv_gnat_version in
  3.1[1-9]*|3.[2-9]*|[4-9].*)
    cf_cv_prog_gnat_correct=yes
    ;;
  *) echo Unsupported GNAT version $cf_cv_gnat_version. Required is 3.11 or better. Disabling Ada95 binding.
     cf_cv_prog_gnat_correct=no
     ;;
esac
case $cf_cv_gnat_version in
  3.1*|[4-9].*)
      cf_compile_generics=generics
      cf_generic_objects="\$(GENOBJS)"
      ;;
  *)  cf_compile_generics=
      cf_generic_objects=
      ;;
esac
changequote([, ])dnl
])


