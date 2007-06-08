fail=0
verbose=0

if [ x$VALGRIND = xyes ]; then
   valgrind="valgrind --tool=memcheck --leak-check=full --num-callers=30 --show-reachable=no --leak-resolution=high --suppressions=valgrind.supp -q --suppressions=valgrind-python.supp --gen-suppressions=no"
   verbose=1
else
   valgrind=""
fi

dotest() {
   lang=$1
   file=$2
   expected=$3
   if test $verbose = 1; then
      echo "==== ./testsuite $lang $file"
   fi
   $valgrind ./testsuite $lang $file > out.$$
   if diff $expected out.$$ > /dev/null; then
      :
   else
      if test $verbose != 1; then
         echo "==== ./testsuite $lang $file"
      fi
      diff $expected out.$$
      fail=1
   fi
   rm -f out.$$
}

./testapi > out.$$
diff testapi.out out.$$

dotest python test1.py  test1.out
dotest shell  test2.gsh test2.out
dotest python test3.py  test3.out

if test $fail = 0; then
   echo "SUCCESS"
fi
