if [ "$OS" != "Windows_NT" ]; then
   export PATH=`pwd`:$PATH
   echo `pwd`/ > home.txt
   $GPS -Pdefault --load=python:test.py
   diff -u home.txt args.txt
fi

