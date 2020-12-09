cp ./orig/* ./
$GPS -Phello --load=python:test.py
diff -c main.adb main.adb.res
diff -c a.ads a.ads.res
diff -c a.adb a.adb.res
