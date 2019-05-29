cp -f main.adb.orig main.adb
$GPS --load=python:test.py --traceon=GPS.LSP.ADA_SUPPORT --traceon=GPS.LSP.CPP_SUPPORT --traceon=GPS.LSP.ADA_SUPPORT.DIAGNOSTICS
rm -f main.adb main.ads
