export GPS_CLANGD=`which clangd-9`
$GPS --load=python:test.py --traceon=GPS.LSP.COMPLETION --traceon=GPS.LSP.CPP_SUPPORT
