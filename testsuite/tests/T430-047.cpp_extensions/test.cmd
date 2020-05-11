export CPLUS_INCLUDE_PATH=`pwd`/include_fake_boost
$GPS -Pp --load=python:test.py
$GPS -Pp --load=python:test.py --traceon=GPS.LSP.CPP_SUPPORT
