rm -rf src
mkdir -p src
$GPS -Pdefault --load=python:test.py --traceon=GPS.LSP.GPR_SUPPORT
