# This test check the presence of default.gpr in multiple directory
if [ "$OS" = "Windows_NT" ]; then
   # In windows ; is both the path and command delimiters, escape it
   # with "
   export GNATSTUDIO_CUSTOM_PATH="nothing_here;custom"
else
   export GNATSTUDIO_CUSTOM_PATH=nothing_here:custom
fi
$GPS --load=python:test.py
