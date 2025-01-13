# This test check the presence of default.gpr in multiple directory
export GNATSTUDIO_CUSTOM_PATH=nothing_here:custom
$GPS --load=python:test.py
