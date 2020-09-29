#!/usr/bin/env sh

# This is a convenience command-line driver made for development mode
# in valgrind memory check mode
./run-tests --noxvfb  --valgrind_memcheck --loglevel INFO --show-error-output $@
