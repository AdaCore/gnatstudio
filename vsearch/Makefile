include ../Makefile.common

PROJECT_FILE = vsearch.gpr

default: ada

test: force
	${MAKE} PROJECT_FILE=vsearch_test.gpr ADA_SOURCES=run.adb build

include $(ROOT)/builder/src/Makefile.glide

