include ../Makefile.common

PROJECT_FILE = kernel.gpr
ADA_SOURCES = kernel.ads 

default: compile

test: force
	$(MAKE) PROJECT_FILE=kernel_test.gpr ADA_SOURCES=read_ali.adb build

include $(ROOT)/builder/src/Makefile.glide

