ifeq ($(BUILDER_PROJECT),)
BUILDER_PROJECT=True

ifeq ($(BASE_DIR),)
   include ../Makefile.common
   BASE_DIR=$(shell pwd)
   BUILDER_ROOT=True
endif

BUILDER_BASE_DIR := $(BASE_DIR)
BASE_DIR=$(BUILDER_BASE_DIR)/../kernel
include $(BASE_DIR)/Makefile.kernel
BASE_DIR=$(BUILDER_BASE_DIR)/../gtkada
include $(BASE_DIR)/Makefile.gtkada

BUILDER_SRC_DIRS = $(BUILDER_BASE_DIR)/src
SRC_DIRS += $(BUILDER_SRC_DIRS)
C_SRCS = \
  $(foreach name,$(BUILDER_SRC_DIRS),$(notdir $(wildcard $(name)/*$(C_EXT))))
BUILDER_OBJ_DIR = $(BUILDER_BASE_DIR)/obj
OBJ_DIR = $(BUILDER_OBJ_DIR)

CFLAGS = -g -O2

PROJECT_FILE = builder
LIBS := $(BUILDER_OBJ_DIR)/libbuilder$(AR_EXT) $(LIBS)

ifeq ($(BUILDER_ROOT),True)
   include ../builder/src/Makefile.glide
else
   DEPS_PROJECTS += $(BUILDER_BASE_DIR)/builder
endif

endif
