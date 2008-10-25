
# /home/charlet/gps/demo/sdc_c/demo/Makefile.sdc

ifeq ($(SDC.project),)
SDC.project=True

ifeq ($(BASE_DIR),)
   SDC.root=True
   SDC.base_dir:=$(shell gprcmd pwd)
   ifeq ($(MAKE_ROOT),)
      $(error MAKE_ROOT variable is undefined, Makefile.prolog cannot be loaded)
   else
      include $(MAKE_ROOT)/share/make/Makefile.prolog
   endif
   OBJ_EXT:=.o
else
   SDC.root=False
   SDC.base_dir:=$(BASE_DIR)
endif

SDC.src_dirs:=$(SDC.base_dir)
SDC.obj_dir:=$(SDC.base_dir)
PROJECT_FILE:=sdc
# project Sdc

# external references

# external ("Build", "DEBUG")
ifeq ($(Build),)
   SDC.external.1:=DEBUG
else
   SDC.external.1:=$(Build)
endif
# end of external references


# for Languages use ...
SDC.languages:=Ada C
LANGUAGES:=ada c
# for Source_Dirs use ...
SDC.source_dirs:=common struct matrix_handling
SDC.src_dirs:=$(shell gprcmd extend $(SDC.base_dir) '$(SDC.source_dirs)')
# for Main use ...
SDC.main:=sdc.adb
ADA_MAINS:=$(SDC.main)
# Build := ...
SDC.BUILD:=$(SDC.external.1)
# for Object_Dir use ...
SDC.object_dir:=obj
SDC.obj_dir:=$(strip $(shell gprcmd to_absolute $(SDC.base_dir) $(SDC.object_dir)))

# package Compiler is ...

# case Sdc.Build is ...
# when "DEBUG" => ...
ifeq ($(SDC.BUILD),DEBUG)
# for Default_Switches ("c") use ...
SDC.compiler.default_switches.C:=-g
CFLAGS:=$(SDC.compiler.default_switches.C)
else
# when "PRODUCTION" => ...
ifeq ($(SDC.BUILD),PRODUCTION)
# for Default_Switches ("c") use ...
SDC.compiler.default_switches.C:=-O2
CFLAGS:=$(SDC.compiler.default_switches.C)
endif
endif
# end case;

# end Compiler;


# package Ide is ...
# for Compiler_Command ("c") use ...
SDC.ide.compiler_command.C:=gcc
CC:=$(SDC.ide.compiler_command.C)
# end Ide;


SRC_DIRS:=$(SRC_DIRS) $(SDC.src_dirs)
OBJ_DIR:=$(SDC.obj_dir)

# get the source files
SDC.src_files:= $(foreach name,$(SDC.src_dirs),$(notdir $(wildcard $(name)/*)))

# get the C sources
C_SRCS:=$(filter %$(C_EXT),$(SDC.src_files))

# no C++ sources
CXX_SRCS=

# if there are C sources, add the library
ifneq ($(strip $(C_SRCS)),)
   LIBS:=$(SDC.obj_dir)/libsdc$(AR_EXT) $(LIBS)
endif

ifeq ($(SDC.root),True)
   include $(MAKE_ROOT)/share/make/Makefile.generic
else
   DEPS_PROJECTS:=$(strip $(DEPS_PROJECTS) $(SDC.base_dir)/sdc)
endif

endif

