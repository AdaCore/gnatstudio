GPRBUILD=gprbuild
GPRCLEAN=gprclean

SYS := $(shell gcc -dumpmachine)
ifeq ($(OS),Windows_NT)
        OS := Windows_NT
else
        ifneq (, $(findstring darwin, $(SYS)))
OS := osx
else
        OS := unix
endif
endif

ifeq ($(BUILD),Production)
  GPRBUILD_BUILD_TYPE_FLAGS=-XBUILD=Production -XOS=${OS} -XLIBRARY_TYPE=relocatable -XXMLADA_BUILD=relocatable
else
  GPRBUILD_BUILD_TYPE_FLAGS=-XBUILD=Debug -XOS=${OS} -XLIBRARY_TYPE=relocatable -XXMLADA_BUILD=relocatable
endif

all default: package
	$(GPRBUILD) -P testsuite_drivers.gpr -m -j0 $(GPRBUILD_BUILD_TYPE_FLAGS)

package:
	mkdir -p share/doc
	cp ../share/predefined_ada.xml share
	cp ../docs/users_guide/GPS.rst share/doc
	cp ../docs/users_guide/GPS.Browsers.rst share/doc
	cp ../docs/users_guide/generate.py share/doc
clean:
	$(GPRCLEAN) -P testsuite_drivers.gpr
	rm -rf share
