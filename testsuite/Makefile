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

all default: package
	$(GPRBUILD) -P testsuite_drivers.gpr -m -XOS=${OS} -j0 -XLIBRARY_TYPE=relocatable -XXMLADA_BUILD=relocatable

package:
	mkdir -p share/doc
	cp ../share/predefined_ada.xml share
	cp ../docs/users_guide/GPS.rst share/doc
	cp ../docs/users_guide/GPS.Browsers.rst share/doc
	cp ../docs/users_guide/generate.py share/doc
clean:
	$(GPRCLEAN) -P testsuite_drivers.gpr
	rm -rf share
