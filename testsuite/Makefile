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

all default:
	gprbuild -P testsuite_drivers.gpr -m -XOS=${OS} -j0

clean:
	gprclean -P testsuite_drivers.gpr
