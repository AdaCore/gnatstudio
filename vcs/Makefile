all build compile link ada c c++ clean internal-clean :
	$(MAKE) -s -f Makefile.vcs $@

shared:
	cd obj; gnat bind -E -P../vcs -C -Lvcs_module_ -o vcs_init.c \
	  vcs.ali vcs_*.ali commands-*.ali vcs-unknown*.ali log_utils.ali
	cd obj; gcc -c -fPIC vcs_init.c
	cd obj; gcc -shared -o libvcs_module.so \
	  vcs.o vcs_*.o vcs-unknown*.o commands*.o log_utils.o pixmaps_vcs.o

	cd obj; gnat bind -E -P../vcs -C -Lvcs__cvs_ -o cvs_init.c \
	  vcs-cvs*.ali
	cd obj; gcc -c -fPIC cvs_init.c
	cd obj; gcc -shared -o libvcs_cvs.so cvs_init.o vcs-cvs*.o

	cd obj; gnat bind -E -P../vcs -C -Lvcs__clearcase_ \
	  -o clearcase_init.c vcs-clearcase*.ali
	cd obj; gcc -c -fPIC clearcase_init.c
	cd obj; gcc -shared -o libvcs_clearcase.so clearcase_init.o \
	  vcs-clearcase*.o

