all build compile link ada c c++ clean internal-clean :
	$(MAKE) -s -f Makefile.vfs $@

shared:
	cd obj; gnat bind -E -P../vfs -C -Lvfs_module_ -o vfs_init.c *.ali
	cd obj; gcc -c -fPIC vfs_init.c
	cd obj; gcc -shared -o libvfs_module.so *.o

