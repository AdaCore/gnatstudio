include ../gtkada/src/Makefile.common

default build compile link ada c c++ clean internal-clean :
	$(MAKE) -s -f Makefile.gps $@

shared: compile
# prepare the objects for the shared library:
	for m in ../kernel .. ../common ../widgets ../gvd ../gvd/common ../vsearch; do \
	  mkdir -p $$m/obj_save; \
	  mv $$m/obj/*.o $$m/obj_save; \
	  chmod -w $$m/obj/*.ali; \
	  for l in `ls $$m/obj/lib*.a 2> /dev/null`; do \
	    for f in `ar t $$l`; do \
	      mv $$m/obj_save/$$f $$m/obj; \
	    done \
	  done \
	done

# restore gvd_module:
	mv ../gvd/obj_save/gvd_module*.o ../gvd/obj

# create libkernel.so that includes the following projects:
# kernel, gnat, common, widgets, gvd, gvd_common, vsearch

	cd ../kernel/obj; gcc -shared -o libkernel$(SO_EXT) ../obj_save/*.o \
	  ../../obj_save/*.o ../../common/obj_save/*.o \
	  ../../widgets/obj_save/*.o ../../gvd/obj_save/*.o \
	  ../../gvd/common/obj_save/*.o ../../vsearch/obj_save/*.o

# rebind/link gps with libkernel.so:
	-rm -f obj/gps
	$(MAKE) -s -f Makefile.gps default ADAFLAGS="-largs -lkernel"

# put back objects and ali files in their original state:
	for m in ../kernel .. ../common ../widgets ../gvd ../gvd/common ../vsearch; do \
	  mv $$m/obj_save/*.o $$m/obj; \
	  rmdir $$m/obj_save; \
	  chmod +w $$m/obj/*.ali; \
	done

