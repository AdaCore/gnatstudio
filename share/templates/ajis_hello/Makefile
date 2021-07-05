ifeq ($(OS),Windows_NT)
	BUILD_CLASSPATH="./src_java/;./binding/java;$$CLASSPATH"
	RUN_LD_LIBRARY_PATH=$(LD_LIBRARY_PATH)
	RUN_CLASSPATH="bin;$$CLASSPATH"
else
	BUILD_CLASSPATH="./src_java/:./binding/java:$$CLASSPATH"
	GNATSTUDIO_BIN=$(shell (which gnatstudio))
	GNATSTUDIO_LIB=$(shell (dirname $(GNATSTUDIO_BIN)))/../lib/gnatstudio
	RUN_LD_LIBRARY_PATH=$(GNATSTUDIO_LIB):binding/ada/lib:$(LD_LIBRARY_PATH)
	RUN_CLASSPATH="bin:$$CLASSPATH"
endif


all: gen_binding build_ada build_java run_java

gen_binding:
	ada2java -P @_project_name_@.gpr src_ada/hello.ads -o binding/ada -c binding/java -L @_project_name_@_lib -b com.adacore.example

build_ada:
	gprbuild -p -P binding/ada/@_project_name_@_lib.gpr

build_java:
	CLASSPATH=$(BUILD_CLASSPATH) \
	javac src_java/com/adacore/example/@_Main_Name_@.java -d bin

run_java:
	CLASSPATH=$(RUN_CLASSPATH) \
	LD_LIBRARY_PATH=$(RUN_LD_LIBRARY_PATH) \
	PATH="binding/ada/lib:$$PATH" \
	java com.adacore.example.@_Main_Name_@

