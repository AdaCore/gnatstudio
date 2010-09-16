all_win32: gen_binding build_ada build_java_win32 run_java_win32

all_linux: gen_binding build_ada build_java_linux run_java_linux

gen_binding:
	ada2java -P ajis_hello.gpr src_ada/hello.ads -o binding/ada -c binding/java -L @_project_name_@_lib -b com.adacore.example

build_ada:
	gprbuild -p -P binding/ada/@_project_name_@_lib.gpr

build_java_win32:
	CLASSPATH="./src_java/;./binding/java;$$CLASSPATH" \
	javac src_java/com/adacore/example/@_Main_Name_@.java -d bin

build_java_linux:
	CLASSPATH="./src_java/:./binding/java:$$CLASSPATH" \
	javac src_java/com/adacore/example/@_Main_Name_@.java -d bin

run_java_win32:
	CLASSPATH="./bin;$$CLASSPATH" \
	PATH="binding/ada/lib:$$PATH" \
	java com.adacore.example.@_Main_Name_@

run_java_linux:
	CLASSPATH="./bin:$$CLASSPATH" \
	PATH="binding/ada/lib:$$PATH" \
	java com.adacore.example.@_Main_Same_@
