SHELL=/bin/sh

harness_dir = make_harness
suite_dir = make_suite
test_dir = make_test
utils_dir = utils

exe_dir = .

harness_exe = aunit_make_harness
suite_exe = aunit_make_suite 
test_exe = aunit_make_test 

MV = mv

all:
	${MAKE} -C $(harness_dir)
	${MAKE} -C $(suite_dir)
	${MAKE} -C $(test_dir)
	-${MV} ${harness_dir}/${harness_exe} ${exe_dir}
	-${MV} ${suite_dir}/${suite_exe} ${exe_dir}
	-${MV} ${test_dir}/${test_exe} ${exe_dir}

clean-generic:
	-${RM} core
	-${RM} .\#*

clean: clean-generic
	-${RM} ${harness_exe}
	-${RM} ${suite_exe}
	-${RM} ${test_exe}
	-${MAKE} -C $(harness_dir) clean
	-${MAKE} -C $(suite_dir) clean
	-${MAKE} -C $(test_dir) clean
	-${MAKE} -C $(utils_dir) clean
