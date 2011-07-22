all: libgdbmi

.PHONY: libgdbmi
.PHONY: tests

libgdbmi: libgdbmi.gpr tests/mi_tests.gpr
	gprbuild -p libgdbmi.gpr
	gprbuild -p tests/mi_tests.gpr

clean: libgdbmi.gpr tests/mi_tests.gpr
	gnatclean -P libgdbmi.gpr
	gnatclean -P tests/mi_tests.gpr

distclean: clean
	rm -rf obj/ lib/ tests/obj/

fast-tests: libgdbmi
	python -B tests/run_tests.py -v

full-tests: libgdbmi
	python -B tests/run_tests.py -v --with-valgrind

parser-fast-tests: libgdbmi
	python -B tests/run_tests.py -v --parser

parser-full-tests: libgdbmi
	python -B tests/run_tests.py -v --parser --with-valgrind

utils-fast-tests: libgdbmi
	python -B tests/run_tests.py -v --utils

utils-full-tests: libgdbmi
	python -B tests/run_tests.py -v --utils --with-valgrind
