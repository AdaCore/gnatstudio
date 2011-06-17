all: libgdbmi tests

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

tests: libgdbmi
	python -B tests/run_tests.py -v
