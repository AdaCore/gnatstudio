all: libgmop tests

.PHONY: libgmop
.PHONY: tests

libgmop: libgmop.gpr tests/gmop.gpr
	gprbuild -p libgmop.gpr
	gprbuild -p tests/gmop.gpr

clean: libgmop.gpr tests/gmop.gpr
	gnatclean -P libgmop.gpr
	gnatclean -P tests/gmop.gpr

distclean: clean
	rm -rf obj/ lib/ tests/obj/

tests: libgmop
	python -B tests/run_tests.py -v
