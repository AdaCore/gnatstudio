
.SILENT:

force:

gen: force
	-mkdir tsrc
	echo Generate specs
	-rm -f gen/templates.cds
	(cd tmplt && templates2ada -d . -e .thtml -e .txml \
		-o ../gen/templates.cds \
		-t ../gen/templates.tada)
	gnat chop -wpq gen/templates.cds tsrc/
	-rm -f gen/templates.cds
