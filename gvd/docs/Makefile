all: gvd.texi
	echo x | tex gvd.texi
	texindex gvd.cp
	tex gvd.texi
	dvips gvd.dvi -o gvd.ps
	perl texi2html -menu -monolithic gvd.texi
	makeinfo --force -o gvd.info gvd.texi
	makeinfo --force --no-headers --no-split -o gvd.txt gvd.texi
	#-$(RM) *.aux *.cp *.cps *.dvi *.fn *.ky *.log *.pg *.toc *.tp *.vr

clean:
	-$(RM) *.aux *.cp *.cps *.dvi *.fn *.ky *.log *.pg *.toc *.tp *.vr
	-$(RM) gvd.html gvd.info* gvd.ps gvd.txt

