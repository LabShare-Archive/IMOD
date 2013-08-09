#
# Makefile for IMOD man pages in nroff format
#
#  $Id$
#
#
include ../configure

O        = $(OBJEXT)
HERE = .

install : mantohtml autodoc2man
	./convert $(HERE)
	if `grep -q '\"' ../com/directives.csv` ; then echo "QUOTES IN directives.csv" ; exit 1 ; fi
	./csvtohtml -m 7 ../com/directives.csv ../html/directives.html directives.stub
	\mv *.1 $(INSTDIR)/man/cat1
	\mv *.html ../html/man
#	echo 'User-agent: *' > $(INSTDIR)/html/man/robots.txt
#	echo 'Disallow: /' >> $(INSTDIR)/html/man/robots.txt

autodoc2man : autodoc2man.$(O) ../libcfshr/parse_params.$(O)
	$(CC) $(CFLAGS) -o autodoc2man autodoc2man.$(O) \
	../libcfshr/parse_params.$(O) $(CLIBS)

mantohtml : mantohtml.$(O)
	$(CC) $(CFLAGS) -o mantohtml mantohtml.$(O) $(CLIBS)

mantohtml.$(O) : proglist.h

clean :
	$(FIND) . -type f -name "*.1"   -exec rm "{}" \;
	$(FIND) . -type f -name "*.html"   -exec rm "{}" \;
	$(FIND) . -type f -name "*.$(OBJEXT)"   -exec rm "{}" \;
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "*exe"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
	$(FIND) . -type f -name "mantohtml"   -exec rm -f "{}" \;
	$(FIND) . -type f -name "autodoc2man"   -exec rm -f "{}" \;
	./convert clean
