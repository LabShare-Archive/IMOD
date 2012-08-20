#
# Makefile for flib man pages
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
include ../../configure
O        = $(OBJEXT)

THERE = ../../manpages

install : $(THERE)/mantohtml $(THERE)/autodoc2man
	./makedist $(THERE)
	$(CP) *.1 $(INSTDIR)/man/cat1
	\mv *.html ../../html/man
	./makedist clean

$(THERE)/mantohtml : 
	(cd $(THERE) ; $(CC) $(CFLAGS) -o mantohtml mantohtml.c)

$(THERE)/autodoc2man : $(THERE)/autodoc2man.$(O) ../../libcfshr/parse_params.$(O)
	(cd $(THERE) ; $(CC) $(CFLAGS) -o autodoc2man autodoc2man.$(O) \
	../libcfshr/parse_params.$(O) $(CLIBS) )

clean :
	$(FIND) . -type f -name "*.html"   -exec rm "{}" \;
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
	./makedist clean

ALWAYS :

