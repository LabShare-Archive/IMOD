#
# Makefile for flib man pages
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#
include ../configure

THERE = ../../manpages

install : $(THERE)/mantohtml
	./makedist $(THERE)
	$(CP) *.1 $(INSTDIR)/man/cat1
	\mv *.html $(INSTDIR)/html/man

$(THERE)/mantohtml : ALWAYS
	(cd $(THERE) ; $(CC) $(CFLAGS) -o mantohtml mantohtml.c)

clean :
	$(FIND) . -type f -name "*.html"   -exec rm "{}" \;
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;

ALWAYS :

