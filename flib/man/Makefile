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
#  Revision 3.1  2003/04/29 05:25:36  mast
#  Include configure from top level so fresh checkout can make src
#
#  Revision 3.0  2001/11/29 18:10:17  rickg
#  *** empty log message ***
#
#  Revision 1.2  2001/11/28 15:43:20  mast
#  Changed to do install in conventional manner
#
#
include ../../configure

THERE = ../../manpages

install : $(THERE)/mantohtml $(THERE)/autodoc2man
	./makedist $(THERE)
	$(CP) *.1 $(INSTDIR)/man/cat1
	\mv *.html $(INSTDIR)/html/man
	./makedist clean

$(THERE)/mantohtml : 
	(cd $(THERE) ; $(CC) $(CFLAGS) -o mantohtml mantohtml.c)

$(THERE)/autodoc2man : $(THERE)/autodoc2man.o ../../libimod/parse_params.o
	(cd $(THERE) ; $(CC) $(CFLAGS) -o autodoc2man autodoc2man.o \
	../libimod/parse_params.o $(CLIBS) )

clean :
	$(FIND) . -type f -name "*.html"   -exec rm "{}" \;
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
	./makedist clean

ALWAYS :

