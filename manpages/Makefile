#
# Makefile for IMOD man pages in nroff format
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#  Revision 1.2  2001/11/28 15:45:42  mast
#  Changed to do install in conventional manner
#
#
include ../configure

HERE = .

install : mantohtml
	./convert $(HERE)
	\mv *.1 $(INSTDIR)/man/cat1
	$(CP) *.5 $(INSTDIR)/man/cat5
	\mv *.html $(INSTDIR)/html/man

clean :
	$(FIND) . -type f -name "*.1"   -exec rm "{}" \;
	$(FIND) . -type f -name "*.html"   -exec rm "{}" \;
	$(FIND) . -type f -name "*.o"   -exec rm "{}" \;
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
	$(FIND) . -type f -name "mantohtml"   -exec rm "{}" \;


