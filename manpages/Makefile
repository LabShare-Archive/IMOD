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
#  Revision 3.1  2003/10/08 17:16:15  mast
#  changes to work with autodoc files
#
#  Revision 3.0  2001/11/29 18:11:00  rickg
#  *** empty log message ***
#
#  Revision 1.3  2001/11/29 00:43:53  mast
#  changed $(PWD) to .
#
#  Revision 1.2  2001/11/28 15:45:42  mast
#  Changed to do install in conventional manner
#
#
include ../configure

O        = $(OBJEXT)
HERE = .

install : mantohtml autodoc2man
	./convert $(HERE)
	\mv *.1 $(INSTDIR)/man/cat1
	$(CP) *.5 $(INSTDIR)/man/cat5
	\mv *.html $(INSTDIR)/html/man

autodoc2man : autodoc2man.$(O) ../libimod/parse_params.$(O)
	$(CC) $(CFLAGS) -o autodoc2man autodoc2man.$(O) \
	../libimod/parse_params.$(O) $(CLIBS)

mantohtml : mantohtml.$(O)
	$(CC) $(CFLAGS) -o mantohtml mantohtml.$(O) $(CLIBS)


clean :
	$(FIND) . -type f -name "*.1"   -exec rm "{}" \;
	$(FIND) . -type f -name "*.html"   -exec rm "{}" \;
	$(FIND) . -type f -name "*.o*"   -exec rm "{}" \;
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "*exe"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
	$(FIND) . -type f -name "mantohtml"   -exec rm -f "{}" \;
	$(FIND) . -type f -name "autodoc2man"   -exec rm -f "{}" \;
	./convert clean

