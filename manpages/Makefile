#
# Makefile for IMOD man pages in nroff format
#
#  $Id$
#
#  Log at end of file
#
include ../configure

O        = $(OBJEXT)
HERE = .

install : mantohtml autodoc2man
	./convert $(HERE)
	\mv *.1 $(INSTDIR)/man/cat1
	\mv *.html $(INSTDIR)/html/man
	echo 'User-agent: *' > $(INSTDIR)/html/man/robots.txt
	echo 'Disallow: /' >> $(INSTDIR)/html/man/robots.txt

autodoc2man : autodoc2man.$(O) ../libcfshr/parse_params.$(O)
	$(CC) $(CFLAGS) -o autodoc2man autodoc2man.$(O) \
	../libcfshr/parse_params.$(O) $(CLIBS)

mantohtml : mantohtml.$(O)
	$(CC) $(CFLAGS) -o mantohtml mantohtml.$(O) $(CLIBS)

mantohtml.$(O) : proglist.h

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

#
#  $Log$
#  Revision 3.5  2007/09/20 14:18:37  mast
#  Changed location of pip
#
#  Revision 3.4  2007/02/20 19:27:58  mast
#  Added robots.txt file to prevent scanning to the html/man directory
#
#  Revision 3.3  2006/02/10 14:54:20  mast
#  Added dependency onproglist.h
#
#  Revision 3.2  2003/10/24 04:11:46  mast
#  Changes for Windows/Intel compiler
#
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
