#
# Makefile for autodoc directory: installs all files with .adoc
#
# $Id$
#
include ../configure

install :
	if [ `pwd` != $(INSTDIR)/autodoc ] ; then $(CP) *.adoc $(INSTDIR)/autodoc ; fi
	$(CP) ../dist/*.adoc $(INSTDIR)/SystemTemplate

clean : 
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
