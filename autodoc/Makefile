#
# Makefile for autodoc directory: installs all files with .adoc
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#
#
include ../configure

install :
	if (`pwd` != $(INSTDIR)/autodoc) $(CP) *.adoc $(INSTDIR)/autodoc

clean : 
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
