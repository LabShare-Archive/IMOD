#
# Makefile for autodoc directory: installs all files with .adoc
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
include ../configure

install :
	if [ `pwd` != $(INSTDIR)/autodoc ] ; then $(CP) *.adoc $(INSTDIR)/autodoc ; fi

clean : 
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
