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
#  Revision 1.1  2003/10/08 17:18:12  mast
#  Initial creation
#
#
#
include ../configure

install :
	if [ `pwd` != $(INSTDIR)/autodoc ] ; then $(CP) *.adoc $(INSTDIR)/autodoc ; fi

clean : 
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
