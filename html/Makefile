#
# Makefile for html directory: installs all files with extensions; add other
# files to the NOEXT list
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

NOEXT = 

install : clean
	if (`pwd` != $(INSTDIR)/html) $(CP) *.* $(NOEXT) $(INSTDIR)/html

clean : 
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
