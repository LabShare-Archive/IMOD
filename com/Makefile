#
# Makefile for com directory: installs all files with extensions; add other
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
	if (`pwd` != $(INSTDIR)/com) $(CP) *.* $(NOEXT) $(INSTDIR)/com

clean : 
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
