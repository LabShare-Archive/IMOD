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
#  Revision 1.1  2001/11/28 15:40:53  mast
#  Added Makefile so the make install will install these files also.
#
#
include ../configure

NOEXT = 

install : clean
	if (`pwd` != $(INSTDIR)/com) $(CP) *.* $(NOEXT) $(INSTDIR)/com

clean : 
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
