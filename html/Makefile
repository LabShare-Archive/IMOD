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
#  Revision 1.1  2001/11/28 15:39:48  mast
#  Added Makefile so that make install will install these files
#
#
include ../configure

NOEXT = 

install : clean
	if (`pwd` != $(INSTDIR)/html) $(CP) *.* $(NOEXT) $(INSTDIR)/html

clean : 
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
