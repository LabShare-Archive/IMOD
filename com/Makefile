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
include ../configure

NOEXT = 

install : clean
	if [ `pwd` != $(INSTDIR)/com ] ; then $(CP) *.* $(NOEXT) $(INSTDIR)/com ; fi

clean : 
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
