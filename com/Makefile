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
#  Revision 3.0  2001/11/29 18:10:07  rickg
#  *** empty log message ***
#
#  Revision 1.1  2001/11/28 15:40:53  mast
#  Added Makefile so the make install will install these files also.
#
#
include ../configure

NOEXT = 

install : clean
	if [ `pwd` != $(INSTDIR)/com ] ; then $(CP) *.* $(NOEXT) $(INSTDIR)/com ; fi

clean : 
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
