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
#  Revision 3.3  2004/11/22 05:31:37  mast
#  Add 3dmodHelp
#
#  Revision 3.2  2003/11/08 03:43:50  mast
#  *** empty log message ***
#
#  Revision 3.1  2003/05/08 19:06:27  mast
#  Added -r flag to copy, added 3dmodimages directory to NOEXT
#
#  Revision 3.0  2001/11/29 18:10:45  rickg
#  *** empty log message ***
#
#  Revision 1.1  2001/11/28 15:39:48  mast
#  Added Makefile so that make install will install these files
#
#
include ../configure

NOEXT = 3dmodimages etomoImages 3dmodHelp

install : clean
	if [ `pwd` != $(INSTDIR)/html ] ; then $(CP) -r *.* $(NOEXT) $(INSTDIR)/html ; fi

clean : 
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
