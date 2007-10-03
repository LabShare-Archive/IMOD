#
# Makefile for html directory: installs all files with extensions; add other
# files and directories to the NOEXT list
#
# $Id$
# Log at end
#
include ../configure

NOEXT = 3dmodimages etomoImages 3dmodHelp libhelp joinImages ctfHelp midasHelp

install : clean
	cd libdoc ; $(MAKE)
	./makeadp $(INSTDIR)
	if [ `pwd` != $(INSTDIR)/html ] ; then $(CP) -r *.* $(NOEXT) $(INSTDIR)/html ; fi

clean : 
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;

#  $Log$
#  Revision 3.12  2007/09/20 20:31:27  mast
#  *** empty log message ***
#
#  Revision 3.11  2006/06/20 22:08:08  mast
#  Added joinImages
#
#  Revision 3.10  2006/06/19 14:11:48  mast
#  Passed install dir to makeadp
#
#  Revision 3.9  2006/06/18 23:43:55  mast
#  Added makeadp
#
#  Revision 3.8  2005/07/01 14:09:18  mast
#  Do not clean up libhelp - it is an install directory
#
#  Revision 3.7  2005/03/20 20:43:35  mast
#  Clean libhelp here not from libdoc
#
#  Revision 3.6  2005/03/20 19:45:07  mast
#  Added libhelp directory to distribution
#
#  Revision 3.5  2005/02/25 04:26:24  mast
#  Added libdoc calls
#
#  Revision 3.4  2005/01/17 19:33:44  mast
#  Switched to sh format of if
#
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
