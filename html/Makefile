#
# Makefile for html directory: installs all files with extensions; add other
# files and directories to the NOEXT list
#
# $Id$
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

