#
# Makefile for html directory: Needs to be run after make in manpages and flib/man
# Updates library help, builds Qt compressed help and copies it to install dir

# $Id$
#
include ../configure

NOEXT = 3dmodimages etomoImages 3dmodHelp libhelp joinImages ctfHelp midasHelp plughelp \
	man

install : cleanjunk
	cd libdoc ; $(MAKE)
	./makeqhp
	chmod a+rx IMOD.qch IMOD.qhc
	if [ `pwd` != $(INSTDIR)/html ] ; then cp -f IMOD.qhc IMOD.qch $(INSTDIR)/html ; fi

cleanjunk : 
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
	$(FIND) . -type f -name "IMOD.qhp"   -exec rm "{}" \;

clean : cleanjunk
	$(FIND) plughelp -type f -name "*"   -exec rm "{}" \;
	$(FIND) libhelp -type f -name "*"   -exec rm "{}" \;
	$(FIND) man -type f -name "*"   -exec rm "{}" \;
	$(FIND) . -type f -name "directives.html"   -exec rm "{}" \;

docs :
	mkdir -p $(HTML_ARCDIR)
	$(CP) -r *.[^q]* $(NOEXT) $(HTML_ARCDIR)
