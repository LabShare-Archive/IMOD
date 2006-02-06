# Makefile for BL3DFS fortran programs.
include ../configure

default : configure all

configure : ALWAYS
	if [ ! -e configure ] ; then ln -s ../configure configure ; fi



all install : configure
	cd subrs;     $(MAKE) $@
	cd image;     $(MAKE) $@
	cd model;     $(MAKE) $@
	cd blend;     $(MAKE) $@
	cd tiltalign; $(MAKE) $@
	cd beadtrack; $(MAKE) $@
	cd tilt;      $(MAKE) $@
	cd graphics;  $(MAKE) $@
	cd ndasda;    $(MAKE) $@
	cd distort;   $(MAKE) $@

cyginstall : configure
	$(FIND) . -name "*.exe" -exec strip "{}" \; -exec $(CP) "{}" $(INSTBINDIR) \;

libs : configure
	cd subrs; $(MAKE) all

installlibs : configure
	cd subrs; $(MAKE) install

cleanlibs : ALWAYS
	cd subrs; $(MAKE) clean


clean : ALWAYS
	cd subrs; $(MAKE) $@
	cd image; $(MAKE) $@
	cd model; $(MAKE) $@
	cd blend; $(MAKE) $@
	cd tiltalign; $(MAKE) $@
	cd beadtrack; $(MAKE) $@
	cd tilt;      $(MAKE) $@
	cd graphics; $(MAKE) $@
	cd ndasda; $(MAKE) $@
	cd distort;   $(MAKE) $@

ALWAYS:



