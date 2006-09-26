include ../configure
# Makefile for the python components, installs scripts to bin and modules
# to pylib
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#

SCRIPTS = vmstopy
MODULES = imodpy.py pysed.py

all:
	cd pip; $(MAKE) $@

install:	all $(CMDWRAPPERS)
	mkdir -p $(INSTDIR)/pylib
	cd pip; $(MAKE) $@
	$(CP) $(SCRIPTS) $(INSTBINDIR)
	$(CP) $(MODULES) $(INSTDIR)/pylib

cmdwrappers:
	../scripts/copywrappers $(INSTBINDIR) pywrapper.cmd $(SCRIPTS)

clean:
	cd pip; $(MAKE) $@
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;
