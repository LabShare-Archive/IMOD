include ../configure
# Makefile for the python components, installs scripts to bin and modules
# to pylib
#
# $Id$
#

SCRIPTS = vmstopy tomostitch edgepatches fitpatches sirtsetup splitblend setupcombine \
splittilt imodcpuload gpumonitor splitcombine copytomocoms squeezevol tomosnapshot \
trimvol makejoincom finishjoin chunksetup collectmmm sampletilt matchorwarp alignlog \
gputilttest b3dremove b3dcopy b3dcatfiles findsirtdiffs dm2mrc archiveorig runraptor \
splitcorrection transferfid xfalign startprocess imodhelp submfg etomo colornewst \
autofidseed onegenplot setupstitch makecomfile batchruntomo makepyramid tomodataplots \
imodkillgroup

MODULES = imodpy.py pysed.py supermont.py pip.py comchanger.py

all:

install:  $(CMDWRAPPERS)
	mkdir -p $(INSTDIR)/pylib
	$(CP) $(SCRIPTS) $(INSTBINDIR)
	$(CP) $(MODULES) $(INSTDIR)/pylib

cmdwrappers:
	../scripts/copywrappers $(INSTBINDIR) . $(SCRIPTS)

clean:
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;
