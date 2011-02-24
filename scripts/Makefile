include ../configure

#  $Id$
#  No more Log

SCRIPTS = copytomocoms finishjoin makejoincom matchorwarp matchshifts\
 newst onepatchcorr patchcorr3d patchcrawl3d recombine reduceby2 selfalign\
 setupcoms setuplong splitmrc squeezevol trimvol xf2xg xfalign\
 dm2mrc alignlog transferfid submfg imodpsview settmpdir matchrotpairs\
 chunksetup tomosnapshot clipflipyz prochunks.csh \
 archiveorig imodkillgroup collectmmm sampletilt runpyscript imodhelp\
 queuechunk splitcorrection runraptor $(GENERATED_SCRIPTS)

all:

install:  $(CMDWRAPPERS)
	$(CP) $(SCRIPTS) $(INSTBINDIR)

cmdwrappers:
	./copywrappers $(INSTBINDIR) . $(SCRIPTS)

clean:
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;

