include ../configure

#  $Id$
#  No more Log

SCRIPTS = matchshifts onepatchcorr patchcorr3d patchcrawl3d recombine selfalign\
 setupcoms setuplong splitmrc  submfg imodpsview settmpdir matchrotpairs\
 clipflipyz prochunks.csh imodkillgroup runpyscript imodhelp\
 queuechunk $(GENERATED_SCRIPTS)

all:

install:  $(CMDWRAPPERS)
	$(CP) $(SCRIPTS) $(INSTBINDIR)

cmdwrappers:
	./copywrappers $(INSTBINDIR) . $(SCRIPTS)

clean:
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;

