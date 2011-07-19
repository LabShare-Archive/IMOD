include ../configure

#  $Id$
#  No more Log

SCRIPTS = matchshifts\
 newst onepatchcorr patchcorr3d patchcrawl3d recombine reduceby2 selfalign\
 setupcoms setuplong splitmrc xf2xg xfalign\
 dm2mrc transferfid submfg imodpsview settmpdir matchrotpairs\
 clipflipyz prochunks.csh archiveorig imodkillgroup runpyscript imodhelp\
 queuechunk splitcorrection runraptor $(GENERATED_SCRIPTS)

all:

install:  $(CMDWRAPPERS)
	$(CP) $(SCRIPTS) $(INSTBINDIR)

cmdwrappers:
	./copywrappers $(INSTBINDIR) . $(SCRIPTS)

clean:
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;

