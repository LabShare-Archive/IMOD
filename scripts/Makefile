include ../configure

#  $Id$

SCRIPTS = matchshifts onepatchcorr patchcorr3d patchcrawl3d recombine selfalign\
 setupcoms setuplong splitmrc  imodpsview settmpdir matchrotpairs\
 prochunks.csh queuechunk $(GENERATED_SCRIPTS)

all:

install:  $(CMDWRAPPERS)
	$(CP) $(SCRIPTS) $(INSTBINDIR)

cmdwrappers:
	./copywrappers $(INSTBINDIR) . $(SCRIPTS)

clean:
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;

