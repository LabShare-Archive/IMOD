include ../configure

# 3/12/01 take bgimod out, no longer needed

SCRIPTS = copytomocoms finishjoin makejoincom matchorwarp matchshifts\
 newst onepatchcorr patchcorr3d patchcrawl3d recombine reduceby2 selfalign\
 setupcombine setupcoms setuplong splitmrc squeezevol trimvol xf2xg xfalign 

all:

install:
	$(CP) $(SCRIPTS) $(INSTBINDIR)
#	(cd $(INSTBINDIR) ; if (! -e bgimodv) ln -s bgimod bgimodv)

clean:
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;

