include ../../configure

OBJS = bshst.o impak.o bsplt.o graphpak.o imsymb.o scale.o\
 chrout.o grupnt.o label_axis.o shiftpt.o dsaxes.o imdash.o minmax.o flnam.o\
 imgrid.o plax.o fracbx.o immisc.o pwrit.o psf.o ps.o trnc.o


all: $(OBJS)

plax.o: plax.h

ps.o: ps.h

clean:
	$(FIND) . -type f -name "*.o" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
