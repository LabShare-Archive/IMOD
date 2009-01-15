include ../../configure

CFLAGS = $(LIBCFLAGS)
CXXFLAGS = $(LIBCFLAGS) $(QTFLAGS)
FFLAGS = $(LIBFFLAGS)

O        = $(OBJEXT)
OBJS = bshst.$(O) impak.$(O) bsplt.$(O) graphpak.$(O) imsymb.$(O) scale.$(O) \
 chrout.$(O) grupnt.$(O) label_axis.$(O) shiftpt.$(O) dsaxes.$(O) imdash.$(O) \
 minmax.$(O) flnam.$(O) imgrid.$(O) $(XPLAX) fracbx.$(O) immisc.$(O) \
 pwrit.$(O) psf.$(O) ps.$(O) trnc.$(O)


all: $(OBJS)

moc_qtplax.cpp: qtplax.h
	$(MOC) qtplax.h -o moc_qtplax.cpp

qtplax.$(O): qtplax.h
xplax.$(O): xplax.h
noplax.$(O): xplax.h

ps.$(O): ps.h

clean:
	$(FIND) . -type f -name "*.o*" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
	$(FIND) . -type f -name "moc*" -exec rm "{}" \;
