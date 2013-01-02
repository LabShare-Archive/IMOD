include ../../configure

CFLAGS = $(LIBCFLAGS)
CXXFLAGS = $(LIBCFLAGS) $(QTFLAGS)
FFLAGS = $(LIBFFLAGS)

O        = $(OBJEXT)
OBJS = bshst.$(O) psplotpak.$(O) bsplt.$(O) screenpak.$(O) pssymbol.$(O) scale.$(O) \
 chrout.$(O) grupnt.$(O) label_axis.$(O) shiftpt.$(O) dsaxes.$(O) psdashline.$(O) \
 minmax.$(O) flnam.$(O) psgrid.$(O) $(XPLAX) fracbx.$(O) psmisc.$(O) \
 psf.$(O) ps.$(O) trnc.$(O) plotvars.$(O)


all: $(OBJS)

bsplt.$(O): plotvars.$(O)
fraxbx.$(O): psplotpak.$(O)
pssymbol.$(O): psplotpak.$(O)
trnc.$(O): psplotpak.$(O)

moc_qtplax.cpp: qtplax.h
	$(MOC) qtplax.h -o moc_qtplax.cpp

qtplax.$(O): qtplax.h

ps.$(O): ps.h

clean:
	$(FIND) . -type f -name "*.$(OBJEXT)" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "*.mod" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
	$(FIND) . -type f -name "moc*" -exec rm "{}" \;
