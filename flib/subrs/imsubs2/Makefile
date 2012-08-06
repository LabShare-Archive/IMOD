# Makefile for imsubs2 routines
#
# $Id$

include ../../configure

FFLAGS = $(LIBFFLAGS)

O        = $(OBJEXT)
OBJS =  bigfft.$(O) bigfilt.$(O) cinterp.$(O) filter.$(O) icalc_angles.$(O) \
 icalc_matrix.$(O) iclcdn.$(O) iclden.$(O) iclscl.$(O) inside.$(O) \
 interp.$(O) qdinterp.$(O) \
 strupcase.$(O) iclavgsd.$(O) b3dxor.$(O)

bigfft.$(O): $(INCDIR)/ftbuf.inc
bigfilt.$(O): $(INCDIR)/ftbuf.inc


all: $(OBJS)


clean:
	$(FIND) . -type f -name "*.$(OBJEXT)" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
