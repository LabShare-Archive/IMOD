# Makefile for imsubs2 routines
#
# $Id$

include ../../configure

FFLAGS = $(LIBFFLAGS)

O        = $(OBJEXT)
OBJS =  cinterp.$(O) icalc_angles.$(O) icalc_matrix.$(O) iclcdn.$(O) inside.$(O) \
 strupcase.$(O) b3dxor.$(O)


all: $(OBJS)


clean:
	$(FIND) . -type f -name "*.$(OBJEXT)" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
