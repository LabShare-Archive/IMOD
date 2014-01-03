include ../../configure
#
# $Id$
#

CFLAGS = $(LIBCFLAGS)
FFLAGS = $(LIBFFLAGS)

O        = $(OBJEXT)
OBJS = irdhdr.$(O) wrap_iiunit.$(O) irdbinned.$(O) irdreduced.$(O) strupcase.$(O) \
	irepak.$(O) isetdn.$(O) blockio.$(O) convert_vms.$(O) cinterp.$(O) iclcdn.$(O)

all: $(OBJS)

blockio.$(O): environ.h

clean:
	$(FIND) . -type f -name "*.$(OBJEXT)" -exec rm "{}" \;
	$(FIND) . -type f -name "*.mod" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
