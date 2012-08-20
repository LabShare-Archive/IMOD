include ../../configure
#
# $Id$
#

CFLAGS = $(LIBCFLAGS)
FFLAGS = $(LIBFFLAGS)

O        = $(OBJEXT)
OBJS = blockio.$(O) altskip.$(O) icllim.$(O) imopen.$(O) imposn.$(O) \
 imunit.$(O) irdhdr.$(O) irdlin.$(O) irdpas.$(O) irepak.$(O) isetdn.$(O) \
 iwrlin.$(O) iwrpas.$(O) convert_vms.$(O) nbytes_and_flags.$(O) \
 irdbinned.$(O)
MODULES = imsubs.$(O)

all: $(OBJS) $(MODULES)

blockio.$(O): environ.h
$(OBJS):   imsubs.$(O)

clean:
	$(FIND) . -type f -name "*.$(OBJEXT)" -exec rm "{}" \;
	$(FIND) . -type f -name "*.mod" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
