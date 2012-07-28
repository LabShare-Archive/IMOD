include ../../configure

FFLAGS = $(LIBFFLAGS)

O        = $(OBJEXT)
OBJS = amat_to_rotmag.$(O) rotmag_to_amat.$(O) xflincom.$(O) \
 xfrdall.$(O) xfread.$(O) xfwrite.$(O) readdistortions.$(O) findxf.$(O) \
 writedistortions.$(O)

all: $(OBJS)

clean:
	$(FIND) . -type f -name "*.$(OBJEXT)" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;

findxf.$(O): $(INCDIR)/statsize.inc
