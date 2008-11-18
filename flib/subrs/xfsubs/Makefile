include ../../configure

FFLAGS = $(LIBFFLAGS)

O        = $(OBJEXT)
OBJS = amat_to_rotmag.$(O) rotmag_to_amat.$(O)\
 rotmagstr_to_amat.$(O) xfapply.$(O) xfinvert.$(O) xflincom.$(O) xfcopy.$(O) \
 xfmult.$(O) xfrdall.$(O) xfread.$(O) xfunit.$(O) xfwrite.$(O) \
 readdistortions.$(O) interpolategrid.$(O) maggradfield.$(O) findxf.$(O) \
 undistinterp.$(O) writedistortions.$(O)

all: $(OBJS)

clean:
	$(FIND) . -type f -name "*.o*" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;

findxf.$(O): $(INCDIR)/statsize.inc
