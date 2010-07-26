include ../../configure

FFLAGS = $(LIBFFLAGS)

O        = $(OBJEXT)
OBJS = dirnm.$(O) hdiag.$(O) multr.$(O) stepwz.$(O) correl.$(O) \
 eigenv.$(O) matinv.$(O) polyfit.$(O) nestanova.$(O) polyterm.$(O) metro.$(O) \
 weightpoly.$(O) localpolyfit.$(O) gaussj.$(O) statfuncs.$(O)

all: $(OBJS)


metro.$(O): metro.f
	$(FC) -c $(FFLAGS) $(OPENMPF) metro.f


clean:
	$(FIND) . -type f -name "*.o*" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;

$(OBJS): $(INCDIR)/statsize.inc
