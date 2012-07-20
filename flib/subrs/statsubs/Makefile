include ../../configure

FFLAGS = $(LIBFFLAGS)

O        = $(OBJEXT)
OBJS = dirnm.$(O) hdiag.$(O) stepwz.$(O) eigenv.$(O) matinv.$(O) polyfit.$(O) \
 nestanova.$(O) polyterm.$(O) metro.$(O) statfuncs.$(O)

all: $(OBJS)


metro.$(O): metro.f90
	$(FC) -c $(FFLAGS) $(OPENMPF) metro.f90


clean:
	$(FIND) . -type f -name "*.$(OBJEXT)" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;

$(OBJS): $(INCDIR)/statsize.inc
