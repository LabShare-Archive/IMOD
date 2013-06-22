include ../configure
E        = $(BINEXT)
O        = $(OBJEXT)
LDFLG    = $(LDFFLAGS)

PROGS = genhstplt$(E) avgstatplot$(E) mtpairing$(E) mtoverlap$(E) \
filterplot$(E)

LIBS = $(GRAPHLIBS) $(FORTCLIBS)

all: $(PROGS)

genhstplt$(E): genhstplt.$(O)
	$(FC) -o genhstplt genhstplt.$(O) $(LDFLG) $(LIBS)

avgstatplot$(E): avgstatplot.$(O)
	$(FC) -o avgstatplot avgstatplot.$(O) $(LDFLG) $(LIBS)

mtpairing$(E): mtpairing.$(O)
	$(FC) -o mtpairing mtpairing.$(O) $(LDFLG) $(LWMOD) $(LIMOD) $(LIBS)

mtoverlap$(E): mtoverlap.$(O)
	$(FC) -o mtoverlap mtoverlap.$(O) $(LDFLG) $(LWMOD)  $(LIMOD) $(LIBS)

filterplot$(E): filterplot.$(O)
	$(FC) -o filterplot filterplot.$(O) $(LDFLG) $(LWMOD)  $(LIMOD) $(LIBS)

genhstplt.$(O): plotvars.mod
mtpairing.$(O): plotvars.mod

plotvars.mod: ../subrs/graphics/plotvars.f90
	$(FC) -c $(LIBFFLAGS) ../subrs/graphics/plotvars.f90

install: $(PROGS)
	$(STRIP) $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.$(OBJEXT) *.exe *~ $(PROGS) *.mod

mtpairing.$(O): $(INCDIR)/model.inc
mtoverlap.$(O): $(INCDIR)/model.inc

