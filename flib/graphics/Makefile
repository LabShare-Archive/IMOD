include ../configure
E        = $(BINEXT)
O        = $(OBJEXT)
LDFLG    = $(LDFFLAGS) $(LDFLAGS)

# DNM 3/11/01: renamed MODLIBS and MODIMLIBS to GRAPHLIBS

PROGS = genhstplt$(E) avgstatplot$(E) mtpairing$(E) mtoverlap$(E)

all: $(PROGS)

genhstplt$(E): genhstplt.$(O)
	$(FC) -o genhstplt genhstplt.$(O) $(LDFLG) $(GRAPHLIBS) $(FORTCLIBS)

avgstatplot$(E): avgstatplot.$(O)
	$(FC) -o avgstatplot avgstatplot.$(O) $(LDFLG) $(GRAPHLIBS) $(FORTCLIBS)

mtpairing$(E): mtpairing.$(O)
	$(FC) -o mtpairing mtpairing.$(O) $(LDFLG) $(GRAPHLIBS) $(FORTCLIBS)

mtoverlap$(E): mtoverlap.$(O)
	$(FC) -o mtoverlap mtoverlap.$(O) $(LDFLG) $(GRAPHLIBS) $(FORTCLIBS)

install: $(PROGS)
	strip $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.o* *.exe *~ $(PROGS)

mtpairing.$(O): $(INCDIR)/model.inc
mtoverlap.$(O): $(INCDIR)/model.inc

