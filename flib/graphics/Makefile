include ../configure
E        = $(BINEXT)
O        = $(OBJEXT)
LDFLG    = $(LDFFLAGS)

# DNM 3/11/01: renamed MODLIBS and MODIMLIBS to GRAPHLIBS

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

install: $(PROGS)
	$(STRIP) $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.o* *.exe *~ $(PROGS)

mtpairing.$(O): $(INCDIR)/model.inc
mtoverlap.$(O): $(INCDIR)/model.inc

