include ../configure

# DNM 3/11/01: renamed MODLIBS and MODIMLIBS to GRAPHLIBS

PROGS = genhstplt avgstatplot mtpairing mtoverlap

all: $(PROGS)

genhstplt: genhstplt.o
	$(FC) -o genhstplt genhstplt.o $(LDFLAGS) $(GRAPHLIBS) $(FORTCLIBS)

avgstatplot: avgstatplot.o
	$(FC) -o avgstatplot avgstatplot.o $(LDFLAGS) $(GRAPHLIBS) $(FORTCLIBS)

mtpairing: mtpairing.o
	$(FC) -o mtpairing mtpairing.o $(LDFLAGS) $(GRAPHLIBS) $(FORTCLIBS)

mtoverlap: mtoverlap.o
	$(FC) -o mtoverlap mtoverlap.o $(LDFLAGS) $(GRAPHLIBS) $(FORTCLIBS)

install: $(PROGS)
	strip $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.o *~ $(PROGS)

mtpairing.o: $(INCDIR)/model.inc
mtoverlap.o: $(INCDIR)/model.inc

