include ../configure

# DNM 3/11/01: renamed MODIMLIBS to GRAPHLIBS

PROGS = nda mtk sda

NDAOBJS = nda.o ndmodel.o graphden.o angdist.o ndrandom.o ndsubs.o ndmtkrand.o\
 ndmtksubs.o convexbound.o manipgraphs.o

MTKOBJS = mtk.o mtkmodel.o closedist.o mtksubs.o mtkrandom.o mtkransubs.o\
 ndmtksubs.o ndmtkrand.o bundledist.o convexbound.o manipgraphs.o

SDAOBJS = sda.o sdmodel.o sddist.o sd3dsubs.o surfden.o kerneldens.o\
 find_points_area.o ndmtksubs.o ndmtkrand.o ndsubs.o sdrandom.o manipgraphs.o\
 sdlength.o

INCOBJS = sda.o sdmodel.o sddist.o surfden.o kerneldens.o find_points_area.o



#CFLAGS = -O -I$(INCDIR)
#FFLAGS = -O -I$(INCDIR) 

NDALIBS = $(GRAPHLIBS) $(FORTCLIBS)
MTKLIBS = $(GRAPHLIBS) $(FORTCLIBS)
SDALIBS = $(GRAPHLIBS) $(FORTCLIBS)

all: $(PROGS)

nda: $(NDAOBJS)
	$(FC)  -o nda $(LDFLAGS) $(NDAOBJS) $(NDALIBS)

mtk: $(MTKOBJS)
	$(FC)  -o mtk $(LDFLAGS) $(MTKOBJS) $(MTKLIBS)

sda: $(SDAOBJS)
	$(FC)  -o sda $(LDFLAGS) $(SDAOBJS) $(SDALIBS)


$(INCOBJS): sda.inc
mtkmodel.o: $(INCDIR)/model.inc mtk.inc
mtkransubs.o: $(INCDIR)/model.inc mtk.inc
ndmodel.o: $(INCDIR)/model.inc
sdmodel.o: $(INCDIR)/model.inc
closedist.o: mtk.inc
mtksubs.o: mtk.inc
mtkrandom.o: mtk.inc


install: $(PROGS)
	strip $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.o *~ nda mtk sda


