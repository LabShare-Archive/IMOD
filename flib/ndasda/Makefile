include ../configure
O        = $(OBJEXT)
E        = $(BINEXT)
LDFLG    = $(LDFFLAGS) $(LDFLAGS)

# DNM 3/11/01: renamed MODIMLIBS to GRAPHLIBS

PROGS = nda$(E) mtk$(E) sda$(E)

NDAOBJS = nda.$(O) ndmodel.$(O) graphden.$(O) angdist.$(O) ndrandom.$(O) \
 ndsubs.$(O) ndmtkrand.$(O) ndmtksubs.$(O) convexbound.$(O) manipgraphs.$(O)

MTKOBJS = mtk.$(O) mtkmodel.$(O) closedist.$(O) mtksubs.$(O) mtkrandom.$(O)\
 mtkransubs.$(O) ndmtksubs.$(O) ndmtkrand.$(O) bundledist.$(O) \
 convexbound.$(O) manipgraphs.$(O)

SDAOBJS = sda.$(O) sdmodel.$(O) sddist.$(O) sd3dsubs.$(O) surfden.$(O) \
 kerneldens.$(O) find_points_area.$(O) ndmtksubs.$(O) ndmtkrand.$(O) \
 ndsubs.$(O) sdrandom.$(O) manipgraphs.$(O) sdlength.$(O)

INCOBJS = sda.$(O) sdmodel.$(O) sddist.$(O) surfden.$(O) kerneldens.$(O) \
 find_points_area.$(O)



#CFLAGS = -O -I$(INCDIR)
#FFLAGS = -O -I$(INCDIR) 

NDALIBS = $(GRAPHLIBS) $(FORTCLIBS)
MTKLIBS = $(GRAPHLIBS) $(FORTCLIBS)
SDALIBS = $(GRAPHLIBS) $(FORTCLIBS)

all: $(PROGS)

nda$(E): $(NDAOBJS)
	$(FC)  -o nda $(NDAOBJS) $(LDFLG) $(NDALIBS)

mtk$(E): $(MTKOBJS)
	$(FC)  -o mtk $(MTKOBJS) $(LDFLG) $(MTKLIBS)

sda$(E): $(SDAOBJS)
	$(FC)  -o sda $(SDAOBJS) $(LDFLG) $(SDALIBS)


$(INCOBJS): sda.inc
mtkmodel.$(O): $(INCDIR)/model.inc mtk.inc
mtkransubs.$(O): $(INCDIR)/model.inc mtk.inc
ndmodel.$(O): $(INCDIR)/model.inc
sdmodel.$(O): $(INCDIR)/model.inc
closedist.$(O): mtk.inc
mtksubs.$(O): mtk.inc
mtkrandom.$(O): mtk.inc


install: $(PROGS)
	strip $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.o* *.exe *~ nda mtk sda


