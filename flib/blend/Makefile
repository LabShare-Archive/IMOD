# Makefile for blendmont and related programs
#
# $Id$
#

include ../configure

O        = $(OBJEXT)
E        = $(BINEXT)
LDFLG    = $(LDFFLAGS)
PROGS = blendmont$(E) reducemont$(E)

BLOBJS = blendmont.$(O) bsubs.$(O) edgesubs.$(O) shuffler.$(O) smoothgrid.$(O)\
 sdsearch.$(O) sdcalc.$(O) setoverlap.$(O) blendvars.$(O)

REDOBJS = reducemont.$(O) shuffler.$(O) setoverlap.$(O) blendvars.$(O)

SLOPOBJS = sloppymont.$(O) shuffler.$(O) setoverlap.$(O) blendvars.$(O)

all: $(PROGS)

# Add $(LAPACKLIBS) to test find_best_shifts with SVD
blendmont$(E): $(BLOBJS)
	$(FC)  -o blendmont $(BLOBJS) $(LDFLG) $(LSMLMOD) $(LIMOD) \
	$(LIWARP) $(IMLIBS) $(LIFFT) $(FORTCLIBS)


reducemont$(E): $(REDOBJS)
	$(FC) -o reducemont $(REDOBJS) $(LDFLG) $(LIWARP) $(IMLIBS) $(FORTCLIBS)

sloppymont$(E): $(SLOPOBJS)
	$(FC) -o sloppymont $(SLOPOBJS) $(LDFLG) $(LIWARP) $(IMLIBS) $(FORTCLIBS)

install: $(PROGS)
	$(STRIP) $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.$(OBJEXT) *~ *.exe blendvars.mod blendmont reducemont sloppymont

blendmont.$(O):	blendvars.$(O)
bsubs.$(O):	blendvars.$(O)
shuffler.$(O):	blendvars.$(O)
reducemont.$(O):	blendvars.$(O)
