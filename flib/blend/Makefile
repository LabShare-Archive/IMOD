#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#  Revision 3.2  2003/10/24 02:30:57  mast
#  Changes for windows/Intel compiler
#
#  Revision 3.1  2002/08/19 04:29:03  mast
#  Added dependencies on blend.inc
#

include ../configure

O        = $(OBJEXT)
E        = $(BINEXT)
LDFLG    = $(LDFFLAGS) $(LDFLAGS)
PROGS = blendmont$(E) reducemont$(E)

BLOBJS = blendmont.$(O) bsubs.$(O) edgesubs.$(O) shuffler.$(O) smoothgrid.$(O)\
 sdsearch.$(O) sdcalc.$(O) setoverlap.$(O)

REDOBJS = reducemont.$(O) shuffler.$(O) setoverlap.$(O)

SLOPOBJS = sloppymont.$(O) shuffler.$(O) setoverlap.$(O)

all: $(PROGS)

blendmont$(E): $(BLOBJS)
	$(FC)  -o blendmont $(BLOBJS) $(LDFLG) $(IMLIBS) $(FORTCLIBS)

reducemont$(E): $(REDOBJS)
	$(FC) -o reducemont $(REDOBJS) $(LDFLG) $(IMLIBS) $(FORTCLIBS)

sloppymont$(E): $(SLOPOBJS)
	$(FC) -o sloppymont $(SLOPOBJS) $(LDFLG) $(IMLIBS) $(FORTCLIBS)

install: $(PROGS)
	strip $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.o* *~ *.exe blendmont reducemont sloppymont

blendmont.$(O):	blend.inc
bsubs.$(O):	blend.inc
shuffler.$(O):	blend.inc
reducemont.$(O):	blend.inc
