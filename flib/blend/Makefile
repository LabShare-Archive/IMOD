#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#  Revision 3.6  2005/11/23 16:50:36  mast
#  Remove dependency on recl_bytes since it is created by setup
#
#  Revision 3.5  2005/11/19 17:01:44  mast
#  Added dependency on recl_bytes.inc
#
#  Revision 3.4  2005/05/12 05:07:40  mast
#  Use strip variable
#
#  Revision 3.3  2003/12/12 20:47:53  mast
#  Added edgesubs.f
#
#  Revision 3.2  2003/10/24 02:30:57  mast
#  Changes for windows/Intel compiler
#
#  Revision 3.1  2002/08/19 04:29:03  mast
#  Added dependencies on blend.inc
#

include ../configure

O        = $(OBJEXT)
E        = $(BINEXT)
LDFLG    = $(LDFFLAGS)
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
	$(STRIP) $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.o* *~ *.exe blendmont reducemont sloppymont

blendmont.$(O):	blend.inc
bsubs.$(O):	blend.inc
shuffler.$(O):	blend.inc
reducemont.$(O):	blend.inc
