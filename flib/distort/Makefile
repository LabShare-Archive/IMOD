#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#

include ../configure

O        = $(OBJEXT)
E        = $(BINEXT)
LDFLG    = $(LDFFLAGS) $(LDFLAGS)
PROGS = finddistort$(E)

DISTOBJS = finddistort.$(O) edgesubs.$(O) smoothgrid.$(O) \
 sdsearch.$(O) sdcalc.$(O) lsqr.$(O) lsqrblas.$(O) multrd.$(O) \
 matinvd.$(O)

all: $(PROGS)

finddistort$(E): $(DISTOBJS)
	$(FC) -o finddistort $(DISTOBJS) $(LDFLG) $(IMLIBS) $(FORTCLIBS)

edgesubs.$(O):       ../blend/edgesubs.f
	$(FC) $(FFLAGS) -c -o edgesubs.$(O) ../blend/edgesubs.f

smoothgrid.$(O):       ../blend/smoothgrid.f
	$(FC) $(FFLAGS) -c -o smoothgrid.$(O) ../blend/smoothgrid.f

sdsearch.$(O):       ../blend/sdsearch.f
	$(FC) $(FFLAGS) -c -o sdsearch.$(O) ../blend/sdsearch.f

sdcalc.$(O):       ../blend/sdcalc.f
	$(FC) $(FFLAGS) -c -o sdcalc.$(O) ../blend/sdcalc.f

install: $(PROGS)
	strip $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.o* *~ *.exe finddistort

