# Makefile for distortion-related programs
#
# $Id$
#

include ../configure

O        = $(OBJEXT)
E        = $(BINEXT)
LDFLG    = $(LDFFLAGS)
PROGS = finddistort$(E) idfprocess$(E) findgradient$(E) makegradtable$(E) \
        xf2rotmagstr$(E)

DISTOBJS = finddistort.$(O) edgesubs.$(O) smoothgrid.$(O) \
 sdsearch.$(O) sdcalc.$(O) multrd.$(O) matinvd.$(O)

all: $(PROGS)

finddistort$(E): $(DISTOBJS)
	$(FC) -o finddistort $(DISTOBJS) $(LDFLG) $(LIWARP) $(IMLIBS) $(FORTCLIBS)

edgesubs.$(O):       ../blend/edgesubs.f
	$(FC) $(FFLAGS) -c -o edgesubs.$(O) ../blend/edgesubs.f

smoothgrid.$(O):       ../blend/smoothgrid.f
	$(FC) $(FFLAGS) -c -o smoothgrid.$(O) ../blend/smoothgrid.f

sdsearch.$(O):       ../blend/sdsearch.f
	$(FC) $(FFLAGS) -c -o sdsearch.$(O) ../blend/sdsearch.f

sdcalc.$(O):       ../blend/sdcalc.f
	$(FC) $(FFLAGS) -c -o sdcalc.$(O) ../blend/sdcalc.f

idfprocess$(E): idfprocess.$(O)
	$(FC) -o idfprocess idfprocess.$(O) $(LDFLG) $(LIWARP) $(IMLIBS) $(FORTCLIBS)

findgradient$(E): findgradient.$(O)
	$(FC) -o findgradient findgradient.$(O) $(LDFLG) $(IMLIBS) $(FORTCLIBS)

makegradtable$(E): makegradtable.$(O)
	$(FC) -o makegradtable makegradtable.$(O) $(LDFLG) $(IMLIBS) $(FORTCLIBS)

xf2rotmagstr$(E): xf2rotmagstr.$(O)
	$(FC) -o xf2rotmagstr xf2rotmagstr.$(O) $(LDFLG) $(LIWARP) $(IMLIBS) $(FORTCLIBS)

install: $(PROGS)
	$(STRIP) $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.$(OBJEXT) *~ *.exe finddistort idfprocess findgradient \
	makegradtable xf2rotmagstr

findgradient.$(O):	findgradient.inc
