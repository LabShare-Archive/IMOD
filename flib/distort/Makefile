#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#  Revision 1.6  2006/10/05 18:01:43  mast
#  *** empty log message ***
#
#  Revision 1.5  2006/06/22 01:09:45  mast
#  Replaced LDFLAGS with LDFFLAGS for Intel Mac
#
#  Revision 1.4  2006/03/04 00:35:46  mast
#  Added fitrotsearch
#
#  Revision 1.3  2006/02/06 21:31:21  mast
#  Added several programs
#
#  Revision 1.2  2005/05/12 05:07:40  mast
#  Use strip variable
#
#  Revision 1.1  2003/12/12 20:53:22  mast
#  Preliminary addition to repository
#
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
	$(FC) -o finddistort $(DISTOBJS) $(LDFLG) $(IMLIBS) $(FORTCLIBS)

edgesubs.$(O):       ../blend/edgesubs.f
	$(FC) $(FFLAGS) -c -o edgesubs.$(O) ../blend/edgesubs.f

smoothgrid.$(O):       ../blend/smoothgrid.f
	$(FC) $(FFLAGS) -c -o smoothgrid.$(O) ../blend/smoothgrid.f

sdsearch.$(O):       ../blend/sdsearch.f
	$(FC) $(FFLAGS) -c -o sdsearch.$(O) ../blend/sdsearch.f

sdcalc.$(O):       ../blend/sdcalc.f
	$(FC) $(FFLAGS) -c -o sdcalc.$(O) ../blend/sdcalc.f

idfprocess$(E): idfprocess.$(O)
	$(FC) -o idfprocess idfprocess.$(O) $(LDFLG) $(IMLIBS) $(FORTCLIBS)

findgradient$(E): findgradient.$(O)
	$(FC) -o findgradient findgradient.$(O) $(LDFLG) $(IMLIBS) $(FORTCLIBS)

makegradtable$(E): makegradtable.$(O)
	$(FC) -o makegradtable makegradtable.$(O) $(LDFLG) $(IMLIBS) $(FORTCLIBS)

xf2rotmagstr$(E): xf2rotmagstr.$(O)
	$(FC) -o xf2rotmagstr xf2rotmagstr.$(O) $(LDFLG) $(IMLIBS) $(FORTCLIBS)

install: $(PROGS)
	$(STRIP) $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.o* *~ *.exe finddistort idfprocess findgradient \
	makegradtable xf2rotmagstr

findgradient.$(O):	findgradient.inc
