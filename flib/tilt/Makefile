# Makefile for tilt program
#
# configure supplies TILTOBJS which specifies fortran subroutines or 
# hand-optimized assembly routines for PC
# hand optimized routines were prepared by compiling g77 -S -c -O1, then
# knocking out appropriate mode change code
#
# configure supplies FBPLIB and USFFTLIBS, which specify libfbp.a and the
# USFFT libraries needed, or nolibfbp.a and nothing if USFFT not available
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#  Revision 3.3  2003/10/24 02:30:57  mast
#  Changes for windows/Intel compiler
#
#  Revision 3.2  2003/03/28 23:54:20  mast
#  Add ranlib to make Mac happy
#
#  Revision 3.1  2002/07/26 19:16:29  mast
#  Added dependency on fbpswitch.inc
#

include ../configure

O        = $(OBJEXT)
PROG = tilt$(BINEXT)

all: $(PROG)

$(PROG): tilt.$(O) licenseusfft.$(O) $(TILTOBJS) $(FBPLIB) 
	$(FC) $(FFLAGS) -o tilt tilt.$(O) licenseusfft.$(O) $(TILTOBJS) \
	 $(FBPLIB) $(LDFFLAGS) $(LDFLAGS) $(IMLIBS) $(USFFTLIBS) $(FORTCLIBS)

libfbp.a: fbplib/*.f
	(cd fbplib ; $(MAKE) )

nolibfbp.a: nolibfbp.$(O)
	$(MAKESTATICLIB)nolibfbp.a nolibfbp.$(O)
	$(STATICRANLIB) nolibfbp.a

handnox.$(O): handnox.s
handxtilt.$(O): handxtilt.s
handlocal.$(O): handlocal.s

tilt.$(O): tilt.inc fbpswitch.inc

install: $(PROG)
	strip $(PROG)
	$(CP) $(PROG) $(INSTBINDIR)

clean:
	(cd fbplib ; $(MAKE) clean)
	-\rm -f *.o* *.exe *.a *~ tilt

