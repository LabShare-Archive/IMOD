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

include ../configure

all: tilt

tilt: tilt.o licenseusfft.o $(TILTOBJS) $(FBPLIB) 
	$(FC) $(FFLAGS) -o tilt tilt.o licenseusfft.o $(TILTOBJS) \
	$(LDFLAGS) $(IMLIBS) $(FBPLIB) $(USFFTLIBS) $(FORTCLIBS)

libfbp.a: fbplib/*.f
	(cd fbplib ; $(MAKE) )

nolibfbp.a: nolibfbp.o
	ar cr nolibfbp.a nolibfbp.o

handnox.o: handnox.s
handxtilt.o: handxtilt.s
handlocal.o: handlocal.s

tilt.o: tilt.inc fbpswitch.inc

install: tilt
	strip tilt
	$(CP) tilt $(INSTBINDIR)

clean:
	(cd fbplib ; $(MAKE) clean)
	-\rm -f *.o *.a *~ tilt fasttilt

