# Makefile for tilt program
#
# configure supplies TILTOBJS which specifies fortran subroutines or 
# hand-optimized assembly routines for PC
# hand optimized routines were prepared by compiling g77 -S -c -O1, then
# knocking out appropriate mode change code
#
#  $Id$
#

include ../configure

O        = $(OBJEXT)
PROG = tilt$(BINEXT)

all: $(PROG)

$(PROG): tilt.$(O) tiltvars.$(O) $(TILTOBJS) $(TILTGPUOBJ) 
	$(FC) $(FFLAGS) -o tilt tilt.$(O) tiltvars.$(O) \
	$(TILTOBJS) $(TILTGPUOBJ) $(LDFFLAGS) $(LWMOD) $(LIMOD) \
	$(IMLIBS) $(LIFFT) $(FORTCLIBS) $(CUDALIBS)


gpubp.$(O): gpubp.cu
	nvcc -c $(NVCC_FLAGS)  gpubp.cu

handnox.$(O): handnox.s
handxtilt.$(O): handxtilt.s
handlocal.$(O): handlocal.s

tilt.$(O): tiltvars.$(O) 

install: $(PROG)
	$(STRIP) $(PROG)
	$(CP) $(PROG) $(INSTBINDIR)

clean:
	-\rm -f *.$(OBJEXT) *.exe *.mod *.linkinfo \#* *~ tilt

