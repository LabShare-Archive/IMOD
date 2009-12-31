# Makefile for tilt program
#
# configure supplies TILTOBJS which specifies fortran subroutines or 
# hand-optimized assembly routines for PC
# hand optimized routines were prepared by compiling g77 -S -c -O1, then
# knocking out appropriate mode change code
#
#  $Id$
#
#  Log at end
#

include ../configure

O        = $(OBJEXT)
PROG = tilt$(BINEXT)

all: $(PROG)

$(PROG): tilt.$(O) tiltvars.$(O) $(TILTOBJS) $(TILTGPUOBJ) 
	$(FC) $(FFLAGS) -o tilt tilt.$(O) tiltvars.$(O) \
	$(TILTOBJS) $(TILTGPUOBJ) $(LDFFLAGS) $(LWMOD) $(LIMOD) \
	$(IMLIBS) $(FORTCLIBS) $(CUDALIBS)


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
	-\rm -f *.o* *.exe *.linkinfo \#* *~ tilt

#  $Log$
#  Revision 3.10  2009/12/31 20:37:21  mast
#  GPU code added
#
#  Revision 3.9  2009/06/26 05:17:46  mast
#  Switched to module
#
#  Revision 3.8  2009/05/22 22:55:16  mast
#  Switch to large model
#
#  Revision 3.7  2008/11/14 17:16:10  mast
#  Added smallmodel lib to link
#
#  Revision 3.6  2006/06/22 01:09:45  mast
#  Replaced LDFLAGS with LDFFLAGS for Intel Mac
#
#  Revision 3.5  2005/05/12 05:07:41  mast
#  Use strip variable
#
#  Revision 3.4  2003/11/12 22:02:32  mast
#  Use variables from config for making static library
#
#  Revision 3.3  2003/10/24 02:30:57  mast
#  Changes for windows/Intel compiler
#
#  Revision 3.2  2003/03/28 23:54:20  mast
#  Add ranlib to make Mac happy
#
#  Revision 3.1  2002/07/26 19:16:29  mast
#  Added dependency on fbpswitch.inc
#
