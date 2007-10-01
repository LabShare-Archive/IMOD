# Makefile for imsubs2 routines
#
# $Id$
# Log at end

include ../../configure

FFLAGS = $(LIBFFLAGS)

O        = $(OBJEXT)
OBJS =  bigfft.$(O) bigfilt.$(O) cinterp.$(O) filter.$(O) icalc_angles.$(O) \
 icalc_matrix.$(O) iclcdn.$(O) iclden.$(O) iclscl.$(O) inside.$(O) \
 interp.$(O) qdinterp.$(O) \
 strupcase.$(O) iclavgsd.$(O) b3dxor.$(O)

bigfft.$(O): $(INCDIR)/ftbuf.inc
bigfilt.$(O): $(INCDIR)/ftbuf.inc


all: $(OBJS)


clean:
	$(FIND) . -type f -name "*.o*" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;


#
#  $Log$
#  Revision 3.9  2007/09/20 02:34:16  mast
#  Removed cubinterp
#
#  Revision 3.8  2005/12/09 06:23:05  mast
#  Needed to move b3dxor into  here
#
#  Revision 3.7  2005/02/28 15:09:33  mast
#  undistinterp had to go in libhvem
#
#  Revision 3.6  2005/02/27 15:57:28  mast
#  Added undistinterp
#
#  Revision 3.5  2004/10/24 20:42:29  mast
#  Added dependencies on ftbuf.inc
#
#  Revision 3.4  2004/04/18 22:22:41  mast
#  Added LIBFFLAGS etc for 64-bit compilation
#
#  Revision 3.3  2003/12/24 18:08:43  mast
#  Remove get_nxyz
#
#  Revision 3.2  2003/10/24 02:31:04  mast
#  Changes for windows/Intel compiler
#
#  Revision 3.1  2002/08/18 23:01:58  mast
#  Added iclavgsd
#
# 7/18/00 CER add move strupcase.o here from hvem
#

