#
# Makefile for libcfshr
#
# $Id$
# Log at end
#
include ../configure

LIBI   = libcfshr.$(DLLEXT)
O     = $(OBJEXT)
CFLAGS = $(LIBCFLAGS)

UTILOBJS = b3dutil.$(O) ilist.$(O) parselist.$(O) amoeba.$(O) \
	samplemeansd.$(O) colormap.$(O) cubinterp.$(O) reduce_by_binning.$(O) \
	scaledsobel.$(O) filtxcorr.$(O) taperpad.$(O) circlefit.$(O) \
	insidecontour.$(O) islice.$(O) taperatfill.$(O) histogram.$(O) \
	simplestat.$(O) amat_to_rotmagstr.$(O) percentile.$(O) beadutil.$(O) \
	lsqr.$(O) lsqrblas.$(O) sparselsqr.$(O) parallelwrite.$(O) \
	robuststat.$(O) convexbound.$(O)

PIPOBJS = parse_params.$(O) pip_fwrap.$(O) autodoc.$(O) adoc_fwrap.$(O)

all : $(LIBI)

cubinterp.$(O) : cubinterp.c
	$(CC) $(CFLAGS) $(OPENMP) -c cubinterp.c
b3dutil.$(O) : b3dutil.c
	$(CC) $(CFLAGS) $(OPENMP) -c b3dutil.c

$(LIBI) : $(UTILOBJS) $(PIPOBJS)
	$(FIND) . -type f -name $(LIBI) -exec rm "{}" \;
	$(MAKELIB)$(LIBI) $(LIBCFSHRDEF) $(UTILOBJS) $(PIPOBJS)
	$(RANLIB) $(LIBI)
	$(CP) libcfshr.$(LIBEXT) $(LIBDIR)

install : $(LIBI)
	$(CP) $(LIBI) $(INSTLIBDIR)

clean : 
	$(FIND) . -type f -name $(LIBI) -exec rm "{}" \;
	$(FIND) . -type f -name "*.exp" -exec rm "{}" \;
	$(FIND) . -type f -name "*.o*"  -exec rm "{}" \;
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"  -exec rm "{}" \;

#
# Some depends.
#
$(UTILOBJS) : $(INCDIR)/b3dutil.h $(INCDIR)/cfsemshare.h
ilist.c : $(INCDIR)/ilist.h
b3dutil.$(O) :  ../.version
$(PIPOBJS) : $(INCDIR)/parse_params.h
pip_fwrap.$(O) : $(INCDIR)/b3dutil.h
autodoc.$(O) : $(INCDIR)/autodoc.h
adoc_fwrap.$(O) : $(INCDIR)/autodoc.h
lsqr.$(O) : $(INCDIR)/lsqr.h cblas.h stdbool.h

#  $Log$
#  Revision 1.14  2009/11/21 21:15:25  mast
#  Added robuststat
#
#  Revision 1.13  2009/09/08 23:48:54  mast
#  Added openmp flag for b3dutil
#
#  Revision 1.12  2009/04/09 18:56:23  mast
#  Add openmp flag for cubinterp
#
#  Revision 1.11  2009/02/16 06:15:43  mast
#  Added parallelwrite
#
#  Revision 1.10  2008/12/30 00:52:09  mast
#  Added lsqr stuff
#
#  Revision 1.9  2008/12/01 15:30:39  mast
#  Added percentile and beadutil
#
#  Revision 1.8  2008/11/18 22:42:51  mast
#  Added amat_to_rotmagstr
#
#  Revision 1.7  2008/11/14 19:58:11  mast
#  Add simplestat
#
#  Revision 1.6  2008/11/12 05:07:27  mast
#  Add histogram
#
#  Revision 1.5  2008/06/24 04:44:28  mast
#  Added islice and taper
#
#  Revision 1.4  2007/10/01 15:26:09  mast
#  *** empty log message ***
#
#  Revision 1.3  2007/09/20 03:18:57  mast
#  Get the def entry right
#
#
#  Revision 1.1  2007/09/20 02:43:08  mast
#  Moved to new library
#
