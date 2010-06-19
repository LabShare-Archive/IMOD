#
# Makefile for libcfshr
#
# $Id$
# Use cvs-show-log to extract log
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
	robuststat.$(O) convexbound.$(O) statfuncs.$(O) surfacesort.$(O) \
	gaussj.$(O) find_piece_shifts.$(O)

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
