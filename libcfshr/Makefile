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
	insidecontour.$(O) islice.$(O) taperatfill.$(O)

PIPOBJS = parse_params.$(O) pip_fwrap.$(O) autodoc.$(O) adoc_fwrap.$(O)

all : $(LIBI)

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

#  $Log$
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
