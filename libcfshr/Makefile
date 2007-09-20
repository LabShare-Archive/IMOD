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
	samplemeansd.$(O) colormap.$(O) cubinterp.$(O) reduce_by_binning.$(O)

PIPOBJS = parse_params.$(O) pip_fwrap.$(O) autodoc.$(O) adoc_fwrap.$(O)

all : $(LIBI)

$(LIBI) : $(UTILOBJS) $(PIPOBJS)
	$(FIND) . -type f -name $(LIBI) -exec rm "{}" \;
	$(MAKELIB)$(LIBI) $(LIBIMODDEF) $(UTILOBJS) $(PIPOBJS)
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
$(UTILOBJS) : $(INCDIR)/b3dutil.h
ilist.c : $(INCDIR)/ilist.h
b3dutil.$(O) :  ../.version
$(PIPOBJS) : $(INCDIR)/parse_params.h
pip_fwrap.$(O) : $(INCDIR)/b3dutil.h
autodoc.$(O) : $(INCDIR)/autodoc.h
adoc_fwrap.$(O) : $(INCDIR)/autodoc.h

#  $Log$
