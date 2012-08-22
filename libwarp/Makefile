#
# Makefile for libiwarp
#
# $Id$
#
include ../configure

LIBW   = libiwarp.$(DLLEXT)
O     = $(OBJEXT)
CFLAGS = $(LIBCFLAGS)

NNOBJS = delaunay.$(O) hash.$(O) istack.$(O) lpi.$(O) minell.$(O) nnai.$(O) \
	nnpi.$(O) nncommon.$(O) nncommon-vulnerable.$(O)

#TRIOBJS = triangle.$(O)
TRIOBJS = hullwrap.$(O) hull.$(O) hull-fg.$(O) hull-ch.$(O) hull-io.$(O) pointops.$(O)
WARPOBJS = warpfiles.$(O) warputils.$(O) warpwrapfort.$(O) warpinterp.$(O) \
	maggradfield.$(O)

all : $(LIBW)

$(LIBW) : $(NNOBJS) $(TRIOBJS) $(WARPOBJS)
	$(FIND) . -type f -name $(LIBW) -exec rm "{}" \;
	$(MAKELIB)$(LIBW) $(LIBIWARPDEF) $(NNOBJS) $(TRIOBJS) $(WARPOBJS)
	$(RANLIB) $(LIBW)
	$(CP) libiwarp.$(LIBEXT) $(LIBDIR)

triangle.$(O): triangle.c
	$(CC) $(CFLAGS) $(NNVULNFLAGS) -DTRILIBRARY -c triangle.c

nncommon-vulnerable.$(O): nncommon-vulnerable.c
	$(CC) $(CFLAGS) $(NNVULNFLAGS) -c nncommon-vulnerable.c

delaunay.$(O):	delaunay.c
	$(CC) $(CFLAGS) -c delaunay.c -DCLARKSON_HULL 

warputils.$(O) : warputils.c
	$(CC) $(CFLAGS) $(OPENMP) -c warputils.c

warpinterp.$(O) : warpinterp.c
	$(CC) $(CFLAGS) $(OPENMP) -c warpinterp.c

install : $(LIBW)
	$(CP) $(LIBW) $(INSTLIBDIR)

clean : 
	$(FIND) . -type f -name $(LIBW) -exec rm "{}" \;
	$(FIND) . -type f -name "*.exp" -exec rm "{}" \;
	$(FIND) . -type f -name "*.$(OBJEXT)"  -exec rm "{}" \;
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"  -exec rm "{}" \;

#
# Some depends.
#
$(WARPOBJS):	$(INCDIR)/warpfiles.h
delaunay.$(O):  hullwrap.h
