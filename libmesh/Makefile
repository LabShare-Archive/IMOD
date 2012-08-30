#
# Makefile for libimesh
#
# $Id$
#
# Log at end
#
include ../configure

LIBI   = libimesh.$(DLLEXT)
O     = $(OBJEXT)
CFLAGS = $(LIBCFLAGS)

OBJS = skinobj.$(O) mkmesh.$(O) remesh.$(O) objprep.$(O)

all : $(LIBI)

$(LIBI) : $(OBJS)
	$(FIND) . -type f -name $(LIBI) -exec rm "{}" \;
	$(MAKELIB)$(LIBI) $(LIBIMESHDEF) $(OBJS)
	$(RANLIB) $(LIBI)
	$(CP) libimesh.$(LIBEXT) $(LIBDIR)

install : $(LIBI)
	$(CP) $(LIBI) $(INSTLIBDIR)

clean : 
	$(FIND) . -type f -name $(LIBI) -exec rm "{}" \;
	$(FIND) . -type f -name "*.exp" -exec rm "{}" \;
	$(FIND) . -type f -name "*.$(OBJEXT)"  -exec rm "{}" \;
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"  -exec rm "{}" \;

#
# Some depends.
#
$(OBJS) : $(INCDIR)/mkmesh.h $(INCDIR)/iobj.h $(INCDIR)/icont.h \
$(INCDIR)/ipoint.h $(INCDIR)/imesh.h $(INCDIR)/b3dutil.h $(INCDIR)/imodel.h

