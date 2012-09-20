#
# Makefile for libimod
#
# $Id$
#
include ../configure

LIBI   = libimod.$(DLLEXT)
O     = $(OBJEXT)
CFLAGS = $(LIBCFLAGS)

IMODELOBJS = imodel.$(O) iobj.$(O) icont.$(O) imodel_to.$(O)\
             imodel_from.$(O) ipoint.$(O) imodel_files.$(O) imesh.$(O)\
             imodel_fwrap.$(O) imat.$(O) iplane.$(O) iview.$(O)\
	     ilabel.$(O) istore.$(O) objgroup.$(O) autocont.$(O)

all : $(LIBI)

$(LIBI) : $(IMODELOBJS)
	$(FIND) . -type f -name $(LIBI) -exec rm "{}" \;
	$(MAKELIB)$(LIBI) $(LIBIMODDEF) $(IMODELOBJS)
	$(RANLIB) $(LIBI)
	$(CP) libimod.$(LIBEXT) $(LIBDIR)

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
$(IMODELOBJS) : $(INCDIR)/imodel.h $(INCDIR)/ilist.h
imodel.$(O) : $(INCDIR)/iobj.h $(INCDIR)/icont.h \
$(INCDIR)/ipoint.h $(INCDIR)/imesh.h $(INCDIR)/b3dutil.h
iobj.$(O)   : $(INCDIR)/iobj.h $(INCDIR)/icont.h $(INCDIR)/ipoint.h
imesh.$(O)  : $(INCDIR)/imesh.h $(INCDIR)/icont.h $(INCDIR)/ipoint.h
icont.$(O)  : $(INCDIR)/icont.h $(INCDIR)/ipoint.h $(INCDIR)/b3dutil.h
ipoint.$(O) : $(INCDIR)/ipoint.h
iplane.$(O) : $(INCDIR)/iplane.h
iview.$(O)  : $(INCDIR)/iview.h
imat.$(O)   : $(INCDIR)/imat.h
istore.$(O)  : $(INCDIR)/istore.h 
objgroup.$(O)  : $(INCDIR)/objgroup.h 
imodel_files.$(O) : $(INCDIR)/b3dutil.h $(INCDIR)/istore.h $(INCDIR)/objgroup.h
imodel_from.$(O) : $(INCDIR)/b3dutil.h
