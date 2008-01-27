#
# Makefile for libimod
#
# $Id$
# Log at end
#
include ../configure

LIBI   = libimod.$(DLLEXT)
O     = $(OBJEXT)
CFLAGS = $(LIBCFLAGS)

IMODELOBJS = imodel.$(O) iobj.$(O) icont.$(O) imodel_to.$(O)\
             imodel_from.$(O) ipoint.$(O) imodel_files.$(O) imesh.$(O)\
             imodel_fwrap.$(O) imat.$(O) iplane.$(O) iview.$(O)\
	     ilabel.$(O) istore.$(O) objgroup.$(O)\

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
	$(FIND) . -type f -name "*.o*"  -exec rm "{}" \;
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

#  $Log$
#  Revision 3.15  2007/09/19 23:49:29  mast
#  Changes for library rearrangements
#
#  Revision 3.14  2006/10/17 18:15:30  mast
#  Added autodoc
#
#  Revision 3.13  2006/08/27 23:44:46  mast
#  Added colormap
#
#  Revision 3.12  2006/06/26 15:45:02  mast
#  Added samplemeansd
#
#  Revision 3.11  2006/06/05 16:29:22  mast
#  Added translation of amoeba
#
#  Revision 3.10  2005/06/20 22:25:07  mast
#  Added istore
#
#  Revision 3.9  2005/01/18 22:08:23  mast
#  Needed to add / after INCDIR
#
#  Revision 3.8  2004/09/28 22:18:55  mast
#  Added parselist
#
#  Revision 3.7  2004/04/08 17:03:56  mast
#  Changed to use a separate LIBCFLAGS variable
#
#  Revision 3.6  2003/12/30 17:28:54  mast
#  changes for windows DLLs
#
#  Revision 3.5  2003/11/04 17:18:35  mast
#  Add dependencies on b3dutil.h
#
#  Revision 3.4  2003/10/26 15:30:28  mast
#  fix dependency on version -> b3dutil.o
#
#  Revision 3.3  2003/10/24 03:01:06  mast
#  Changes for Windows/Intel
#
#  Revision 3.2  2003/06/05 00:08:50  mast
#  Added PIP files here since this library is already in Fortran builds
#
#  Revision 3.1  2002/07/21 19:26:59  mast
#  Added dependencies on imodel.h
#
#  Revision 3.0  2001/11/29 18:10:57  rickg
#  *** empty log message ***
#
#  Revision 1.2  2001/11/27 16:02:22  mast
#  Added dependency on .version file
#
