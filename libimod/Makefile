#
# Makefile for libimod.a
#
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
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
#
include ../configure

LIB   = libimod.$(LIBEXT)
O     = $(OBJEXT)


IMODELOBJS = imodel.$(O) iobj.$(O) icont.$(O) imodel_to.$(O)\
             imodel_from.$(O) ipoint.$(O) imodel_files.$(O) imesh.$(O)\
             imodel_fwrap.$(O) imat.$(O) iplane.$(O) iview.$(O)\
	     ilist.$(O) ilabel.$(O) b3dutil.$(O)

PIPOBJS = parse_params.$(O) pip_fwrap.$(O)

all : $(LIB)

$(LIB) : $(IMODELOBJS) $(PIPOBJS)
	$(FIND) . -type f -name $(LIB) -exec rm "{}" \;
	$(MAKELIB)$(LIB) $(IMODELOBJS) $(PIPOBJS)
	$(RANLIB) $(LIB)
	$(CP) $(LIB) $(LIBDIR)

install : $(LIB)
	$(CP) $(LIB) $(INSTLIBDIR)

clean : 
	$(FIND) . -type f -name $(LIB) -exec rm "{}" \;
	$(FIND) . -type f -name "*.o*"  -exec rm "{}" \;
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"  -exec rm "{}" \;

# make a working version of the lib while debugging
# other subsystems
work : clean
	touch $(IMODELOBJS) $(PIPOBJS)
	touch $(LIB)

#
# Some depends.
#
$(IMODELOBJS) : $(INCDIR)imodel.h
imodel.$(O) : $(INCDIR)/iobj.h $(INCDIR)/icont.h \
$(INCDIR)/ipoint.h $(INCDIR)/imesh.h $(INCDIR)/b3dutil.h
iobj.$(O)   : $(INCDIR)/iobj.h $(INCDIR)/icont.h $(INCDIR)/ipoint.h
imesh.$(O)  : $(INCDIR)/imesh.h $(INCDIR)/icont.h $(INCDIR)/ipoint.h
icont.$(O)  : $(INCDIR)/icont.h $(INCDIR)/ipoint.h $(INCDIR)/b3dutil.h
ipoint.$(O) : $(INCDIR)/ipoint.h
iplane.$(O) : $(INCDIR)/iplane.h
iview.$(O)  : $(INCDIR)/iview.h
imat.$(O)   : $(INCDIR)/imat.h
ilist.$(O)  : $(INCDIR)/ilist.h
b3dutil.$(O) : $(INCDIR)/b3dutil.h  ../.version
imodel_files.$(O) : $(INCDIR)/b3dutil.h
imodel_from.$(O) : $(INCDIR)/b3dutil.h
$(PIPOBJS) : $(INCDIR)parse_params.h
pip_fwrap.$(O) : $(INCDIR)/b3dutil.h
