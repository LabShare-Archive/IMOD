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

IMODELOBJS = imodel.o iobj.o icont.o imodel_to.o\
             imodel_from.o ipoint.o imodel_files.o imesh.o\
             imodel_fwrap.o imat.o iplane.o iview.o\
	     ilist.o ilabel.o

PIPOBJS = parse_params.o pip_fwrap.o

all : $(LIB)

$(LIB) : $(IMODELOBJS) $(PIPOBJS)
	$(FIND) . -type f -name $(LIB) -exec rm "{}" \;
	$(MAKELIB) $(LIB) $(IMODELOBJS) $(PIPOBJS)
	$(RANLIB) $(LIB)
	$(CP) $(LIB) $(LIBDIR)

install : $(LIB)
	$(CP) $(LIB) $(INSTLIBDIR)

clean : 
	$(FIND) . -type f -name $(LIB) -exec rm "{}" \;
	$(FIND) . -type f -name "*.o"  -exec rm "{}" \;
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
imodel.o : $(INCDIR)/iobj.h $(INCDIR)/icont.h \
$(INCDIR)/ipoint.h $(INCDIR)/imesh.h ../.version
iobj.o   : $(INCDIR)/iobj.h $(INCDIR)/icont.h $(INCDIR)/ipoint.h
imesh.o  : $(INCDIR)/imesh.h $(INCDIR)/icont.h $(INCDIR)/ipoint.h
icont.o  : $(INCDIR)/icont.h $(INCDIR)/ipoint.h
ipoint.o : $(INCDIR)/ipoint.h
iplane.o : $(INCDIR)/iplane.h
iview.o  : $(INCDIR)/iview.h
imat.o   : $(INCDIR)/imat.h
ilist.o  : $(INCDIR)/ilist.h
$(PIPOBJS) : $(INCDIR)parse_params.h
