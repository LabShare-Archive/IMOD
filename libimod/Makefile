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

all : $(LIB)

$(LIB) : $(IMODELOBJS)
	$(FIND) . -type f -name $(LIB) -exec rm "{}" \;
	$(MAKELIB) $(LIB) $(IMODELOBJS)
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
	touch $(IMODELOBJS)
	touch $(LIB)

#
# Some depends.
#
imodel.o : $(INCDIR)/imodel.h $(INCDIR)/iobj.h $(INCDIR)/icont.h \
$(INCDIR)/ipoint.h $(INCDIR)/imesh.h ../.version
iobj.o   : $(INCDIR)/iobj.h $(INCDIR)/icont.h $(INCDIR)/ipoint.h
imesh.o  : $(INCDIR)/imesh.h $(INCDIR)/icont.h $(INCDIR)/ipoint.h
icont.o  : $(INCDIR)/icont.h $(INCDIR)/ipoint.h
ipoint.o : $(INCDIR)/ipoint.h

iplane.o : $(INCDIR)/iplane.h
iview.o  : $(INCDIR)/iview.h
imat.o   : $(INCDIR)/imat.h
ilist.o  : $(INCDIR)/ilist.h
