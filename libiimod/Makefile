##############################################################################
#
# libiimod.a - C library for handling mrc and other types of files and images
#
include ../configure

LIBI   = libiimod.$(LIBEXT)
LIBD   = $(DUMMYTIFF).$(LIBEXT)
LIBS   = $(LIBI) $(LIBD)

OBJS  = iimage.o iitif.o iimrc.o mrcfiles.o mrcsec.o mrcslice.o mrcproc.o \
mrcspectral.o plist.o

INCS  = $(INCDIR)mrcfiles.h $(INCDIR)mrcslice.h $(INCDIR)mrcproc.h \
$(INCDIR)mrcspectral.h $(INCDIR)mrcc.h

all : $(LIBS)

$(LIBI) : $(OBJS)
	$(FIND) . -type f -name $(LIBI) -exec rm "{}" \;
	$(MAKELIB) $(LIBI) $(OBJS)
	$(RANLIB) $(LIBI)
	$(CP) $(LIBI) $(LIBDIR)

# Here and in install, need to wipe out any existing libtiff to make sure
# that the new one is the only one present in the target directory

$(LIBD) : tiffstub.o ALWAYS
	$(FIND) . -type f -name $(LIBD) -exec rm -f "{}" \;
	$(MAKELIB) $(LIBD) tiffstub.o
	$(RANLIB) $(LIBD)
	$(FIND) $(LIBDIR) -type f -name "*libtiff.*" -exec rm -f "{}" \;
	$(CP) $(LIBD) $(LIBDIR)

install : $(LIBS)
	$(FIND) $(INSTLIBDIR) -type f -name "*libtiff.*" -exec rm -f "{}" \;
	$(CP) $(LIBS) $(INSTLIBDIR)

clean : 
	$(FIND) . -type f -name "*.o" -exec rm "{}" \;
	$(FIND) . -type f -name $(LIBI) -exec rm -f "{}" \;
	$(FIND) . -type f -name $(LIBD) -exec rm -f "{}" \;
	$(FIND) . -name "*~" -exec rm "{}" \;
	$(FIND) . -name "#*#"  -exec rm "{}" \;

# make a working version of the lib while debugging
# other subsystems
work : clean
	touch $(OBJS)
	touch $(LIBS)

#
# Some depends.
#
$(OBJS) : $(INCS)
iimage.o : $(INCDIR)iimage.h
iimrc.o : $(INCDIR)iimage.h
iitif.o : $(INCDIR)iimage.h

ALWAYS :


