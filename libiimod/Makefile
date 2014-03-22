##############################################################################
#
# libiimod - C library for handling mrc and other types of files and images
#
#  $Id$
#
include ../configure

O      = $(OBJEXT)
LIBI   = libiimod.$(DLLEXT)
LIBD   = $(DUMMYTIFF)
LIBS   = $(LIBI) $(LIBD)
CFLAGS = $(LIBCFLAGS)

OBJS  = iimage.$(O) iitif.$(O) iimrc.$(O) mrcfiles.$(O) mrcsec.$(O) unit_header.$(O) \
 mrcslice.$(O) plist.$(O) sliceproc.$(O) diffusion.$(O) iilikemrc.$(O) unit_fileio.$(O) \
 $(HDFOBJS)

INCS  = $(INCDIR)/mrcfiles.h $(INCDIR)/mrcslice.h \
 $(INCDIR)/mrcc.h $(INCDIR)/b3dutil.h

all : $(LIBS)

mrcslice.$(O) : mrcslice.c
	$(CC) $(CFLAGS) $(OPENMP) -c mrcslice.c

$(LIBI) : $(OBJS)
	$(FIND) . -type f -name $(LIBI) -exec rm "{}" \;
	$(MAKELIB)$(LIBI) $(OBJS) $(LIBIIMODDEF)
	$(RANLIB) $(LIBI)
	$(CP) libiimod.$(LIBEXT) $(LIBDIR)

# Here and in install, need to wipe out any existing libtiff to make sure
# that the new one is the only one present in the target directory

$(LIBD) : tiffstub.$(O) ALWAYS
	$(FIND) . -type f -name $(LIBD) -exec rm -f "{}" \;
	$(MAKELIB)$(LIBD) tiffstub.$(O)
	$(RANLIB) $(LIBD)
	$(FIND) $(LIBDIR) -type f -name "*libtiff.[^l]*" -exec rm -f "{}" \;
	$(CP) $(LIBD) $(LIBDIR)

install : $(LIBS)
	$(FIND) $(INSTLIBDIR) -type f -name "*libtiff.[^l]*" -exec rm -f "{}" \;
	$(CP) $(LIBS) $(INSTLIBDIR)

clean : 
	$(FIND) . -type f -name "*.$(OBJEXT)" -exec rm "{}" \;
	$(FIND) . -type f -name $(LIBI) -exec rm -f "{}" \;
	$(FIND) . -type f -name "*.dll" -exec rm -f "{}" \;
	$(FIND) . -type f -name "*.so" -exec rm -f "{}" \;
	$(FIND) . -type f -name "*.a" -exec rm -f "{}" \;
	$(FIND) . -type f -name "*.*lib" -exec rm -f "{}" \;
	$(FIND) . -type f -name "*.exp" -exec rm "{}" \;
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
iimage.$(O) : $(INCDIR)/iimage.h
iimrc.$(O) : $(INCDIR)/iimage.h
iilikemrc.$(O) : $(INCDIR)/iimage.h
iitif.$(O) : $(INCDIR)/iimage.h
sliceproc.$(O) : $(INCDIR)/sliceproc.h
unit_fileio.$(O) : $(INCDIR)/iiunit.h $(INCDIR)/iimage.h
unit_header.$(O) : $(INCDIR)/iiunit.h $(INCDIR)/iimage.h

ALWAYS :

