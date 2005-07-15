##############################################################################
#
# libiimod.a - C library for handling mrc and other types of files and images
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#  Revision 3.11  2005/01/27 05:56:56  mast
#  Added anisotropic diffusion
#
#  Revision 3.10  2005/01/18 22:08:30  mast
#  Needed to add / after INCDIR
#
#  Revision 3.9  2005/01/07 20:00:12  mast
#  Added sliceproc
#
#  Revision 3.8  2004/04/08 17:04:36  mast
#  Changed to use a separate LIBCFLAGS variable
#
#  Revision 3.7  2003/12/30 17:29:15  mast
#  changes for windows DLLs and to produce nolibtiff only when needed
#
#  Revision 3.6  2003/11/04 17:21:20  mast
#  Add dependencies on b3dutil.h
#
#
include ../configure

O      = $(OBJEXT)
LIBI   = libiimod.$(DLLEXT)
LIBD   = $(DUMMYTIFF)
LIBS   = $(LIBI) $(LIBD)
CFLAGS = $(LIBCFLAGS)

OBJS  = iimage.$(O) iitif.$(O) iimrc.$(O) mrcfiles.$(O) mrcsec.$(O) \
 mrcslice.$(O) mrcproc.$(O) mrcspectral.$(O) plist.$(O) sliceproc.$(O) \
 diffusion.$(O)

INCS  = $(INCDIR)/mrcfiles.h $(INCDIR)/mrcslice.h $(INCDIR)/mrcproc.h \
 $(INCDIR)/mrcspectral.h $(INCDIR)/mrcc.h $(INCDIR)/b3dutil.h

all : $(LIBS)

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
	$(FIND) . -type f -name "*.o*" -exec rm "{}" \;
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
iitif.$(O) : $(INCDIR)/iimage.h
sliceproc.$(O) : $(INCDIR)/sliceproc.h

ALWAYS :


