include ../configure

O        = $(OBJEXT)
LIBHVEM  = libhvem.$(LIBEXT)
LIBIFFT  = libifft.$(LIBEXT)
LIBIM    = libim.$(LIBEXT)
LIBNMCAR = libdnmncar.$(LIBEXT)
LIBDTRIGS = libdtrigs.$(LIBEXT)
LIBRANDM  = librandm.$(LIBEXT)
LIBTRACK  = libtrack.$(LIBEXT)

LIBS = $(LIBHVEM) $(LIBIFFT) $(LIBIM) $(LIBNMCAR) $(LIBTRACK) $(LINUXFLIBS)


all : $(LIBS) $(BLOCKIO)


$(LIBHVEM):	model/*.f piecesubs/*.f hvem/*.f xfsubs/*.f statsubs/*.f
	(cd model ; $(MAKE) all)
	(cd piecesubs ; $(MAKE) all)
	(cd hvem ; $(MAKE) all)
	(cd xfsubs ; $(MAKE) all)
	(cd statsubs ; $(MAKE) all)
	\rm -f $(LIBHVEM)
	$(AR) $(ARFLAGS)$(LIBHVEM) \
	model/*.$(O) piecesubs/*.$(O) hvem/*.$(O) xfsubs/*.$(O) statsubs/*.$(O)
	$(RANLIB) $(LIBHVEM)
	$(CP) $(LIBHVEM) $(LIBDIR)

$(LIBIM):	imsubs/*.f imsubs/*.c imsubs2/*.f
	(cd imsubs ; $(MAKE) all)
	(cd imsubs2 ; $(MAKE) all)
	\rm -f $(LIBIM)
	$(AR) $(ARFLAGS)$(LIBIM) imsubs/$(IMSUBSOBJ) imsubs2/*.$(O)
	$(RANLIB) $(LIBIM)
	$(CP) $(LIBIM) $(LIBDIR)

libblockio.a:  imsubs/blockio.c
	(cd imsubs; make blockio.o)
	\rm -f libblockio.a
	ar ruv libblockio.a imsubs/blockio.o
	ranlib libblockio.a
	$(CP) libblockio.a $(LIBDIR)

$(LIBIFFT):	ifftsub/*.f
	(cd ifftsub ; $(MAKE) all)
	\rm -f $(LIBIFFT)
	$(AR) $(ARFLAGS)$(LIBIFFT) ifftsub/*.$(O)
	$(RANLIB) $(LIBIFFT)
	$(CP) $(LIBIFFT) $(LIBDIR)

$(LIBNMCAR):	graphics/*.f graphics/*.c*
	(cd graphics ; $(MAKE) all)
	\rm -f $(LIBNMCAR)
	$(AR) $(ARFLAGS)$(LIBNMCAR) graphics/*.$(O)
	$(RANLIB) $(LIBNMCAR)
	$(CP) $(LIBNMCAR) $(LIBDIR)

$(LIBDTRIGS):	dtrigs/*.f
	(cd dtrigs ; $(MAKE) all)
	\rm -f $(LIBDTRIGS)
	$(AR) $(ARFLAGS)$(LIBDTRIGS) dtrigs/*.$(O)
	$(RANLIB) $(LIBDTRIGS)
	$(CP) $(LIBDTRIGS) $(LIBDIR)

$(LIBRANDM):	randm/ransub.f randm/ourrnd.c
	(cd randm; $(MAKE) all)
	\rm -f $(LIBRANDM)
	$(AR) $(ARFLAGS)$(LIBRANDM) randm/*.$(O)
	$(RANLIB) $(LIBRANDM)
	$(CP) $(LIBRANDM) $(LIBDIR)

$(LIBTRACK):	linetrack/*.f
	(cd linetrack; $(MAKE) all)
	\rm -f $(LIBTRACK)
	$(AR) $(ARFLAGS)$(LIBTRACK) linetrack/*.$(O)
	$(RANLIB) $(LIBTRACK)
	$(CP) $(LIBTRACK) $(LIBDIR)


install:
	$(CP) $(LIBS) $(INSTLIBDIR)

clean : 
#	\rm -f $(LIBS)
#	\rm -f *.o */*.o *~ */*~ *.a
	$(FIND) . -type f -name "*.so" -exec rm "{}" \;
	$(FIND) . -type f -name "*.a" -exec rm "{}" \;
	$(FIND) . -type f -name "*.*lib" -exec rm "{}" \;
	$(FIND) . -type f -name "*.o*" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
