include ../configure


LIBHVEM  = libhvem.$(LIBEXT)
LIBIFFT  = libifft.$(LIBEXT)
LIBIM    = libim.$(LIBEXT)
LIBNMCAR = libdnmncar.$(LIBEXT)
LIBDTRIGS = libdtrigs.$(LIBEXT)
LIBRANDM  = librandm.$(LIBEXT)

LIBS = $(LIBHVEM) $(LIBIFFT) $(LIBIM) $(LIBNMCAR) $(LINUXFLIBS)


all : $(LIBS) $(BLOCKIO)


$(LIBHVEM):	model/*.f piecesubs/*.f hvem/*.f xfsubs/*.f statsubs/*.f
	(cd model ; $(MAKE) all)
	(cd piecesubs ; $(MAKE) all)
	(cd hvem ; $(MAKE) all)
	(cd xfsubs ; $(MAKE) all)
	(cd statsubs ; $(MAKE) all)
	\rm -f $(LIBHVEM)
	$(AR) $(ARFLAGS) $(LIBHVEM) \
	model/*.o piecesubs/*.o hvem/*.o xfsubs/*.o statsubs/*.o
	$(RANLIB) $(LIBHVEM)
	$(CP) $(LIBHVEM) $(LIBDIR)

$(LIBIM):	imsubs/*.f imsubs/*.c imsubs2/*.f
	(cd imsubs ; $(MAKE) all)
	(cd imsubs2 ; $(MAKE) all)
	\rm -f $(LIBIM)
	$(AR) $(ARFLAGS) $(LIBIM) imsubs/$(IMSUBSOBJ) imsubs2/*.o
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
	$(AR) $(ARFLAGS) $(LIBIFFT) ifftsub/*.o
	$(RANLIB) $(LIBIFFT)
	$(CP) $(LIBIFFT) $(LIBDIR)

$(LIBNMCAR):	graphics/*.f graphics/*.c
	(cd graphics ; $(MAKE) all)
	\rm -f $(LIBNMCAR)
	$(AR) $(ARFLAGS) $(LIBNMCAR) graphics/*.o
	$(RANLIB) $(LIBNMCAR)
	$(CP) $(LIBNMCAR) $(LIBDIR)

$(LIBDTRIGS):	dtrigs/*.f
	(cd dtrigs ; $(MAKE) all)
	\rm -f $(LIBDTRIGS)
	$(AR) $(ARFLAGS) $(LIBDTRIGS) dtrigs/*.o
	$(RANLIB) $(LIBDTRIGS)
	$(CP) $(LIBDTRIGS) $(LIBDIR)

$(LIBRANDM):	randm/*.f randm/*.c
	(cd randm; $(MAKE) all)
	\rm -f $(LIBRANDM)
	$(AR) $(ARFLAGS) $(LIBRANDM) randm/*.o
	$(RANLIB) $(LIBRANDM)
	$(CP) $(LIBRANDM) $(LIBDIR)


install:
	$(CP) $(LIBS) $(INSTLIBDIR)

clean : 
#	\rm -f $(LIBS)
#	\rm -f *.o */*.o *~ */*~ *.a
	$(FIND) . -type f -name "*.so" -exec rm "{}" \;
	$(FIND) . -type f -name "*.a" -exec rm "{}" \;
	$(FIND) . -type f -name "*.dylib" -exec rm "{}" \;
	$(FIND) . -type f -name "*.o" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
