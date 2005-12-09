include ../configure

O        = $(OBJEXT)
LIBHVEM  = libhvem.$(DLLEXT)
LIBIFFT  = libifft.$(DLLEXT)
LIBIM    = libim.$(DLLEXT)
LIBNMCAR = libdnmncar.$(NMCAREXT)
LIBCOMPAT = libb3dcmpt.$(DLLEXT)
LIBTRACK  = libtrack.$(DLLEXT)

COMPATSRC = compat/ransub.f compat/ourrnd.c compat/degtrig.f $(DATESRC)
COMPATOBJ = compat/ransub.$(O) compat/ourrnd.$(O) compat/degtrig.$(O) \
  $(DIRDATEOBJ)

LIBS = $(LIBHVEM) $(LIBIM) $(LIBNMCAR) $(LIBTRACK) $(LINUXFLIBS)

all : $(LIBS) $(BLOCKIO) $(LIBWMOD)


$(LIBHVEM): $(LIBIM) model/*.f piecesubs/*.f hvem/*.f xfsubs/*.f statsubs/*.f \
	$(INCDIR)/model.inc
	(cd model ; $(MAKE) all)
	(cd piecesubs ; $(MAKE) all)
	(cd hvem ; $(MAKE) all)
	(cd xfsubs ; $(MAKE) all)
	(cd statsubs ; $(MAKE) all)
	\rm -f $(LIBHVEM)
	$(FAR) $(ARFLAGS)$(LIBHVEM) $(LIBHVEMDEF) \
	$(MODELALLO) piecesubs/*.$(O) hvem/*.$(O) xfsubs/*.$(O) statsubs/*.$(O)
	$(RANLIB) $(LIBHVEM)
	$(CP) libhvem.$(LIBEXT) $(LIBDIR)

libwmod.lib: model/*.f $(INCDIR)/model.inc
	(cd model ; $(MAKE) all)
	\rm -f $(LIBWMOD)
	$(MAKENONDLL)$(LIBWMOD) model/*.$(O)
	$(RANLIB) $(LIBWMOD)
	$(CP) $(LIBWMOD) $(LIBDIR)

$(LIBIM): imsubs/*.f imsubs/*.c imsubs2/*.f
	(cd imsubs ; $(MAKE) all)
	(cd imsubs2 ; $(MAKE) all)
	\rm -f $(LIBIM)
	$(FAR) $(ARFLAGS)$(LIBIM) $(LIBIMDEF) imsubs/$(IMSUBSOBJ) \
	imsubs2/*.$(O)
	$(RANLIB) $(LIBIM)
	$(CP) libim.$(LIBEXT) $(LIBDIR)

libblockio.a:  imsubs/blockio.c
	(cd imsubs; make blockio.o)
	\rm -f libblockio.a
	ar ruv libblockio.a imsubs/blockio.o
	ranlib libblockio.a
	$(CP) libblockio.a $(LIBDIR)

$(LIBIFFT):	ifftsub/*.f
	(cd ifftsub ; $(MAKE) all)
	\rm -f $(LIBIFFT)
	$(FAR) $(ARFLAGS)$(LIBIFFT) $(LIBIFFTDEF) ifftsub/*.$(O)
	$(RANLIB) $(LIBIFFT)
	$(CP) libifft.$(LIBEXT) $(LIBDIR)

$(LIBNMCAR):	graphics/*.f graphics/*.c*
	(cd graphics ; $(MAKE) all)
	\rm -f $(LIBNMCAR)
	$(MAKENONDLL)$(LIBNMCAR) graphics/*.$(O)
	$(RANLIB) $(LIBNMCAR)
	$(CP) libdnmncar.$(NMCAREXT) $(LIBDIR)

$(LIBCOMPAT):	$(COMPATSRC) 
	(cd compat; $(MAKE) all)
	\rm -f $(LIBCOMPAT)
	$(FAR) $(ARFLAGS)$(LIBCOMPAT) $(COMPATOBJ)
	$(RANLIB) $(LIBCOMPAT)
	$(CP) libb3dcmpt.$(LIBEXT) $(LIBDIR)

$(LIBTRACK):	linetrack/*.f linetrack/*.c*
	(cd linetrack; $(MAKE) all)
	\rm -f $(LIBTRACK)
	$(FAR) $(ARFLAGS)$(LIBTRACK) $(LIBTRACKDEF) linetrack/*.$(O)
	$(RANLIB) $(LIBTRACK)
	$(CP) libtrack.$(LIBEXT) $(LIBDIR)


install:
	$(CP) $(LIBS) $(INSTLIBDIR)

clean : 
#	\rm -f $(LIBS)
#	\rm -f *.o */*.o *~ */*~ *.a
	$(FIND) . -type f -name "*.so" -exec rm "{}" \;
	$(FIND) . -type f -name "*.a" -exec rm "{}" \;
	$(FIND) . -type f -name "*.*lib" -exec rm "{}" \;
	$(FIND) . -type f -name "*.dll" -exec rm "{}" \;
	$(FIND) . -type f -name "*.exp" -exec rm "{}" \;
	$(FIND) . -type f -name "*.o*" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
