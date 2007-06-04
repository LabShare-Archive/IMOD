include ../configure

O        = $(OBJEXT)
LIBHVEM  = libhvem.$(DLLEXT)
LIBIFFT  = libifft.$(DLLEXT)
LIBIM    = libim.$(DLLEXT)
LIBNMCAR = libdnmncar.$(NMCAREXT)
LIBCOMPAT = libb3dcmpt.$(DLLEXT)
LIBTRACK  = libtrack.$(DLLEXT)
LIBSMLMOD = libsmlmod.$(WMODEXT)
LIBWMOD = libwmod.$(WMODEXT)

COMPATSRC = compat/ransub.f compat/ourrnd.c compat/degtrig.f $(DATESRC)
COMPATOBJ = compat/ransub.$(O) compat/ourrnd.$(O) compat/degtrig.$(O) \
  $(DIRDATEOBJ)

LIBS = $(LIBHVEM) $(LIBIM) $(LIBNMCAR) $(LIBTRACK) $(LINUXFLIBS) \
 $(LIBWMOD) $(LIBSMLMOD)

all : $(LIBS)


$(LIBHVEM): $(LIBIM) piecesubs/*.f hvem/*.f xfsubs/*.f statsubs/*.f
	(cd piecesubs ; $(MAKE) all)
	(cd hvem ; $(MAKE) all)
	(cd xfsubs ; $(MAKE) all)
	(cd statsubs ; $(MAKE) all)
	$(RM) $(LIBHVEM)
	$(FAR) $(ARFLAGS)$(LIBHVEM) $(LIBHVEMDEF) \
	piecesubs/*.$(O) hvem/*.$(O) xfsubs/*.$(O) statsubs/*.$(O)
	$(RANLIB) $(LIBHVEM)
	$(CP) libhvem.$(LIBEXT) $(LIBDIR)

$(LIBWMOD): model/*.f $(INCDIR)/model.inc
	(cd model ; $(MAKE) all)
	$(RM) $(LIBWMOD)
	$(MAKEWMOD)$(LIBWMOD) model/*.$(O)
	$(RANLIB) $(LIBWMOD)
	$(CP) $(LIBWMOD) $(LIBDIR)

$(LIBSMLMOD): model/*.f $(INCDIR)/smallmodel.inc
	(cd smallmodel ; $(MAKE) all)
	$(MAKEWMOD)$(LIBSMLMOD) smallmodel/*.$(O)
	$(RANLIB) $(LIBSMLMOD)
	$(CP) libsmlmod.$(LIBEXT) $(LIBDIR)

$(LIBIM): imsubs/*.f imsubs/*.c imsubs2/*.f
	(cd imsubs ; $(MAKE) all)
	(cd imsubs2 ; $(MAKE) all)
	$(RM) $(LIBIM)
	$(FAR) $(ARFLAGS)$(LIBIM) $(LIBIMDEF) imsubs/$(IMSUBSOBJ) \
	imsubs2/*.$(O)
	$(RANLIB) $(LIBIM)
	$(CP) libim.$(LIBEXT) $(LIBDIR)

# libblockio eliminated version 3.15, 6/28/06

$(LIBIFFT):	ifftsub/*.f
	(cd ifftsub ; $(MAKE) all)
	$(RM) $(LIBIFFT)
	$(FAR) $(ARFLAGS)$(LIBIFFT) $(LIBIFFTDEF) ifftsub/*.$(O)
	$(RANLIB) $(LIBIFFT)
	$(CP) libifft.$(LIBEXT) $(LIBDIR)

$(LIBNMCAR):	graphics/*.f graphics/*.c*
	(cd graphics ; $(MAKE) all)
	$(RM) $(LIBNMCAR)
	$(MAKENONDLL)$(LIBNMCAR) graphics/*.$(O)
	$(RANLIB) $(LIBNMCAR)
	$(CP) libdnmncar.$(NMCAREXT) $(LIBDIR)

$(LIBCOMPAT):	$(COMPATSRC) 
	(cd compat; $(MAKE) all)
	$(RM) $(LIBCOMPAT)
	$(FAR) $(ARFLAGS)$(LIBCOMPAT) $(COMPATOBJ)
	$(RANLIB) $(LIBCOMPAT)
	$(CP) libb3dcmpt.$(LIBEXT) $(LIBDIR)

$(LIBTRACK):	linetrack/*.f linetrack/*.c*
	(cd linetrack; $(MAKE) all)
	$(RM) $(LIBTRACK)
	$(FAR) $(ARFLAGS)$(LIBTRACK) $(LIBTRACKDEF) linetrack/*.$(O)
	$(RANLIB) $(LIBTRACK)
	$(CP) libtrack.$(LIBEXT) $(LIBDIR)

track:  $(LIBTRACK)

install:
	$(CP) $(LIBS) $(INSTLIBDIR)

clean : 
#	$(RM) $(LIBS)
#	$(RM) *.o */*.o *~ */*~ *.a
	$(FIND) . -type f -name "*.so" -exec rm "{}" \;
	$(FIND) . -type f -name "*.a" -exec rm "{}" \;
	$(FIND) . -type f -name "*.*lib" -exec rm "{}" \;
	$(FIND) . -type f -name "*.dll" -exec rm "{}" \;
	$(FIND) . -type f -name "*.exp" -exec rm "{}" \;
	$(FIND) . -type f -name "*.o*" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
