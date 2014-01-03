include ../configure

O        = $(OBJEXT)
LIBHVEM  = libhvem.$(DLLEXT)
LIBMRCIM    = libmrcim.$(DLLEXT)
LIBNMCAR = libdnmncar.$(NMCAREXT)
LIBCOMPAT = libb3dcmpt.$(DLLEXT)
LIBTRACK  = libtrack.$(DLLEXT)
LIBSMLMOD = libsmlmod.$(WMODEXT)
LIBWMOD = libwmod.$(WMODEXT)
LIBBLAS = libb3dblas.$(DLLEXT)
LIBLAPACK = libb3dlapk.$(DLLEXT)

COMPATSRC = compat/ransub.f compat/ourrnd.c compat/degtrig.f $(DATESRC)
COMPATOBJ = compat/ransub.$(O) compat/ourrnd.$(O) compat/degtrig.$(O) \
  $(DIRDATEOBJ)

LIBS = $(LIBHVEM) $(LIBMRCIM) $(LIBNMCAR) $(LIBTRACK) $(LINUXFLIBS) \
 $(LIBWMOD) $(LIBSMLMOD) $(LIBBLAS) $(LIBLAPACK)

all : $(LIBS)

$(LIBHVEM): $(LIBMRCIM) piecesubs/*.f hvem/*.f hvem/*.f90 xfsubs/*.f statsubs/*.f \
	statsubs/*.f90 
	(cd piecesubs ; $(MAKE) all)
	(cd hvem ; $(MAKE) all)
	(cd xfsubs ; $(MAKE) all)
	(cd statsubs ; $(MAKE) all)
	$(RM) $(LIBHVEM)
	$(FAR) $(FARFLAGS)$(LIBHVEM) $(LIBHVEMDEF) \
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

$(LIBMRCIM): imsubs/*.f imsubs/*.f90 imsubs/*.c
	(cd imsubs ; $(MAKE) all)
	$(RM) $(LIBMRCIM)
	$(FAR) $(FARFLAGS)$(LIBMRCIM) $(LIBMRCIMDEF) imsubs/$(IMSUBSOBJ)
	$(RANLIB) $(LIBMRCIM)
	$(CP) libmrcim.$(LIBEXT) $(LIBDIR)

# libblockio eliminated version 3.15, 6/28/06

$(LIBNMCAR):	graphics/*.f graphics/*.f90 graphics/*.c graphics/*.cpp
	(cd graphics ; $(MAKE) all)
	$(RM) $(LIBNMCAR)
	$(MAKENMCAR)$(LIBNMCAR) graphics/*.$(O)
	$(RANLIB) $(LIBNMCAR)
	$(CP) libdnmncar.$(NMCAREXT) $(LIBDIR)

$(LIBCOMPAT):	$(COMPATSRC) 
	(cd compat; $(MAKE) all)
	$(RM) $(LIBCOMPAT)
	$(FAR) $(FARFLAGS)$(LIBCOMPAT) $(COMPATOBJ)
	$(RANLIB) $(LIBCOMPAT)
	$(CP) libb3dcmpt.$(LIBEXT) $(LIBDIR)

$(LIBBLAS): blas/*.f
	(cd blas; $(MAKE) all)
	$(RM) $(LIBBLAS)
	$(FAR) $(FARFLAGS)$(LIBBLAS) $(LIBBLASDEF) blas/*.$(O)
	$(RANLIB) $(LIBBLAS)
	$(CP) libb3dblas.$(LIBEXT) $(LIBDIR)

$(LIBLAPACK): $(LIBBLAS) lapack/*.f
	(cd lapack; $(MAKE) all)
	$(RM) $(LIBLAPACK)
	$(FAR) $(FARFLAGS)$(LIBLAPACK) $(LIBLAPACKDEF) lapack/*.$(O)
	$(RANLIB) $(LIBLAPACK)
	$(CP) libb3dlapk.$(LIBEXT) $(LIBDIR)

$(LIBTRACK):	linetrack/*.f linetrack/*.c
	(cd linetrack; $(MAKE) all)
	$(RM) $(LIBTRACK)
	$(FAR) $(FARFLAGS)$(LIBTRACK) $(LIBTRACKDEF) linetrack/*.$(O)
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
	$(FIND) . -type f -name "*.$(OBJEXT)" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
	$(FIND) . -type f -name "moc*" -exec rm "{}" \;
