############################################################################# 
# Makefile for BL3DEMC IMOD distribution.
#
# Copyright (C) 1996-2004
# by Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells
# ("BL3DEMC", formerly "BL3DFS")
# and the Regents of the University of Colorado.
#
# See dist/COPYRIGHT for full copyright notice.
#
#  $Id$
#
#############################################################################
# BUILD AND INSTALLATION INSTRUCTIONS:
#    All build instructions are now in the file BUILDING
#
# IF THERE IS NO TIFF LIBRARY ON YOUR SYSTEM: 
#    Add the -no_tiff flag whenever you give the setup command (not needed
#    for IRIX 6.0-6.2 or Solaris).
#
# TO DEBUG:
# 	"setup -debug",  and then,  "make", for debugging and testing.
#	set LD_LIBRARY_PATH to include the buildlib directory.
#
# MAKE TAR ARCHIVES:
# 	To make the full distribution run "make dist"; it will use the last
#          setup options when running setup again
# 	To archive the source code run "make src"
#
#
#
#############################################################################
# For making distribution, all files will be put in a
# directory called $(ARCNAME) and a final file called
# $(ARCNAME)$(DISTNAME).tar.gz will be made.
#
VERSION = `sed '/[^0-9.]/s///g' .version`
ARCNAME  = imod_$(VERSION)
DISTNAME = `if [ -e .distname ] ; then sed '/[[:cntrl:]]/s///g' .distname ; fi`
LAST_OPTIONS = `if [ -e .options ] ; then sed '/[[:cntrl:]]/s///g' .options ; fi`

#############################################################################
# The Fortran programs, libraries, and man pages are located under
# the flib directory
#
#############################################################################
#
# Define programs and paths we need.
#
PWD      = `pwd | sed '/[[:cntrl:]]/s///g'`

COMPRESS = gzip -f

ARCHIVE  = $(ARCNAME)$(DISTNAME).tar
ARCCOMP  = $(ARCNAME)$(DISTNAME).tar.gz
ARCCSH   = $(ARCNAME)$(DISTNAME).csh
ARC      = tar cvf
IMODDIR  = $(PWD)
ARCDIR   = $(IMODDIR)/$(ARCNAME)

default : all

#
# Make all the fortran programs then all the C programs.
#
all : configure clibs
	cd Etomo      ; $(MAKE) all
	cd flib       ; $(MAKE) all
	cd 3dmod      ; $(MAKE) all
	cd imodutil   ; $(MAKE) all
	cd sendevent  ; $(MAKE) all
	cd qtassist   ; $(MAKE) all
	cd ctfplotter ; $(MAKE) all
	cd processchunks ; $(MAKE) all
	cd mrc        ; $(MAKE) all
	cd clip       ; $(MAKE) all
	cd midas      ; $(MAKE) all
	cd raptor     ; $(MAKE) all
	cd pysrc      ; $(MAKE) all
	cd plugs      ; $(MAKE) all

##############################################################################
# If a setup gets run automatically, it uses the last options
configure : setup .version
	./setup $(LAST_OPTIONS)

#
# Install cstuff
#
install : configure man sourcedoc
	cd libcfshr   ; $(MAKE) $@
	cd libimod    ; $(MAKE) $@
	cd libiimod   ; $(MAKE) $@
	cd libmesh    ; $(MAKE) $@
	cd libdiaqt   ; $(MAKE) $@
	cd libcfft    ; $(MAKE) $@
	cd libwarp    ; $(MAKE) $@
	cd 3dmod      ; $(MAKE) $@
	cd imodutil   ; $(MAKE) $@
	cd sendevent  ; $(MAKE) $@
	cd qtassist   ; $(MAKE) $@
	cd ctfplotter ; $(MAKE) $@
	cd processchunks ; $(MAKE) $@
	cd mrc        ; $(MAKE) $@
	cd midas      ; $(MAKE) $@
	cd raptor     ; $(MAKE) $@
	cd pysrc      ; $(MAKE) $@
	cd plugs      ; $(MAKE) $@
	cd clip       ; $(MAKE) $@
	cd scripts    ; $(MAKE) $@
	cd flib       ; $(MAKE) $@
	cd com        ; $(MAKE) $@
	cd html       ; $(MAKE) $@
	cd Etomo      ; $(MAKE) $@
	./packMacApps
#
# Make the manual pages .1 from .man, and .html from .1, copy to directories
#
man : configure ALWAYS
	(cd manpages ; $(MAKE) install)
	(cd flib/man ; $(MAKE) install)
	(cd autodoc  ; $(MAKE) install)

#
# Make sourcedoc for libdocs.  If the target is the same as the name of an 
# existing directory it seems to need the ALWAYS
#
sourcedoc : configure ALWAYS
	cd sourcedoc ; $(MAKE)

#
# Install clibs only or all libs, helps if doing multiple architectures
#
installclibs : configure
	cd libcfshr  ; $(MAKE) install
	cd libimod   ; $(MAKE) install
	cd libiimod  ; $(MAKE) install
	cd libmesh   ; $(MAKE) install
	cd libdiaqt  ; $(MAKE) install
	cd libcfft   ; $(MAKE) install
	cd libwarp   ; $(MAKE) install

installlibs : installclibs
	cd flib; $(MAKE) installlibs

#
# Clean up our mess.
#
clean : configure ALWAYS
	cd libcfshr   ; $(MAKE) $@
	cd libimod    ; $(MAKE) $@
	cd libiimod   ; $(MAKE) $@
	cd libmesh    ; $(MAKE) $@
	cd libdiaqt   ; $(MAKE) $@
	cd libcfft    ; $(MAKE) $@
	cd libwarp    ; $(MAKE) $@
	cd 3dmod      ; $(MAKE) $@
	cd imodutil   ; $(MAKE) $@
	cd sendevent  ; $(MAKE) $@
	cd qtassist   ; $(MAKE) $@
	cd ctfplotter ; $(MAKE) $@
	cd processchunks ; $(MAKE) $@
	cd mrc        ; $(MAKE) $@
	cd midas      ; $(MAKE) $@
	cd raptor     ; $(MAKE) $@
	cd pysrc      ; $(MAKE) $@
	cd plugs      ; $(MAKE) $@
	cd clip       ; $(MAKE) $@
	cd sourcedoc  ; $(MAKE) $@
	cd scripts    ; $(MAKE) $@
	cd manpages   ; $(MAKE) $@
	cd flib       ; $(MAKE) $@
	cd flib/man   ; $(MAKE) $@
	cd com        ; $(MAKE) $@
	cd html       ; $(MAKE) $@
	(cd include ; \find . -type f -name "GLw*.h" -exec rm "{}" \;)
	(cd buildlib ; \find . -type f -name "libGLw.a" -exec rm "{}" \;)
	\find . -type f -name "configure" -exec rm "{}" \;

#
# Clean up entries for clibs only or all libs, helps for building libs for
# different architectures
#

cleanclibs : configure ALWAYS
	cd libcfshr  ; $(MAKE) clean
	cd libimod   ; $(MAKE) clean
	cd libiimod  ; $(MAKE) clean
	cd libmesh   ; $(MAKE) clean
	cd libdiaqt  ; $(MAKE) clean
	cd libcfft   ; $(MAKE) clean
	cd libwarp   ; $(MAKE) clean

cleanlibs : cleanclibs
	cd flib; $(MAKE) $@

#
# Clean up everything that depends on Qt
#
cleanqt : configure ALWAYS
	cd libdiaqt   ; $(MAKE) clean
	cd plugs      ; $(MAKE) clean
	cd 3dmod      ; $(MAKE) clean
	cd midas      ; $(MAKE) clean
	cd sendevent  ; $(MAKE) clean
	cd qtassist   ; $(MAKE) clean
	cd ctfplotter ; $(MAKE) clean
	cd processchunks ; $(MAKE) clean
	cd sourcedoc  ; $(MAKE) clean
	cd flib/subrs ; \find . -type f -name '*dnmncar*' -exec /bin/rm -f '{}' \;
	cd flib/subrs/graphics ; $(MAKE) clean
	cd flib/ndasda ; $(MAKE) clean
	cd flib/graphics ; $(MAKE) clean

#
# Clean up executables in Windows
#
cleanexe : configure ALWAYS
	\find . \( -type d -name bin -prune \) -o -type f -name '*.exe' -exec /bin/rm -f '{}' \;
#
# Shortcut for making libs only, helps for debugging.
#
clibs : configure
	cd libcfshr  ; $(MAKE) all
	cd libimod   ; $(MAKE) all
	cd libiimod  ; $(MAKE) all
	cd libmesh   ; $(MAKE) all
	cd libdiaqt  ; $(MAKE) all
	cd libcfft   ; $(MAKE) all
	cd libwarp   ; $(MAKE) all

libs : clibs 
	cd flib; $(MAKE) $@

#
# These entries are no longer needed but the build script calls them
#
o32libs : 

installo32libs : 

#
# Shortcut for making FORTRAN libs only
#
flibs: configure
	cd flib; $(MAKE) libs

#
# To make 3dmod and needed libraries
#
3dmod : configure clibs
	cd flib      ;  $(MAKE) configure
	cd flib/subrs  ; $(MAKE) track
	cd 3dmod     ; $(MAKE) all

#
# Make the full software distribution.  Use the options from last setup
#
dist : ALWAYS
	if [ -e $(ARCDIR) ] ; then /bin/rm -rf $(ARCDIR)/ ; fi
	mkdir $(ARCDIR)
	mkdir $(ARCDIR)/licenses
	./setup -inst $(ARCDIR) $(LAST_OPTIONS)
	(cd dist ; \find . -type f -name "*~" -exec rm "{}" \;)
	($(MAKE) install)
	-\cp buildlib/*.so $(ARCDIR)/lib/
	\cp dist/COPYRIGHT dist/start.html dist/installIMOD $(ARCDIR)/
	\cp  dist/[a-zA-Z]*.txt dist/CPOL.html $(ARCDIR)/licenses/
	-\find $(ARCDIR) -depth -name CVS -exec /bin/rm -rf {} \;
	./installqtlib
	@echo "Compressing..."
	$(ARC) $(ARCHIVE) $(ARCNAME); $(COMPRESS) $(ARCHIVE)
	@echo "Making self-installing file..."
	if [ -e $(ARCCSH) ] ; then /bin/rm -f $(ARCCSH) ; fi
	cat dist/installStub $(ARCCOMP) > $(ARCCSH) ; chmod a+x $(ARCCSH)


##################################################################
# Make the full IMOD source distribution
#
src : configure cleansrc csrc fsrc etomosrc
	if [ -e $(ARCDIR)_src/$(ARCDIR)_src/ ] ; then /bin/rm -rf $(ARCDIR)_src/$(ARCDIR)_src/ ; fi
	-\find $(ARCDIR)_src -depth -name CVS -exec /bin/rm -rf {} \;
	tar cf $(ARCNAME)_src.tar $(ARCNAME)_src 
	$(COMPRESS) $(ARCNAME)_src.tar

cleansrc : ALWAYS
	if [ -e $(ARCDIR)_src ] ; then /bin/rm -rf $(ARCDIR)_src/ ; fi
	if [ -e $(ARCNAME)_src.tar ] ; then /bin/rm -rf $(ARCNAME)_src.tar ; fi
	\find dist -type f -name "*~" -exec rm "{}" \;
	\find machines -type f -name "*~" -exec rm "{}" \;
	\find libdiaqt -type f -name "moc_*.cpp" -exec rm "{}" \;
	\find sendevent -type f -name "moc_*.cpp" -exec rm "{}" \;
	\find qtassist -type f -name "moc_*.cpp" -exec rm "{}" \;
	(cd plugs ; make clean)
	(cd raptor ; make clean)
	(cd pysrc ; make clean)
	(cd manpages ; make clean)
	(cd flib/man ; make clean)
	(cd autodoc ; make clean)
	(cd com ; make clean)
	(cd html ; make clean)
# 
# The C source.
#
csrc : ALWAYS
	mkdir -p $(ARCDIR)_src
	cp Makefile setup README History .version vcimod.dsw \
	installqtlib packMacApps setup2 BUILDING $(ARCDIR)_src/
	tar cBf - \
	machines \
	lib*/*.[ch] lib*/*.cpp lib[icmw]*/Makefile libdiaqt/Makefile.dummy \
	libdiaqt/Makefile.unix lib*/*.dsp lib*/*.vcproj libimod/libimod.dsw \
	sysdep/*/* \
	3dmod/*.[ch] 3dmod/*.cpp 3dmod/*.ui 3dmod/3dmod.pro \
	3dmod/*.bits 3dmod/images 3dmod/*.xpm 3dmod/*.qrc \
	3dmod/3dmod.dsw 3dmod/Makefile.dummy 3dmod/b3dicon.i* \
	imodutil/*.[ch] imodutil/*.cpp imodutil/Makefile \
	mrc/*.[ch] mrc/*.cpp   mrc/Makefile \
	clip/*.[ch]   clip/Makefile \
	midas/*.[ch] midas/*.cpp midas/midas.pro midas/images \
	midas/Makefile.dummy \
	sendevent/*.h sendevent/*.cpp sendevent/imodsendevent.pro \
	sendevent/Makefile.dummy sendevent/imodsendevent.dsp \
	qtassist/*qt*.h qtassist/*qt*.cpp qtassist/imodqtassist.pro \
	qtassist/Makefile.dummy qtassist/imodqtassist.dsp \
	sourcedoc/*.cpp sourcedoc/sourcedoc.pro \
	sourcedoc/Makefile.dummy sourcedoc/sourcedoc.dsp \
	ctfplotter/*.cpp ctfplotter/*.h ctfplotter/ctfplotter.pro \
	ctfplotter/Makefile.dummy ctfplotter/*.qrc ctfplotter/images \
	processchunks/*.cpp processchunks/*.h processchunks/processchunks.pro \
	processchunks/Makefile.dummy \
	html/*.* html/Makefile html/3dmodimages html/etomoImages \
	html/3dmodHelp html/joinImages html/adpStub html/makeadp \
	html/ctfHelp html/midasHelp html/libdoc/Makefile html/libdoc/*.html \
	raptor dist scripts pysrc com manpages autodoc \
	plugs/*/*.[chf] plugs/*/*.cpp plugs/*/*.html plugs/*/*.csv plugs/*/Makefile \
	plugs/*/*.png plugs/Makefile.unix plugs/Makefile.dummy \
	plugs/drawingtools/livewire/*.h plugs/drawingtools/livewire/*.cpp \
	plugs/drawingtools/livewire/Makefile \
	include/*.h include/*.inc | (cd $(ARCDIR)_src; tar xBf -)

#
# The Fortran source.
#
fsrc :
	mkdir -p $(ARCDIR)_src/flib/
	cp flib/Makefile $(ARCDIR)_src/flib/
	cp -r flib/man $(ARCDIR)_src/flib/
	(cd flib; tar cBf - \
	*/Makefile */*/Makefile \
	*/*.[chfs] */*.cu */*/*.[chfs] */*/*.cpp */*.inc */*/README)\
	| (cd $(ARCDIR)_src/flib; tar xBf -)

#
# Etomo source
#
etomosrc :
	mkdir -p $(ARCDIR)_src/Etomo/
	cp -r Etomo/Makefile.real Etomo/Makefile.dummy Etomo/build.xml \
	Etomo/*MANIFEST.MF Etomo/.project Etomo/src \
	Etomo/scripts Etomo/doc Etomo/tests $(ARCDIR)_src/Etomo

#
# Tests
#
tests : ImodTests
	cd Etomo ; $(MAKE) tests
	cd ImodTests ; hg pull -u ; $(MAKE) tests

uitestinstall : 
	cd Etomo ; $(MAKE) $@

ImodTests : 
	hg clone ssh://simba.colorado.edu//home/hg/ImodTests 

ALWAYS:

