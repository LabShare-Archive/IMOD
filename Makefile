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
#############################################################################
#  $Id$
#  Log at end of file
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
	cd imod       ; $(MAKE) all
	cd imodutil   ; $(MAKE) all
	cd sendevent  ; $(MAKE) all
	cd qtassist   ; $(MAKE) all
	cd ctfplotter ; $(MAKE) all
	cd mrc        ; $(MAKE) all
	cd clip       ; $(MAKE) all
	cd midas      ; $(MAKE) all
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
	cd imod       ; $(MAKE) $@
	cd imodutil   ; $(MAKE) $@
	cd sendevent  ; $(MAKE) $@
	cd qtassist   ; $(MAKE) $@
	cd ctfplotter ; $(MAKE) $@
	cd mrc        ; $(MAKE) $@
	cd midas      ; $(MAKE) $@
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
	cd imod       ; $(MAKE) $@
	cd imodutil   ; $(MAKE) $@
	cd sendevent  ; $(MAKE) $@
	cd qtassist   ; $(MAKE) $@
	cd ctfplotter ; $(MAKE) $@
	cd mrc        ; $(MAKE) $@
	cd midas      ; $(MAKE) $@
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

cleanlibs : cleanclibs
	cd flib; $(MAKE) $@

#
# Clean up everything that depends on Qt
#
cleanqt : configure ALWAYS
	cd libdiaqt   ; $(MAKE) clean
	cd plugs      ; $(MAKE) clean
	cd imod       ; $(MAKE) clean
	cd midas      ; $(MAKE) clean
	cd sendevent  ; $(MAKE) clean
	cd qtassist   ; $(MAKE) clean
	cd ctfplotter ; $(MAKE) clean
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

libs : clibs 
	cd flib; $(MAKE) $@

#
# These entries are no longer needed by the build script calls them
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
	cd imod      ; $(MAKE) all

#
# Make the full software distribution.  Use the options from last setup
#
dist : ALWAYS
	if [ -e $(ARCDIR) ] ; then /bin/rm -rf $(ARCDIR)/ ; fi
	mkdir $(ARCDIR)
	./setup -inst $(ARCDIR) $(LAST_OPTIONS)
	(cd dist ; \find . -type f -name "*~" -exec rm "{}" \;)
	($(MAKE) install)
	-\cp buildlib/*.so $(ARCDIR)/lib/
	\cp dist/COPYRIGHT dist/start.html dist/installIMOD dist/*GPL.txt $(ARCDIR)/
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
	\find plugs -type f -name "moc_*.cpp" -exec rm "{}" \;
	(cd pysrc ; make clean)
	/bin/rm -f pysrc/Makefile
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
	cp Makefile setup README History .version original_dates vcimod.dsw \
	installqtlib packMacApps setup2 $(ARCDIR)_src/
	tar cBf - \
	machines \
	lib*/*.[ch] lib*/*.cpp lib[icm]*/Makefile libdiaqt/Makefile.dummy \
	libdiaqt/Makefile.unix lib*/*.dsp lib*/*.vcproj libimod/libimod.dsw \
	USFFTlib/*/*.a sysdep/*/* \
	imod/*.[ch] imod/*.cpp imod/*.ui imod/imod.pro \
	imod/*.bits imod/*.png imod/*.xpm imod/*.qrc \
	imod/3dmod.dsw imod/Makefile.dummy imod/b3dicon.i* \
	imodutil/*.[ch] imodutil/Makefile \
	mrc/*.[ch] mrc/*.cpp   mrc/Makefile \
	clip/*.[ch]   clip/Makefile \
	midas/*.[ch] midas/*.cpp midas/midas.pro \
	midas/Makefile.dummy \
	sendevent/*.h sendevent/*.cpp sendevent/imodsendevent.pro \
	sendevent/Makefile.dummy sendevent/imodsendevent.dsp \
	qtassist/*qt*.h qtassist/*qt*.cpp qtassist/imodqtassist.pro \
	qtassist/Makefile.dummy qtassist/imodqtassist.dsp \
	sourcedoc/*.cpp sourcedoc/sourcedoc.pro \
	sourcedoc/Makefile.dummy sourcedoc/sourcedoc.dsp \
	ctfplotter/*.cpp ctfplotter/*.h ctfplotter/ctfplotter.pro \
	ctfplotter/Makefile.dummy ctfplotter/*.qrc ctfplotter/images \
	html/*.* html/Makefile html/3dmodimages html/etomoImages \
	html/3dmodHelp html/joinImages html/adpStub html/makeadp \
	html/ctfHelp html/midasHelp html/libdoc/Makefile html/libdoc/*.html \
	dist scripts pysrc com manpages autodoc \
	plugs/*/*.[chf] plugs/*/*.cpp plugs/*/*.html plugs/*/Makefile \
	plugs/Makefile.unix plugs/Makefile.dummy \
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
	*/*.[chfs] */*.cu */*/*.[chfs] */*/*.cpp */*.inc */*/*.inc */*/README)\
	| (cd $(ARCDIR)_src/flib; tar xBf -)

#
# Etomo source
#
etomosrc :
	mkdir -p $(ARCDIR)_src/Etomo/
	cp -r Etomo/Makefile.real Etomo/Makefile.dummy Etomo/build.xml \
	Etomo/*MANIFEST.MF Etomo/.classpath Etomo/.project Etomo/src \
	Etomo/scripts Etomo/doc Etomo/tests $(ARCDIR)_src/Etomo

#
# Tests
#
tests : ImodTests
	cd Etomo ; $(MAKE) tests
	cd ImodTests ; cvs update ; $(MAKE) tests

uitestinstall : 
	cd Etomo ; $(MAKE) $@

ImodTests : 
	cvs co ImodTests 

ALWAYS:

############################################################################
#  $Log$
#  Revision 3.78  2010/01/01 15:21:36  mast
#  Fix src to include .cu
#
#  Revision 3.77  2009/03/06 17:25:09  mast
#  Add LGPL.txt to dist
#
#  Revision 3.76  2009/01/17 17:03:27  mast
#  updates to make src
#
#  Revision 3.75  2009/01/16 03:00:24  mast
#  Updated notes, maybe made some cleans work better
#
#  Revision 3.74  2009/01/15 16:37:44  mast
#  Change a \rm in make src
#
#  Revision 3.73  2008/01/29 05:32:58  mast
#  Remove devkit from src build
#
#  Revision 3.72  2008/01/24 15:46:08  mast
#  Added html in plugin directories
#
#  Revision 3.71  2007/10/03 21:45:25  mast
#  Added midasHelp
#
#  Revision 3.70  2007/09/20 16:16:34  mast
#  Add mrc/*.cpp to src
#
#  Revision 3.69  2007/09/20 14:21:16  mast
#  There will be no ctfplotter.dsp - can't build fortran libs in Visual C
#
#  Revision 3.68  2007/09/20 03:21:34  mast
#  Needed to make libcfshr before libimod for windows
#
#  Revision 3.67  2007/09/20 02:49:32  mast
#  Changes for ctf stuff and library rearrangement
#
#  Revision 3.66  2007/06/04 20:53:13  sueh
#  Added entry for minimal make for 3dmod
#
#  Revision 3.65  2007/06/04 18:50:52  mast
#  *** empty log message ***
#
#  Revision 3.64  2007/02/15 22:59:22  mast
#  Allow error on find of CVS in dist and src for cygwin
#
#  Revision 3.63  2006/10/06 21:25:51  mast
#  Forgot a quote
#
#  Revision 3.62  2006/10/06 21:14:08  mast
#  Strip ctrl-r from pwd output for new cygwin bash bug
#
#  Revision 3.61  2006/10/03 21:15:34  mast
#  Added pysrc to make
#
#  Revision 3.60  2006/09/12 15:33:49  mast
#  Added libmesh
#
#  Revision 3.59  2006/08/28 21:44:26  mast
#  Added Etomo/tests to make src
#
#  Revision 3.58  2006/06/24 00:06:46  mast
#  Added adp stuff to make src
#
#  Revision 3.57  2006/03/01 02:17:01  mast
#  Add an ALWAYS
#
#  Revision 3.56  2006/02/11 14:56:44  mast
#  Fixed imod/README in make src, and -depth args
#
#  Revision 3.55  2006/01/03 19:57:41  mast
#  Added uitestinstall
#
#  Revision 3.54  2005/10/08 06:06:05  mast
#  Put etomo at top of build for testing
#
#  Revision 3.53  2005/07/06 21:12:13  mast
#  Cleaned up because 032 no longer needed on SGI
#
#  Revision 3.52  2005/06/27 18:58:53  mast
#  Fix cleanqt entry for sourcedoc
#
#  Revision 3.51  2005/05/12 00:31:50  mast
#  Added 3dmodHelp and joinImages to src
#
#  Revision 3.50  2005/04/01 22:32:01  mast
#  Fixed removal of .csh file
#
#  Revision 3.49  2005/03/31 22:49:24  mast
#  Added setup2 to src
#
#  Revision 3.48  2005/03/31 22:28:59  mast
#  Fixed sourcedoc entry in make src
#
#  Revision 3.47  2005/02/25 04:26:43  mast
#  Added entries for sourcedoc and libdoc
#
#  Revision 3.46  2005/01/18 22:34:21  mast
#  Needed to remove dist file before making on Windows
#
#  Revision 3.45  2005/01/17 19:31:56  mast
#  Switched shell to sh so parallel make will work on Linux
#
#  Revision 3.44  2004/12/23 23:38:35  mast
#  Added dsp file for imodqtassist
#
#  Revision 3.43  2004/12/22 05:49:20  mast
#  Added imodqtassist
#
#  Revision 3.42  2004/10/24 21:27:25  mast
#  Changes for addition of libcfft version of libifft
#
#  Revision 3.41  2004/08/31 02:23:48  mast
#  Moved copying of .version to VERSION from dist to install (packMacApps)
#
#  Revision 3.40  2004/07/22 18:20:20  mast
#  Fixed the fix
#
#  Revision 3.39  2004/07/22 18:17:39  mast
#  Change to better sed command for stripping Ctrl M
#
#  Revision 3.38  2004/04/24 01:03:56  mast
#  Added self-installing file output
#
#  Revision 3.37  2004/04/03 21:28:56  mast
#  simplified seds for stripping ^M in cygwin
#
#  Revision 3.36  2004/04/03 19:59:27  mast
#  Changed from SETUP_OPTIONS to using last options stored in .options file
#  Updated make and install instructions to add Mac OSX and include
#  installqtlib in all cases
#
#  Revision 3.35  2004/01/27 06:07:02  mast
#  Don't need to make dist directory
#
#  Revision 3.34  2004/01/27 05:19:59  mast
#  Probably better not to remove ImodTests, it screws up sandbox
#
#  Revision 3.33  2004/01/27 05:15:33  mast
#  Added .distname to dist file name, added make for ImodTests
#
#  Revision 3.32  2004/01/17 20:32:00  mast
#  Fix cleanqt for ndasda and graphics
#
#  Revision 3.31  2003/12/30 17:28:29  mast
#  spare .exes in bin from cleanexe
#
#  Revision 3.30  2003/12/04 16:30:52  mast
#  Remove unused dsp's from make src and add tests entry
#
#  Revision 3.29  2003/12/02 03:27:46  mast
#  add ndasda to cleanqt and revise instructions for Windows
#
#  Revision 3.28  2003/11/26 21:46:31  mast
#  A few additions for make src
#
#  Revision 3.27  2003/11/18 19:31:36  mast
#  Add entry to clean windows .exe files
#
#  Revision 3.26  2003/10/25 16:44:38  mast
#  add new directories to make src
#
#  Revision 3.25  2003/10/24 04:17:18  mast
#  Changes for Windows/Intel compilation
#
#  Revision 3.24  2003/10/16 20:51:35  mast
#  Added packMacApps to make src
#
#  Revision 3.23  2003/10/13 23:07:58  mast
#  Fix make src to add qtplax.cpp
#
#  Revision 3.22  2003/10/08 17:18:35  mast
#  Changes to work with autodoc files
#
#  Revision 3.21  2003/09/23 20:57:32  mast
#  Add entry to pack Mac applications on install
#
#  Revision 3.20  2003/09/18 20:48:10  mast
#  Added entry to clean everything that depends on Qt
#
#  Revision 3.19  2003/09/03 00:37:29  mast
#  Change to ./setup in cygdist
#
#  Revision 3.18  2003/08/10 02:18:35  mast
#  Add hidden files to Etomo source
#
#  Revision 3.17  2003/08/10 02:05:01  mast
#  Added etomo to the src make, fixed this make from 3dmod name change
#
#  Revision 3.16  2003/08/08 05:17:03  rickg
#  Added entries for Etomo
#  explicitly specified ./setup because it was clashing with the system command
#
#  Revision 3.15  2003/06/20 19:34:34  mast
#  Fixed installlibs from making entire flib
#
#  Revision 3.14  2003/05/08 22:07:30  mast
#  Add installqtlib to src package
#
#  Revision 3.13  2003/05/08 20:24:30  mast
#  Add copy of qt libraries to distributions
#
#  Revision 3.12  2003/04/29 05:25:15  mast
#  Make src depend on configure so a fresh checkout will work for make src
#
#  Revision 3.11  2003/04/17 19:29:19  mast
#  Mac has no .so, ignore error in dist
#
#  Revision 3.10  2003/03/28 05:49:47  mast
#  include .xpm's in make src
#
#  Revision 3.9  2003/03/07 22:04:14  mast
#  Fix making of VERSION for windows
#
#  Revision 3.8  2003/03/07 21:49:56  mast
#  Changes for make src and to make a cygwin distribution
#
#  Revision 3.7  2003/02/28 19:46:55  mast
#  Adding instructions for windows
#
#  Revision 3.6  2003/02/28 18:09:32  mast
#  Changes for cygwin/windows
#
#  Revision 3.5  2003/02/27 20:25:32  mast
#  Add new sendevent directory
#
#  Revision 3.4  2003/02/10 23:30:36  mast
#  fixing new o32lib commands
#
#  Revision 3.3  2003/02/10 20:57:47  mast
#  *** empty log message ***
#
#  Revision 3.2.2.1  2003/01/27 00:38:37  mast
#  fine-tuning the build after pure Qt imod
#
#  Revision 3.2  2002/07/30 05:35:51  mast
#  Changes to include GLw in standard build
#
#  Revision 3.1  2001/12/06 15:26:28  mast
#  Added History to make src
#
#  Revision 3.0  2001/11/29 17:25:14  rickg
#  *** empty log message ***
#
#  Revision 1.5  2001/11/28 15:33:57  mast
#  Changes so that man pages install under "install", various fixes after
#  checking function on the SGI, updated instructions, and general cleanup of
#  unused variables.
#
#  Revision 1.4  2001/11/27 16:00:37  mast
#  Eliminate CVS directories when make dist; make configure depend on .version
#
#  Revision 1.3  2001/11/26 23:10:20  mast
#  Added original_dates to source copy
#
#  Revision 1.2  2001/11/22 00:36:23  mast
#  Updated make src to include .version and exclude CVS directories
#
#############################################################################
