############################################################################# 
# Makefile for BL3DEMC IMOD distribution.
#
# Copyright (C) 1996-2004
# by Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells
# ("BL3DEMC", formerly "BL3DFS")
# and the Regents of the University of Colorado.
#
# BL3DEMC reserves the exclusive rights of preparing derivative works,
# distributing copies for sale, lease or lending and dispalying this
# software and documentation.
# Users may reproduce the software and documentation as long as the
# copyright notice and other notices are preserved.
# Neither the software nor the documentation may be distributed for
# profit, either in original form or in derivative works.
#
# THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,
# EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF
# MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.
#
#############################################################################
# BUILD AND INSTALLATION INSTRUCTIONS:
#
# PC UNDER LINUX:
#
# The Qt development package (qt-devel) must be installed
#
# 1. "setup -i [install directory]" to set the install directory.
#
# 2. "make" to make all the programs.
# 
# 3. "make install"
#
# 4. "installqtlibs" to copy install files to install directory.  If you
#    ran "setup" initially with the -packqt flag (which you should do If you 
#    built against a non-default Qt library)," installqtlibs" will also copy
#    the Qt library 
#
#
# MAC OS X:
#
# A developer's version of Qt must be installed and on the path, QTDIR must be
# set in the environment, and QTDIR/lib must be on DYLD_LIBRARY_PATH.
# g77 is also required.  Build and install the tiff libraries to avoid having
# to build with -no_tiff.
# 
# 1. "setup -i [install directory]" to set the install directory.
#
# 2. "make" to make all the programs.
# 
# 3. "make install"
#
# 4. "installqtlibs" to copy Qt library and install files to install directory
#
#
# IRIX 6.5, MIXED OLD AND NEW 32-BIT CODE:
#
# 1. "setup -m irix6-32 -tiff -i [install directory]" 
#
# 2. "make o32libs"     to make the old 32-bit libraries
#
# 3. "make installo32libs"    to install the libraries
#
# 4. "make cleanlibs"     to clean directories for new make
#
# 5. "setup -i [install directory]"
#
# 6. "make"  to make all new 32-bit libraries and programs
#
# 7. "make install"
#
# 8. "installqtlibs" to copy Qt library and install files to install directory
#
#
# BUILDING UNDER CYGWIN/WINDOWS WITH Visual C++ ONLY
#
# Basic Cygwin development packages, libtiff-devel and Microsoft Visual C++
# are required for this build.  A development version of Qt must be installed.
#
# 0. If the source came from a unix tar, convert the project files with:
#    find . -name '*.ds*' -exec unix2dos '{}' \;
#
# 1. "setup -i [install directory] -compiler gnu" to set the compiler and
#    install directory.
#
# 2. "make" to make all the non-graphical programs under Cygwin
#
# 3. In Visual C++, open the workspace libimod/libimod.dsw and build release
#    versions of all three libraries
#
# 4. Open the workspace vcimod.dsw and build 3dmod, midas, and imodsendevent.
# 
# 5. "make install"
#
#
# BUILDING UNDER CYGWIN/WINDOWS WITH INTEL COMPILERS
#
# In addition to requirements listed above, this build requires Intel Fortran
# and C++ compilers.  Instead of Cygwin libtiff-devel, it needs Tiff Binaries
# from GnuWin32.
#
# 1. "setup -i [install directory]" to set the install directory.
#
# 2. "make" to make everything
#
# 3. "make install"
#
# In either case, if there are other users who do not have the build tools
# on their path, run ./installqtlib to copy the Qt, Tiff, and Intel DLLs to
# the install bin.
#
#
# The install commands will build .1 and .html versions of all man pages and
# install them in man and html/man directories.  If the install directory is
# omitted, then installation will be into bin, lib, man, and html
# directories under the top-level source directory.
#
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
#  $Author$
#
#  $Date$
#
#  $Revision$
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
PWD      = `pwd`

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
	cd flib      ; $(MAKE) all
	cd imod      ; $(MAKE) all
	cd imodutil  ; $(MAKE) all
	cd sendevent ; $(MAKE) all
	cd qtassist  ; $(MAKE) all
	cd mrc       ; $(MAKE) all
	cd clip      ; $(MAKE) all
	cd midas     ; $(MAKE) all
	cd plugs     ; $(MAKE) all
	cd Etomo     ; $(MAKE) all

##############################################################################
# If a setup gets run automatically, it uses the last options
configure : setup .version
	./setup $(LAST_OPTIONS)

#
# Install cstuff
#
install : configure man sourcedoc
	cd libimod   ; $(MAKE) $@
	cd libiimod  ; $(MAKE) $@
	cd libdiaqt  ; $(MAKE) $@
	cd libcfft   ; $(MAKE) $@
	cd imod      ; $(MAKE) $@
	cd imodutil  ; $(MAKE) $@
	cd sendevent ; $(MAKE) $@
	cd qtassist  ; $(MAKE) $@
	cd mrc       ; $(MAKE) $@
	cd midas     ; $(MAKE) $@
	cd plugs     ; $(MAKE) $@
	cd clip      ; $(MAKE) $@
	cd scripts   ; $(MAKE) $@
	cd flib      ; $(MAKE) $@
	cd com       ; $(MAKE) $@
	cd html      ; $(MAKE) $@
	cd Etomo	 ; $(MAKE) $@
	./packMacApps
#
# Make the manual pages .1 from .man, and .html from .1, copy to directories
#
man : configure ALWAYS
	(cd manpages ; $(MAKE) install)
	(cd flib/man ; $(MAKE) install)
	(cd autodoc  ; $(MAKE) install)

#
# Make sourcedoc for libdocs
#
sourcedoc : configure
	cd sourcedoc ; $(MAKE)

#
# Install clibs only or all libs, helps if doing multiple architectures
#
installclibs : configure
	cd libimod   ; $(MAKE) install
	cd libiimod  ; $(MAKE) install
	cd libdiaqt  ; $(MAKE) install
	cd libcfft   ; $(MAKE) install

installlibs : installclibs
	cd flib; $(MAKE) installlibs

#
# Clean up our mess.
#
clean : configure
	cd libimod   ; $(MAKE) $@
	cd libiimod  ; $(MAKE) $@
	cd libdiaqt  ; $(MAKE) $@
	cd libcfft   ; $(MAKE) $@
	cd imod      ; $(MAKE) $@
	cd imodutil  ; $(MAKE) $@
	cd sendevent ; $(MAKE) $@
	cd qtassist  ; $(MAKE) $@
	cd mrc       ; $(MAKE) $@
	cd midas     ; $(MAKE) $@
	cd plugs     ; $(MAKE) $@
	cd clip      ; $(MAKE) $@
	cd sourcedoc ; $(MAKE) $@
	cd scripts   ; $(MAKE) $@
	cd manpages  ; $(MAKE) $@
	cd flib      ; $(MAKE) $@
	cd flib/man  ; $(MAKE) $@
	cd com       ; $(MAKE) $@
	cd html      ; $(MAKE) $@
	(cd include ; \find . -type f -name "GLw*.h" -exec rm "{}" \;)
	(cd buildlib ; \find . -type f -name "libGLw.a" -exec rm "{}" \;)
	\find . -type f -name "configure" -exec rm "{}" \;

#
# Clean up entries for clibs only or all libs, helps for building libs for
# different architectures
#

cleanclibs : configure
	cd libimod   ; $(MAKE) clean
	cd libiimod  ; $(MAKE) clean
	cd libdiaqt  ; $(MAKE) clean
	cd libcfft   ; $(MAKE) clean

cleanlibs : cleanclibs
	cd flib; $(MAKE) $@

#
# Clean up everything that depends on Qt
#
cleanqt : configure
	cd libdiaqt  ; $(MAKE) clean
	cd plugs     ; $(MAKE) clean
	cd imod      ; $(MAKE) clean
	cd midas     ; $(MAKE) clean
	cd sendevent ; $(MAKE) clean
	cd qtassist  ; $(MAKE) clean
	cd sourcedoc ; $(MAKE) $@
	cd flib/subrs ; \find . -type f -name '*dnmncar*' -exec /bin/rm -f '{}' \;
	cd flib/subrs/graphics ; $(MAKE) clean
	cd flib/ndasda ; $(MAKE) clean
	cd flib/graphics ; $(MAKE) clean

#
# Clean up executables in Windows
#
cleanexe : configure
	\find . \( -type d -name bin -prune \) -o -type f -name '*.exe' -exec /bin/rm -f '{}' \;
#
# Shortcut for making libs only, helps for debugging.
#
clibs : configure
	cd libimod   ; $(MAKE) all
	cd libiimod  ; $(MAKE) all
	cd libdiaqt  ; $(MAKE) all
	cd libcfft   ; $(MAKE) all

libs : clibs
	cd flib; $(MAKE) $@

#
# Use these entries for old 32-bit build under irix to skip libdiaqt
#
o32clibs : configure
	cd libimod   ; $(MAKE) all
	cd libiimod  ; $(MAKE) all
	cd libcfft   ; $(MAKE) all

o32libs : o32clibs
	cd flib; $(MAKE) libs

installo32clibs : configure
	cd libimod   ; $(MAKE) install
	cd libiimod  ; $(MAKE) install
	cd libcfft   ; $(MAKE) install

installo32libs : installo32clibs
	cd flib; $(MAKE) installlibs

#CER
#CER Shortcut for making FORTRAN libs only
#CER
flibs: configure
	cd flib; $(MAKE) libs

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
	\cp dist/COPYRIGHT dist/start.html dist/installIMOD $(ARCDIR)/
	\find $(ARCDIR) -name CVS -depth -exec /bin/rm -rf {} \;
	./installqtlib
	@echo "Compressing..."
	$(ARC) $(ARCHIVE) $(ARCNAME); $(COMPRESS) $(ARCHIVE)
	@echo "Making self-installing file..."
	if [ -e $(ARCCSH) ] ; then /bin/rm -rf $(ARCCSH)/ ; fi
	cat dist/installStub $(ARCCOMP) > $(ARCCSH) ; chmod a+x $(ARCCSH)


##################################################################
# Make the full IMOD source distribution
#
src : configure cleansrc csrc fsrc etomosrc
	if [ -e $(ARCDIR)_src/$(ARCDIR)_src/ ] ; then /bin/rm -rf $(ARCDIR)_src/$(ARCDIR)_src/ ; fi
	\find $(ARCDIR)_src -name CVS -depth -exec /bin/rm -rf {} \;
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
	lib*/*.[ch] lib*/*.cpp lib[ic]*/Makefile libdiaqt/Makefile.dummy \
	libdiaqt/Makefile.unix lib*/*.dsp libimod/libimod.dsw \
	USFFTlib/*/*.a sysdep/*/* \
	imod/*.[ch] imod/*.cpp imod/*.ui imod/imod.pro imod/imodhelp \
	imod/*.bits imod/*.png imod/*.xpm imod/README \
	imod/3dmod.dsw imod/Makefile.dummy imod/b3dicon.i* \
	imodutil/*.[ch] imodutil/Makefile \
	mrc/*.[ch]    mrc/Makefile \
	clip/*.[ch]   clip/Makefile \
	midas/*.[ch] midas/*.cpp midas/midas.pro \
	midas/Makefile.dummy \
	sendevent/*.h sendevent/*.cpp sendevent/imodsendevent.pro \
	sendevent/Makefile.dummy sendevent/imodsendevent.dsp \
	qtassist/*qt*.h qtassist/*qt*.cpp qtassist/imodqtassist.pro \
	qtassist/Makefile.dummy qtassist/imodqtassist.dsp \
	sourcedoc/*.cpp sourcedoc/sourcedoc.pro \
	sourcedoc/Makefile.dummy sourcedoc/sourcedoc.dsp \
	html/*.* html/Makefile html/3dmodimages html/etomoImages \
	html/libdoc/Makefile html/libdoc/*.html \
	dist scripts com manpages autodoc \
	plugs/*/*.[chf] plugs/*/*.cpp plugs/*/Makefile \
	plugs/Makefile.unix plugs/Makefile.dummy \
	devkit/*.[ch] devkit/*++ devkit/README devkit/Makefile \
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
	*/*.[chfs] */*/*.[chfs] */*/*.cpp */*.inc */*/*.inc */*/README)\
	| (cd $(ARCDIR)_src/flib; tar xBf -)

#
# Etomo source
#
etomosrc :
	mkdir -p $(ARCDIR)_src/Etomo/
	cp -r Etomo/Makefile.real Etomo/Makefile.dummy Etomo/build.xml \
	Etomo/*MANIFEST.MF Etomo/.classpath Etomo/.project Etomo/src \
	Etomo/scripts Etomo/doc $(ARCDIR)_src/Etomo

#
# Tests
#
tests : ImodTests
	cd Etomo ; $(MAKE) tests
	cd ImodTests ; cvs update ; $(MAKE) tests

ImodTests : 
	cvs co ImodTests 

ALWAYS:

############################################################################
#  $Log$
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
