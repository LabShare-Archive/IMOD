############################################################################# 
# Makefile for BL3DFS IMOD distribution.
#
# Copyright (C) 1996-2002
# by Boulder Laboratory for 3-Dimensional Fine Structure ("BL3DFS" or "3DFS")
# and the Regents of the University of Colorado.
#
# BL3DFS reserves the exclusive rights of preparing derivative works,
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
# SIMPLE CASE OF PC UNDER LINUX:
#
# 1. "setup -i [install directory]" to set the install directory.
#
# 2. "make" to make all the programs.
# 
# 3. "make install"
#
#
# CASE OF MIXED OLD AND NEW 32-BIT CODE, IRIX 6.3 - 6.5:
#
# 1. "setup -m irix6-32 -tiff -i [install directory]" 
#
# 2. "make o32libs"     to make the old 32-bit libraries
#
# 3. "make installo32libs"    to install the libraries
#
# 4. "make cleanlibs"     to clean directories for new make
#
# 5.  "setup -i [install directory]"
#
# 6.  "make"  to make all new 32-bit libraries and programs
#
# 7.  "make install"
#
#
# BUILDING UNDER CYGWIN/WINDOWS
#
# 0. If the source came from a unix tar, convert the project files with:
#    find . -name '*.ds*' -exec unix2dos '{}' \;
#
# 1. "setup -i [install directory]" to set the install directory.
#
# 2. "make" to make all the non-graphical programs under Cygwin
#
# 3. In VisualC++, open the workspace libimod/libimod.dsw and build release
#    versions of all three libraries
#
# 4. Open the workspace vcimod.dsw and build imod, midas, and imodsendevent.
# 
# 5. "make cyginstall"
#
#
# The install commands will build .1 and .html versions of all man pages and
# install them in man and html/man directories.  If the install directory is
# omitted, then installation will be into bin, lib, com, man, and html
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
# 	To make the full distribution run "make dist" or "make cygdist"
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
# $(ARCNAME).tar.gz will be made.
VERSION = `sed '/.\(*\[0-9.]*\).*/s//\1/' .version`
ARCNAME  = imod_$(VERSION)

#############################################################################
# The Fortran programs, libraries, and man pages are located under
# the flib directory
#
#############################################################################
#
# Define programs and paths we need.
#
#SHELL   = /usr/bin/csh
SHELL    = /bin/csh
PWD      = `pwd`

COMPRESS = gzip

ARCHIVE  = $(ARCNAME).tar
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
	cd mrc       ; $(MAKE) all
	cd clip      ; $(MAKE) all
	cd midas     ; $(MAKE) all
	cd plugs     ; $(MAKE) all

##############################################################################
# set environment variable SETUP_OPTIONS to change configuration.
# type setup -help for list of options.
configure : setup .version
	setup $(SETUP_OPTIONS)

#
# Install cstuff
#
install : configure man
	cd libimod   ; $(MAKE) $@
	cd libiimod  ; $(MAKE) $@
	cd libdiaqt  ; $(MAKE) $@
	cd imod      ; $(MAKE) $@
	cd imodutil  ; $(MAKE) $@
	cd sendevent ; $(MAKE) $@
	cd mrc       ; $(MAKE) $@
	cd midas     ; $(MAKE) $@
	cd plugs     ; $(MAKE) $@
	cd clip      ; $(MAKE) $@
	cd scripts   ; $(MAKE) $@
	cd flib      ; $(MAKE) $@
	cd com       ; $(MAKE) $@
	cd html      ; $(MAKE) $@

#
# Install under cygwin
#
cyginstall : configure man
	cd imod      ; $(MAKE) install
	cd imodutil  ; $(MAKE) $@
	cd sendevent ; $(MAKE) install
	cd mrc       ; $(MAKE) $@
	cd midas     ; $(MAKE) install
	cd plugs     ; $(MAKE) install
	cd clip      ; $(MAKE) $@
	cd scripts   ; $(MAKE) install
	cd flib      ; $(MAKE) $@
	cd com       ; $(MAKE) install
	cd html      ; $(MAKE) install

#
# Make the manual pages .1 from .man, and .html from .1, copy to directories
#
man : configure ALWAYS
	(cd manpages ; make install)
	(cd flib/man ; make install)

#
# Install clibs only or all libs, helps if doing multiple architectures
#
installclibs : configure
	cd libimod   ; $(MAKE) install
	cd libiimod  ; $(MAKE) install
	cd libdiaqt  ; $(MAKE) install

installlibs : installclibs
	cd flib; $(MAKE) install

#
# Clean up our mess.
#
clean : configure
	cd libimod   ; $(MAKE) $@
	cd libiimod  ; $(MAKE) $@
	cd libdiaqt  ; $(MAKE) $@
	cd imod      ; $(MAKE) $@
	cd imodutil  ; $(MAKE) $@
	cd sendevent ; $(MAKE) $@
	cd mrc       ; $(MAKE) $@
	cd midas     ; $(MAKE) $@
	cd plugs     ; $(MAKE) $@
	cd clip      ; $(MAKE) $@
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

cleanlibs : cleanclibs
	cd flib; $(MAKE) $@

#
# Shortcut for making libs only, helps for debugging.
#
clibs : configure
	cd libimod   ; $(MAKE) all
	cd libiimod  ; $(MAKE) all
	cd libdiaqt  ; $(MAKE) all

libs : clibs
	cd flib; $(MAKE) $@

#
# Use these entries for old 32-bit build under irix to skip libdiaqt
#
o32clibs : configure
	cd libimod   ; $(MAKE) all
	cd libiimod  ; $(MAKE) all

o32libs : o32clibs
	cd flib; $(MAKE) libs

installo32clibs : configure
	cd libimod   ; $(MAKE) install
	cd libiimod  ; $(MAKE) install

installo32libs : installo32clibs
	cd flib; $(MAKE) installlibs

#CER
#CER Shortcut for making FORTRAN libs only
#CER
flibs: configure
	cd flib; $(MAKE) libs

#
# Make the full software distribution.  Be sure SETUP_OPTIONS is set if 
# anything is going to get made and non-default options are needed
#
dist : ALWAYS
	if (-e $(ARCDIR)) /bin/rm -rf $(ARCDIR)/
	if (! (-e $(ARCDIR)))  mkdir $(ARCDIR)
	setup -inst $(ARCDIR) $(SETUP_OPTIONS)
	(cd dist ; \find . -type f -name "*~" -exec rm "{}" \;)
	($(MAKE) install)
	-\cp buildlib/*.so $(ARCDIR)/lib/
	\cp -r dist/* $(ARCDIR)/
	\find $(ARCDIR) -name CVS -depth -exec /bin/rm -rf {} \;
	./installqtlib
	echo "Compressing..."
	$(ARC) $(ARCHIVE) $(ARCNAME); $(COMPRESS) $(ARCHIVE)

cygdist : ALWAYS
	if (-e $(ARCDIR)) /bin/rm -rf $(ARCDIR)/
	if (! (-e $(ARCDIR)))  mkdir $(ARCDIR)
	setup -inst $(ARCDIR) $(SETUP_OPTIONS)
	(cd dist ; \find . -type f -name "*~" -exec rm "{}" \;)
	($(MAKE) cyginstall)
	\cp -r dist/* $(ARCDIR)/
	\find $(ARCDIR) -name CVS -depth -exec /bin/rm -rf {} \;
	./installqtlib
	echo "Compressing..."
	$(ARC) $(ARCHIVE) $(ARCNAME); $(COMPRESS) $(ARCHIVE)


##################################################################
# Make the full IMOD source distribution
#
src : configure cleansrc csrc fsrc
	if (-e $(ARCDIR)_src/$(ARCDIR)_src/) /bin/rm -rf $(ARCDIR)_src/$(ARCDIR)_src/
	\find $(ARCDIR)_src -name CVS -depth -exec /bin/rm -rf {} \;
	tar cf $(ARCNAME)_src.tar $(ARCNAME)_src

cleansrc : ALWAYS
	if (-e $(ARCDIR)_src) /bin/rm -rf $(ARCDIR)_src/
	if (-e $(ARCNAME)_src.tar) /bin/rm -rf $(ARCNAME)_src.tar	
	\find dist -type f -name "*~" -exec rm "{}" \;
	\find machines -type f -name "*~" -exec rm "{}" \;
	\find libdiaqt -type f -name "moc_*.cpp" -exec rm "{}" \;
	\find sendevent -type f -name "moc_*.cpp" -exec rm "{}" \;
	\find plugs -type f -name "moc_*.cpp" -exec rm "{}" \;
	(cd manpages ; make clean)
	(cd flib/man ; make clean)
	(cd com ; make clean)
	(cd html ; make clean)
# 
# The C source.
#
csrc : ALWAYS
	if (! (-e $(ARCDIR)_src)) mkdir $(ARCDIR)_src/
	cp Makefile setup README History .version original_dates vcimod.dsw \
	installqtlib $(ARCDIR)_src/
	tar cBf - \
	machines \
	lib*/*.[ch] lib*/*.cpp libi*/Makefile libdiaqt/Makefile.dummy \
	libdiaqt/Makefile.unix lib*/*.dsp libimod/libimod.dsw \
	USFFTlib/*/*.a \
	imod/*.[ch] imod/*.cpp imod/*.ui imod/imod.pro imod/imodhelp \
	imod/*.bits imod/*.png imod/*.xpm imod/README imod/imod.dsp \
	imod/imod.dsw imod/Makefile.dummy \
	imodutil/*.[ch] imodutil/Makefile \
	mrc/*.[ch]    mrc/Makefile \
	clip/*.[ch]   clip/Makefile \
	midas/*.[ch] midas/*.cpp midas/midas.pro midas/midas.dsp \
	midas/Makefile.dummy \
	sendevent/*.h sendevent/*.cpp sendevent/imodsendevent.pro \
	sendevent/Makefile.dummy sendevent/imodsendevent.dsp \
	html/*.* html/Makefile html/3dmodimages \
	dist scripts com manpages \
	plugs/*/*.[chf] plugs/*/*.cpp plugs/*/Makefile \
	plugs/Makefile.unix plugs/Makefile.dummy \
	devkit/*.[ch] devkit/*++ devkit/README devkit/Makefile \
	include/*.h include/*.inc | (cd $(ARCDIR)_src; tar xBf -)

#
# The Fortran source.
#
fsrc :
	if (! (-e $(ARCDIR)_src/flib/)) mkdir $(ARCDIR)_src/flib/
	cp flib/Makefile $(ARCDIR)_src/flib/
	cp -r flib/man $(ARCDIR)_src/flib/
	(cd flib; tar cBf - \
	*/Makefile */*/Makefile \
	*/*.[chfs] */*/*.[chfs] */*.inc */*/*.inc */*/README)\
	| (cd $(ARCDIR)_src/flib; tar xBf -)

ALWAYS:

############################################################################
#  $Log$
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
