############################################################################# 
# Makefile for BL3DFS IMOD distribution.
#
# Copyright (C) 1996-2001
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
# SIMPLE CASE OF OLD 32-BIT CODE UNDER IRIX 6.0 - 6.2, OR PC UNDER LINUX:
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
# 1. "setup -m irix6-32 -i [install directory]" 
#
# 2. "make libs"     to make the old 32-bit libraries
#
# 3. "make installlibs"    to install the libraries
#
# 4. "make cleanlibs"     to clean directories for new make
#
# 5.  "setup -i [install directory]"
#
# 6.  "make"  to make all new 32-bit libraries and programs
#
# 7.  "make install"
#
# TO MAKE THE MAN PAGES
#
#     "make man"  to produce .1 files in the man/cat1 directory and .html files
#     in the html/man directory
#
#
# IF THERE IS NO TIFF LIBRARY ON YOUR SYSTEM: 
#
# 1. In include, rename notiff.h to tiff.h and notiffio.h to tiffio.h
#
# 2. Add the -no_tiff flag whenever you give the setup command (not needed
#    for IRIX 6.0-6.2).
#
# TO DEBUG:
# 	"setup -debug",  and then,  "make", for debugging and testing.
#	set LD_LIBRARY_PATH to include the buildlib directory.
#
# MAKE TAR ARCHIVES:
# 	To make the full distribution run "make dist"  (do "make man" first)
# 	To archive the source code run "make src"
#
#############################################################################
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#  Revision 1.3  2001/11/26 23:10:20  mast
#  Added original_dates to source copy
#
#  Revision 1.2  2001/11/22 00:36:23  mast
#  Updated make src to include .version and exclude CVS directories
#
#############################################################################
# For making distribution, all files will be put in a
# directory called $(ARCNAME) and a final file called
# $(ARCNAME).tar.gz will be made.
VERSION = `cat .version`
ARCNAME  = imod_$(VERSION)

#############################################################################
# Support for the Fortran programs included with the IMOD distribution.
# FLIBDIR is the directory in which the fortran programs and libraries
# are made.  
# 
# The FORTRAN programs are made with the 
# 'all', 'libs', 'clean', 'installlibs', 'cleanlibs', 'dist' and 'src' make
# options.
#

PWD      = `pwd`
FLIBDIR  = $(PWD)/flib

#############################################################################
#
# Define programs and paths we need.
#
#SHELL   = /usr/bin/csh
SHELL    = /bin/csh

STOREDIR = /usr/local/src
BINDIR   = /usr/local/bin
COMPRESS = gzip

ARCHIVE  = $(ARCNAME).tar
ARC      = tar cvf
IMODDIR  = $(PWD)
MRCDIR   = mrc
DIADIR   = dia
ARCDIR   = $(IMODDIR)/$(ARCNAME)

default : all

#
# Make all the fortran programs then all the C programs.
#
all : configure clibs
	cd $(FLIBDIR); $(MAKE) all
	cd imod      ; $(MAKE) all
	cd imodutil  ; $(MAKE) all
	cd mrc       ; $(MAKE) all
	cd clip      ; $(MAKE) all
	cd midas     ; $(MAKE) all
	cd plugs     ; $(MAKE) all

##############################################################################
# set environment variable SETUP_OPTIONS to change configurateion.
# type setup -help for list of options.
configure : setup .version
	setup $(SETUP_OPTIONS)

#
# Install cstuff
#
install : configure
	cd libimod   ; $(MAKE) $@
	cd libiimod  ; $(MAKE) $@
	cd libdia    ; $(MAKE) $@
	cd imod      ; $(MAKE) $@
	cd imodutil  ; $(MAKE) $@
	cd mrc       ; $(MAKE) $@
	cd midas     ; $(MAKE) $@
	cd plugs     ; $(MAKE) $@
	cd clip      ; $(MAKE) $@
	cd scripts   ; $(MAKE) $@
	cd $(FLIBDIR); $(MAKE) $@

#
# Install clibs only or all libs, helps if doing multiple architectures
#
installclibs : configure
	cd libimod   ; $(MAKE) install
	cd libiimod  ; $(MAKE) install
	cd libdia    ; $(MAKE) install

installlibs : installclibs
	cd $(FLIBDIR); $(MAKE) $@

#
# Clean up our mess.
#
clean : configure
	cd libimod   ; $(MAKE) $@
	cd libiimod  ; $(MAKE) $@
	cd libdia    ; $(MAKE) $@
	cd imod      ; $(MAKE) $@
	cd imodutil  ; $(MAKE) $@
	cd mrc       ; $(MAKE) $@
	cd midas     ; $(MAKE) $@
	cd plugs     ; $(MAKE) $@
	cd clip      ; $(MAKE) $@
	cd scripts   ; $(MAKE) $@
	cd manpages  ; $(MAKE) $@
	cd localman  ; $(MAKE) $@
	cd $(FLIBDIR); $(MAKE) $@
	cd $(FLIBDIR)/man; $(MAKE) $@
	\find . -type f -name "configure" -exec rm "{}" \;

#
# Clean up entries for clibs only or all libs, helps for building libs for
# different architectures
#

cleanclibs : configure
	cd libimod   ; $(MAKE) clean
	cd libiimod  ; $(MAKE) clean
	cd libdia    ; $(MAKE) clean

cleanlibs : cleanclibs
	cd $(FLIBDIR); $(MAKE) $@

#
# Shortcut for making c libs only, helps for debugging.
#
clibs : configure
	cd libimod   ; $(MAKE) all
	cd libiimod  ; $(MAKE) all
	cd libdia    ; $(MAKE) all

libs : clibs
	cd $(FLIBDIR); $(MAKE) $@

#CER
#CER Shortcut for making FORTRAN libs only
#CER
flibs: configure
	echo "CER: FLIBDIR: " $(FLIBDIR)
	cd $(FLIBDIR); $(MAKE) libs

glw  : configure
	cd GLw  ; $(MAKE)

installglw : configure
	cd GLw  ; $(MAKE) install

cleanglw : configure
	cd GLw  ;  $(MAKE) clean

#
# Make the full software distribution.  Run make man first
#
dist : ALWAYS
	if (! (-e $(ARCDIR)))       mkdir $(ARCDIR)
	if (! (-e $(ARCDIR)/lib/))  mkdir $(ARCDIR)/lib/
	if (! (-e $(ARCDIR)/lib32/))  mkdir $(ARCDIR)/lib32/
	if (! (-e $(ARCDIR)/bin/))  mkdir $(ARCDIR)/bin/
	if (! (-e $(ARCDIR)/man/))  mkdir $(ARCDIR)/man/
	if (! (-e $(ARCDIR)/com/))  mkdir $(ARCDIR)/com/
	setup -inst $(ARCDIR) $(SETUP_OPTIONS)
	(cd dist ; \find . -type f -name "*~" -exec rm "{}" \;)
	(cd com ; \find . -type f -name "*~" -exec rm "{}" \;)
	(cd html ; \find . -type f -name "*~" -exec rm "{}" \;)
	($(MAKE) install)
	(cd $(ARCDIR)/bin/; touch imodv; \rm -f imodv; ln -s imod imodv)
	\cp buildlib/*.so $(ARCDIR)/lib/
	\cp -r dist/* $(ARCDIR)/
	\cp -r html/ $(ARCDIR)/
	\cp -r man/ $(ARCDIR)/
	\cp -r com/ $(ARCDIR)/
	\find $(ARCDIR) -name CVS -depth -exec /bin/rm -rf {} \;
	echo "Compressing..."
	$(ARC) $(ARCHIVE) $(ARCNAME); $(COMPRESS) $(ARCHIVE)

#
# Make the manual pages .1 from .man, and .html from .1, copy to directories
# The conversions are called from the localman directory
#
man : ALWAYS
	if (! (-e man))  mkdir man
	if (! (-e man/cat1))  mkdir man/cat1
	if (! (-e man/cat5))  mkdir man/cat5
	if (! (-e html/man))  mkdir html/man
	(cd localman; $(MAKE))
	\cp manpages/*.5 man/cat5
	\cp flib/man/*.1 man/cat1
	\mv manpages/*.1 man/cat1
	\mv manpages/*.html flib/man/*.html html/man/


##################################################################
# Make the full IMOD source distribution
#
src : cleansrc csrc fsrc
	if (-e $(ARCDIR)_src/$(ARCDIR)_src/) /bin/rm -rf $(ARCDIR)_src/$(ARCDIR)_src/
	\find $(ARCDIR)_src -name CVS -depth -exec /bin/rm -rf {} \;
	tar cf $(ARCNAME)_src.tar $(ARCNAME)_src

cleansrc : ALWAYS
	if (-e $(ARCDIR)_src) /bin/rm -rf $(ARCDIR)_src/
	if (-e $(ARCNAME)_src.tar) /bin/rm -rf $(ARCNAME)_src.tar	

# 
# The C source.
#
csrc : ALWAYS
	if (! (-e $(ARCDIR)_src)) mkdir $(ARCDIR)_src/
	cp Makefile setup README .version original_dates $(ARCDIR)_src/
	tar cBf - \
	machines/*[^~] \
	lib*/*.[ch] libdia/*.symbol lib*/Makefile \
	GLw/*.[ch] GLw/Makefile* GLw/README \
	USFFTlib/*/*.a \
	imod/*.[ch] imod/Makefile imod/imodhelp imod/*.bits imod/README \
	imodutil/*.[ch] imodutil/Makefile \
	mrc/*.[ch]    mrc/Makefile \
	clip/*.[ch]   clip/Makefile \
	midas/*.[ch] midas/Makefile  \
	localman/*.c localman/Makefile \
	html/*.*[^~] dist/*[^~] scripts/*[^~] com/*[^~] manpages/*[^~] \
	plugs/*/*.[chf] plugs/*/Makefile plugs/Makefile \
	devkit/*.[ch] devkit/*++ devkit/README devkit/Makefile \
	include/*.h include/*.inc | (cd $(ARCDIR)_src; tar xBf -)

#	replaced this to avoid man pages in man and html, and to avoid ~ files
#	cp -r html dist scripts com man manpages $(ARCDIR)_src/

#   Took this out right after mrc line
#	xmrcv/*.[ch]  xmrcv/Makefile xmrcv/makevms.com \

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

