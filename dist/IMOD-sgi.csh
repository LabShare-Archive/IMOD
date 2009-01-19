# IMOD 3.13.1
#
# Startup file for users of IMOD on an SGI (if they are running csh or tcsh)
#
# It assumes that IMOD is located in /usr/local/IMOD - if not, modify IMOD_DIR
# here or set IMOD_DIR before sourcing this file
#
# Source this file from the user's .cshrc or from the system cshrc file
# (probably /etc/cshrc) by inserting sgi.cshrc
#
# It assumes that the java run-time environment is installed at the
# recommended location - to use a different jre, modify IMOD_JAVADIR here or
# set IMOD_JAVADIR before sourcing this file

#
# Set IMOD_DIR if it is not set already
#
if (! $?IMOD_DIR) setenv IMOD_DIR /usr/local/IMOD

# Set IMOD_JAVADIR if it is not set already
#
if (! $?IMOD_JAVADIR) setenv IMOD_JAVADIR /usr/java2v14

# Put the IMOD programs on the path
#
if ($?PATH) then
    setenv PATH "$IMOD_DIR/bin:$PATH"
else
    setenv PATH $IMOD_DIR/bin
endif

# Set variable with location of the IMOD plugins
#
setenv IMOD_PLUGIN_DIR $IMOD_DIR/lib/imodplug

# Tell the system where the IMOD libraries are located.
#
if ($?LD_LIBRARY_PATH) then
	setenv LD_LIBRARY_PATH "$IMOD_DIR/lib:$LD_LIBRARY_PATH"
else
	setenv LD_LIBRARY_PATH $IMOD_DIR/lib
endif
if ($?LD_LIBRARYN32_PATH) then
	setenv LD_LIBRARYN32_PATH "$IMOD_DIR/lib32:$LD_LIBRARYN32_PATH"
else
	setenv LD_LIBRARYN32_PATH $IMOD_DIR/lib32
endif

# Put the man pages on the man path
#
if ($?MANPATH) then
	setenv MANPATH "$IMOD_DIR/man:$MANPATH"
else
	setenv MANPATH $IMOD_DIR/man
endif

# Set a variable with the location of configuration/calibration/data files
#
if (! $?IMOD_CALIB_DIR) setenv IMOD_CALIB_DIR /usr/local/ImodCalib

# Source local startup file in ImodCalib if it exists
#
if (-r $IMOD_CALIB_DIR/IMOD.csh) source $IMOD_CALIB_DIR/IMOD.csh

# A subm alias to run command files in the background with submfg
#
alias subm 'submfg \!* &'

# This command allows fast backprojection if the USFFT license file exists
#
# in either /usr/local/USFFT by hostname, or in IMOD_DIR
#
if (-d /usr/local/USFFT) then
    setenv USFFT2_LICENSE_FILE /usr/local/USFFT/license.clo.$HOST
else
    setenv USFFT2_LICENSE_FILE $IMOD_DIR/license.clo
endif

# Set a variable to indicate where our copy of Qt library is
#
setenv IMOD_QTLIBDIR $IMOD_DIR/qtlib

# Set up aliases so that the Qt library is put on the path just for running
# each program, to avoid conflicts with other installed programs
#
alias 3dmod 'runimodqtapp 3dmod'
alias 3dmodv 'runimodqtapp 3dmodv'
alias imod 'runimodqtapp imod'
alias imodv 'runimodqtapp imodv'
alias midas 'runimodqtapp midas'
alias ctfplotter 'runimodqtapp ctfplotter'
alias imodsendevent 'runimodqtapp imodsendevent'
alias genhstplt 'runimodqtapp genhstplt'
alias mtpairing 'runimodqtapp mtpairing'
alias avgstatplot 'runimodqtapp avgstatplot'
alias mtoverlap 'runimodqtapp mtoverlap'
alias nda 'runimodqtapp nda'
alias sda 'runimodqtapp sda'
alias mtk 'runimodqtapp mtk'
