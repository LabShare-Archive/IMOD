# IMOD 3.4.17
#
# Startup file for users of IMOD on a Macintosh (if they are running tcsh)
#
# It assumes that IMOD is located in /Applications/IMOD - if not, modify 
# IMOD_DIR here or set IMOD_DIR before sourcing this file
#
# Source this file from the user's .cshrc or from a system file /etc/csh.login
# by inserting mac.cshrc (i.e., add to end of /etc/csh.login)

#
# Set IMOD_DIR if it is not set already
#
if (! $?IMOD_DIR) setenv IMOD_DIR /Applications/IMOD

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

#
# Tell the system where the IMOD libraries are located.
#
if ($?DYLD_LIBRARY_PATH) then
	setenv DYLD_LIBRARY_PATH "$IMOD_DIR/lib:$DYLD_LIBRARY_PATH"
else
	setenv DYLD_LIBRARY_PATH $IMOD_DIR/lib
endif

# Set a variable with the location of calibration/data files
#
if (! $?IMOD_CALIB_DIR) setenv IMOD_CALIB_DIR /usr/local/ImodCalib

# A subm alias to run command files in the background with submfg
#
alias subm 'submfg \!* &'

# Set a variable to indicate where our copy of Qt library is
#
setenv IMOD_QTLIBDIR $IMOD_DIR/qtlib

# Set up aliases so that the Qt library is put on the path just for running
# each program, to avoid conflicts with other installed programs
#
alias genhstplt 'runimodqtapp genhstplt'
alias mtpairing 'runimodqtapp mtpairing'
alias avgstatplot 'runimodqtapp avgstatplot'
alias mtoverlap 'runimodqtapp mtoverlap'
alias nda 'runimodqtapp nda'
alias sda 'runimodqtapp sda'
alias mtk 'runimodqtapp mtk'
