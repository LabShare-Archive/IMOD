# IMOD 3.4.17
#
# Startup file for users of IMOD on a Macintosh, running bash
#
# It assumes that IMOD is located in /Applications/IMOD - if not, modify 
# IMOD_DIR here or set IMOD_DIR before sourcing this file
#
# Source this file from the user's .bashrc or from the system file /etc/profile
# by inserting mac.bashrc (i.e., at the end of /etc/profile) 

#
# Set IMOD_DIR if it is not set already
#
export IMOD_DIR=${IMOD_DIR:=/Applications/IMOD}

# Put the IMOD programs on the path
#
export PATH=$IMOD_DIR/bin:$PATH

# Set variable with location of the IMOD plugins
#
export IMOD_PLUGIN_DIR=$IMOD_DIR/lib/imodplug

# Tell the system where the IMOD libraries are located.
#
export DYLD_LIBRARY_PATH=$IMOD_DIR/lib:$DYLD_LIBRARY_PATH

# Set a variable with the location of calibration/data files
#
export IMOD_CALIB_DIR=${IMOD_CALIB_DIR:=/usr/local/ImodCalib}

# A subm function to run command files in the background with submfg
#
function subm () { submfg $* & }

# Set a variable to indicate where our copy of Qt library is
#
export IMOD_QTLIBDIR=$IMOD_DIR/qtlib

# Set up aliases to run qt programs through runimodqtapp
#
alias genhstplt='runimodqtapp genhstplt'
alias mtpairing='runimodqtapp mtpairing'
alias avgstatplot='runimodqtapp avgstatplot'
alias mtoverlap='runimodqtapp mtoverlap'
alias nda='runimodqtapp nda'
alias sda='runimodqtapp sda'
alias mtk='runimodqtapp mtk'
