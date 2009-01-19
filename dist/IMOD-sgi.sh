# IMOD 3.13.1
#
# Startup file for bash users of IMOD on an SGI
#
# It assumes that IMOD is located in /usr/local/IMOD - if not, modify IMOD_DIR
# here or set IMOD_DIR before sourcing this file
#
# Source this file from the user's .bashrc or from the system file
# /etc/profile by inserting sgi.bashrc
#
# It assumes that the java run-time environment is installed at the
# recommended location - to use a different jre, modify IMOD_JAVADIR here or
# set IMOD_JAVADIR before sourcing this file

#
# Set IMOD_DIR if it is not set already
#
export IMOD_DIR=${IMOD_DIR:=/usr/local/IMOD}

# Set IMOD_JAVADIR if it is not set already
#
export IMOD_JAVADIR=${IMOD_JAVADIR:=/usr/local/java2v14}

# Put the IMOD programs on the path
#
export PATH=$IMOD_DIR/bin:$PATH

# Set variable with location of the IMOD plugins
#
export IMOD_PLUGIN_DIR=$IMOD_DIR/lib/imodplug

# Tell the system where the IMOD libraries are located.
#
export LD_LIBRARY_PATH=$IMOD_DIR/lib:$LD_LIBRARY_PATH
export LD_LIBRARYN32_PATH=$IMOD_DIR/lib32:$LD_LIBRARYN32_PATH

# Put the man pages on the man path
#
export MANPATH=$IMOD_DIR/man:$MANPATH

# Set a variable with the location of configuration/calibration/data files
#
export IMOD_CALIB_DIR=${IMOD_CALIB_DIR:=/usr/local/ImodCalib}

# Source local startup file in ImodCalib if it exists
#
if [ -r $IMOD_CALIB_DIR/IMOD.sh ] ; then
    . $IMOD_CALIB_DIR/IMOD.sh
fi

# A subm function to run command files in the background with submfg
#
function subm () { submfg $* & }

# This command allows fast backprojection if the USFFT license file exists
# in either /usr/local/USFFT by hostname, or in IMOD_DIR
#
if [ -d /usr/local/USFFT ] ; then
    export USFFT2_LICENSE_FILE=/usr/local/USFFT/license.clo.$HOST
else
    export USFFT2_LICENSE_FILE=$IMOD_DIR/license.clo
fi

# Set a variable to indicate where our copy of Qt library is
#
export IMOD_QTLIBDIR=$IMOD_DIR/qtlib

# Set up aliases to run all qt programs through runimodqtapp
#
alias 3dmod='runimodqtapp 3dmod'
alias 3dmodv='runimodqtapp 3dmodv'
alias imod='runimodqtapp imod'
alias imodv='runimodqtapp imodv'
alias midas='runimodqtapp midas'
alias ctfplotter='runimodqtapp ctfplotter'
alias imodsendevent='runimodqtapp imodsendevent'
alias genhstplt='runimodqtapp genhstplt'
alias mtpairing='runimodqtapp mtpairing'
alias avgstatplot='runimodqtapp avgstatplot'
alias mtoverlap='runimodqtapp mtoverlap'
alias nda='runimodqtapp nda'
alias sda='runimodqtapp sda'
alias mtk='runimodqtapp mtk'
