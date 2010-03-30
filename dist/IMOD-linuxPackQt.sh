# IMOD 4.0.22
#
# Startup file for bash users of IMOD under Linux - place it in /etc/profile.d
#
# This version provides setup for a distribution that includes a Qt library
#
# It assumes that IMOD is located in /usr/local/IMOD - if not, modify IMOD_DIR
# here or set IMOD_DIR before sourcing this file
#
# It assumes that there is link to the java run-time environment in /usr/local
# to use a specific jre, modify IMOD_JAVADIR here or set IMOD_JAVADIR before
# sourcing this file

#
# Set IMOD_DIR if it is not set already
#
export IMOD_DIR=${IMOD_DIR:=/usr/local/IMOD}

# Set IMOD_JAVADIR if it is not set already
#
export IMOD_JAVADIR=${IMOD_JAVADIR:=/usr/local/java}

# Put the IMOD programs on the path
#
if ! echo ${PATH} | /bin/grep -q "$IMOD_DIR/bin" ; then
    export PATH=$IMOD_DIR/bin:$PATH
fi

# Set variable with location of the IMOD plugins
#
export IMOD_PLUGIN_DIR=$IMOD_DIR/lib/imodplug

# Tell the system where the IMOD libraries are located.
#
export LD_LIBRARY_PATH=$IMOD_DIR/lib:$LD_LIBRARY_PATH

# Put the man pages on the man path only if it exists
#
if [ ! -z "$MANPATH" ] ; then
    export MANPATH=$IMOD_DIR/man:$MANPATH
fi

# Set a variable with the location of configuration/calibration/data files
#
export IMOD_CALIB_DIR=${IMOD_CALIB_DIR:=/usr/local/ImodCalib}

# Source local startup file in ImodCalib if it exists
#
if [ -r $IMOD_CALIB_DIR/IMOD.sh ] ; then
    . $IMOD_CALIB_DIR/IMOD.sh
fi

# Disable alarming stack traces from Intel Fortran
#
export FOR_DISABLE_STACK_TRACE=1

# Set a variable to indicate where our copy of Qt library is
#
export IMOD_QTLIBDIR="$IMOD_DIR/qtlib"

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
alias filterplot='runimodqtapp filterplot'
alias mtoverlap='runimodqtapp mtoverlap'
alias nda='runimodqtapp nda'
alias sda='runimodqtapp sda'
alias mtk='runimodqtapp mtk'

# A subm function to run command files in the background with submfg
#
if [ -z "$BASH" ] ; then return 0 ; fi
function subm () { submfg $* & }
