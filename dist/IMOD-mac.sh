# IMOD 4.0.29
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

# Put the man pages on the man path only if it exists
#
if [ ! -z "$MANPATH" ] ; then
    export MANPATH=$IMOD_DIR/man:$MANPATH
fi

# Set a variable with the location of calibration/data files
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

# A subm function to run command files in the background with submfg
#
function subm () { submfg $* & }

# Set a variable to indicate where our copy of Qt library is
#
export IMOD_QTLIBDIR=$IMOD_DIR/qtlib
