# IMOD 3.4.17
#
# Startup file for bash users of IMOD under Linux - place it in /etc/profile.d
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
export PATH=$IMOD_DIR/bin:$PATH

# Set variable with location of the IMOD plugins
#
export IMOD_PLUGIN_DIR=$IMOD_DIR/lib/imodplug

# Tell the system where the IMOD libraries are located.
#
export LD_LIBRARY_PATH=$IMOD_DIR/lib:$LD_LIBRARY_PATH

# Set a variable with the location of calibration/data files
#
export IMOD_CALIB_DIR=${IMOD_CALIB_DIR:=/usr/local/ImodCalib}

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
