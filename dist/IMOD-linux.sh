# IMOD 3.0.7
#
# Startup file for bash users of IMOD under Linux
#
# It assumes that IMOD is located in /usr/local - if not, modify IMOD_DIR
#
# Place this file in /etc/profile.d
#

#
# Set IMOD_DIR if it is not set already
#
export IMOD_DIR=${IMOD_DIR:=/usr/local/IMOD}

#
# Put the IMOD programs on the path
#
export PATH=$IMOD_DIR/bin:$PATH

#
# Set variable with location of the IMOD plugins
#
export IMOD_PLUGIN_DIR=$IMOD_DIR/lib/imodplug

#
# Tell the system where the IMOD libraries are located.
#
export LD_LIBRARY_PATH=$IMOD_DIR/lib:$LD_LIBRARY_PATH

#
# A subm function to run command files in the background with submfg
#
function subm () { submfg $* & }

#
# This command allows fast backprojection if the USFFT license file exists
#
export USFFT2_LICENSE_FILE=$IMOD_DIR/license.clo
