# IMOD 3.0.9
#
# Startup file for bash users of IMOD under Linux
#
# It assumes that IMOD is located in /usr/local - if not, modify IMOD_DIR here
# or set IMOD_DIR before sourcing this file
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
# A subm function to run command files in the background with submfg
#
function subm () { submfg $* & }

#
# Aliases to run imod/3dmod and imodv/3dmodv in background
#
alias imod 3dmodbg
alias 3dmod 3dmodbg
alias imodv 3dmodv
