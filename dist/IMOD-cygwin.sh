# IMOD 3.5.2
#
# Startup file for bash users of IMOD under Linux
#
# It assumes that IMOD is located in /usr/local - if not, modify IMOD_DIR here
# or set IMOD_DIR before sourcing this file
#
# Place this file in /etc/profile.d
#

#
# Set IMOD_DIR if it is not set already; use Windows path format with double \
#
export IMOD_DIR=${IMOD_DIR:=`/usr/bin/cygpath -w /usr/local/IMOD`}

# Put the IMOD programs on the path
#
export PATH=`cygpath $IMOD_DIR`/bin:$PATH

# Specify the location of plugins if any
#
export IMOD_PLUGIN_DIR=${IMOD_PLUGIN_DIR:=$IMOD_DIR\\lib\\imodplug}

# Set a variable with the location of calibration/data files, in Windows format
#
export IMOD_CALIB_DIR=${IMOD_CALIB_DIR:=`/usr/bin/cygpath -w /usr/local/ImodCalib`}

# A subm function to run command files in the background with submfg
#
function subm () { submfg $* & }

# Aliases to run imod/3dmod and imodv/3dmodv in background
#
alias imod=3dmodbg
alias 3dmod=3dmodbg
alias imodv=3dmodv
