# IMOD 3.2.10
#
# Startup file for bash users of IMOD under Linux
#
# It assumes that IMOD is located in /usr/local - if not, modify IMOD_DIR here
# or set IMOD_DIR before sourcing this file
#
# Place this file in /etc/profile.d
#

#
# Set IMOD_DIR if it is not set already; use Windows path format
#
export IMOD_DIR=${IMOD_DIR:=C:\cygwin\usr\local\IMOD}

# Put the IMOD programs on the path
#
export PATH=`cygpath $IMOD_DIR`/bin:$PATH

# Set a variable with the location of calibration/data files
#
export IMOD_CALIB_DIR=${IMOD_CALIB_DIR:=/usr/local/ImodCalib}

# A subm function to run command files in the background with submfg
#
function subm () { submfg $* & }

# Aliases to run imod/3dmod and imodv/3dmodv in background
#
alias imod=3dmodbg
alias 3dmod=3dmodbg
alias imodv=3dmodv
