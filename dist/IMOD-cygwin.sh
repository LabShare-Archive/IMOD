# IMOD 4.4.7
#
# Startup file for bash users of IMOD under Cygwin
#
# It assumes that IMOD is located in /usr/local/IMOD - if not, modify 
# IMOD_DIR here or set IMOD_DIR before sourcing this file
#
# Place this file in /etc/profile.d
#

#
# Set IMOD_DIR if it is not set already; use Windows path format with double \
#
if [ -z "$IMOD_DIR" ]; then
    export IMOD_DIR=`/usr/bin/cygpath -w "/usr/local/IMOD"`
else
    /usr/bin/echo "$IMOD_DIR" | grep ' $' > /dev/null
    if [ $? -eq 0 ] ; then
        echo "Environment variable IMOD_DIR has a space at the end.  You should fix this."
        export IMOD_DIR=`/usr/bin/cygpath -w "$IMOD_DIR"`
    fi
fi

# Put the IMOD programs on the path
#
export PATH=`/usr/bin/cygpath $IMOD_DIR`/bin:$PATH

# Specify the location of plugins if any
#
export IMOD_PLUGIN_DIR=${IMOD_PLUGIN_DIR:=$IMOD_DIR\\lib\\imodplug}

# Set a variable with the location of calibration/data files, in Windows format
#
export IMOD_CALIB_DIR=${IMOD_CALIB_DIR:=`/usr/bin/cygpath -w /usr/local/ImodCalib`}

# Source local startup file in ImodCalib if it exists
#
IMOD_CALIB_CYG=`/usr/bin/cygpath "$IMOD_CALIB_DIR"`
if [ -r "$IMOD_CALIB_CYG/IMOD.sh" ] ; then
    . "$IMOD_CALIB_CYG/IMOD.sh"
fi

# Disable stack traces from Intel Fortran that can hang DOS windows
#
export FOR_DISABLE_STACK_TRACE=1

# A subm function to run command files in the background with submfg
#
function subm () { submfg $* & }

# Aliases to run imod/3dmod and imodv/3dmodv in background
#
alias imod=3dmodbg
alias 3dmod=3dmodbg
alias imodv=3dmodv
alias processchunks=processchunksbg
