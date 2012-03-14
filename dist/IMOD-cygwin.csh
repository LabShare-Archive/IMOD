# IMOD 4.4.7
#
# Startup file for tcsh users of IMOD under Cygwin
#
# It assumes that IMOD is located in /usr/local/IMOD - if not, modify 
# IMOD_DIR here or set IMOD_DIR before sourcing this file
#
# Place this file in /etc/profile.d

#
# Set IMOD_DIR if it is not set already, use Windows path format with double \
#
if (! $?IMOD_DIR) then
    setenv IMOD_DIR `/usr/bin/cygpath -w "/usr/local/IMOD"`
else
    /usr/bin/echo "$IMOD_DIR" | grep ' $' > /dev/null
    if (! $?) then
        echo "Environment variable IMOD_DIR has a space at the end.  You should fix this."
        setenv IMOD_DIR `/usr/bin/cygpath -w "$IMOD_DIR"`
    endif
endif

# Put the IMOD programs on the path
#
if ($?PATH) then
    setenv PATH `/usr/bin/cygpath "$IMOD_DIR"`/bin:"$PATH"
else
    setenv PATH `/usr/bin/cygpath "$IMOD_DIR"`/bin
endif

# Specify the location of plugins if any
#
if (! $?IMOD_PLUGIN_DIR) setenv IMOD_PLUGIN_DIR "$IMOD_DIR\lib\imodplug"


# Set a variable with the location of calibration/data files, in Windows format
#
if (! $?IMOD_CALIB_DIR) setenv IMOD_CALIB_DIR `/usr/bin/cygpath -w "/usr/local/ImodCalib"`

# Source local startup file in ImodCalib if it exists
#
set IMOD_CALIB_CYG = `/usr/bin/cygpath "$IMOD_CALIB_DIR"`
if (-r "$IMOD_CALIB_CYG/IMOD.csh") source "$IMOD_CALIB_CYG/IMOD.csh"

# Disable stack traces from Intel Fortran which sometimes hang DOS windows
#
setenv FOR_DISABLE_STACK_TRACE 1

# A subm alias to run command files in the background with submfg
#
alias subm 'submfg \!* &'

# Aliases to run imod/3dmod and imodv/3dmodv in background
#
alias imod 3dmodbg
alias 3dmod 3dmodbg
alias imodv 3dmodv
alias processchunks processchunksbg
