# IMOD 3.2.10
#
# Startup file for tcsh users of IMOD under Cygwin
#
# It assumes that IMOD is located in /usr/local - if not, modify IMOD_DIR here
# or set IMOD_DIR before sourcing this file
#
# Place this file in /etc/profile.d

#
# Set IMOD_DIR if it is not set already, use Windows path format with double \
#
if (! $?IMOD_DIR) setenv IMOD_DIR C:\\cygwin\\usr\\local\\IMOD

# Put the IMOD programs on the path
#
if ($?PATH) then
    setenv PATH `/usr/bin/cygpath $IMOD_DIR`/bin:"$PATH"
else
    setenv PATH `/usr/bin/cygpath $IMOD_DIR`/bin
endif

# Specify the location of plugins if any
#
if (! $?IMOD_PLUGIN_DIR) setenv IMOD_PLUGIN_DIR $IMOD_DIR\\lib\\imodplug


# Set a variable with the location of calibration/data files, in Windows format
#
if (! $?IMOD_CALIB_DIR) setenv IMOD_CALIB_DIR C:\\cygwin\\usr\\local\\ImodCalib

# A subm alias to run command files in the background with submfg
#
alias subm 'submfg \!* &'

# Aliases to run imod/3dmod and imodv/3dmodv in background
#
alias imod 3dmodbg
alias 3dmod 3dmodbg
alias imodv 3dmodv
