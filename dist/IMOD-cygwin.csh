# IMOD 3.0.9
#
# Startup file for tcsh users of IMOD under Cygwin
#
# It assumes that IMOD is located in /usr/local - if not, modify IMOD_DIR here
# or set IMOD_DIR before sourcing this file
#
# Place this file in /etc/profile.d

#
# Set IMOD_DIR if it is not set already
#
if (! $?IMOD_DIR) setenv IMOD_DIR /usr/local/IMOD

#
# Put the IMOD programs on the path
#
if ($?PATH) then
    setenv PATH "$IMOD_DIR"/bin:"$PATH"
else
    setenv PATH "$IMOD_DIR"/bin
endif

#
# Set variable with location of the IMOD plugins
#
setenv IMOD_PLUGIN_DIR "$IMOD_DIR"/lib/imodplug

#
# A subm alias to run command files in the background with submfg
#
alias subm 'submfg \!* &'

#
# Aliases to run imod/3dmod and imodv/3dmodv in background
#
alias imod 3dmodbg
alias 3dmod 3dmodbg
alias imodv 3dmodv
