# IMOD 3.0.7
#
# Startup file for tcsh users of IMOD under Linux
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
    setenv PATH $IMOD_DIR/bin:$PATH
else
    setenv PATH $IMOD_DIR/bin
endif

#
# Set variable with location of the IMOD plugins
#
setenv IMOD_PLUGIN_DIR $IMOD_DIR/lib/imodplug

#
# Tell the system where the IMOD libraries are located.
#
if ($?LD_LIBRARY_PATH) then
	setenv LD_LIBRARY_PATH $IMOD_DIR/lib:$LD_LIBRARY_PATH
else
	setenv LD_LIBRARY_PATH $IMOD_DIR/lib
endif

#
# A subm alias to run command files in the background with submfg
#
alias subm 'submfg \!* &'

#
# This command allows fast backprojection if the USFFT license file exists
#
setenv USFFT2_LICENSE_FILE $IMOD_DIR/license.clo
