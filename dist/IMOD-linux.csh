# IMOD 4.0.22
#
# Startup file for tcsh users of IMOD under Linux - place it in /etc/profile.d
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
if (! $?IMOD_DIR) setenv IMOD_DIR /usr/local/IMOD

# Set IMOD_JAVADIR if it is not set already
#
if (! $?IMOD_JAVADIR) setenv IMOD_JAVADIR /usr/local/java

# Put the IMOD programs on the path
#
if ($?PATH) then
    if ( "$PATH" !~ *"$IMOD_DIR"/bin* ) then
        setenv PATH "$IMOD_DIR/bin:$PATH"
    endif
else
    setenv PATH "$IMOD_DIR/bin"
endif

# Set variable with location of the IMOD plugins
#
setenv IMOD_PLUGIN_DIR "$IMOD_DIR/lib/imodplug"

# Tell the system where the IMOD libraries are located.
#
if ($?LD_LIBRARY_PATH) then
    setenv LD_LIBRARY_PATH "$IMOD_DIR/lib:$LD_LIBRARY_PATH"
else
    setenv LD_LIBRARY_PATH "$IMOD_DIR/lib"
endif

# Put the man pages on the man path only if it exists
#
if ($?MANPATH) then
    setenv MANPATH "$IMOD_DIR/man:$MANPATH"
endif

# Set a variable with the location of configuration/calibration/data files
#
if (! $?IMOD_CALIB_DIR) setenv IMOD_CALIB_DIR /usr/local/ImodCalib

# Source local startup file in ImodCalib if it exists
#
if (-r $IMOD_CALIB_DIR/IMOD.csh) source $IMOD_CALIB_DIR/IMOD.csh

# Disable alarming stack traces from Intel Fortran
#
setenv FOR_DISABLE_STACK_TRACE 1

# A subm alias to run command files in the background with submfg
#
alias subm 'submfg \!* &'

# Set a variable to indicate where our copy of Qt library is
#
setenv IMOD_QTLIBDIR "$IMOD_DIR/qtlib"
