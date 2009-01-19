# Copy this startup file to /etc/profile.d if 3dmod and other Qt-based programs
# will not run because of a conflicting Qt library placed on your 
# LD_LIBRARY_PATH by some other installed package.
#
# If this script fails to detect the location of the system Qt libraries,
# uncomment the following statement and fill in the proper path
# 
# set qtdir = 


#
# If qtdir is not yet set and QTDIR is set by a qt-devel installation, then
# use the QTDIR location
#
if (! $?qtdir) then
    if ($?QTDIR) then
        set qtdir = $QTDIR/lib
    else
        #
        # otherwise look for qt3 or just qt entries in /etc/ld.so.conf
        # 
        set qt3 = `grep '/qt-3' /etc/ld.so.conf`
        set qtlink = `grep '/qt/' /etc/ld.so.conf`
        set qtfile = ""
        if (-e /etc/ld.so.conf.d) set qtfile = `\find /etc/ld.so.conf.d -name 'qt*' -exec cat '{}' \;`
        if ($#qtlink > 0) then
            set qtdir = $qtlink[1]
        else if ($#qt3 > 0) then
            set qtdir = $qt3[1]
        else if ($#qtfile > 0) then
            set qtdir = $qtfile[1]
        else
            echo 'WARNING: IMOD-qtconflict.csh cannot find the system Qt libraries'
            echo 'IMOD-qtconflict.csh should be edited to set qtdir to the path'
            echo 'to the system Qt libraries'
            goto end
        endif
    endif
endif

#
# Set this variable to be used to set LD_LIBRARY_PATH in runimodqtapp
# Put other libs on this list to avoid conflicts with GL libraries placed
# on LD_LIBRARY_PATH by other installed packages
#
setenv IMOD_QTLIBDIR ${qtdir}:/usr/lib:/usr/X11R6/lib

#
# Set up aliases to run all qt programs through runimodqtapp
#
alias 3dmod 'runimodqtapp 3dmod'
alias 3dmodv 'runimodqtapp 3dmodv'
alias imod 'runimodqtapp imod'
alias imodv 'runimodqtapp imodv'
alias midas 'runimodqtapp midas'
alias ctfplotter 'runimodqtapp ctfplotter'
alias imodsendevent 'runimodqtapp imodsendevent'
alias genhstplt 'runimodqtapp genhstplt'
alias mtpairing 'runimodqtapp mtpairing'
alias avgstatplot 'runimodqtapp avgstatplot'
alias mtoverlap 'runimodqtapp mtoverlap'
alias nda 'runimodqtapp nda'
alias sda 'runimodqtapp sda'
alias mtk 'runimodqtapp mtk'

end:
