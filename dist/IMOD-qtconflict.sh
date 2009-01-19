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
if [ -z "$qtdir" ] ; then
    if [ ! -z "$QTDIR" ] ; then
        qtdir=$QTDIR/lib
    else
        #
        # otherwise look for qt3 or just qt entries in /etc/ld.so.conf
        # 
        qt3=`grep '/qt-3' /etc/ld.so.conf | awk '{print $1}'`
        qtlink=`grep '/qt/' /etc/ld.so.conf | awk '{print $1}'`
        qtfile=""
        if [ -e /etc/ld.so.conf.d ] ; then 
            qtfile=`\find /etc/ld.so.conf.d -name 'qt*' -exec cat '{}' \;`
        fi
        if [ ! -z "$qtlink" ] ; then
            qtdir=$qtlink
        elif [ ! -z "$qt3" ] ; then
            qtdir=$qt3
        elif [ ! -z "$qtfile" ] ; then
            qtdir=$qtfile
        else
            echo 'WARNING: IMOD-qtconflict.sh cannot find the system Qt libraries'
            echo 'IMOD-qtconflict.sh should be edited to set qtdir to the path'
            echo 'to the system Qt libraries'
            return
        fi
    fi
fi

#
# Set this variable to be used to set LD_LIBRARY_PATH in runimodqtapp
# Put other libs on this list to avoid conflicts with GL libraries placed
# on LD_LIBRARY_PATH by other installed packages
#
export IMOD_QTLIBDIR=${qtdir}:/usr/lib:/usr/X11R6/lib

#
# Set up aliases to run all qt programs through runimodqtapp
#
alias 3dmod='runimodqtapp 3dmod'
alias 3dmodv='runimodqtapp 3dmodv'
alias imod='runimodqtapp imod'
alias imodv='runimodqtapp imodv'
alias midas='runimodqtapp midas'
alias ctfplotter='runimodqtapp ctfplotter'
alias imodsendevent='runimodqtapp imodsendevent'
alias genhstplt='runimodqtapp genhstplt'
alias mtpairing='runimodqtapp mtpairing'
alias avgstatplot='runimodqtapp avgstatplot'
alias mtoverlap='runimodqtapp mtoverlap'
alias nda='runimodqtapp nda'
alias sda='runimodqtapp sda'
alias mtk='runimodqtapp mtk'
