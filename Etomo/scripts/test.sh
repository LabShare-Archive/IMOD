OS_TYPE=linux

if [ -z "$1" ] ; then
    echo usage $0 `$HOME/bin/getRepository.sh -h` "make-target"
    return
fi
if [ $1 == '-h' ] ; then
    echo usage $0 `$HOME/bin/getRepository.sh -h` "make-target"
    return
fi

if [ $1 == 'v' ] ; then
    echo $1 $2 $3
    REPOSITORY=`$HOME/bin/getRepository.sh $1 $2`
    source $HOME/bin/setupIMOD.sh $1 $2
    TARGET=$3
else
    REPOSITORY=`$HOME/bin/getRepository.sh $1`
    source $HOME/bin/setupIMOD.sh $1
    TARGET=$2
fi

REPOSITORY_HOME=$HOME/workspace/$REPOSITORY
echo repository is $REPOSITORY_HOME
ETOMO_REPOSITORY_HOME=$REPOSITORY_HOME/Etomo

# update repositories

cd $REPOSITORY_HOME
hg update
if [ ! $? -eq 0 ] ; then
    return
fi

echo $IMOD_UITEST_IMAGE_DATA
cd $IMOD_UITEST_IMAGE_DATA
cvs update -Pd
if [ ! $? -eq 0 ] ; then
    return
fi

# make jar
cd $ETOMO_REPOSITORY_HOME
pwd
make uitestjarfile
# Use a different jar for different OS's because linux and mac run from the
# same repository at the same time
cd jar_dir
pwd
mv -f etomoUITest.jar $IMOD_ETOMO_JAR

# go to test area
cd /home/NOBACKUP/sueh/test\ datasets
if [ ! -e "${REPOSITORY}" ]; then
    mkdir $REPOSITORY
fi
cd $REPOSITORY
if [ ! -e "${OS_TYPE}" ]; then
    mkdir $OS_TYPE
fi
cd $OS_TYPE
pwd

# test
# create modifiable local uitest.make
if [ ! -e "uitest.make" ]; then
    cp $IMOD_UITEST_SOURCE/uitest.make uitest.make
fi
make -f uitest.make $TARGET
ret=$?
if [ ! $ret -eq 0 ] ; then
    if [ ! $ret -eq 130 ] ; then
	ll -rt|tail>nada
	mail -s "uitest $TARGET on $REPOSITORY failed on `hostname` error: $ret" sueh@colorado.edu < nada
	return
    fi
fi
if [ ! $ret -eq 130 ] ; then
    ll -rt|tail>nada
    mail -s "uitest $TARGET on $REPOSITORY succeeded on `hostname`" sueh@colorado.edu < nada
fi
