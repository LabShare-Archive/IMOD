OS_TYPE=windows

if [ -z "$1" ] ; then
    echo usage $0 `$HOME/bin/getRepository.sh -h` "make-target"
    return
fi
if [ $1 == '-h' ] ; then
    echo usage $0 `$HOME/bin/getRepository.sh -h` "make-target"
    return
fi

REPOSITORY=`$HOME/bin/getRepository.sh $1`

# setup IMOD, unit tests, and uitest
source $HOME/bin/setupIMOD.sh $1

CYGHOME=C:\\cygwin\home\sueh

REPOSITORY_HOME=$HOME/workspace/$REPOSITORY
echo repository is $REPOSITORY_HOME
ETOMO_REPOSITORY_HOME=$REPOSITORY_HOME/Etomo

# update repositories

cd $REPOSITORY_HOME
hg update
if [ ! $? -eq 0 ] ; then
    return
fi

cd $HOME/workspace/TomoData
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
cd $HOME/test\ datasets
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
    cp $ETOMO_REPOSITORY_HOME/tests/uitest.make uitest.make
fi

# change power schema so the computer won't sleep
powercfg -l
echo
echo Changing to high performance power schema
powercfg -setactive 8c5e7fda-e8bf-4a96-9a85-a6e23a8c635c
powercfg -l

make -f uitest.make $2
ret=$?
if [ ! $ret -eq 0 ] ; then
    if [ ! $ret -eq 130 ] ; then
	touch nada
	email -s "uitest failed on `hostname` error: $ret" sueh@colorado.edu < nada
	powercfg -setactive 381b4222-f694-41f0-9685-ff5bb260df2e
	powercfg -l
	return
    fi
fi
if [ ! $ret -eq 130 ] ; then
    touch nada
    email -s "uitest $2 succeeded on `hostname`" sueh@colorado.edu < nada
fi

# change power schema so the computer can sleep
powercfg -setactive 381b4222-f694-41f0-9685-ff5bb260df2e
powercfg -l

