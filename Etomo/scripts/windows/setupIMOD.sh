OS_TYPE=windows

if [ ! -z "$1" ] ; then
    if [ $1 == '-h' ] ; then
	echo usage $0 `$HOME/bin/getRepository.sh -h`
	exit
    fi
fi

REPOSITORY=`$HOME/bin/getRepository.sh $1`

# setup IMOD
export IMOD_DIR=C:\\cygwin\\home\\mast\\CUDA2.1-Build\\works
#export LD_LIBRARY_PATH="${QTDIR}/lib:$LD_LIBRARY_PATH"
source "${IMOD_DIR}/IMOD-cygwin.sh"
#export IMOD_TMPDIR="${IMOD_DIR}/tmp"
export PIP_PRINT_ENTRIES=1
echo IMOD_DIR=$IMOD_DIR

# setup compile for processchunks
#export LD_LIBRARY_PATH="${IMOD_QTLIBDIR}:$LD_LIBRARY_PATH"

CYGHOME=C:\\cygwin\\home\\sueh

# setup test
export IMOD_SELF_TEST_DIR=$CYGHOME\\sueh\\SelfTestDir
export IMOD_UNIT_TEST_DATA=$CYGHOME\\workspace\\ImodTests\\EtomoTests\\vectors

# setup java
export IMOD_JAVADIR=C:\\Program\ Files\ \(x86\)\\Java\\jre6
echo IMOD_JAVADIR=$IMOD_JAVADIR

# setup uitest
if [ ! -z "$REPOSITORY" ]; then
    ETOMO_REPOSITORY_HOME=$CYGHOME\\workspace\\$REPOSITORY\\Etomo
    export IMOD_UITEST_SCRIPT=$ETOMO_REPOSITORY_HOME\\scripts
    export IMOD_UITEST_SOURCE=$ETOMO_REPOSITORY_HOME\\tests
    export IMOD_UITEST_DATA=$ETOMO_REPOSITORY_HOME\\uitestData
    export IMOD_UITEST_IMAGE_DATA=$CYGHOME\\workspace\\TomoData
    export IMOD_JUNIT_HOME=C:\\Users\\sueh\\bin\\eclipse\\plugins\\org.junit_4.8.1.v4_8_1_v20100427-1100
    export IMOD_JUNIT_JAR=junit.jar
    export IMOD_JFCUNIT_HOME=C:\\\Users\\sueh\\bin
    export IMOD_JFCUNIT_JAR=jfcunit.jar
    export IMOD_JAKARTA_REGEXP_HOME=C:\\cygwin\\usr\\local\\jfcunit
    export IMOD_JAKARTA_REGEXP_JAR=jakarta-regexp-1.2.jar
    export IMOD_CLASS_SEPARATOR=;
    export IMOD_PATH_SEPARATOR=\\
    export IMOD_ETOMO_HOME=$ETOMO_REPOSITORY_HOME\\jar_dir
    export IMOD_ETOMO_JAR=etomoUITest${OS_TYPE}.jar
    echo Etomo repository is $ETOMO_REPOSITORY_HOME
    echo IMOD_UITEST_SOURCE=$IMOD_UITEST_SOURCE
fi

#temp
#export IMOD_CALIB_DIR=$HOME/ImodCalib
