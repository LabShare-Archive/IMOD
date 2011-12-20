OS_TYPE=windows

if [[ ! -z "$1" && $1 == '-h' ]] ; then
    echo usage $0 `$HOME/bin/getRepository.sh -h`
    exit
fi

REPOSITORY=`$HOME/bin/getRepository.sh $1`

# setup IMOD, unit tests, and uitest
source $HOME/bin/setupIMOD.sh $1

# run eclipse
/cygdrive/c/Users/sueh/bin/eclipse/eclipse.exe
