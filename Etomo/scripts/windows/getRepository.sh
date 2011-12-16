if [ -z "$1" ] ; then
    echo
elif [ $1 == '-h' ] ; then
    echo "[1|2|c|t|i|m]"
elif [ $1 == '1' ] ; then
    echo Development1
elif [ $1 == '2' ] ; then
    echo Development2
elif [ $1 == 'c' ] ; then
    echo Completed
elif [ $1 == 't' ] ; then
    echo DevelopmentToInt
elif [ $1 == 'i' ] ; then
    echo Integration
elif [ $1 == 'm' ] ; then
    echo DevelopmentToMaster
fi
