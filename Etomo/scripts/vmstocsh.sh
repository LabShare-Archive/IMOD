#/bin/bash
vmstocsh < $1 $2  | csh -ef
exitValue=$?
#echo "Exit value: " $exitValue
#exit $exitValue
