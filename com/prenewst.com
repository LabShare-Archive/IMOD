# THIS IS A COMMAND FILE TO PRODUCE A PRE-ALIGNED STACK
# 
# The stack will be floated and converted to bytes under the assumption that
# you will go back to the raw stack to make the final aligned stack
#
$xftoxg
0	global fit
g5a.prexf
g5a.prexg
$newstack -fl 2 -mo 0 -xf g5a.prexg g5a.st g5a.preali
