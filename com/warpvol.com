# THIS FILE RUNS WARPVOL 
#
# IT IS RUN ONLY IF YOU HAVE TO DO FINDWARP BY HAND AND
# RESTART THE PROCESS
#
$warpvol
g5b.rec
g5b.mat
g5tmpdir
/	NX,NY,NZ of output: / for NZ,NY,NX of input
warp.xf
