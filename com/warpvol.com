#$if (! -e g5tmpdir) mkdir g5tmpdir
#
# THIS FILE RUNS WARPVOL 
#
# IT IS RUN ONLY IF YOU HAVE TO DO FINDWARP BY HAND AND
# RESTART THE PROCESS
#
$warpvol -StandardInput
InputFile       g5b.rec
OutputFile      g5b.mat
#TemporaryDirectory      g5tmpdir
# The default output size is NZ, NY, NX
OutputSizeXYZ   /
TransformFile warp.xf
