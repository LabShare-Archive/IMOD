#$if (! -e g5tmpdir) mkdir g5tmpdir
#
$if (-e savework-file) savework-file
#
# THIS DOES THE FIRST RUN OF MATCHVOL TO MAKE AN INITIAL MATCHING VOLUME
#
####CreatedVersion#### 3.4.4
#
$matchvol -StandardInput
InputFile       g5b.rec
OutputFile      g5b.mat
#TemporaryDirectory      g5tmpdir
# The default output size is NZ, NY, NX
OutputSizeXYZ   /
TransformFile solve.xf
# Include this output in case you have to run warpvol instead of matchvol later
InverseFile inverse.xf
# Linear interpolation might be better for first stage and will be faster
InterpolationOrder 1
