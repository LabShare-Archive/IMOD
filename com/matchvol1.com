$if (-e savework-file) savework-file
#
# THIS DOES THE FIRST RUN OF MATCHVOL TO MAKE AN INITIAL MATCHING VOLUME
#     First two lines are input and output files
#
$matchvol
g5b.rec
g5b.mat
g5tmpdir
/	NX,NY,NZ of output: / for NZ,NY,NX of input
1	Number of transformations: then file for each, or blank line & xform
solve.xf
# Final entry is file name for inverse xform output, or blank line for none
# Include name here in case you have to run warpvol instead of matchvol later
inverse.xf
