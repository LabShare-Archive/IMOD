#$if (! -e g5tmpdir) mkdir g5tmpdir
#
# THIS FILE RUNS A SECOND MATCHVOL
# IT IS RUN ONLY IF YOU HAVE TO DO FINDWARP BY HAND, GET A GOOD
# REFINE.XF BY OMITTING ROWS OR COLUMNS, AND HAVE TO RESTART THE PROCESS
#
$matchvol
g5b.rec
g5b.mat
g5tmpdir
/	NX,NY,NZ of output: / for NZ,NY,NX of input
2	Number of transformations: then file for each, or blank line & xform
solve.xf
refine.xf
inverse.xf
