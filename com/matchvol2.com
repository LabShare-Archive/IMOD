#$if (! -e g5tmpdir) mkdir g5tmpdir
#
# THIS FILE RUNS A SECOND MATCHVOL
# IT IS RUN ONLY IF YOU HAVE TO DO FINDWARP BY HAND, GET A GOOD
# REFINE.XF BY OMITTING ROWS OR COLUMNS, AND HAVE TO RESTART THE PROCESS
#
$matchvol -StandardInput
InputFile       g5b.rec
OutputFile      g5b.mat
#TemporaryDirectory      g5tmpdir
# The default output size is NZ, NY, NX
OutputSizeXYZ   /
TransformFile solve.xf
TransformFile refine.xf
InverseFile inverse.xf
