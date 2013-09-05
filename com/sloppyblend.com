# Command file to run BLENDMONT on an arbitrary montage such as a Navigator map
#
# For a montage from SerialEM, first extract the piece list:
#   extractpieces filename.st filename.pl
#
# Modify all the g5a's here to match your filename
#
# Use VerySloppyMontage for stage montages 
#
$blendmont -StandardInput
ImageInputFile          g5a.st
PieceListInput          g5a.pl
ImageOutputFile         g5a.bl
RootNameForEdges        g5a
#
# Uncomment to make an aligned stack after getting transforms
#TransformFile          g5a.xf
#
# Use VerySloppyMontage instead if overlaps are particularly variable
SloppyMontage   1
#
# Change this to 1 after the first run if you fix edges in midas
ReadInXcorrs    0
OldEdgeFunctions        0
StartingAndEndingX      /
StartingAndEndingY      /
