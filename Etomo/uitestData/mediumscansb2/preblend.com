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
ImageInputFile	mediumscansb2.rec
PieceListInput	mediumscansb2.pl
ImageOutputFile	mediumscansb2_preblend.mrc
RootNameForEdges	mediumscansb2
#
# Change this to 1 after the first run if you fix edges in midas
ReadInXcorrs	0
OldEdgeFunctions	0
AdjustOrigin	
VerySloppyMontage	
