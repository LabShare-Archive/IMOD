# Command file to run BLENDMONT
#
####CreatedVersion#### 3.6.1
#
$ blendmont -StandardInput
ImageInputFile		g5a.st
PieceListInput		g5a.pl
ImageOutputFile		g5a.ali
RootNameForEdges	g5a
TransformFile		g5a.xf
#DistortionField	.idf
ImagesAreBinned	1
#GradientFile	g5a.maggrad
AdjustedFocus   0
SloppyMontage	0
ShiftPieces	0
ReadInXcorrs	0
OldEdgeFunctions	0
StartingAndEndingX	/
StartingAndEndingY	/
$mrctaper g5a.ali
