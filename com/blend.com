# Command file to run BLENDMONT
#
####CreatedVersion#### 3.4.4
#
$ blendmont -StandardInput
ImageInputFile		g5a.st
PieceListInput		g5a.pl
ImageOutputFile		g5a.ali
RootNameForEdges	g5a
TransformFile		g5a.xf
#SloppyMontage
#ShiftPieces
#ReadInXcorrs
#OldEdgeFunctions
StartingAndEndingX	/
StartingAndEndingY	/
$mrctaper g5a.ali
