# THIS IS A COMMAND FILE TO PRODUCE A PRE-ALIGNED STACK
#
####CreatedVersion#### 3.4.19
# 
$xftoxg
0	global fit
g5a.prexf
g5a.prexg
#
$ blendmont -StandardInput
ImageInputFile		g5a.st
PieceListInput		g5a.pl
ImageOutputFile		g5a.preali
RootNameForEdges	g5a
TransformFile		g5a.prexg
SloppyMontage	1
ShiftPieces	1
ReadInXcorrs	1
#
# To rebuild edge functions, set this to 0
#
OldEdgeFunctions	1
StartingAndEndingX	/
StartingAndEndingY	/
