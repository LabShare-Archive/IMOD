# THIS IS A COMMAND FILE TO PRODUCE A PRE-ALIGNED STACK
# 
# The stack will be floated and converted to bytes under the assumption that
# you will go back to the raw stack to make the final aligned stack
#
$xftoxg
0
g5a.prexf
g5a.prexg
$newstack -StandardInput
InputFile	g5a.st
OutputFile	g5a.preali
TransformFile	g5a.prexg
ModeToOutput	0
FloatDensities 2
BinByFactor	1
#DistortionField	.idf
ImagesAreBinned	1
#GradientFile	g5a.maggrad
