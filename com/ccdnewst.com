# THIS IS A COMMAND FILE TO MAKE AN ALIGNED STACK FROM THE ORIGINAL STACK
#
####CreatedVersion#### 4.6.18
#
# It assumes that the views are in order in the image stack
#  
# The size argument should be ,, for the full area or specify the desired 
# size (e.g.: ,10)
#
# The offset argument should be 0,0 for no offset, 0,300 to take an area
# 300 pixels above the center, etc.
#
$newstack -StandardInput
InputFile	g5a.st
OutputFile	g5a.ali
TransformFile	g5a.xf
TaperAtFill	1,0
AdjustOrigin
SizeToOutputInXandY	,,
OffsetsInXandY	0,0
#DistortionField	.idf
ImagesAreBinned	1
#GradientFile	g5a.maggrad
