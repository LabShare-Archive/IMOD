# THIS IS A COMMAND FILE TO RUN TILTXCORR AND DETERMINE CROSS-CORRELATION
# ALIGNMENT OF A TILT SERIES
#
# Set the following goto to 'doxcorr' to skip the blend
$goto doxcorr
$doblend:
# Command file to run BLENDMONT
#
####CreatedVersion#### 3.10.23
#
$blendmont -StandardInput
ImageInputFile		testmidzone2b.st
PieceListInput		testmidzone2b.pl
ImageOutputFile	testmidzone2b.bl
RootNameForEdges	testmidzone2b
#TransformFile		testmidzone2b.xf
#DistortionField	.idf
ImagesAreBinned	1
#GradientFile	testmidzone2b.maggrad
AdjustedFocus   0
SloppyMontage	1
ShiftPieces	1
ReadInXcorrs	1
OldEdgeFunctions	1
StartingAndEndingX	0 1023
StartingAndEndingY	0 1945
$doxcorr:
#
# TO RUN TILTXCORR
#
####CreatedVersion#### 3.4.4
#
# Add BordersInXandY to use a centered region smaller than the default
# or XMinAndMax and YMinAndMax  to specify a non-centered region
#
$tiltxcorr -StandardInput
InputFile	testmidzone2b.bl
OutputFile	testmidzone2b.prexf
TiltFile	testmidzone2b.rawtlt
RotationAngle	0.0
FilterSigma1	0.03
FilterRadius2	0.25
FilterSigma2	0.05
$if (-e ./savework) ./savework
