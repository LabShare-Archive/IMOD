#
# TO RUN TILTXCORR
#
# Add BordersInXandY to use a centered region smaller than the default
# or XMinAndMax and YMinAndMax  to specify a non-centered region
#
$tiltxcorr
InputFile	g5a.st
OutputFile	g5a.prexf
FirstTiltAngle	-60.
TiltIncrement	1.5
RotationAngle	0.
FilterSigma1	0.03
FilterRadius2	0.25
FilterSigma2	0.05
