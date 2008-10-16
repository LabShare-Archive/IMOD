# Command file to run ctfphaseflip
#
####CreatedVersion#### 3.12.20
# 
$ctfphaseflip -StandardInput
InputStack  g5a.ali
AngleFile   g5a.tlt
OutputFileName g5a_ctfcorr.ali
#
# Defocus file from ctfplotter (see man page for format)
DefocusFile g5a.defocus
#
# Microscope voltage in kV
Voltage      200
#
# Microscope spherical aberration in millimeters
SphericalAberration 2
#
# Defocus tolerance in nanometers limiting the strip width
DefocusTol   200
#
# Image pixel size in nanometers
PixelSize  0.906
#
# Fraction of amplitude contrast
AmplitudeContrast 0.07
#
# The distance in pixels between two consecutive strips
InterpolationWidth 20
