# Command file to run ctfphaseflip
#
####CreatedVersion#### 3.10.21
# 
$ctfphaseflip -StandardInput
InputStack  series4-8um.ali
AngleFile   series4-8um.tlt
OutputFileName series4-8umCorrected.ali
#
# Defocus file from ctfplotter (see man page for format)
DefocusFile series4-8um.defocus
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
