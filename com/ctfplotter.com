# command file to run ctfplotter
#
####CreatedVersion#### 3.12.20
# 
#
$ctfplotter -StandardInput
#
# You must enter a file that specifies the noise images after "ConfigFile"
# Your entry should be something like:
#   ConfigFile  /usr/local/ImodCalib/CTFnoise/F20.cfg
ConfigFile  
#
InputStack  g5a.st
#
# The tilt angle file - .rawtlt could be used instead if .tlt not available yet
AngleFile   g5a.tlt
DefocusFile g5a.defocus
#
# How many degrees the tilt axis deviates from vertical (Y axis) (CCW positive)
AxisAngle   -4.8 
#
# Image pixel size in nanometers
PixelSize  0.906
#
# Expected defocus at the tilt axis in nanometers (underfocus is positive)
ExpectedDefocus 8000.0 
#
# Starting and ending tilt angles for initial analysis
AngleRange  -20.0  20.0
#
# Microscope voltage in kV
Voltage      200
#
# Microscope spherical aberration in millimeters
SphericalAberration 2
#
# Fraction of amplitude contrast
AmplitudeContrast 0.07
#
# Defocus tolerance in nanometers defining the center strips
DefocusTol   200
PSResolution 101
TileSize     256
LeftDefTol  2000.0
RightDefTol 2000.0 
