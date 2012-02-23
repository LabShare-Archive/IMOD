# Command file to run Tilt
#
####CreatedVersion#### 4.0.15
# 
# RADIAL specifies the frequency at which the Gaussian low pass filter begins
#   followed by the standard deviation of the Gaussian roll-off
#
# LOG takes the logarithm of tilt data after adding the given value
#
$tilt -StandardInput
InputProjections testmidzone2b.ali
OutputFile testmidzone2b.rec
IMAGEBINNED 1
FULLIMAGE 1024 1950
LOCALFILE testmidzone2blocal.xf
LOG 0.0
MODE 1
OFFSET 0.0
PERPENDICULAR 
AdjustOrigin 
RADIAL 0.35 0.05
SCALE 0.0 1000.0
SHIFT 0.0 0.0
SUBSETSTART -13 -25
THICKNESS 84
TILTFILE testmidzone2b.tlt
XAXISTILT 0.18
XTILTFILE testmidzone2b.xtilt
ZFACTORFILE testmidzone2b.zfac
ActionIfGPUFails 1,2
$if (-e ./savework) ./savework
