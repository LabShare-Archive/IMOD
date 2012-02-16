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
InputProjections testmidzone2a_3dfind.ali
OutputFile testmidzone2a_3dfind.rec
IMAGEBINNED 2
FULLIMAGE 1950 1024
LOCALFILE testmidzone2alocal.xf
LOG 0.0
MODE 1
OFFSET 0.0
PERPENDICULAR 
AdjustOrigin 
RADIAL 0.35 0.05
SCALE 0.0 500.0
SHIFT 0.0 28.9
SUBSETSTART -25 -14
THICKNESS 154
TILTFILE testmidzone2a.tlt
XAXISTILT 1.71
XTILTFILE testmidzone2a.xtilt
ZFACTORFILE testmidzone2a.zfac
ActionIfGPUFails 1,2
$if (-e ./savework) ./savework
