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
InputProjections testBBa.ali
OutputFile testBBa_3dfind.rec
IMAGEBINNED 1
FULLIMAGE 512 512
LOCALFILE testBBalocal.xf
LOG 0.0
MODE 1
OFFSET 0.0
PERPENDICULAR 
AdjustOrigin 
RADIAL 0.35 0.05
SCALE 0.0 1000.0
SHIFT 0.0 -12.9
SUBSETSTART -19 -19
THICKNESS 98
TILTFILE testBBa.tlt
XAXISTILT 1.76
XTILTFILE testBBa.xtilt
ZFACTORFILE testBBa.zfac
ActionIfGPUFails 1,2
$if (-e ./savework) ./savework
