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
InputProjections BBb.ali
OutputFile BBb_3dfind.rec
IMAGEBINNED 1
FULLIMAGE 512 512
LOCALFILE BBblocal.xf
LOG 0.0
MODE 1
OFFSET 0.0
PERPENDICULAR 
AdjustOrigin 
RADIAL 0.35 0.05
SCALE 0.0 1000.0
SHIFT 0.0 -12.3
SUBSETSTART -19 -19
THICKNESS 98
TILTFILE BBb.tlt
XAXISTILT -0.08
XTILTFILE BBb.xtilt
ZFACTORFILE BBb.zfac
ActionIfGPUFails 1,2
EXCLUDELIST 1
$if (-e ./savework) ./savework
