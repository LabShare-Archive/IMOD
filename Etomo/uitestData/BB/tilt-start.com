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
InputProjections BBa.ali
OutputFile BBa_full.rec
IMAGEBINNED 1
TILTFILE BBa.tlt
THICKNESS 76
SLICE -1 -1
TOTALSLICES 0 549
RADIAL 0.35 0.05
XAXISTILT 1.76
LOG 0.0
SCALE 0.0 1000.0
PERPENDICULAR 
MODE 1
FULLIMAGE 512 512
SUBSETSTART -19 -19
AdjustOrigin 
XTILTFILE BBa.xtilt
LOCALFILE BBalocal.xf
OFFSET 0.0
SHIFT 0.0 0.0
ZFACTORFILE BBa.zfac
ActionIfGPUFails 1,2
EXCLUDELIST 1
$sync
