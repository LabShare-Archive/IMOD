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
OutputFile BBa_3dfind.rec
IMAGEBINNED 1
FULLIMAGE 512 512
LOG 0.0
MODE 1
PERPENDICULAR 
RADIAL 0.35 0.05
SCALE 0.0 1000.0
SUBSETSTART -19 -19
THICKNESS 98
TILTFILE BBa.tlt
XAXISTILT 1.76
AdjustOrigin 
XTILTFILE BBa.xtilt
LOCALFILE BBalocal.xf
OFFSET 0.0
SHIFT 0.0 -12.5
ZFACTORFILE BBa.zfac
ActionIfGPUFails 1,2
EXCLUDELIST 1
$if (-e ./savework) ./savework
