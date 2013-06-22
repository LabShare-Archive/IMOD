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
InputProjections midzone2b_3dfind.ali
OutputFile midzone2b_3dfind.rec
IMAGEBINNED 2
FULLIMAGE 1024 1950
LOCALFILE midzone2blocal.xf
LOG 0.0
MODE 1
OFFSET 0.0
PERPENDICULAR 
AdjustOrigin 
RADIAL 0.35 0.05
SCALE 0.0 1000.0
SHIFT 0.0 32.8
SUBSETSTART -14 -25
THICKNESS 148
TILTFILE midzone2b.tlt
XAXISTILT 0.21
XTILTFILE midzone2b.xtilt
ZFACTORFILE midzone2b.zfac
ActionIfGPUFails 1,2
EXCLUDELIST 1
$if (-e ./savework) ./savework
