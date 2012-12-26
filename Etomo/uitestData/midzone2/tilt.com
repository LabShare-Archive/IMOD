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
InputProjections midzone2a.ali
OutputFile midzone2a_full.rec
IMAGEBINNED 1
FULLIMAGE 1950 1024
LOCALFILE midzone2alocal.xf
LOG 0.0
MODE 1
OFFSET 0.0
PERPENDICULAR 
AdjustOrigin 
RADIAL 0.35 0.05
SCALE 0.0 500.0
SHIFT 0.0 0.0
SUBSETSTART -25 -13
THICKNESS 96
TILTFILE midzone2a.tlt
XAXISTILT 2.03
XTILTFILE midzone2a.xtilt
ZFACTORFILE midzone2a.zfac
ActionIfGPUFails 1,2
EXCLUDELIST 1
$if (-e ./savework) ./savework
