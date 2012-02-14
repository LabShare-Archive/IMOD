$sync
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
OutputFile testBBa_full.rec
IMAGEBINNED 1
TILTFILE testBBa.tlt
THICKNESS 76
SLICE 400 449
#
TOTALSLICES 0 549
BoundaryInfoFile tilt-bound.info
FBPINTERP 0
RADIAL 0.35 0.05
XAXISTILT 1.76
LOG 0.0
SCALE 0.0 1000.0
PERPENDICULAR 
MODE 1
FULLIMAGE 512 512
SUBSETSTART -19 -19
AdjustOrigin 
XTILTFILE testBBa.xtilt
LOCALFILE testBBalocal.xf
OFFSET 0.0
SHIFT 0.0 0.0
ZFACTORFILE testBBa.zfac
ActionIfGPUFails 1,2
$echo CHUNK DONE
