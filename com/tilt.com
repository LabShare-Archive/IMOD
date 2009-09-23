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
InputProjections g5a.ali
OutputFile g5a.rec
IMAGEBINNED 1
TILTFILE g5a.tlt
XTILTFILE g5a.xtilt
THICKNESS 100
RADIAL .35 .05
XAXISTILT 0.
LOG 0
SCALE 0 500
PERPENDICULAR
MODE 1
EXCLUDELIST
FULLIMAGE
SUBSETSTART 0 0
AdjustOrigin 1
