# Command file to run Tilt
#
####CreatedVersion#### 3.4.4
# 
# RADIAL specifies the frequency at which the Gaussian low pass filter begins
#   followed by the standard deviation of the Gaussian roll-off
#
# LOG takes the logarithm of tilt data after adding the given value
#
$tilt
g5a.ali
g5a.rec
TILTFILE g5a.tlt
THICKNESS 100
RADIAL .35 .05
XAXISTILT 0.
LOG 0
SCALE 1.39 500
PERPENDICULAR
MODE 1
EXCLUDELIST
FULLIMAGE
SUBSETSTART 0 0
DONE
(You can park unused lines after the DONE)
