#
# TO RUN TILTXCORR
#
#  The first three lines are:
#     Input image file
#     Piece list for reordering Z values, or blank if none
#     Output file for f transforms
#
# Be sure to set the tilt angles, the tilt axis rotation, and whether to
# exclude a central peak due to fixed pattern noise.
#
$tiltxcorr 
g5a.st

g5a.prexf
1	tilts: 1 for start & increment, 0 for tilt file, -1 to enter all angles
-60,1.5	start and increment if 1
0	rotation angle from vertical to tilt axis
.03,.05,0,.25	filter parameters Sigma1, Sigma2, Radius1, Radius2
0	1 to exclude central peak due to fixed pattern noise, 0 not to
#
# YOU'LL ONLY HAVE TO FIDDLE WITH THE PARAMETERS BELOW IF SOMETHING
# DOESN'T WORK WELL WITH THE DEFAULTS
#
/	Amounts to trim each edge in X and Y (/ for none)
/	Amounts to pad each edge in X and Y (/ for 5% of size up to 20,20)
/	Distance over which to taper image in X and Y (/ for 10% up to 100,100)
/	views to do (numbered from 1), or / for all
