# THIS IS A VERSION OF SOLVEMATCH THAT IS FOLLOWED BY MATCHSHIFTS
#    first two lines are names of fiducial coordinate files
#
$ solvematch
g5afid.xyz
g5bfid.xyz
/	list of points in 1st series with corresponding points in 2nd series
/	list of corresponding points in 2nd series (/ on both lines if all 1:1)
0,0	X-axis tilt used in generating first and second tomograms
8	Limiting value for maximum residual: higher residuals will give error
2	0 to use models, 2 if 2 surfaces, 1/-1 if one surface, -1 if INVERTED
solvezero.xf
#
$echo "SOLVEMATCH RAN SUCCESSFULLY, NEXT RUNNING MATCHSHIFTS"
#
# Run matchshifts to get absolute shifts; the three numbers are the nx, ny,
# and nz of the block used for 3-D cross-correlation
#
$matchshifts g5a g5b 64 32 64
