#
# THIS IS A VERSION OF SOLVEMATCH THAT USES MATCHING MODEL FILES
#
$ solvematch
g5afid.xyz
g5bfid.xyz
/	list of points in 1st series with corresponding points in 2nd series
/	list of corresponding points in 2nd series (/ on both lines if all 1:1)
0,0	X-axis tilt used in generating first and second tomograms
8	Limiting value for maximum residual: higher residuals will give error
0	0 TO USE MODELS, 2 if 2 surfaces, 1/-1 if one surface, -1 if INVERTED
g5a.rec
#     Next line is model of matching points in first tomogram
g5a.matmod
g5b.rec
#     Next line is model of matching points in second tomogram
g5b.matmod
solve.xf
