$tiltalign
$! next line, name of fiducial model file
g5a.fid
$! next line, name of image file, or blank line followed by NX,NY/

1024,1024/	size and origin information; comment out if use image file
$! next line, filename for model output of X-Y-Z coordinates, blank for none

$! next line, filename for text output of X-Y-Z coordinates, blank for none
g5afid.xyz
$! next line, filename for output of tilt angles, blank for none
g5a.tlt
$! next line, filename for solution/transform output
g5a.xf
1	-1 solutions only, 1 xforms only, or 0 both in output file
0	0 all z values, 1 specify start, end, increment, or 2 specify all z
0.0	Initial rotation angle (90 horizontal)
0	0 to vary all rotations, or # of view to fix at initial angle
0	# of separate sets for automapping
#	!list of reshoots
1	-1 to enter all tilts, 1 to specify starting & increment, 0 from file
-60,1.5	start and increment if 1
0	add to all angles
2	0 fixed, 1/2/3 all vary but specified/minimum/both, 4 other, 5 automap
#5,0
1	# of reference view with mag fixed at 1.0, or / for default
1	0 fix all mags, 1 all independent, 2 specify mapping of mags, 3 automap
0	0 for no compression variables, or # of view to fix at no compression
0	0 no distortion, 1 map x-stretch and skew same, 2 map differently
#3	1 all independent, 2 specify mapping, 3 automap (distortion/X-stretch)
#7,0	default group size, # of separate ranges
#3	1 all independent, 2 specify mapping, 3 automap (skew if different)
#11,0	default group size, # of separate ranges
3.	Criterion # of S.d.s above mean residual to report (- for local mean)
2	0 no surface analysis, 1 or 2 for analysis of 1 or 2 surfaces
.25,1000 METRO factor, limit on # of cycles
0	IF getting xforms, amount to move tilt axis in Z, or 1000 to midpoint
0	IF getting xforms, amount to move tilt axis in X from center
0	0 to exit, or 1 to do local alignments
