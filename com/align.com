# THIS IS A COMMAND FILE TO RUN TILTALIGN
#
# THE MOST COMMON ENTRIES TO CHANGE ARE DESCRIBED IN CAPITAL LETTERS
#
####CreatedVersion#### 3.4.4
#
$tiltalign
# next line, name of fiducial model file
g5a.fid
# next line, name of image file, or blank line followed by NX,NY/
g5a.st
#1024,1024/	size and origin information; comment out if use image file
# next line, filename for model output of X-Y-Z coordinates, or
# a name containing ".res" for text output of residuals, 
# a name without a "." for both outputs, or blank for neither
g5a
# next line, filename for text output of X-Y-Z coordinates, blank for none
g5afid.xyz
# next line, filename for output of tilt angles, blank for none
g5a.tlt
# next line, filename for solution/transform output
g5a.xf
1	-1 solutions only, 1 xforms only, or 0 both in output file
3	0 do all views, 1 start, end, inc, 2 list to include, 3 list to exclude
#
# SPECIFY LIST OF VIEWS TO EXCLUDE ON THE FOLLOWING LINE OR LEAVE EMPTY
#
	!list of views to exclude
0.0	Initial rotation angle (90 horizontal)
0	0 to vary all rotations, or # of view to fix at initial angle
#
# SPECIFY VIEWS TO BE GROUPED SEPARATELY ON NEXT TWO LINES
#
0	# of separate sets for automapping
#	!list of reshoots
1	-1 to enter all tilts, 1 to specify starting & increment, 0 from file
-60,1.5	start and increment if 1
#
# *ADD* A RECOMMENDED TILT ANGLE CHANGE TO THE NUMBER ON THE NEXT LINE
#
0	ANGLE OFFSET; AMOUNT TO ADD TO ALL ANGLES (DEGREES)
#
# ON THE NEXT LINE, 2 SOLVES FOR A TILT ANGLE FOR EACH VIEW
# TO SOLVE FOR FEWER TILTS BY GROUPING VIEWS, CHANGE 2 TO 5 AND
# UNCOMMENT THE LINE AFTER THAT
#
2	0 fixed, 1/2/3 all vary but specified/minimum/both, 4 other, 5 automap
#5,0	GROUP SIZE
1	# of reference view with mag fixed at 1.0, or / for default
1	0 fix all mags, 1 all independent, 2 specify mapping of mags, 3 automap
0	0 for no compression variables, or # of view to fix at no compression
#
# TO SOLVE FOR DISTORTION, CHANGE 0 TO 2 ON NEXT LINE,
# UNCOMMENT THE 4 LINES AFTER THAT
#
0	0 no distortion, 1 map x-stretch and skew same, 2 map differently
#3	1 all independent, 2 specify mapping, 3 automap (distortion/X-stretch)
#7,0	default group size, # of separate ranges
#3	1 all independent, 2 specify mapping, 3 automap (skew if different)
#11,0	default group size, # of separate ranges
#
3.	CRITERION # OF S.D.S ABOVE MEAN RESIDUAL TO REPORT (- FOR LOCAL MEAN)
2	0 no surface analysis, 1 or 2 for analysis of 1 or 2 surfaces
.25,1000 metro factor, limit on # of cycles
#
# *ADD* A RECOMMENDED AMOUNT TO SHIFT UP TO THE NUMBER ON THE NEXT LINE
#
0	AMOUNT TO MOVE TILT AXIS IN Z, OR 1000 to move midpoint 
0	Amount to move tilt axis in X from center
0	0 TO EXIT, OR 1 TO DO LOCAL ALIGNMENTS
