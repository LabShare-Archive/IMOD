# Command file for running BEADTRACK
#
####CreatedVersion#### 3.4.4
#
# First 4 lines:
#   Image file
#   Piece list name if montage (or scrambled Z), blank line if not
#   Name of input model with seed objects
#   Name of output model
$beadtrack
g5a.st
g5a.pl
g5a.seed
g5a.fid
	!list of views to skip (numbered from 1)
0	initial angle of rotation as in TILTALIGN
0	# sets of reshoots: list reshoots on following line(s) as in TILTALIGN
1	-1 to enter all tilts, 1 to specify starting & increment, 0 read file
-60,1.5	start and increment if 1
7,0	grouping for tilt variables
5,0	grouping for mag variables
4	minimum views for doing tilt alignment (make bigger if 1 deg tilts?)
5,0	radius of beads for center of gravity, 0 for dark beads 
1	to fill in existing gaps in seed model
5	maximum gap to create
#
# CONTROL PARAMETER FOR EXPERTS, EXPERIMENTATION, OR SPECIAL CASES
#
10,20	minimum range of tilt angles for finding axis and for finding tilts
32,32	box size
4	maximum beads to average
7,3	# points and minimum for extrapolation
.6,1	fraction of mean, and # of SD below mean: density criterion for rescue
10	distance criterion for rescue
.7,.9	relaxation of criterion for density and distance rescues
2.5	distance for rescue after fit
.9,2.5	relaxation of density criterion, maximum radius to search
9,5	Max and min residual changes to use to get mean and SD change
.04,2	minimum residual difference, criterion # of sd's
