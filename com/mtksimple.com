# THIS IS AN EXAMPLE OF A COMMAND FILE TO RUN MTK IN A FAIRLY SIMPLE MODEL
# AND DO A SERIES OF RANDOM SHIFTS TO CHECK FOR SIGNIFICANCE
#
# First two entries are filenames for commands and output - leave blank
$mtk


1	1 to turn off plax window
0	0 for closest approach analysis
# model file name next, then optional file for serial section tilt info
tst.mod

	! list of sections across which surfaces should be connected
0,0	start,end Z (0,0 for all, or 0,-1 to enter a new model)
.002,250	bin width, # of bins
0.01	sampling length for lines, or 0 to measure distance from whole line
1,0	power for density estimate, # points to fit over (for whole lines)
1	0 from start of segment or 1 from closest point along sample segment
1	0 for center, 1 for surface of scattered points
1	# of graphs (enter reference then neighbor types for each)
1	!reference
2	!neighbor
#
# AT THIS POINT THE GRAPHS ARE DONE AND COULD BE SAVED AS BELOW
#
31	save graph
1	graph #; next line is filename, or blank to use last one after 1st time
tst.grf
23	Do series of random shifts (use 20 to do one shift, same entries)
0,5	min & max to shift in X/Y
.5	relative shift in Z
2	!list of objects to shift
1	!list of objects to check against
1	Number of curves for rejection (then following two lines for each)
5,.002	# of bins, bins size for rejection
0,0,0,.3,.6	acceptance probabilities
# If more than one curve, enter list with curve number per object being 
# checked against
# 1,2
.01	distance outside original bounding box
3	boundary object
0	1 to check shifted against ones yet to be shifted
400	total trials
400,.3	trials/cycle, factor to change maximum shift by
#
# The following additional entries are needed by 23 and not 20
1	0 to specify separate integral bins for each graph, 1 same bins for all
2,5	starting and ending bins to integrate
10,15	starting and ending bins to use for baseline to subtract from integral
1	1 to accumulate mean and sd graphs, 0 not to
50	# of shifted sets to run, or 0 to go on
0	0 to go on
31	save mean graph to same file
2

25	exit program
