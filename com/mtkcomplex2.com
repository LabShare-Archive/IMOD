# THIS IS AN EXAMPLE OF RUNNING MTK WITH SHIFTING ANALYSIS IN A COMPLEX MODEL
# Leave the first two lines blank
$mtk


1       1 for no graphics window
0       0 for regular density analysis
# Name of model file, or blank line to skip to option entry point
brad.mod
# Name of file with tilt information, or blank line if none

# List of Z values in gaps across which surfaces should connect, blank if none
99-109,210-220
0,0	start,end Z (0,0 except for old whole line analysis)
.005,50        bin size in microns, number of bins
# Next 4 entries are line/point parameters
0.01	sampling length for lines in microns, or 0 to do whole lines
2,0	When doing whole lines: distance power, # points to fit over
# For line segments: 0 to measure from start of segment or 1 to measure from
# closest point along segment
0	
1	0 to measure from center, or 1 from surface of scattered points
4	# of graphs
4	reference object
12,14,15,16	neighbor objects
4       reference
13,17,18        neighbor
11      reference
12,14,15,16     neighbor
11      reference
13,17,18        neighbor
41	EXCLUDE FAILURES TO SHIFT
20      shift items
0,5	min & max distance to shift in X/Y
.5	shift in Z relative to X/Y
4,11	objects to shift
1-3,5-10,12-18,20	objects to check against
2	Number of curves for rejection of close approaches
5,.002	First curve: # of bins, bin size for rejection (microns)
0,0,0,.3,.6	acceptance probabilities in each bin
1,.015	Second curve: # of bins, bins size for rejection
0	acceptance probabilities (reject all approaches within 15 nm)
# For each object to check against, the number of the probability curve
1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1
.01	distance outside original bounding box
21	boundary object
0	1 to check potential shift against items yet to be shifted, 0 not to
400	total trials
400,.3	trials/cycle, factor to change max distance by per cycle
31      save graph to file
1       graph number to save
# File name, or blank line to save in last file
movedpoints.grf
31
2

31
3

31
4

16      redo with new parameters or objects
/       bin size and #
0       0 to keep line/point parameters
1       1 to set new graphs
2       # of graphs
10      reference object
4	neighbor object
10      reference
11      neighbor
1	to work with randomized data not original data
31      save graph to file
1

31
2

40	unshift objects
4       object to unshift
40      unshift
11      object to unshift
16      redo with new graphs
/
0
1       reset the original set of graphs
4	# of graphs
4	reference
12,14,15,16	neighbor
4
13,17,18
11
12,14,15,16
11
13,17,18
1	1 to use random data (even though they are shifted back)
31      save to file
1
movedpoints.grf
31
2

31
3

31
4

16      redo second set of graphs
/
0
1
2
10      reference
4	neighbor
10
11
1	to work with randomized data
31      save to file
1

31
2

25
