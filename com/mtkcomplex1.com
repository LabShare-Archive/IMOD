# THIS IS A SAMPLE COMMAND FILE FOR RUNNING MTK IN A COMPLEX MODEL
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
1	# of graphs
11	reference object
7	neighbor object
31      save graph to file
1       graph number
# File name, or blank line to save in last file
points.grf
16      redo with new parameters or objects
/       bin size, # of bins
0       0 to keep line/point parameters
1       1 to change graph objects
1       # of graphs
11      reference objects
12,14,16	neighbor objects
31      save graph to file
1       graph to save
# File name, or blank line to save in last file

25      to exit program
