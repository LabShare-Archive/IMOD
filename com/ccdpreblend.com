# THIS IS A COMMAND FILE TO PRODUCE A PRE-ALIGNED STACK
# 
$xftoxg
0	global fit
g5a.prexf
g5a.prexg
#
# To run BLENDMONT
# First two entries are:
#
#  Input image file name
#  Output file for blended stack
#
$ blendmont
g5a.st
g5a.preali
1	Data mode of output file.
0	1 float to max range, 0 not
# Next line, file of G transformations, if any, or blank line if none
g5a.prexg
# Next line, name of input piece list file
g5a.pl
# Next line, use negative of an option for sloppy montages
#   (6/-6 reads in existing edge correlation displacements)
-6	2/-2 shift using edges, 5/-5/6/-6 using best of correlation & edges
# Next line, name of output piece list file
g5a.blpl
/	X & Y center of tranforms; COMMENT THIS LINE OUT IF NO G TRANSFORMS
/	List of sections to include in output (ranges ok); / for all
/	Min & max X, Min & max Y coordinates to put in output image; / for all
10000,10000,2,2	Maximum new X&Y frame size, minimum X&Y overlaps. 
0	To accept selected frame sizes
1	0 build new/1 read old edge function files.
# Next line, root name of edge function files
g5a
50,50	Width over which to blend positions in X and Y directions
