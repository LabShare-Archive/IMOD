# AN EXAMPLE COMMAND FILE TO RUN MTK, READ IN A STORED GRAPH OR TWO, AND PLOT
$mtk


1
0

32	read in graph
1	graph number to read into: next line filename or blank for same as last
tst.grf
3	# of graph in file
32
2

10
33	combine bins
3,1,100	# of bins to combine, starting and ending bins to combine
	! list of graphs to apply this to
5	display graphs in windows	
1-2	!list of graphs
7	scale to same Y value
8	do plot
2,0	window to plot; plot # or 0 to specify
2.5,2.5,1.6,3.5		X & Y size, lower left X and Y (inches)
-10,-10,-.06,5,7,1	# of ticks in X & Y and tick size (- outward), axis&graph line thickness, 1 for box
6,0	# of ticks to label, and # of text labels for X axis
1,2	# of first tick, spacing between ticks to label: follow with labels
0,40,80,120,160,200
.14,.12	character size, spacing from axis
6,0	# of ticks to label, and # of text labels for Y axis
1,2	# of first tick, spacing between ticks to label: follow with labels
0,5,10,15,20,25
.14,.11		label size & separation from axis
0,0,0,0	# of text strings, letters in circles, symbols in boxes, lines
8
1,0
0	new page
2.5,2.5,1.6,3.5
-10,-10,-.06,1,3,0
0,0
0,0
0,0,0,0
25
# move the graph file to a specific name
$\mv gmeta.dat MT-sac.ps
# Redo as many times as you wish
