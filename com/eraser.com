# THIS IS A COMMAND FILE TO RUN CCDERASER
#
# First three lines are:
# Input image file
# Output file, or blank line to rewrite into input file
# Model file specifying points and lines to erase
#
$ccderaser
g5a.st

g5a.erase
	List of objects to replace on all sections (/ all, blank for none)
	List of objects with horizontal or vertical lines (/ all, blank none)
/	border to use to fit to (/ for 3)
/	order of fit (/ for 2)
1	include adjacent points in fit
