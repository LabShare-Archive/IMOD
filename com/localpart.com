5,5	NUMBER OF LOCAL PATCHES TO SOLVE FOR IN X AND Y
0.5,0.5	minimum size of patches (> 1) or minimum fractional overlap (< 1)
8,3	MINIMUM FIDUCIALS TOTAL AND ON ONE SURFACE IF TWO SURFACES
1	1 to fix XYZ coords at global values, 0 to solve for them also
1,0,1	1 for output of variables, XYZ coordinates, and residuals each time
#
3	0 fix all rots, 1 all independent, 2 specify mapping of rots, 3 automap
6,0	default group size for rotations, # of separate ranges 
#
5	0 fixed tilts, 1/2/3 all vary but one/minimum/both, 4 other, 5 automap
6,0	default group size for tilts, # of separate ranges 
#
1	# of reference view with mag fixed at 1.0, or / for default
3	0 fix all mags, 1 all independent, 2 specify mapping of mags, 3 automap
7,0	default group size for magnifications, # of separate ranges 
#
2	0 no distortion, 1 map x-stretch and skew same, 2 map differently
3	1 all independent, 2 specify mapping, 3 automap (distortion/X-stretch)
7,0	default group size, # of separate ranges
3	1 all independent, 2 specify mapping, 3 automap (skew if different)
11,0	default group size, # of separate ranges
