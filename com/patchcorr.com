# THIS FILE RUNS PATCHCRAWL3D
#
####CreatedVersion#### 3.4.4
#
#  Arguments are size of patches, # of positions, and lower and upper limits
#  of sampled volume in each dimension.  IF YOU HAVE TO ADJUST THE NUMBER OF
#  PATCHES AND LIMITS, REMEMBER THAT THEY APPLY TO THE FILE BEING MATCHED TO, 
#  AND THAT Y IS THICKNESS HERE
#
#  TO SPECIFY A MODEL FILE WITH CONTOURS ENCLOSING THE PATCHES TO ANALYZE,
#  ADD THE NAME OF THE FILE AT THE END OF THE LINE STARTING WITH solve.xf
#
#  The three entries starting with solve.xf enable analysis of which patches
#  come from bad places in the second volume, not the file being matched to.
#  Eliminate these entries to disable this analysis; change the four numbers
#  (lower and upper X, lower and upper Z borders) to set different border sizes
#  in the second volume.
#
#            Patch Size  # patches    Lower and Upper Limits
#             X   Y   Z   X  Y  Z   XL - XU   YL -YU   ZL - ZU   
$patchcrawl3d  56 20 56   3  2  5   36  988  10  72  36  988 \
 10 g5a.rec g5b.mat patch.out \
 solve.xf g5b.rec 36,36,36,36 boundary_model
#
# Make a patch vector model
$patch2imod patch.out patch_vector.mod
$if (-e savework-file) savework-file
