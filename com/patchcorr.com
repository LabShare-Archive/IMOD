# THIS FILE RUNS PATCHCRAWL3D WITH OUTPUT TEE'D TO A FILE
#
#  Arguments are size of patches, # of positions, and lower and upper limits
#  of sampled volume in each dimension.  IF YOU HAVE TO ADJUST THE NUMBER OF
#  PATCHES AND LIMITS, REMEMBER THAT THEY APPLY TO THE FILE BEING MATCHED TO, 
#  AND THAT Y IS THICKNESS HERE
#
#  TO SPECIFY A MODEL FILE WITH CONTOURS ENCLOSING THE PATCHES TO ANALYZE,
#  ADD THE NAME OF THE FILE AT THE END OF THE LINE STARTING WITH solve.xf
#
#            Patch Size  # patches    Lower and Upper Limits
#             X   Y   Z   X  Y  Z   XL - XU   YL -YU   ZL - ZU   
$patchcrawl3d  56 20 56   3  2  5   36  988  10  72  36  988 \
 10 g5a.rec g5b.mat patch.out \
 solve.xf g5b.rec boundary_model
#
$if (-e savework-file) savework-file
