# THIS FILE DOES EVERYTHING FOR COMBINING VOLUMES
#
####CreatedVersion#### 3.8.26
#
# CHANGE THIS VALUE TO SET ReductionFactor NONZERO IN COMBINEFFT
#
$set combinefft_reduce = 0
#
# CHANGE THIS VALUE TO SET LowFromBothRadius NONZERO IN COMBINEFFT
#
$set combinefft_lowboth = 0
#
$setenv IMOD_BRIEF_HEADER 1
$setenv PIP_PRINT_ENTRIES 0
#
# TO RESTART AT A PARTICULAR PIECE, CHANGE 0 IN THE FOLLOWING "goto dopiece0"
# TO THE DESIRED PIECE NUMBER
#
$goto dopiece0
#
$dopiece0:
#
$if (-e savework-file) savework-file
#
$echo "STATUS: RUNNING DENSMATCH TO MATCH DENSITIES"
$echo
#
# Scale the densities in the match file to match the first tomogram.  Inputs:
#     File being matched (first tomogram)
#     File to be scaled (the second tomogram)
#     BLANK LINE TO HAVE SCALED VALUES PUT BACK IN THE SAME FILE
#
$densmatch
g5a.rec
g5b.mat

#
#
# purge some previous versions if necessary: these are the huge files
#
