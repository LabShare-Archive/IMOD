# THIS FILE DOES EVERYTHING FOR COMBINING VOLUMES
#
$if (-e savework-file) savework-file
#
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
$echo "STATUS: DENSMATCH FINISHED, PROCEEDING TO COMBINE TOMOGRAMS"
#
# purge some previous versions if necessary: these are the huge files
#
