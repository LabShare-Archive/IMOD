# THIS IS A MASTER COMMAND FILE TO COMBINE TOMOGRAMS FROM A TWO-AXIS TILT
# SERIES
# 
# It runs a command file for each step in turn.  To change parameters for
# a particular procedure, edit the command file for that step.
#
# To restart at a particular place, just change the following goto
# to go to the destination needed (patchcorr, matchorwarp, warpvol, etc.)
#
$goto solvematch
#
$echo "This is the master log file for combine.com"
$echo "It will end with COMBINE SUCCESSFULLY COMPLETED upon success"
$echo "Look in individual log files for the results of particular operations"
$echo " "
#
# Run solvematch to find the initial correspondence between volumes
#
$solvematch:
$echo "Running solvematch.com"
$vmstocsh solvematch.log < solvematch.com | csh -ef
#
# Next run matchvol to make matching volume
#
$matchvol1:
$echo "Initial registration found, next running Matchvol in matchvol1.com"
$vmstocsh matchvol1.log < matchvol1.com | csh -ef
#
#
# Next run patchcorr3d to find patches
# If you change your patch sizes or limits, restart here
#
$patchcorr:
$echo "Matchvol finished, next running Patchcrawl3d in patchcorr.com"
$vmstocsh patchcorr.log < patchcorr.com | csh -ef
#
# Matchorwarp runs Refinematch and Matchvol, or Findwarp and Warpvol.
# If you edit your patches, restart here
#
$matchorwarp:
$echo "Patchcrawl3d found displacements, next running matchorwarp.com"
$vmstocsh matchorwarp.log < matchorwarp.com | csh -ef
$goto volcombine
#
# If you run Findwarp by hand and get a warp.xf, restart here
#
$warpvol:
$vmstocsh warpvol.log < warpvol.com | csh -ef
$goto volcombine
#
# This Matchvol is run only if you have to do Findwarp by hand, get a good
# refine.xf by omitting rows or columns, and have to restart the process
#
$matchvol2:
$vmstocsh matchvol2.log < matchvol2.com | csh -ef
$goto volcombine
#
# This runs everything else to combine volumes
#
$volcombine:
$echo "Matchvol or Warpvol finished, next running volcombine.com"
$vmstocsh volcombine.log < volcombine.com | csh -ef
$echo "COMBINE SUCCESSFULLY COMPLETED"
