# THIS IS A MASTER COMMAND FILE TO COMBINE TOMOGRAMS FROM A TWO-AXIS TILT
# SERIES
# 
# It runs a command file for each step in turn.  To change parameters for
# a particular procedure, edit the command file for the particular step.
#
# To skip some steps, comment them out (place a # in front of them)
# To include steps that are skipped, uncomment them (remove the #)
# To restart at a particular place, comment out everything above there and
# uncomment that step if necessary.  Or, just uncomment the following goto
# and change the destination as needed (patchcorr, matchorwarp, warpvol, etc.)
#
#$goto patchcorr
#
$echo "This is the master log file for combine.com"
$echo "It will end with COMBINE SUCCESSFULLY COMPLETED upon success"
$echo "Look in individual log files for the results of particular operations"
$echo " "
#
# Solvematchshift.com uses Matchshifts and solvematchmod.com uses
# matching model files to find the initial registration between tomograms.
# To switch between using Matchshifts and using matching models, comment one
# out and uncomment the other.  If you have to change any parameters, be 
# sure to do it in the version that is being run.
#
$echo "Running solvematchshift.com"
$vmstocsh solvematchshift.log < solvematchshift.com | csh -ef
#
#$echo "Running solvematchmod.com"
#$vmstocsh solvematchmod.log < solvematchmod.com | csh -ef
#
# Next run matchvol to make matching volume
#
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
#
# If you run Findwarp by hand and get a warp.xf, restart here
#
$warpvol:
#$vmstocsh warpvol.log < warpvol.com | csh -ef
#
# This Matchvol is run only if you have to do Findwarp by hand, get a good
# refine.xf by omitting rows or columns, and have to restart the process
#
$matchvol2:
#$vmstocsh matchvol2.log < matchvol2.com | csh -ef
#
# This runs everything else to combine volumes
#
$volcombine:
$echo "Matchvol or Warpvol finished, next running volcombine.com"
$vmstocsh volcombine.log < volcombine.com | csh -ef
$echo "COMBINE SUCCESSFULLY COMPLETED"
