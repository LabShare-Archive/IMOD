# THIS COMMAND FILE RUNS SOLVEMATCH
#
# It contains all of the entries needed for the three different modes of
# operation: fiducials only, fiducials and matching models (obsolete), and
# matching models only.
#
$solvematch -StandardInput
AFiducialFile	g5afid.xyz
BFiducialFile	g5bfid.xyz
#
# list of points in 1st series with corresponding points in 2nd series
#
ACorrespondenceList	/
#
# list of corresponding points in 2nd series (/ on both lines if all 1:1)
#
BCorrespondenceList	/
XAxisTilts	0,0
SurfacesOrUseModels	2
AMatchingModel	g5a.matmod
BMatchingModel	g5b.matmod
ATomogramOrSizeXYZ	g5a.rec
BTomogramOrSizeXYZ	g5b.rec
OutputFile	solvezero.xf
MaximumResidual	8.
#
$echo "STATUS: SOLVEMATCH RAN SUCCESSFULLY, NEXT RUNNING MATCHSHIFTS"
#
# Run matchshifts.  If solvezero.xf has shifts, it will just be copied to 
# solve.xf.  Otherwise, if the coordinates in the fiducial lists were not
# absolute, it will get the absolute shifts; the three numbers are the nx, ny,
# and nz of the block used for 3-D cross-correlation.
#
$matchshifts g5a g5b 64 32 64
