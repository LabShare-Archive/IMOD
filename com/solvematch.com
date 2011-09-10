# THIS COMMAND FILE RUNS SOLVEMATCH
#
####CreatedVersion#### 4.2.24
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
#
# Uncomment and insert name to use coordinate file from transferfid, makes
# the correspondence lists irrelevant
#
#TransferCoordinateFile	
UsePoints	/
XAxisTilts	0,0
AngleOffsetsToTilt	0,0
ZShiftsToTilt	0,0
SurfacesOrUseModels	2
AMatchingModel	g5a.matmod
BMatchingModel	g5b.matmod
AFiducialModel	g5a.fid
BFiducialModel	g5b.fid
MatchingAtoB	0
ATomogramOrSizeXYZ	g5a.rec
BTomogramOrSizeXYZ	g5b.rec
OutputFile	solve.xf
MaximumResidual	8.
LocalFitting	10
CenterShiftLimit	10.
#
$echo "STATUS: SOLVEMATCH RAN SUCCESSFULLY"
