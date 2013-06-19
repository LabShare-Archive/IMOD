# THIS IS A COMMAND FILE TO RUN TILTALIGN
#
####CreatedVersion#### 3.10.4
#
# To exclude views, add a line "ExcludeList view_list" with the list of views
#
# To specify sets of views to be grouped separately in automapping, add a line
# "SeparateGroup view_list" with the list of views, one line per group
#
$tiltalign -StandardInput
ModelFile	g5a.fid
ImageFile	g5a.st
#ImageSizeXandY	1024,1024
ImagesAreBinned	1
OutputModelFile	g5a.3dmod
OutputResidualFile	g5a.resid
OutputFidXYZFile	g5afid.xyz
OutputTiltFile	g5a.tlt
OutputXAxisTiltFile	g5a.xtilt
OutputTransformFile	g5a.xf
#ExcludeList
RotationAngle	0.0
FirstTiltAngle	-60.
TiltIncrement	1.5
#
# ADD a recommended tilt angle change to the existing AngleOffset value
#
AngleOffset	0.
RotOption	1
RotDefaultGrouping	5
#
# TiltOption 0 fixes tilts, 2 solves for all tilt angles; change to 5 to solve
# for fewer tilts by grouping views by the amount in TiltDefaultGrouping
#
TiltOption	5
TiltDefaultGrouping	5
MagReferenceView	1
MagOption	1
MagDefaultGrouping	4
#
# To solve for distortion, change both XStretchOption and SkewOption to 3;
# to solve for skew only leave XStretchOption at 0
#
XStretchOption	0
SkewOption	0
XStretchDefaultGrouping	7
SkewDefaultGrouping	11
BeamTiltOption	0
# 
# Criterion # of S.D's above mean residual to report (- for local mean)
#
ResidualReportCriterion	3.0
SurfacesToAnalyze	2
MetroFactor	.25
MaximumCycles	1000
KFactorScaling	1.
#
# ADD a recommended amount to shift up to the existing AxisZShift value
#
AxisZShift	0.
ShiftZFromOriginal      1
#
# Set to 1 to do local alignments
#
LocalAlignments	0
OutputLocalFile	g5alocal.xf
#
# Target size of local patches to solve for in X and Y
#
TargetPatchSizeXandY	700,700
MinSizeOrOverlapXandY	0.5,0.5
#
# Minimum fiducials total and on one surface if two surfaces
#
MinFidsTotalAndEachSurface	8,3
FixXYZCoordinates	1
LocalOutputOptions	1,0,1
LocalRotOption	3
LocalRotDefaultGrouping	6
LocalTiltOption	5
LocalTiltDefaultGrouping	6
LocalMagReferenceView	1
LocalMagOption	3
LocalMagDefaultGrouping	7
LocalXStretchOption	0
LocalXStretchDefaultGrouping	7
LocalSkewOption	0
LocalSkewDefaultGrouping	11
