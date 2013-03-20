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
ModelFile	BBa.fid
ImageFile	BBa.preali
#ImageSizeXandY	512,512
ImagesAreBinned	1
OutputModelFile	BBa.3dmod
OutputResidualFile	BBa.resid
OutputFidXYZFile	BBafid.xyz
OutputTiltFile	BBa.tlt
OutputXAxisTiltFile	BBa.xtilt
OutputTransformFile	BBa.tltxf
RotationAngle	-12.5
TiltFile	BBa.rawtlt
#
# ADD a recommended tilt angle change to the existing AngleOffset value
#
AngleOffset	-0.03
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
XStretchOption	3
SkewOption	3
        
XStretchDefaultGrouping	7
SkewDefaultGrouping	11
# 
# Criterion # of S.D's above mean residual to report (- for local mean)
#
ResidualReportCriterion	3.0
SurfacesToAnalyze	2
MetroFactor	0.25
MaximumCycles	1000
KFactorScaling	1.0
#
# ADD a recommended amount to shift up to the existing AxisZShift value
#
AxisZShift	6.6
ShiftZFromOriginal      
#
# Set to 1 to do local alignments
#
LocalAlignments	1
OutputLocalFile	BBalocal.xf
#
# Target size of local patches to solve for in X and Y
#
TargetPatchSizeXandY	700, 700
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
LocalXStretchOption	3
LocalXStretchDefaultGrouping	7
LocalSkewOption	3
LocalSkewDefaultGrouping	11
ExcludeList	1 
OutputZFactorFile	BBa.zfac
BeamTiltOption	0
#
# COMBINE TILT TRANSFORMS WITH PREALIGNMENT TRANSFORMS
#
$xfproduct -StandardInput
InputFile1	BBa.prexg
InputFile2	BBa.tltxf
OutputFile	BBa_fid.xf
$b3dcopy -p BBa_fid.xf BBa.xf
$b3dcopy -p BBa.tlt BBa_fid.tlt
#
# CONVERT RESIDUAL FILE TO MODEL
#
$if (-e BBa.resid) patch2imod -s 10 BBa.resid BBa.resmod
$if (-e ./savework) ./savework
