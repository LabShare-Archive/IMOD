# THIS FILE RUNS CORRSEARCH3D
#
####CreatedVersion#### 3.8.25
#
# TO SPECIFY A MODEL FILE WITH CONTOURS ENCLOSING THE PATCHES TO ANALYZE,
# ADD A LINE WITH "RegionModel   filename"
#
# TO APPLY A SMOOTHING FILTER ADD A LINE WITH "KernelSmooth   sigma"
#
# REMEMBER THAT Y IS THICKNESS HERE
#
$corrsearch3d -StandardInput
ReferenceFile   g5a.rec
FileToAlign     g5b.mat
OutputFile      patch.out
RegionModel     patch_region.mod
PatchSizeXYZ    56,20,56
NumberOfPatchesXYZ      3,2,5
XMinAndMax      36,988
YMinAndMax      10,72
ZMinAndMax      36,988
BSourceOrSizeXYZ        g5b.rec
BSourceTransform        solve.xf
BSourceBorderXLoHi      36,36
BSourceBorderYZLoHi     36,36
FlipYZMessages
#
# Make a patch vector model
$patch2imod -n "Values are correlation coefficients" patch.out \
patch_vector_ccc.mod
$if (-e savework-file) savework-file
