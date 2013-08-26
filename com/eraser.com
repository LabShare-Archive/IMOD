# THIS IS A COMMAND FILE TO RUN CCDERASER
#
####CreatedVersion#### 3.7.2
#
#   To run in trial mode where it will not replace pixels or produce an output
#   file, include a line with the entry "TrialMode"
#
#   To run with a model specifying points to replace, add a line like:
# ModelFile  g5a.erase
#    and specify objects to replace on all sections with
# AllSectionObjects  <list of objects>
#   and indicate objects that specify lines with
# LineObjects <list of objects>
#
$ccderaser -StandardInput
InputFile       g5a.st
#
# This output file will be created unless 
OutputFile      g5a_fixed.st
#
# This line enables automatic X-ray peak search and replacement
FindPeaks
#
# This is the criterion # of SDs above mean background for identifying a peak
PeakCriterion   10.
#
# This is the criterion # of SDs above the mean pixel-to-pixel difference
DiffCriterion   8.
#
# These are criteria controlling elimination of an extra-large peak
BigDiffCriterion 19.
GiantCriterion   12.
ExtraLargeRadius 8.

#
# This is the criterion # of SDs for including points in a peak
GrowCriterion   4.
#
# This is the number of pixels to exclude from replacement near image edges
EdgeExclusionWidth      4
#
# This output model file will have points at all pixels replaced
PointModel      g5a_peak.mod
MaximumRadius   3.6
AnnulusWidth    2.0
XYScanSize      100
ScanCriterion   3.
BorderSize      2
PolynomialOrder 2
