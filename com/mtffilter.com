# THIS IS A COM FILE FOR RUNNING MTFFILTER
#
####CreatedVersion#### 3.4.4
#
# THE LOW PASS RADIUS AND SIGMA WILL APPLY A TWO-DIMENSIONAL FILTER TO HIGH
# FREQUENCIES AND CAN BE USED TO REPLACE THE ONE-DIMENSIONAL RADIAL
# FILTER IN TILT
#
# TO TEST ON A SUBSET OF VIEWS, INSERT A LINE WITH
#   StartingAndEndingZ      View1,View2
#
# TO APPLY AN INVERSE MTF FILTER, INSERT A LINE WITH
#   MtfFile       filename.mtf
#
# TO USE THE OUTPUT FOR GENERATING A TOMOGRAM, 
#  RENAME g5a_filt.ali TO g5a.ali
#
$mtffilter -StandardInput
InputFile       g5a.ali
OutputFile      g5a_filt.ali
LowPassRadiusSigma        0.35,0.05
InverseRolloffRadiusSigma       .12,.05
MaximumInverse  4.
#
# INSERT NEW LINES ABOVE HERE
