$! THIS IS A COMMAND FILE TO COMBINE TOMOGRAMS FROM A TWO-AXIS TILT SERIES
$! Existing blank lines below should not be deleted, nor should blank lines 
$! be added.
$! 
$!if (! -e g5tmpdir) mkdir g5tmpdir
$!
$! IF YOU HAVE TO RUN FINDWARP BY HAND, OUTPUT YOUR WARPING TRANSFORMS TO
$! warp.xf, UNCOMMENT THE FOLLOWING GOTO (REMOVE THE !), AND RERUN THIS FILE
$!
$!goto dowarp
$!
$! BUT IF YOU RUN FINDWARP AND SUCCEED IN GETTING A SINGLE REFINE.XF BY
$! OMITTING ROWS OR COLUMNS, UNCOMMENT THE FOLLOWING GOTO INSTEAD
#!
$!goto domatch
$!
$! IF YOU WANT TO RUN MATCHORWARP AGAIN (BECAUSE YOU EDITED YOUR PATCH 
$! DISPLACEMENTS, ADDED A PATCH BOUNDARY MODEL, OR ADJUSTED WARPLIMITS),
$! UNCOMMENT THE FOLLOWING GOTO
$!
$!goto domatchorwarp
$!
$! IF YOU NEED TO RUN PATCHCRAWL3D AGAIN, UNCOMMENT THE FOLLOWING GOTO 
$!
$!goto dopatch
$!
$! TO SWITCH BETWEEN USING MATCHSHIFTS AND USING MATCHING MODEL FILES TO FIND
$! THE INITIAL REGISTRATION BETWEEN TOMOGRAMS, ADJUST THE FOLLOWING GOTO.
$! UNCOMMENT IT (REMOVE THE !) TO USE MODEL FILES; COMMENT IT OUT (ADD AN !)
$! TO USE MATCHSHIFTS.  IF YOU HAVE TO CHANGE ANY PARAMETERS, BE SURE TO DO IT
$! IN THE VERSION THAT IS BEING RUN.
$!
$!goto usemodels
$!
$! THIS IS A VERSION OF SOLVEMATCH THAT IS FOLLOWED BY MATCHSHIFTS
$!    first two lines are names of fiducial coordinate files
$!
$ solvematch
g5afid.xyz
g5bfid.xyz
/	list of points in 1st series with corresponding points in 2nd series
/	list of corresponding points in 2nd series (/ on both lines if all 1:1)
0,0	X-axis tilt used in generating first and second tomograms
8	Limiting value for maximum residual: higher residuals will give error
2	0 to use models, 2 if 2 surfaces, 1/-1 if one surface, -1 if INVERTED
solvezero.xf
$!
$echo "STATUS: SOLVEMATCH RAN SUCCESSFULLY, NEXT RUNNING MATCHSHIFTS"
$!
$! Run matchshifts to get absolute shifts; the three numbers are the nx, ny,
$! and nz of the block used for 3-D cross-correlation
$!
$matchshifts g5a g5b 64 32 64
$!
$echo "STATUS: MATCHSHIFTS FINISHED, NEXT RUNNING INITIAL MATCHVOL"
$!
$goto skipmodels
$!
$usemodels:
$!
$! THIS IS A VERSION OF SOLVEMATCH THAT USES MATCHING MODEL FILES
$!
$ solvematch
g5afid.xyz
g5bfid.xyz
/	list of points in 1st series with corresponding points in 2nd series
/	list of corresponding points in 2nd series (/ on both lines if all 1:1)
0,0	X-axis tilt used in generating first and second tomograms
8	Limiting value for maximum residual: higher residuals will give error
0	0 TO USE MODELS, 2 if 2 surfaces, 1/-1 if one surface, -1 if INVERTED
g5a.rec
$!     Next line is model of matching points in first tomogram
g5a.matmod
g5b.rec
$!     Next line is model of matching points in second tomogram
g5b.matmod
solve.xf
$!
$echo "STATUS: SOLVEMATCH RAN WITH MODEL FILES, NEXT RUNNING INITIAL MATCHVOL"
$!
$skipmodels:
$if (-e savework-file) savework-file
$!
$! Next run matchvol to make matching volume
$!     First two lines are input and output files
$!
$matchvol
g5b.rec
g5b.mat
g5tmpdir
/	NX,NY,NZ of output: / for NZ,NY,NX of input
1	Number of transformations: then file for each, or blank line & xform
solve.xf
$! Final entry is file name for inverse xform output, or blank line for none
$! Include name here in case you have to run warpvol instead of matchvol later
inverse.xf
$!
$echo "STATUS: MATCHVOL FINISHED, NEXT RUNNING PATCHCRAWL3D"
$!
$dopatch:
$!
$! Next run patchcorr3d with output tee'd to a file
$!  Arguments are size of patches, # of positions, and lower and upper limits
$!  of sampled volume in each dimension.  IF YOU HAVE TO ADJUST THE NUMBER OF
$!  PATCHES AND LIMITS, REMEMBER THAT THEY APPLY TO THE FILE BEING MATCHED TO, 
$!  AND THAT Y IS THICKNESS HERE
$!
$!  TO SPECIFY A MODEL FILE WITH CONTOURS ENCLOSING THE PATCHES TO ANALYZE,
$!  ADD THE NAME OF THE FILE AT THE END OF THE LINE STARTING WITH solve.xf
$!
$!            Patch Size  # patches    Lower and Upper Limits
$!             X   Y   Z   X  Y  Z   XL - XU   YL -YU   ZL - ZU   
$patchcrawl3d  56 20 56   3  2  5   36  988  10  72  36  988 \
 10 g5a.rec g5b.mat patch.out \
 solve.xf g5b.rec boundary_model
$!
$if (-e savework-file) savework-file
$!
$echo "STATUS: PATCHCRAWL3D FOUND DISPLACEMENTS, NEXT RUNNING MATCHORWARP"
$!
$domatchorwarp:
$!
$! MATCHORWARP RUNS REFINEMATCH AND MATCHVOL, OR FINDWARP AND WARPVOL.
$!
$! -refinelimit #   SPECIFIES THE MAXIMUM MEAN RESIDUAL ALLOWED BY REFINEMATCH;
$! ABOVE THIS, WARPING WILL BE USED.
$!
$! -warplimit #,#,#...   SPECIFIES A SERIES OF MEAN RESIDUALS THAT WARPVOL WILL
$! TRY TO ACHIEVE; ABOVE THE HIGHEST LIMIT, YOU HAVE TO RUN FINDWARP BY HAND
$!
$! TO SPECIFY A MODEL FILE WITH CONTOURS ENCLOSING THE PATCHES TO INCLUDE IN
$! THE FIT, ADD AN ARGUMENT "-modelfile filename" RIGHT AFTER "matchorwarp".
$!
$matchorwarp -size g5a.rec -refinelimit 0.3 \
 -warplimit 0.2,0.27,0.35 -tempdir g5tmpdir g5b.rec g5b.mat
$!
$goto skipwarp
$!
$domatch:
$!
$! THIS MATCHVOL IS RUN ONLY IF YOU HAVE TO DO FINDWARP BY HAND, GET A GOOD
$! REFINE.XF BY OMITTING ROWS OR COLUMNS, AND HAVE TO RESTART THE PROCESS
$!
$! THERE IS NOTHING BELOW HERE THAT YOU SHOULD HAVE TO CHANGE
$!
$matchvol
g5b.rec
g5b.mat
g5tmpdir
/	NX,NY,NZ of output: / for NZ,NY,NX of input
2	Number of transformations: then file for each, or blank line & xform
solve.xf
refine.xf
inverse.xf
$!
$goto skipwarp
$!
$dowarp:
$!
$! THIS WARPVOL IS RUN ONLY IF YOU HAVE TO DO FINDWARP BY HAND AND
$! RESTART THE PROCESS
$!
$warpvol
g5b.rec
g5b.mat
g5tmpdir
/	NX,NY,NZ of output: / for NZ,NY,NX of input
warp.xf
$!
$skipwarp:
$if (-e savework-file) savework-file
$!
$echo "STATUS: MATCHVOL OR WARPVOL FINISHED, NEXT RUNNING DENSMATCH"
$!
$!
$! Scale the densities in the match file to match the first tomogram.  Inputs:
$!     File being matched (first tomogram)
$!     File to be scaled (the second tomogram)
$!     BLANK LINE TO HAVE SCALED VALUES PUT BACK IN THE SAME FILE
$!
$densmatch
g5a.rec
g5b.mat

$!
$echo "STATUS: DENSMATCH FINISHED, PROCEEDING TO COMBINE TOMOGRAMS"
$!
$! purge some previous versions if necessary: these are the huge files
$!
