* * * * * * *  TILTALIGN  * * * * * * *
c
c	  This program will solve for the displacements, rotations, tilts, and
c	  magnification differences relating a set of tilted views of an
c	  object.  It uses a set of fiducial points that have been identified
c	  in a series of views.	 These input data are read from a model in
c	  which each fiducial point is a separate object or contour.
c
c		This program uses the same variable metric minimization
c	  approach used in the programs ALIGN and ALIGNXYZ (Mike Lawrence,
c	  1982) obtained from R.A. Crowther at the MRC.	 This program has five
c	  major additional features:
c
c	  1) Any given fiducial point need not be present in every view.
c	  Thus, one can track each fiducial point only through the set of
c	  views in which it can be reliably identified, and one can even skip
c	  views in the middle of that set.
c
c	  2) The program can solve for distortion in the plane of the section.
c	  It does so with two additional variables: "dmag", an increment to the
c	  magnification along the X axis; and "skew", the difference in
c	  rotation between the X and Y axis.
c
c	  3) It is possible to constrain several views to have the same
c	  unknown value of tilt angle, magnification, compression, or
c	  distortion.  This can reduce the number of unknowns and can give
c	  more accurate overall solutions and estimates of section thinning.
c
c	  4) If the fiducial points are supposed to lie in one or two planes,
c	  then after the minimization procedure is complete, the program can
c	  analyze the solved point positions and determine the slope of this
c	  plane.  It uses this slope to indicate two different ways that you
c	  can make the plane be horizontal.  One way is to change ALL of the
c	  tilt angles by the recommended amount, which you might choose to do
c	  if the INTERVALS between tilts were thought to be accurate.  The
c	  other way is to change the maximum tilt angle by a different
c	  recommended amount, which you might choose to do if you were
c	  confident that the plane should be flat at a tilt of zero degrees
c	  but less confident of the interval between tilts.
c
c	  5) The program can solve for a series of local alignments using
c	  subsets of the fiducial points.  This can be useful when aligning a
c	  large area which does not behave uniformly during the tilt series. 
c	  The local alignments can then be used to obtain a single large
c	  reconstruction whose resolution is as good as would be attained in
c	  a smaller volume.
c
c	  The constraining of different views to have related values of some
c	  unknown variable is called "mapping"; it works differently for tilt
c	  than for other variables.  For variables other than tilt, if two or
c	  more views are mapped to the same variable, then all of those
c	  views will have the same value.  For tilt angle, if two views are
c	  mapped to the same tilt variable, then the DIFFERENCE between their
c	  tilt angles is constrained to be a constant equal to the difference
c	  between their initial tilt angles.  So, if they have the same
c	  initial tilt angle, they will always have the same tilt; and if
c	  their initial tilt angles differ by 10, their tilt angles will
c	  always differ by 10.
c	  
c	  Mapping can be set up relatively easily with "automapping".  When you
c	  select automapping, the program will map views in a group of adjacent
c	  views to the same variable, and it will determine a set of groups of
c	  a specified size.  The mappings may not be the same as what a person
c	  would do.  The program writes out a list of mapping variables which
c	  could be modified and used to specify a better mapping.
c	
c	  With automapping, the program can also set up variables that change
c	  linearly from one group to the next, rather than being constrained to
c	  the same value for all views in a group.  In other words, the values
c	  for all of the views in a group will be a linear combination of the
c	  same two actual variables (typically the first one in the group and
c	  the first one in the next group).  This feature usually gives a
c	  solution with less error.  The distinction between actual
c	  variables and combinations can be seen in the "Variable mappings"
c	  table.  Actual variables would appear as, e.g., "tilt	 15" and
c	  "tilt	 25", while combinations of the two appear as "t 15+ 20".
c	  There are also linear combinations between a variable and a fixed
c	  value, which appear in the table as "t 70+fix".  Currently, the
c	  linear mapping is available only with automapping, not with manually
c	  specified mappings.
c	  
c	  With automapping, the size of the groups will be adjusted
c	  dynamically for two variables, tilt angle and x-axis stretch, so that
c	  groups become smaller at higher tilt angles.  This is done because
c	  it is easier to solve accurately for tilt angle at higher tilt, and
c	  because the solution for x-axis stretch tends to change rapidly at
c	  high tilts.  The group size that you specify for these variables will
c	  be the average size of the whole range of tilts.  If this dynamic
c	  automapping gives problems with tilt angle, use mapping in blocks
c	  rather than linear mapping to have stricter control over the mapping
c	  process.  The dynamic automapping is used for both kinds of mapping
c	  of x-axis stretch because the mapping in blocks is the preferred
c	  method for grouping this variable.  (Linear mapping does not always 
c	  work properly.)
c	  
c	  The program can embark on local alignments after obtaining the
c	  standard global solution with all of the fiducials.  The program
c	  divides the image area into a regular array of overlapping subareas.
c	  Fiducials whose X and Y coordinates fall within a subarea are
c	  included in the computations for that subarea.  A subarea is expanded
c	  if necessary to include a certain minimum number of fiducials.  The
c	  program then seeks a solution for the subset of fiducials that is,
c	  for all variables, "incremental" to the global solution; that is, it
c	  solves for variables that are added to the parameters from the global
c	  solution.  This method allows a dramatic reduction in the number of
c	  variables to be solved for, mostly because rotation and
c	  magnification can be mapped to a much smaller number of variables
c	  than in the global solution.  The usual need for each view to have
c	  its own rotation and magnification variable is already accommodated
c	  in the global solution.
c	  
c	  One option in the local alignment is whether to solve for the X-Y-Z
c	  coordinates of the subset of fiducials, or to fix them at their
c	  values from the global solution.  Solving for the coordinates may
c	  give a more accurate solution but it does require more fiducials to
c	  get a reliable result.  Fixing the coordinates reduces the number of
c	  variables to be solved for and allows a reliable solution with only
c	  a relatively few fiducials; it also avoids distortions in the
c	  resulting reconstruction that could be difficult to account for when
c	  trying to combine reconstructions from tilts around two axes.
c
c	  Parameters are entered in the following order.  Lines starting with
c	  IF are entered only for particular choices on the preceding line.
c
c	  Model file name
c
c	  Name of image file on which model was built, or a blank line
c
c	  IF a blank line was entered, next enter the image dimensions NX & NY,
c	  .   the X and Y origin (default is 0,0), and the X and Y delta
c	  .   values (default is 1,1).
c
c	  Either a filename with an extension other than ".res" to output a
c	  .     3-D model of the fiducials based on their solved positions,
c	  .   or a filename including ".res" for a list of all residuals,
c	  .     which can be converted to a model with Patch2imod,
c	  .   or a filename with NO extension to get both outputs, one with
c	  .     extension .3dmod and one with extension .resid,
c	  .   or a blank line for neither output
c
c	  Name of file to which to write (in ASCII text) the solved X-Y-Z
c	  .   coordinates, or a blank line for no text output
c
c	  Name of file to which to write a list of the solved tilt angles,
c	  .   or a blank line for no tilt angle output
c
c	  Name of output file for solutions and/or transformations
c
c	  -1 to put only the solved values of the parameters in the output file
c	  .   or 1 to put only transformations for aligning the images
c	  .	into the output file,
c	  .   or 0 to put both in the file.
c
c	  0 to include all views that have points in the model file
c	  .   or 1 to specify the starting, ending, and increment views to
c	  .     include
c	  .   or 2 to enter a list of the individual views to include
c	  .   or 3 to enter a list of views to exclude
c
c	  IF 1 was selected, next enter the starting and ending view number
c	  .    and the increment to use between those values
c
c	  OR IF 2 was selected, enter a list of views to be included.  Ranges 
c	  .   may be entered; e.g. 0-3,6-8
c	  
c	  OR IF 3 was selected, enter a list of views to be excluded
c
c	  Initial angle of rotation in the plane of projection.	 This is the
c	  .   rotation (CCW positive) from the Y-axis (the tilt axis after the
c	  .   views are aligned) to the suspected tilt axis in the unaligned
c	  .   views.
c
c	  0 to solve for all rotation angles (and thus to solve for tilt axis),
c	  .   or the number of the view to fix at the initial rotation angle
c	  .	(and thus fix the tilt axis at that angle for that view)
c	  .   or -1 to solve for a single global rotation of the tilt axis
c	  .   or -2 to fix all rotation angles at the initial angle
c
c	  Number of sets of views to treat separately from the main set of
c	  .   views in any automapping of variables.  Enter 0 if you will not
c	  .   be using automapping or if all views can be treated together
c	  .   when automapping.	 These sets would typically be lists of views
c	  .   that were reshot so they will be referred to as reshoot sets.
c	  
c	  IF a number other than 0 was entered, next enter one line for each
c	  .   separate set, giving a list of the views included in that set.
c	  .   Ranges are allowed here.
c
c	  -1 to enter initial tilt angles individually, 1 to enter starting
c	  .   and increment tilt angles, or 0 to read tilt angles from a file
c
c	  Either the individual tilt angles (ranges NOT allowed), the
c	  .   starting and increment tilt angles, or the name of a file with
c	  .   tilt angles, depending on whether you just entered -1, 1, or 0.
c
c	  Tilt angle offset, i.e. amount to ADD to all entered tilt angles
c
c	  0 to fix all tilt angles at their initial values
c	  .   or 1 to solve for all tilt angles except for a specified view
c	  .   or 2 to solve for all tilt angles except for the view at minimum
c	  .	tilt 
c	  .   or 3 to solve for all tilt angles except for a specified view
c	  .	and the view at minimum tilt
c	  .   or 4 to specify some other combination of fixed, variable, and
c	  .	mapped tilt angles
c	  .   or 5 or 6 to automap groups of tilt angles; 5 for linearly 
c	  .      changing values or 6 for values all the same within a group 
c	  .   or 7 or 8 to automap and fix two tilt angles; 7 for linearly 
c	  .      changing values or 8 for values all the same within a group 
c	  
c	  IF 1, 3, 7 or 8 was selected, next enter the number of the view 
c	  .	whose tilt angle will be fixed.
c
c	  IF 4 was selected, next enter a variable number for each view, or 0
c	  .   to fix the view at its initial tilt.  For convenience, these
c	  .   variable numbers can be completely arbitrary; e.g. 0,1,1,1,2,2
c	  .   and 0,9,9,9,5,5 both do the same thing: fix the tilt for view 1,
c	  .   map the tilts for views 2, 3 and 4 to the first tilt variable,
c	  .   and map the tilts for views 5 and 6 to the second tilt variable.
c
c	  IF 5 to 8 was selected, next enter the default number of views to
c	  .   group together, and the number of ranges of views that should
c	  .   have some grouping other than the default.  If a negative number
c	  .   of views is entered, then reshoot sets will NOT be segregated
c	  .   from the rest of the views in this default mapping.
c	  
c	  .   IF you entered a non-zero number of ranges to be treated
c	  .	 separately, then for each such range, enter the starting and
c	  .	 ending view number and the number of views that should be
c	  .	 grouped in that range.	 If a negative number of views is
c	  .	 entered, then reshoot sets will NOT be segregated from the
c	  .	 rest of the views in this range.
c
c	  Number of "reference" view whose magnification will be fixed at 1.0.
c	  .   Enter "/" to accept the default, which is the view at minimum
c	  .   tilt.
c 
c	  0 to fix all magnifications at 1.0,
c	  .   or 1 to vary all magnifications independently,
c	  .   or 2 to specify a mapping of magnification variables
c	  .   or 3 or 4 for automapping of variables; 3 for linearly changing 
c	  .   values or 4 for values all the same within a group 
c
c	  IF 2 was selected, enter a magnification variable number for each
c	  .   view.  As for tilts, these variable numbers can be completely
c	  .   arbitrary.  The reference view (typically the one at minimum
c	  .   tilt angle) will be constrained to a mag of 1.0; to constrain
c	  .   any other views to a mag of 1.0, simply map them to the same
c	  .   variable number as for the reference view.
c
c	  IF 3 or 4 was selected, enter default group size, number of special
c	  .   ranges, and group size for each such range just as for tilt
c	  .   variable automapping.
c
c	  0 to omit section compression,
c	  .   or the number of the view to fix at compression 1.0 (something
c	  .	other than a view whose tilt angle is fixed at zero.)
c
c	  IF compression is being solved for, next enter 1 to have all
c	  .   compressions be independent, or 2 to specify mappings of
c	  .   compression variables, or 3 or 4 for automapping; 3 for linearly 
c	  .   changing values or 4 for values all the same within a group
c	  
c	  .   IF 2 was selected, enter a compression variable number for each
c	  .	 view.	As for tilts, these variable numbers can be completely
c	  .	 arbitrary.  The reference view will be constrained to a comp
c	  .	 of 1.0; to constrain any other views to a comp of 1.0, simply
c	  .	 map them to the same variable number as for the reference
c	  .	 view.	A View with tilt fixed at zero will automatically have
c	  .	 its comp fixed at 1.0 UNLESS you map it to another view whose
c	  .	 tilt is not fixed at zero.
c
c	  .   IF 3 or 4 was selected, enter default group size, number of 
c	  .	 special ranges, and group size for each such range just as
c	  .	 for tilt variable automapping.
c
c	  0 to omit section distortion, 1 to include distortion and use the
c	  .   same set of mappings for X-axis stretch and skew variables, or
c	  .   2 to specify separate mappings for these two variables.
c	  
c	  IF 1 or 2 was selected, answer the next two queries either once
c	  .   (for distortion variables in general) or twice (first for the
c	  .   X-axis stretch, then for the skew):
c	  
c	  .   1 for independent variables, 2 to specify a mapping, or 3 or 4
c	  .	 for automapping.  For X-axis stretch, 3 will make values be 
c	  .      all the same within a group and 4 will make values change
c	  .      linearly (not recommended).  For skew, use 3 for linearly
c	  .      changing values or 4 for values all the same within a group.
c	  
c	  .   IF 2 was selected, enter a variable number for each view.	 The
c	  .	 reference view for magnification will be constrained to
c	  .	 distortion values of 0; to constrain any other views to 0
c	  .	 distortion, map them to the same variable number as for the
c	  .	 reference view.
c
c	  .   IF 3 or 4 was selected, enter default group size, number of 
c	  .	 special ranges, and group size for each such range just as
c	  .	 for tilt variable automapping.
c
c	  Criterion number of standard deviations above mean residual error
c	  .   that should be reported.	This can be based on either the overall
c	  .   mean and S.d. of the residual errors, or on a mean and S.d.
c	  .   computed from points in nearby views.  Enter a positive value 
c	  .   for a report based on overall mean, or a negative value for a
c	  .   report based on the mean residual in the same and nearby views.
c
c	  0 to omit analysis of surfaces,
c	  .   or 1 or 2 to derive a new maximum tilt angle by assuming that
c	  .   points lie on 1 or 2 surfaces.
c
c	  "Factor", and limit on number of cycles, for the variable metric
c	  .   minimization (METRO).  "Factor" determines how large a step METRO
c	  .   tries to take.  The default for "factor" is 0.5, but smaller
c	  .   values of 0.35 or even 0.25 are needed for large data sets.  The
c	  .   default # of cycles is 500.
c
c	  IF transformations are being computed, next enter two more lines:
c
c	  .   Amount to shift the tilt axis in Z, relative to the centroid in 
c	  .   Z of the fiducial points, or 1000 to shift the tilt axis to the
c	  .   midpoint of the range of Z values 
c
c	  .   Amount to shift the tilt axis in X away from the center of the 
c	  .   image
c	  
c	  0 to exit, or 1 to do a series of local alignments.  If you enter 1
c	  .   to do local alignments, a series of entries is required to
c	  .   specify the special parameters for local alignment, then a
c	  .   series of entries to specify the mapping of variables.  Entries
c	  .   continue with:
c	  
c	  Name of output file in which to place transformations for local
c	  .   alignment.
c	  
c	  Number of local patches in X and in Y in which to obtain a solution
c	  .   from the fiducials located in that patch
c	  
c	  Either the minimum size of each patch in X and Y (enter values > 1)
c         .   or the minimum fractional overlap between patches (values < 1)
c	  
c	  Minimum total number of fiducials, and minimum number present on each
c	  .   surface if two surfaces were assumed in the analysis of
c	  .   surfaces.  A patch will be expanded about its center until it
c	  .   contains enough points to meet both of these criteria.
c	  
c	  0 to solve for the X-Y-Z coordinates of the fiducials independently
c	  .   in each local area, or 1 to fix them at their values from the
c	  .   global solution
c	  
c	  Three entries to control the output of results for each local
c	  .   alignment: 1 to output the values of the parameters for each
c	  .   view or 0 not to; 1 to output the X-Y-Z coordinates of fiducials
c	  .   or 0 not to; 1 to output points with high residuals, or 0 not to
c	  
c	  Finally, there are a series of entries just as above, to control the
c	  mapping of rotation, tilt, magnification and distortion variables:
c	  
c	  0 to fix all rotations, 1 to have them all be independent, 2 to
c	  .   specify a mapping of rotation variables, 3 for automapping with
c	  .   linearly changing values, or 4 for automapping in blocks of
c	  .   values.  After this, enter mapping information as appropriate.
c	  
c	  0-8 to specify treatment of tilt variables, as described above.
c	  .   After this, enter mapping information for tilt variables as
c	  .   appropriate.
c
c	  Number of "reference" view whose magnification will be fixed at 1.0.
c	  .   Enter "/" to use the view at minimum tilt.
c 
c	  0-3 to specify treatment of magnification variables, as described
c	  .   above.  Then enter mapping information as appropriate.
c	  
c	  0 to omit section distortion, 1 to include distortion and use the
c	  .   same set of mappings for X-axis stretch and skew variables, or
c	  .   2 to specify separate mappings for these two variables.  After
c	  .   this, enter specifications for treatment of each or both sets of
c	  .   parameters, just as above.
c
c
c	  Note: when compression is solved for, the program prints both the
c	  absolute and the incremental compression for each view.  When no
c	  compression is solved for, the program prints instead two additional
c	  columns: "deltilt" is the difference between the solved and original
c	  tilt angles, and "mean resid" is the mean residual error for each
c	  view.
c
c	  David Mastronarde  March 1989
c	  5/19/89 added model output, changed format of output table
c	  6/21/89 added mean residual output to find_surfaces, changed to
c	  get recommendation on maximum FIXED tilt angle
c	  4/9/93 allow full mapping of compression variables
c	  10/30/95 added distortion, automapping, point & angle output.
c	  10/17/98 added linear combinations to automapping
c	  2/12/98 added local alignments; changed find_surfaces to find and
c	  recommend an X-axis tilt.
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.8  2003/10/03 00:59:07  mast
c	  Changed terminology to refered to tilt angle offset
c	
c	  Revision 3.7  2003/01/30 20:54:51  mast
c	  Made fields for residuals bigger, amplified IER error messages
c	
c	  Revision 3.6  2002/12/21 00:00:33  mast
c	  Add ability to get both residual output and 3D model
c	
c	  Revision 3.5  2002/10/17 23:18:31  mast
c	  Added proper error message and exit for minimum number of beads too
c	  high in local alignments
c	
c	  Revision 3.4  2002/07/28 23:02:54  mast
c	  Needed to declare lnblnk for SGI
c	
c	  Revision 3.3  2002/07/28 22:42:35  mast
c	  Changes to output a residual listing file and to standardize error
c	  exits and output
c	
c	  Revision 3.2  2002/05/09 03:48:38  mast
c	  Fixed a line length that did not compile on SGI
c	
c	  Revision 3.1  2002/05/07 02:05:19  mast
c	  Changes to handle subset of views better: output of transforms and
c	  tilt angles for all views in file, and interpretation of user input
c	  and all output in terms of view numbers in file rather than in
c	  program.  Also changed the surface analysis output to make it more
c	  understandable and machine readable.
c	
c
	implicit none
	include 'alivar.inc'
	integer maxvar,maxmetro
	parameter (maxvar=7*maxview)
	parameter (maxmetro=1300)
c
	integer*4 ninreal(maxreal),igroup(maxreal)
	integer*4 imodobj(maxreal),imodcont(maxreal)
	real*4 var(maxvar),varerr(maxvar),xyzerr(3,maxreal)
	real*4 grad(maxmetro),h(maxmetro*(maxmetro+3))
	character*8 varname(maxvar)
	double precision error
	real*4 erlist(100),tiltorig(maxview),viewres(maxview)
	integer*4 ninview(maxview),indsave(maxprojpt),jptsave(maxprojpt)
	real*4 errsave(maxprojpt)
	real*4 viewerrsum(maxview),viewerrsq(maxview)
	real*4 viewmeanres(maxview),viewsdres(maxview)
	
	logical ordererr,nearbyerr,residualout, resbothout
	character*120 modelfile,residualfile
c
	real*4 fl(2,3,maxview),fa(2,3),fb(2,3),fc(2,3)
c
	real*4 allxyz(3,maxreal)
	real*4 allxx(maxprojpt),allyy(maxprojpt)
	real*4 glbfl(2,3,maxview)
	integer*4 iallsecv(maxprojpt),iallrealstr(maxreal)
	integer*4 indallreal(maxreal)
c
	logical firsttime,xyzfixed,toofewfid
	common /functfirst/ firsttime,xyzfixed
	integer*4 ncycle/500/,nsolve/95/
	real*4 DTOR/0.0174532/
c	  
	integer*4 nlocalres,nsurface,iwhichout,metroerror,isolmin,i,itry
	integer*4 inputalf,mapalfend,ifvarout,ifresout,ifxyzout,iflocal
	integer*4 isolmininit,iv,nvarsrch,nvargeom,index,nvarang,nvarscl
	real*4 errcrit,facm,znew,xtiltnew,scalexy,ermin,ermininit,errsum
	real*4 errsqsm,residerr,vwerrsum,vwerrsq,sxoz,szox,sxox,szoz
	real*4 xo,zo,xshft,zshft,rollpts,costh,sinth,xtmp,compinc,compabs
	integer*4 nvadd,ninvsum,ivst,ivnd,iunit2,nunknowtot,iunit
	real*4 unkrat,tiltout,zmin,zmax,zmiddle,dysum,cosphi,sinphi
	real*4 dyavg,offmin,dxmid,offsum,dxtry,xtfac,xtconst,off,yshft
	integer*4 j,iuangle,iuxtilt,iupoint,ndxtry,iunlocal,nallrealpt
	integer*4 mapalfstart,nord,jpt,npatchx,npatchy,kount,ivt,ipt
	integer*4 nxpmin,nypmin,minfidtot,minfidsurf,ifxyzfix,nallprojpt
	real*4 errmean,errsd,errnosd,tiltmax,fixedmax,xsum,ysum,zsum
	integer*4 idxpatch,idypatch,ipatchx,ipatchy,ixspatch,iyspatch
	integer*4 nxp,nyp,minsurf,nbot,ntop,ixmin,ixmax,iymin,iymax,kk
	integer*4 nprojpt,imintilt,ncompsrch,maptiltstart,isolve,ier
	real*4 xcen,ycen,finit,f,ffinal,dxmin,tmp,tiltnew,fixeddum,tiltadd
	integer*4 ixtry,itmp,iord,ixpatch,iypatch,ivdel
	real*4 xpmin,ypmin
	real*4 atand,sind,cosd
	integer*4 nearest_view,lnblnk
	character*80 concat
c
	nlocalres=50
	firsttime=.true.
	xyzfixed=.false.
	toofewfid=.false.
	incrgmag=0
	incrdmag=0
	incrskew=0
	incrrot=0
	incrtilt=0
	incralf=0
c	  
c	  set this to 1 to get inputs for X-axis tilting
c	  
	inputalf=0
c
	iuxtilt=inputalf
	call input_model(xx,yy,isecview,maxprojpt,maxreal,irealstr,
     &	    ninreal,imodobj,imodcont,nview,nprojpt, nrealpt,iwhichout,
     &	    xcen,ycen, mapviewtofile,mapfiletoview,nfileviews,modelfile,
     &	    iupoint,iuangle,iuxtilt)
c	  
	if(nview.gt.maxview)call errorexit('TOO MANY VIEWS FOR ARRAYS',
     &	    0)

	call input_vars(var,varname,inputalf,nvarsrch,nvarang,nvarscl,
     &	    imintilt, ncompsrch,0,maptiltstart,mapalfstart,tiltorig,
     &	    tiltadd)
	mapalfend=nvarsrch
c
	do i=1,nview
	  viewres(i)=0.
	  ninview(i)=0
	enddo
c
	write(*,'(1x,a,/,a,$)') 'Criterion # of sd above mean residual'
     &	    //' error to report (+ for ',
     &	    'relative to absolute mean,  - for relative to mean '//
     &	    'of nearby views): '
	read(5,*)errcrit
	ordererr=.true.
	nearbyerr=errcrit.lt.0.
	errcrit=abs(errcrit)
c
	write(*,'(1x,a,$)')'1 or 2 to derive a tilt angle assuming'//
     &	    ' points are on 1 or 2 surfaces: '
	read(5,*)nsurface
c
	facm=0.5
	write(*,'(1x,a,f5.2,i5,a,$)')
     &	    'Factor for METRO, limit on # of cycles [',facm,ncycle,']: '
	read(5,*)facm,ncycle
c
	if(iwhichout.ge.0)then
c	  
c	  find out what to do with z value of axis
c
	  print *,'Z shift in tilt axis relative to centroid,'
	  write(*,'(1x,a,$)')
     &	      '	  or 1000 to shift to middle of z range: '
	  read(5,*)znew
c	    
c	    get new position of tilt axis in x
c
	  write(*,'(1x,a,$)')
     &	      'New X position of tilt axis relative to center: '
	  read(5,*)xtiltnew
	endif
C	  
c	  scale the points down to range of 1.0: helps convergence
c
	ifvarout=1
	ifresout=1
	ifxyzout=1
	iflocal=0
	metroerror=0
	do i=1,nrealpt
	  indallreal(i)=i
	enddo
c
	scalexy=0.
	do i=1,nprojpt
	  scalexy=max(scalexy,abs(xx(i)),abs(yy(i)))
	enddo
	do i=1,nprojpt
	  xx(i)=xx(i)/scalexy
	  yy(i)=yy(i)/scalexy
	enddo
c	  
c	  call ye olde init_dxy to get initial dx and dy, and solve_xyzd to
c	  get initial values of x,y,z
c	  NOTE that these routines are probably more complicated than necessary
c	  to get the minimization going; they were written for an earlier
c	  attempt to solve for alignment variables and were simply adopted here
c	  without assessing their necessity.  They do start the process out
c	  with x,y,z values that are nearly correct for the initial angles.
c	  
c	  try either with initial dxy solved to equalize centroids section-to-
c	  section, or with dxy 0.  Find which way gives lowest error somewhere
C	  along the line, and redo it that way to do just the best number of
c	  iterations
c
	call remap_params(var)
c	  
c	  initial trial with call to INIT_DXY
c
	call init_dxy(xx,yy,isecview,irealstr,
     &	    nview,nrealpt,imintilt,dxy)
c
	do itry=1,2
c	    
c	    second time through, save minimum error and iteration # from
c	    first trial that used call to init_dxy
c
	  isolmininit=isolmin
	  ermininit=ermin
c
	  call solve_xyzd(xx,yy,isecview,irealstr,nview, nrealpt,tilt,rot,
     &	      gmag,comp,xyz,dxy,nsolve,error,erlist,isolve)
c	    
c	    find iteration with minimum error
c
	  ermin=1.e30
	  do i=1,isolve-1
	    if(erlist(i).lt.ermin)then
	      isolmin=i
	      ermin=erlist(i)
	    endif
	  enddo
c	  print *,itry,isolve,ermin,isolmin
c	    
c	    set dxy to 0 for second try, or leave at zero for final setup
c
	  do iv=1,nview
	    dxy(1,iv)=0.
	    dxy(2,iv)=0.
	  enddo
	enddo
c
	if(ermininit.lt.ermin)then
	  isolmin=isolmininit
	  call init_dxy(xx,yy,isecview,irealstr,
     &	      nview,nrealpt,imintilt,dxy)
	  print *,
     &	      'DXY set to equalize centroids gave best initialization'
	else
	  print *, 'DXY set to zero gave best initialization'
	endif
c
	call solve_xyzd(xx,yy,isecview,irealstr,nview, nrealpt,tilt,rot,
     &	    gmag,comp,xyz,dxy,isolmin,error,erlist,isolve)
c
c	    
c	  pack the xyz into the var list
c	  
180	nvargeom=nvarsrch
	if(nvargeom+3*nrealpt.gt.min(maxvar,maxmetro))call errorexit(
     &	    'TOO MANY VARIABLES FOR VAR, GRAD, AND METRO H ARRAYS', 0)
	do jpt=1,nrealpt-1
	  do i=1,3
	    nvarsrch=nvarsrch+1
	    var(nvarsrch)=xyz(i,jpt)
	  enddo
	enddo
c
	firsttime=.true.
	call funct(nvarsrch,var,finit,grad)
	WRITE(6,70)FINIT
70	FORMAT(/' Variable Metric minimization',T48,
     &	    'Initial F:',T65,E14.7)
C
C  -----------------------------------------------------
C  Call variable metric minimizer
C  CALL METRO(N,X,F,G,FACTOR,EST,EPS,LIMIT,IER,H,KOUNT)
C  -----------------------------------------------------
C
	CALL METRO (nvarsrch,var,F,Grad,facm,.0000001,.00001,NCYCLE,IER,
     &	    H,KOUNT)
C Final call to FUNCT
	CALL FUNCT(nvarsrch,var,FFINAL,Grad)
	WRITE(6,98)FFINAL,KOUNT
98	FORMAT(/T48,'Final   F : ',T65,E14.7/
     &	    /' Number of cycles : ',I5)
C-----------------------------------------------------------------------
C Error returns:
	IF(IER.NE.0)THEN
	  if(ier.eq.1)then
	    call errorexit('IER=1  DG > 0; try changing metro factor',
     &		iflocal)
	  elseif(ier.eq.2)then
	    call errorexit('IER=2  Linear search lost; try changing '
     &		//'metro factor', iflocal)
	  elseif(ier.eq.4)then
	    call errorexit('IER=4  Matrix non-positive definite; try '
     &		//'changing metro factor', iflocal)
	  else
	    WRITE(6,930)
930	    FORMAT(/' IER=3  Iteration limit exceeded....')
	  endif
	  metroerror=metroerror+1
	END IF
c	  
c	  unscale all the points, dx, dy, and restore angles to degrees
c
	index=0
	do i=1,nvarang
	  var(i)=var(i)/dtor
	  index=index+1
	  varerr(i)=(sqrt(h(index*nvarsrch-nvarsrch+index))/nvarsrch)
     &	      /dtor
	enddo
c
	do i=nvarang+1,nvarscl
	  index=index+1
	  varerr(i)=sqrt(h(index*nvarsrch-nvarsrch+index))/nvarsrch
	enddo
c
	do i=nvarscl+1,nvargeom
	  var(i)=var(i)/dtor
	  index=index+1
	  varerr(i)=(sqrt(h(index*nvarsrch-nvarsrch+index))/nvarsrch)
     &	      /dtor
	enddo
c
	do i=1,nrealpt
	  do j=1,3
	    xyz(j,i)=xyz(j,i)*scalexy
	  index=index+1
	  if(i.lt.nrealpt)xyzerr(j,i)=
     &	      scalexy*sqrt(h(index*nvarsrch-nvarsrch+index))/nvarsrch
	  enddo
	enddo
c
	errsum=0.
	errsqsm=0.
	do i=1,nview
	  viewres(i)=0.
	  ninview(i)=0
	  viewerrsum(i)=0.
	  viewerrsq(i)=0.
	enddo
	do i=1,nprojpt
	  xx(i)=xx(i)*scalexy
	  yy(i)=yy(i)*scalexy
	  xresid(i)=xresid(i)*scalexy
	  yresid(i)=yresid(i)*scalexy
	  residerr=sqrt(xresid(i)**2 + yresid(i)**2)
	  iv=isecview(i)
	  ninview(iv)=ninview(iv)+1
	  viewerrsum(iv)=viewerrsum(iv)+residerr
	  viewerrsq(iv)=viewerrsq(iv)+residerr**2
	enddo
c
	do iv=1,nview
	  dxy(1,iv)=dxy(1,iv)*scalexy
	  dxy(2,iv)=dxy(2,iv)*scalexy
	  rot(iv)=rot(iv)/dtor
	  tilt(iv)=tilt(iv)/dtor
	  skew(iv)=skew(iv)/dtor
	  alf(iv)=alf(iv)/dtor
	  viewres(iv)=viewerrsum(iv)/ninview(iv)
	  errsum=errsum+viewerrsum(iv)
	  errsqsm=errsqsm+viewerrsq(iv)
c	    
c	    find mean and sd residual of minimum number of points in a local
c	    group of views
c
	  nvadd=1
	  ninvsum=0
	  do while(ninvsum.lt.nlocalres.and.nvadd.lt.nview)
	    ivst=iv-nvadd/2
	    if(ivst.lt.1)ivst=1
	    ivnd=ivst+nvadd-1
	    ninvsum=0
	    do ivt=ivst,ivnd
	      ninvsum=ninvsum+ninview(ivt)
	    enddo
	    nvadd=nvadd+1
	  enddo
	  vwerrsum=0.
	  vwerrsq=0.
	  do ivt=ivst,ivnd
	    vwerrsum=vwerrsum+viewerrsum(ivt)
	    vwerrsq=vwerrsq+viewerrsq(ivt)
	  enddo
	  viewmeanres(iv)=vwerrsum/ninvsum
	  viewsdres(iv)=sqrt((vwerrsq-vwerrsum**2/ninvsum)/(ninvsum-1))
	enddo
c
c	  if doing local solution, need to find rotation to match
c	  the original set of points
c	  
	if(iflocal.ne.0)then
	  sxoz=0.
	  szox=0.
	  sxox=0.
	  szoz=0.
	  do i=1,nrealpt
	    xo=allxyz(1,indallreal(i))-xcen-xshft
	    zo=allxyz(3,indallreal(i))-zshft
	    sxox=sxox+xo*xyz(1,i)
	    sxoz=sxoz+xo*xyz(3,i)
	    szox=szox+zo*xyz(1,i)
	    szoz=szoz+zo*xyz(3,i)
	  enddo
	  rollpts=0.
	  if((sxox+szoz).gt.1.e-5*abs(sxoz-szox))
     &	      rollpts=atand((sxoz-szox)/(sxox+szoz))
c	    
c	    rolls the points, reduce this amount from the tilts
c	    
	  costh=cosd(rollpts)
	  sinth=sind(rollpts)
	  do i=1,nrealpt
	    xtmp=xyz(1,i)*costh+xyz(3,i)*sinth
	    xyz(3,i)=-xyz(1,i)*sinth+xyz(3,i)*costh
	    xyz(1,i)=xtmp
	  enddo
	  do i=1,nview
	    tilt(i)=tilt(i)-rollpts
	  enddo
	endif
c
	iunit2=7
	if(iwhichout.gt.0)iunit2=6
	compinc=1.
	compabs=1.
	nunknowtot=nvargeom+3*(nrealpt-1)
	if(xyzfixed)nunknowtot=nvargeom
	unkrat=(2.*nprojpt)/nunknowtot
	do iunit=6,iunit2
	  write (iunit,113)nview,nvargeom,nrealpt,nprojpt,
     &	      2*nprojpt,nunknowtot,unkrat
113	  format(i4,' views,',i5,' geometric variables,',i5,
     &	      ' 3-D points,',i6,' projection points',/,
     &	      '  Ratio of total measured values to total unknowns =',
     &	      i5,'/',i3,' =',f7.2)
	  if(ifvarout.ne.0)then
	    if(iunit.ne.6)write(iunit,'(/,21x,a)')
     &		'Geometric variable values and errors'
	    if(iunit.ne.6)write(iunit,'(3(f10.4,f7.4,a9,1x))',err=85)
     &		(var(i),varerr(i),varname(i),i=1,nvargeom)
85	    if(ncompsrch.eq.0)then
	      if(mapalfstart.gt.mapalfend)then
		write(iunit,'(/,a)') ' view   rotation    tilt    '//
     &		    'deltilt     mag      dmag      skew    mean resid'
		do i=1,nview
		  j=mapviewtofile(i)
		  write(iunit,'(i4,2f10.1,f10.2,2f10.4,2f10.2)')
     &		      j,rot(i), tilt(i), tilt(i)-tiltorig(j),
     &		      gmag(i),dmag(i), skew(i), viewres(i)
		enddo
	      elseif(ifrotfix.eq.-1.or.ifrotfix.eq.-2)then
		if(ifrotfix.eq.-1)write(iunit,'(/,a,f7.2)')
     &		    ' Fixed rotation angle is',rot(1)
		if(ifrotfix.eq.-2)write(iunit,'(/,a,f7.2)')
     &		    ' Overall rotation angle is',rot(1)
		write(iunit,'(/,a)') ' view     tilt    deltilt   '//
     &		    '  mag      dmag      skew     X-tilt   mean resid'
		do i=1,nview
		  j=mapviewtofile(i)
		  write(iunit,'(i4,f10.1,f10.2,2f10.4,3f10.2)')
     &		      j, tilt(i), tilt(i)-tiltorig(j),gmag(i),dmag(i),
     &		      skew(i), alf(i),viewres(i)
		enddo		
	      else
		write(iunit,'(/,a)')'WARNING: SOLUTIONS FOR BOTH '//
     &		    'ROTATION AND X-AXIS TILT ARE VERY UNRELIABLE'
		write(iunit,'(/,a)') ' view rotation  tilt  deltilt'
     &		    //'    mag     dmag    skew   X-tilt  mean resid'
		do i=1,nview
		  j=mapviewtofile(i)
		  write(iunit,'(i4,2f8.1,f8.2,2f9.4,3f8.2)') j,rot(i),
     &		      tilt(i), tilt(i)-tiltorig(j),gmag(i),dmag(i),skew(i),
     &		      alf(i),viewres(i)
		enddo
	      endif
	    else
	      write(iunit,'(/,a)') ' view   rotation    tilt      mag'
     &		  //'    comp-inc  comp-abs    dmag      skew'
	      do i=1,nview
c		  
c		  for 0 tilts, output same compression values as last view
c		  
		if(tilt(i).ne.0.)then
		  compinc=comp(i)	
		  compabs=compinc*gmag(i)
		endif
		write(iunit,'(i4,2f10.1,4f10.4,f10.2)')mapviewtofile(i)
     &		    ,rot(i),tilt(i),
     &		    gmag(i),compinc,compabs,dmag(i),skew(i)
	      enddo
	    endif
	    write(iunit,*)
	    if((iuangle.eq.0.or.iunit.ne.6).and.iflocal.eq.0)
     &		write(iunit,116)(tilt(i),i=1,nview)
116	    format(' ANGLES',10f7.2)
	    if(ncompsrch.gt.0)write(iunit,117)(comp(i),i=1,nview)
117	    format(' COMPRESS',10f7.4)
	  endif
	enddo
	if(ifxyzout.ne.0)then
	  write(iunit2,111)
111	  format(/,21x,'3-D point coordinates'
     &	      ,/,'   #',7x,'X',9x,'Y',9x,'Z',6x,'obj  cont')
	  write(iunit2,'(i4,3f10.2,i7,i5)',err=86)
     &	      (indallreal(j),(xyz(i,j),i=1,3),imodobj(indallreal(j)),
     &	      imodcont(indallreal(j)),j=1,nrealpt)
	endif
c	  
	if(iflocal.eq.0.and.iupoint.ne.0)then
	  write(iupoint,'(i4,3f10.2,i7,i5)')(j,(xyz(i,j),i=1,3),
     &	      imodobj(j),imodcont(j),j=1,nrealpt)
	  close(iupoint)
	endif
c	  
c	  output lists of angles that are complete for all file views
c
	if(iflocal.eq.0.and.iuangle.ne.0)then
	  do i=1,nfileviews
	    tiltout=tiltorig(i)
	    if(mapfiletoview(i).ne.0)tiltout=tilt(mapfiletoview(i))
	    write(iuangle,'(f7.2)')tiltout
	  enddo
	  close(iuangle)
	endif
c
	if(iflocal.eq.0.and.iuxtilt.ne.0)then
	  do i=1,nfileviews
	    tiltout=0.
	    if(mapfiletoview(i).ne.0)tiltout=alf(mapfiletoview(i))
	    write(iuxtilt,'(f7.2)')tiltout
	  enddo
	  close(iuxtilt)
	endif
c	  
c	  get min, max and midpoint of z values
c	  
86	zmin=1.e10
	zmax=-1.e10
	do ipt=1,nrealpt
	  zmin=min(zmin,xyz(3,ipt))
	  zmax=max(zmax,xyz(3,ipt))
	enddo
	zmiddle=(zmax+zmin)/2.
	if(iflocal.eq.0)write(*,'(/,a,f8.2)')
     &	    ' Midpoint of Z range relative to centroid in Z:',zmiddle
c
c	  compute xforms, shift the dy's to minimize total shift, allow
c	  user to shift dx's (and tilt axis) similarly or specify new location
c	  of tilt axis
c	    shift axis in z by making proper shifts in x
c
	if(iwhichout.ge.0)then
	  if(znew.eq.1000.)znew=zmiddle
	  if(iflocal.ne.0)znew=-zshft
	  dysum=0.
	  do iv=1,nview
c	      
c	      set the distortion matrix into fa and the rotation matrix into fb
c
	    fa(1,1)=(gmag(iv)+dmag(iv))*cosd(skew(iv))
	    fa(2,1)=(gmag(iv)+dmag(iv))*sind(skew(iv))/cosd(tilt(iv))
	    fa(2,2)=gmag(iv)
	    fa(1,2)=0.
	    fa(1,3)=0.
	    fa(2,3)=0.
	    cosphi=cosd(rot(iv))
	    sinphi=sind(rot(iv))
	    fb(1,1)=cosphi
	    fb(1,2)=-sinphi
	    fb(2,1)=sinphi
	    fb(2,2)=cosphi
	    fb(1,3)=0.
	    fb(2,3)=0.
c	      
c	      get product, then add the dx's and dy's, then invert
c	      
	    call xfmult(fa,fb,fc)
	    fc(1,3)=dxy(1,iv)
	    fc(2,3)=dxy(2,iv)
	    call xfinvert(fc,fl(1,1,iv))
c	      
c	      adjust dx by the factor needed to shift axis in Z
c
	    fl(1,3,iv)=fl(1,3,iv) -znew*sind(tilt(iv))
	    h(iv)=1.-cosd(tilt(iv))
	    dysum=dysum+fl(2,3,iv)
	  enddo
	  dyavg=dysum/nview
	  if(iflocal.eq.0)then
c	    
c	    find value of X shift that minimizes overall loss of image - do
c	    exhaustive scan centered on dx of the minimum tilt image
c
	    offmin=1.e10
	    dxmid=fl(1,3,imintilt)
c	      
c	      DNM 11/10/01: eliminate real variable do loop in deference to f95
c	    do dxtry=dxmid-0.1*xcen,dxmid+0.1*xcen,0.1
c
	    ndxtry=2.*xcen
	    do ixtry=0,ndxtry
	      dxtry=dxmid+0.1*(ixtry-xcen)
	      offsum=0.
	      xtfac=xtiltnew+dxtry
	      xtconst=xtiltnew-xtfac
	      do iv=1,nview
		off=abs(fl(1,3,iv)+xtconst+xtfac*h(iv))-xcen*h(iv)
		if(off.gt.0.)offsum=offsum+off
	      enddo
	      if(offsum.lt.offmin)then
		offmin=offsum
		dxmin=dxtry
	      endif
	    enddo
	    xtfac=xtiltnew+dxmin
	    xtconst=xtiltnew-xtfac
c
c	      Put tilt axis at the new position, and get the final dy to
c	      add up to 0.
c
	    do iv=1,nview
	      fl(2,3,iv)=fl(2,3,iv)-dyavg
	      fl(1,3,iv)=fl(1,3,iv)+xtconst+xtfac*h(iv)
	    enddo
c	      
c	      output a transform for each file view, find the nearest one
c	      for non-included view
c
	    do iv=1,nfileviews
	      i=nearest_view(iv)
	      call xfwrite(7,fl(1,1,i),*99)
99	    enddo
c	      
c	      7/26/02: if modelfile contains .res, output residuals
c	      12/20/02: if there is no extension, output both residual and
c	      model
c	      
	    residualout = .false.
	    resbothout = modelfile .ne. ' '
	    
	    do i = 1, lnblnk(modelfile)-3
	      if (modelfile(i:i).eq.'.' .and. modelfile(i+1:i+1).eq.'r'
     &		  .and. modelfile(i+2:i+2).eq.'e' .and.
     &		  modelfile(i+3:i+3).eq.'s') residualout = .true.
	      if (modelfile(i:i).eq.'.')  resbothout = .false.
	    enddo
	    if (residualout.or.resbothout) then
	      if (residualout) then
		residualfile = modelfile
	      else
		residualfile = concat(modelfile, '.resid')
	      endif
	      call dopen(13,residualfile, 'new', 'f')
	      write(13,'(i6,a)')nprojpt,' residuals'
	      do i=1,nprojpt
		write(13, '(2f10.2,i5,3f8.2)')xx(i)+xcen,yy(i)+ycen,
     &		    mapviewtofile(isecview(i))-1,
     &		    xresid(i),yresid(i)
	      enddo
	      close(13)
c		
c		manage the model file name now; attach extension if model
c		wanted, or null it out if not
c		
	      if (residualout) then
		modelfile = ' '
	      else
		modelfile = concat(modelfile, '.3dmod')
	      endif
	    endif
	  else
c	      
c	      if local, output an angle for all file views
c	      
	    do i=1,nfileviews
	      iv=mapfiletoview(i)
	      h(i)=0.
	      if(iv.gt.0)h(i)=tilt(iv)-glbtilt(iv)/dtor
	    enddo
	    write(iunlocal,'(10f7.2)')(h(i),i=1,nfileviews)
c	    write(6,'(f8.2)')(tilt(iv),iv=1,nview)
	    if(mapalfstart.le.mapalfend)then
	      do i=1,nfileviews
		iv=mapfiletoview(i)
		h(i)=0.
		if(iv.gt.0)h(i)=alf(iv)-glbalf(iv)/dtor
	      enddo
	      write(iunlocal,'(10f7.2)')(h(i),i=1,nfileviews)
	    endif
c	      
c	      add the shifts to the dx and dy to get transforms that
c	      work to get back to the original point positions.
c	      Compose the inverse of an adjusting transform
c
	    do i=1,nfileviews
	      iv=mapfiletoview(i)
	      if(iv.gt.0)then
		fl(1,3,iv)=fl(1,3,iv)+xshft*cosd(tilt(iv))
		fl(2,3,iv)=fl(2,3,iv)+yshft
c		  call xfwrite(6,fl(1,1,iv),*199)
		call xfinvert(glbfl(1,1,iv),fa)
		call xfmult(fa,fl(1,1,iv),fb)
		call xfinvert(fb,fc)
	      else
		call xfunit(fc,1.)
	      endif
	      call xfwrite(iunlocal,fc,*199)
199	    enddo
	  endif
	endif
c	    
c	  print out points with high residuals
c
	errmean=errsum/nprojpt
	errsd=sqrt((errsqsm-errsum**2/nprojpt)/(nprojpt-1))
	write(*,'(/,a,2f8.3)')' Residual error mean and sd:'
     &	    ,errmean,errsd
	if(ifresout.gt.0)then
	  write(*,112)
c	    
c	    DEPENDENCY WARNING: Beadfixer relies on the # # ... line up to the
c	    second X
c
112	  format(/,9x,'Projection points with large residuals',/,
     &	      ' obj  cont  view   index coordinates      residuals',
     &	      '        # of',/,
     &	      '   #     #     #      X         Y        X        Y',
     &	      '        S.D.')
	  nord=0
	  do jpt=1,nrealpt
	    do i=irealstr(jpt),irealstr(jpt+1)-1
	      if(nearbyerr)then
		iv=isecview(i)
		errnosd=(sqrt(xresid(i)**2+yresid(i)**2)-
     &		    viewmeanres(iv))/viewsdres(iv)
	      else
		errnosd=(sqrt(xresid(i)**2+yresid(i)**2)-errmean)/errsd
	      endif
	      if(errnosd.gt.errcrit)then
		if(ordererr)then
		  nord=nord+1
		  errsave(nord)=errnosd
		  indsave(nord)=i
		  jptsave(nord)=jpt
		else
		  write(*,114) imodobj(indallreal(jpt)),
     &		      imodcont(indallreal(jpt)), mapviewtofile(isecview(i))
     &		      ,xx(i)+xcen
     &		      ,yy(i)+ycen, xresid(i), yresid(i),errnosd
114		  format(i4,2i6,2f10.2,3f9.2)
		endif
	      endif
	    enddo
	  enddo
	  if(ordererr)then
	    do i=1,nord-1
	      do j=i+1,nord
		if(errsave(i).lt.errsave(j))then
		  tmp=errsave(i)
		  errsave(i)=errsave(j)
		  errsave(j)=tmp
		  itmp=indsave(i)
		  indsave(i)=indsave(j)
		  indsave(j)=itmp
		  itmp=jptsave(i)
		  jptsave(i)=jptsave(j)
		  jptsave(j)=itmp
		endif
	      enddo
	    enddo
	    do iord=1,nord
	      i=indsave(iord)
	      write(*,114) imodobj(indallreal(jptsave(iord))),
     &		  imodcont(indallreal(jptsave(iord))),
     &		  mapviewtofile(isecview(i)),
     &		  xx(i)+xcen, yy(i)+ycen,xresid(i), yresid(i),
     &		  errsave(iord)
	    enddo
	  endif
	endif
	if(iflocal.ne.0)go to 200
c	  
c	  analyze for surfaces if desired.  Find the biggest tilt and the
c	  biggest fixed tilt, get recommended new value for the biggest fixed 
c	  tilt if it is not too small
c
	tiltmax=0.
	fixedmax=0.
	do iv=1,nview
	  if(abs(tilt(iv)).gt.abs(tiltmax))tiltmax=tilt(iv)
	  if(maptilt(iv).eq.0.and.abs(tilt(iv)).gt.abs(fixedmax))
     &	      fixedmax=tilt(iv)
	enddo
	if(fixedmax.ge.5.)tiltmax=fixedmax
	if(nsurface.gt.0)call find_surfaces(xyz,nrealpt,nsurface,
     &	    tiltmax,iunit2,tiltnew,igroup,ncompsrch,tiltadd)
	call write_xyz_model(modelfile,xyz,igroup,nrealpt)
c	  
c	  Ask about local alignments
c
	write(*,'(1x,a,$)')
     &	    '1 to do series of local alignments, 0 to exit: '
	read(5,*,err=209,end=209)iflocal
	if(iflocal.eq.0)go to 209
c
	if(iwhichout.lt.0)call errorexit(
     &	    'SOLUTION TRANSFORMS MUST BE OUTPUT TO DO LOCAL ALIGNMENTS',
     &	    0)
	write(*,'(1x,a,$)')
     &	    'Name of output file for local transformations: '
	read(5,'(a)')modelfile
	iunlocal=9
	call dopen(iunlocal,modelfile,'new','f')
c
	write(*,'(1x,a,$)')'Number of patches in X and Y: '
	read(5,*)npatchx,npatchy
	write(*,'(1x,a,/,a,$)')'Enter either the minimum size of '//
     &	    'patches in X and Y (values > 1) or the',
     &	    'minimum fractional overlap between patches in'//
     &	    ' X and Y (values < 1): '
	read(5,*)xpmin,ypmin
	write(*,'(1x,a,$)')'Minimum total # of fiducials, minimum '//
     &	    'on one surface if two surfaces: '
	read(5,*)minfidtot,minfidsurf
	write(*,'(1x,a,$)')'1 to fix XYZ coordinates to global '//
     &	    'solution, 0 to solve for them also: '
	read(5,*)ifxyzfix
	iflocal=1
	xyzfixed=ifxyzfix.ne.0
	if(xyzfixed)iflocal=2
	ifvarout=0
	ifresout=0
	ifxyzout=0
	write(*,'(1x,a,/,a,$)')'Enter 1 for full output of variables,'
     &	    //' 1 for output of XYZ coordinates,', ' and 1 for output'
     &	    //' of points with high residuals (0 for no output): '
	read(5,*)ifvarout,ifxyzout,ifresout
c	  
c	  set for incremental solution - could be input as option at this point
c
	incrdmag=1
	incrgmag=1
	incrskew=1
	incrtilt=1
	incrrot=1
	incralf=1
c	  
c	  save all aspects of global solution; scale angles back to radians
c	  
	do iv=1,nview
	  glbrot(iv)=rot(iv)*dtor
	  glbtilt(iv)=tilt(iv)*dtor
	  glbskew(iv)=skew(iv)*dtor
	  glbalf(iv)=alf(iv)*dtor
	  glbgmag(iv)=gmag(iv)
	  glbdmag(iv)=dmag(iv)
	  call xfcopy(fl(1,1,iv),glbfl(1,1,iv))
	  tilt(iv)=glbtilt(iv)
	enddo
c	  
	nallprojpt=nprojpt
	do i=1,nprojpt
	  allxx(i)=xx(i)
	  allyy(i)=yy(i)
	  iallsecv(i)=isecview(i)
	enddo
	nallrealpt=nrealpt
	do i=1,nrealpt
	  iallrealstr(i)=irealstr(i)
c	    
c	    shift the fiducials to real positions in X and Y
c
	  allxyz(1,i)=xyz(1,i)-dxmin+xcen
	  allxyz(2,i)=xyz(2,i)-dyavg+ycen
	  allxyz(3,i)=xyz(3,i)-znew
	enddo
c	write(*,121)
c121	format(/,11x,'Absolute 3-D point coordinates'
c     &	    ,/,'   #',7x,'X',9x,'Y',9x,'Z')
c	write(*,'(i4,3f10.2)',err=86)
c     &	    (j,(allxyz(i,j),i=1,3),j=1,nrealpt)

	call input_vars(var,varname,inputalf,nvarsrch,nvarang,nvarscl,
     &	    imintilt, ncompsrch,iflocal,maptiltstart,mapalfstart,
     &	    tiltorig,tiltadd)
	mapalfend=nvarsrch
c
c	  get the minimum patch size
c	  
	npatchx = max(1,npatchx)
	npatchy = max(1,npatchy)
	if (xpmin.gt.1.) then
	  nxpmin = xpmin
	else
	  nxpmin = 2*xcen/(npatchx - xpmin * (npatchx - 1))
	endif
	if (ypmin.gt.1.) then
	  nypmin = ypmin
	else
	  nypmin = 2*ycen/(npatchy - ypmin * (npatchy - 1))
	endif
c	  
c	  set up starting patch locations and intervals
c
	idxpatch=(nint(2*xcen)-nxpmin)/max(1,npatchx-1)
	idypatch=(nint(2*ycen)-nypmin)/max(1,npatchy-1)
	ipatchx=0
	ipatchy=1
	ixspatch=nxpmin/2
	iyspatch=nypmin/2
	write(iunlocal,'(7i6)')npatchx,npatchy,ixspatch,iyspatch,
     &	    idxpatch,idypatch,mapalfend+1-mapalfstart
c	  
c	  START OR CONTINUE LOOPING ON LOCAL REGIONS
c
200	ipatchx=ipatchx+1
	if(ipatchx.gt.npatchx)then
	  ipatchx=1
	  ipatchy=ipatchy+1
	  if(ipatchy.gt.npatchy)then
	    close(iunlocal)
	    go to 209
	  endif
	endif
	ixpatch=ixspatch+(ipatchx-1)*idxpatch
	iypatch=iyspatch+(ipatchy-1)*idypatch
c	  
c	  find the points whose real X and Y coordinates are within the bounds
c	  of the patch; expand the patch if necessary to achieve the minimum
c	  number of fiducials.  Load points from the "all" arrays into the
c	  current arrays
c
	nxp=nxpmin-40
	nyp=nypmin-40
	nrealpt=0
	minsurf=0
	do while (nxp.lt.4*xcen.and.nyp.lt.4*ycen.and.
     &	    (nrealpt.lt.minfidtot.or.
     &	    (nsurface.ge.2.and.minsurf.lt.minfidsurf)))
	  nxp=nxp+40
	  nyp=nyp+40
	  nrealpt=0
	  nbot=0
	  ntop=0
	  nprojpt=0
	  ixmin=ixpatch-nxp/2
	  ixmax=ixmin+nxp
	  iymin=iypatch-nyp/2
	  iymax=iymin+nyp
	  do i=1,nallrealpt
	    if(allxyz(1,i).ge.ixmin.and.allxyz(1,i).le.ixmax.and.
     &		allxyz(2,i).ge.iymin.and.allxyz(2,i).le.iymax)then
	      nrealpt=nrealpt+1
	      indallreal(nrealpt)=i
	      if(nsurface.ge.2)then
		if(igroup(i).eq.1)nbot=nbot+1
		if(igroup(i).eq.2)ntop=ntop+1
	      endif
	      irealstr(nrealpt)=nprojpt+1
	      do j=1,ninreal(i)
		nprojpt=nprojpt+1
		kk=J+iallrealstr(i)-1
		xx(nprojpt)=allxx(kk)
		yy(nprojpt)=allyy(kk)
		isecview(nprojpt)=iallsecv(kk)
	      enddo
	      do j=1,3
		xyz(j,nrealpt)=xyz(j,i)
	      enddo
	    endif
	  enddo
	  irealstr(nrealpt+1)=nprojpt+1
	  minsurf=min(nbot,ntop)
	enddo
	if(nxp.ge.4*xcen.and.nyp.ge.4*ycen)then
	  toofewfid=.true.
	  go to 209
	endif
c	  
c	  take care of initializing the mapped variables properly
c	  
c$$$	if(ifrotfix.eq.0)then
c$$$	  globrot=glbrot(1)
c$$$	  var(1)=globrot
c$$$	  maptiltstart=nview+1
c$$$	else
c$$$	  globrot=glbrot(ifrotfix)
c$$$	  rot(ifrotfix)=globrot
c$$$	  maptiltstart=nview
c$$$	endif
c$$$	do i=1,nview
c$$$	  if((ifrotfix.eq.0.and.i.gt.1) .or. i.lt.ifrotfix)then
c$$$	    var(i)=glbrot(i)-globrot
c$$$	  elseif(ifrotfix.gt.0 .and. i.gt.ifrotfix)then
c$$$	    var(i-1)=glbrot(i)-globrot
c$$$	  endif
c$$$	enddo
c	  
c	  reload the geometric variables
c
	nvarsrch=mapalfend
	call reload_vars(glbrot,rot,maprot,frcrot,nview,
     &	    1,maptiltstart-1,var,fixeddum,1)
	call reload_vars(glbtilt,tilt,maptilt,frctilt,nview,
     &	    maptiltstart, nvarang,var,fixeddum,incrtilt)
c	  
c	  if doing tilt incremental, just set tiltinc to the global tilt and
c	  all the equations work in map_vars
c
	if(incrtilt.ne.0)then
	  fixedtilt2=0.
	  fixedtilt=0.
	  do i=1,nview
	    tiltinc(i)=glbtilt(i)
	  enddo
	endif
	call reload_vars(glbgmag,gmag,mapgmag,frcgmag,nview,
     &	    nvarang+1,mapdmagstart-ncompsrch-1,var,fixedgmag,incrgmag)
	call reload_vars(glbdmag,dmag,mapdmag,frcdmag,nview,
     &	    mapdmagstart,nvarscl,var,fixeddmag,incrdmag)
	call reload_vars(glbskew,skew,mapskew,frcskew,nview,
     &	    nvarscl+1,mapalfstart-1,var,fixedskew,incrskew)
	call reload_vars(glbalf,alf,mapalf,frcalf,nview,
     &	    mapalfstart,nvarsrch,var,fixedalf,incralf)
c	  
c	  get new scaling and scale projection points
c
	scalexy=0.
	do i=1,nprojpt
	  scalexy=max(scalexy,abs(xx(i)),abs(yy(i)))
	enddo
	do i=1,nprojpt
	  xx(i)=xx(i)/scalexy
	  yy(i)=yy(i)/scalexy
	enddo
c	  
c	  load the xyz's and shift them to zero mean and scale them down
c	  
	xsum=0.
	ysum=0.
	zsum=0.
	do i=1,nrealpt
	  j=indallreal(i)
	  xyz(1,i)=allxyz(1,j)-xcen
	  xsum=xsum+xyz(1,i)
	  xyz(2,i)=allxyz(2,j)-ycen
	  ysum=ysum+xyz(2,i)
	  xyz(3,i)=allxyz(3,j)
	  zsum=zsum+xyz(3,i)
	enddo
	xshft=xsum/nrealpt
	yshft=ysum/nrealpt
	zshft=zsum/nrealpt
	do i=1,nrealpt
	  xyz(1,i)=(xyz(1,i)-xshft)/scalexy
	  xyz(2,i)=(xyz(2,i)-yshft)/scalexy
	  xyz(3,i)=(xyz(3,i)-zshft)/scalexy
	enddo
	write(*,'(/,a,2i3,a,2i5,a,2i5,a,i3,a)')' Doing local area',
     &	    ipatchx,ipatchy, ', centered on',ixpatch,iypatch,', size',
     &	    nxp,nyp,',  ',nrealpt,' fiducials'
	if(minsurf.gt.0)write(*,'(a,i3,a,i3,a)')'    (',nbot,
     &	    ' on bottom and',ntop,' on top)'
	ncycle=-abs(ncycle)
	go to 180
209	close(7)
	if(metroerror.ne.0)print *,'WARNING:',metroerror,
     &	    ' MINIMIZATION ERRORS OCCURRED (IER=x)'
	if (toofewfid) call errorexit(
     &	    'Minimum numbers of fiducials are too high - check if '//
     &	    'there are enough fiducials on the minority surface', 0)

	call exit(0)
	end


	subroutine errorexit(message, iflocal)
	implicit none
	integer*4 iflocal
	character*(*) message
	print *
	if (iflocal.ne.0) then
	  print *,'WARNING: ', message
	  return
	endif
	print *,'ERROR: TILTALIGN - ', message
	call exit(1)
	end
