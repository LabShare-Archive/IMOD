* * * * * * FINDWARP * * * * * *
c
c	  FINDWARP will solve for a series of general 3-dimensional linear
c	  transformations that can then be used by WARPVOL to align two
c	  volumes to each other.  It performs a series of multiple linear
c	  regression on subsets of the displacements between the volumes
c	  determined at a matrix of positions (patches).  The displacements
c	  must be contained in a file with the following form: 
c
c	  Number of displacements
c	  One line for each displacement consisting of the X, Y, and Z
c	  .  coordinates in the first volume, then the displacements in X, Y
c	  .  and Z involved in moving from the first to the second volume
c	  
c	  If the program is run interactively, it loops on the specification
c	  of the subsets of displacements to use until the user decides to
c	  write out a particular subset.  However, it can also be told to find
c	  the best warping automatically, in which case it does so then exits.
c
c	  The program will automatically eliminate "outliers", patch
c	  displacements that are likely to be incorrect because they are so
c	  extreme, when compared to the rest of the displacements.  This
c	  elimination is conservative, but if for some reason it operates
c	  incorrectly, you can control the parameters of elimination or stop
c	  the elimination from occurring.  By default, the program will
c	  eliminate up to 10% of the patches from each local fit.
c	  
c	  The program also has two options for fitting to only a subset of the 
c	  data.  One option is to eliminate whole rows or columns of patches.
c	  The other is to use a model file to  specify which patches to
c	  include in the fit.  This model can be quite simple, consisting of
c	  just a single contour enclosing the region  where patches are
c	  generally good.  This contour can be drawn in any Z plane of the
c	  flipped tomogram.  However, if the good region changes through the
c	  depth of the tomogram, you can draw contours at several Z levels.
c	  If you have two layers of patches, draw two contours, one near the
c	  top and one near the bottom of the tomogram; if you have three
c	  layers, add another contour in the middle, etc.  For a given patch,
c	  the program will find the contour at the nearest Z level and use
c	  that one to determine whether to include the patch.
c
c	  In addition, the program will work with a patch file from which
c	  bad patches have been removed by hand.  This may become necessary
c	  if bad patches are too frequent in some location to be eliminated
c	  as outliers.  To use this feature, use Patch2imod to convert the
c	  patch file to a model file where displacements are represented
c	  by vectors, examine the file in Imod, eliminate aberrant contours,
c	  and convert the model file to a new patch file with Imod2patch.
c
c	  If there is only one layer of patches in the Y dimension, there is
c	  insufficient information to solve for the full transformation, so
c	  the program will solve for only two of the three columns of each
c	  local transformation matrix, and set the second column of each
c	  matrix to 0, 1, 0.  The same procedure is used if a particular
c	  local area does not have sufficient data on more than one layer in Y.
c
c	  Inputs to the program:
c	  
c	  Name of file with positions and displacements
c
c	  (At this point the program will report the number of patches in X, Y,
c	  .  and Z in this file, or complain if the data do not have the
c	  .  proper form.)
c	  
c	  Either the file name of one of the volumes, or the X, Y, and
c	  .  Z dimensions of the volumes.
c	  
c	  Name of an Imod model file with contours enclosing the patches to
c	  .  be included in the fit, or Return to use all patches.
c
c	  0 to proceed interactively, or 1 to find best warping automatically
c	  
c	  IF you enter 1, next enter:
c	  
c	  .  The target mean residual to achieve.
c
c	  .  The minimum and maximum ratio of measurements to unknowns to be
c	  .    allowed in the local fits, or / to use the default values.
c
c	  0 to use all of the data, or 1 to specify a subset of patches to use
c	  
c	  IF you enter 1, next enter three lines:
c	  
c	  .  Number of columns of patches to eliminate from the left and right
c	  .    sides of the data, 0,0 to use all patches in X, or / to use
c	  .    previous values.
c
c	  .  Number of slabs of patches to eliminate from the bottom and top
c	  .    of the section, 0,0 to use all patches in Y, or / to use
c	  .    previous values.
c
c	  .  Number of rows of patches to eliminate from the bottom and top
c	  .    (when viewing the flipped tomogram), 0,0 to use all patches in
c	  .    Z, or / to use previous values.
c
c	  IF you selected automatic warping, the program now proceeds by
c	  .  fitting to the largest possible area in X and Z that does not
c	  .  exceed the maximum ratio of measurements to unknowns, and it
c	  .  tries progressively smaller areas until the desired mean residual
c	  .  is achieved.  It does this using the parameters (e.g., row or
c	  .  column elimination) that were set on any previous interactive
c	  .  rounds of fitting.  If the target residual is reached, it
c	  .  requests the names of the initial transformation file and the
c	  .  output file, as described below, and exits.  If the residual is
c	  .  not reached, it exits with an error.
c	  
c	  IF you are proceeding interactively, continue with the following:
c
c	  IF there are more than 2 patches in Y, the program next asks whether
c	  .  you always want to do the local fits to all patches in Y.  Just
c	  .  enter 0 for the typical situation.
c	  
c	  0 to use default parameters for outlier elimination, or 1 to adjust
c	  .  any of these parameters.  Just enter 0 unless you know better.
c	  
c	  IF you enter 1, then make four entries, or / to take the default:
c	  
c	  .  Maximum fraction of patches to eliminate from each fit.  Set this
c	  .    to 0 to stop the outlier elimination from occurring.
c	  
c	  .  The minimum residual for elimination; patches with residuals
c	  .    smaller than this value will be retained no matter how extreme
c	  .    they are relative to the other patches.
c	  
c	  .  Criterion probability for patches to be considered candidates for
c	  .    for elimination.  A smaller value will eliminate fewer patches.
c	  
c	  .  Criterion probability for patches to be eliminated regardless of
c	  .    the pattern of outliers.  A higher value may force the
c	  .    elimination of more patches.
c
c	  Number of local patches in X and Z (or X, Y, and Z) to include in
c	  .  each fit. These values must be at least 2 and no larger than the
c	  .  total number of patches in the respective direction.
c	  
c	  The program will loop on the last entry until the same numbers are
c	  entered twice in a row (e.g. with a /).  If you enter 0 for the
c	  number of patches in X, it will loop back to the query about whether
c	  you want to find warping automatically.  When you do enter a /, 
c	  it will request:
c	  
c	  Name of file with initial 3D transformation that was applied to one
c	  .  of the volumes before the patch correlation, or Return if there
c	  .  was no such file.  
c
c	  Name of output file in which to place the transformations.  These
c	  .  will be inverse transformations, ready for use by WARPVOL.
c	  
c	  An exception to the above occurs if you specify that all available
c	  patches are to be used in a single fit.  You would do this if you
c	  just needed to eliminate a row or column of patches from the fit.
c	  In this case, when you enter a /, the program will simply ask for
c	  the name of a file in which to place the single refining
c	  transformation, just as with REFINEMATCH.
c
c	  For a given arrangement of patches, the program finds a mean and
c	  maximum residual for each of the fits.  It first reports which points
c	  have been eliminated as outliers, and in how many fits they appeared
c	  to be outliers.  On one line, it next reports the average and the
c	  maximum of the mean residuals.  On the next line, it reports the
c	  average and maximum of the maximum residuals.  The goal is to find
c	  an arrangement that contains as many patches as possible in each
c	  direction yet has residuals comparable to those found with a volume
c	  that does not need warping (typically 0.1 to 0.2).
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.6  2003/10/24 03:47:17  mast
c	  fix bug of using maxdrop before setting it
c	
c	  Revision 3.5  2002/10/23 15:41:44  mast
c	  Added ability to get solutions with only one layer of patches in Y,
c	  and to drop back to a solution that is fixed in Y when there are
c	  too few points on multiple levels in Y.
c	
c	  Revision 3.4  2002/09/09 21:36:00  mast
c	  Eliminate stat_source: and nimp_source: from all includes
c	
c	  Revision 3.3  2002/09/06 00:40:18  mast
c	  Had it exit with error if no patch arrangements will fit the minimum
c	  required measurement to unknown ratio in automatic mode
c	
c	  Revision 3.2  2002/09/06 00:38:33  mast
c	  Wrong message!
c	
c	  Revision 3.1  2002/07/21 00:05:00  mast
c	  Rearranged input order so that it was easier to run from Matchorwarp
c	  with columns or rows excluded.  Standardized error output and made
c	  declarations for implicit none.
c	
c
c	  David Mastronarde, January 30, 1997
c	  12/24/98: added outlier elimination, integrated complex options.
c	  6/6/99: added ability to output single refining transformation.
c	  1/1/00: added model exclusion and automatic finding of best warp
c	  6/7/01: rewrote data input to handle data with missing patches
c
	implicit none
        include 'statsize.inc'
	include 'model.inc'
	integer idim,limpatch,limvert,limaxis
        parameter (idim=10000,limpatch=10000,limvert=100000)
	parameter (limaxis=1000)
        real*4 xr(msiz,idim)
        real*4 cx(limpatch,3),dx(limpatch,3)
	real*4 firstm(3,3),firstd(3),a(3,3),dxyz(3),devxyz(3)
	real*4 devxyzmax(3),cenloc(3),atmp(3,3),dtmp(3)
	real*4 asave(3,3,limpatch),censave(3,limpatch)
	real*4 dxyzsave(3,limpatch),xyzsum(3)
	logical solved(limpatch),exists(limpatch)
        integer*4 nxyz(3),idrop(idim)
c	integer*4 ixdrop(idim),iydrop(idim),izdrop(idim),ntimes(idim)
	integer*4 inddrop(idim),ntimes(idim)
	real*4 dropsum(idim)
	logical inside,exist,readw_or_imod
	integer getimodhead,getimodscales
	real*4 xvert(limvert),yvert(limvert),zcont(idim)
	integer*4 indvert(idim),nvert(idim)
	integer*4 nfxauto(limpatch),nfzauto(limpatch)
	integer*4 inrowx(limaxis),inrowy(limaxis),inrowz(limaxis)
        character*80 filename
	integer*4 npatxyz(3),listpos(limaxis,3),indxyz(3)
	real*4 cxyzin(3),dxyzin(3)
	equivalence (npatx,npatxyz(1)),(npaty,npatxyz(2)),
     &	    (npatz,npatxyz(3))
	integer*4 ndat,npatx,npaty,npatz,i,j,ipos,inlist,k,itmp,ind
	integer*4 ncont,ierr,indy,indz,iobj,ip,indcur,ipt
	integer*4 nxtot,nytot,nztot,nofsx,nofsy,nofsz,ifsubset,ifdoy
	real*4 xofs,yofs,zofs,ximscale,yimscale,zimscale
	real*4 ratmin,ratmax,fracdrop,crit,critabs,elimmin,targetres
	integer*4 ifauto,nauto,niny,ix,iy,iz,iauto,nfitx,nfity,nfitz
	integer*4 nlocdone,nexclxhi,nexclyhi,nexclzhi
	integer*4 ifdiddle,nfitxin,nfityin,nfitzin,nlocx,nlocy,nlocz
	integer*4 induse,nlistd,ndroptot,locx,locy,locz,lx,ly,lz,ifuse
	real*4 ratio,devavmin,dist,distmin,devavsum,devmaxsum,devmaxmax
	real*4 dzmin,dz,devmax,devavg,devsd,zscal,xyscal
	real*4 devavavg,devmaxavg,devavmax
	integer*4 icmin,icont,indv,indlc,ipntmax,maxdrop,ixd,iyd,izd
	integer*4 left,ifon,ndrop,ifflip,indpat,indloc,icolfix
	indpat(ix,iy,iz)=ix + (iy-1)*npatx + (iz-1)*npatx*npaty
	indloc(ix,iy,iz)=ix + (iy-1)*nlocx + (iz-1)*nlocx*nlocy
c
        write(*,'(1x,a,$)')
     &      'Name of file with correlation positions and results: '
        read(*,'(a)')filename
        call dopen(1,filename,'old','f')
        read(1,*)ndat
	if(ndat.gt.limpatch)call errorexit
     &	    ('TOO MANY PATCHES FOR ARRAYS')
	npatx=0
	npaty=0
	npatz=0
	do i=1,ndat
	  read(1,*)(cxyzin(j),j=1,3)
	  do j=1,3
	    ipos=nint(cxyzin(j))
	    inlist=0
	    k=1
	    do while(inlist.eq.0.and.k.le.npatxyz(j))
	      if(ipos.eq.listpos(k,j))inlist=k
	      k=k+1
	    enddo
	    if(inlist.eq.0)then
	      if(npatxyz(j).gt.limaxis)then
		write(*,'(//,a,i2)')
     &		    'ERROR: FINDWARP - TOO MANY POSITIONS ALONG AXIS',j
		call exit(1)
	      endif
	      npatxyz(j)=npatxyz(j)+1
	      listpos(npatxyz(j),j)=ipos
	    endif
	  enddo
	enddo
c	  
c	  sort the position lists
c
	do i=1,3
	  do j=1,npatxyz(i)-1
	    do k=j,npatxyz(i)
	      if(listpos(j,i).gt.listpos(k,i))then
		itmp=listpos(j,i)
		listpos(j,i)=listpos(k,i)
		listpos(k,i)=itmp
	      endif
	    enddo
	  enddo
	enddo
c
	print *,'Number of patches in X, Y and Z is:',npatx,npaty,npatz
	rewind(1)
	read(1,*)ndat
c
	print *,'Enter NX, NY, NZ of tomogram',
     &	      ', or name of either tomogram file'
	call get_nxyz(.false., ' ', 'FINDWARP', 1,nxyz)
c	  
c	  mark positions as nonexistent and fill cx from list positions
c
	do k=1,npatz
	  do j=1,npaty
	    do i=1,npatx
	      ind=indpat(i,j,k)
	      exists(ind)=.false.	      
	      cx(ind,1)=listpos(i,1)
	      cx(ind,2)=listpos(j,2)
	      cx(ind,3)=listpos(k,3)
	    enddo
	  enddo
	enddo
c	  
c	  read each line, look up positions in list and store in right place
c
	do i=1,ndat
c	    
c	    these are center coordinates and location of the second volume
c	    relative to the first volume
c
	  read(1,*)(cxyzin(j),j=1,3),(dxyzin(j),j=1,3)
	  do j=1,3
	    ipos=nint(cxyzin(j))
	    k=1
	    do while(k.le.npatxyz(j).and.ipos.gt.listpos(k,j))
	      k=k+1
	    enddo
	    indxyz(j)=k
	  enddo
	  ind=indpat(indxyz(1),indxyz(2),indxyz(3))
	  exists(ind)=.true.
	  do j=1,3
	    cx(ind,j)=cxyzin(j)
	    dx(ind,j)=dxyzin(j)
	  enddo
	enddo
c
	close(1)
c	  
        write(*,'(1x,a,/,a,$)')
     &      'Enter name of model file with contour enclosing area to '
     &	    //'use,',' or Return to use all patches: '
        read(*,'(a)')filename
	ncont=0
	if(filename.ne.' ')then
	  exist=readw_or_imod(filename)
	  if(.not.exist)call errorexit('ERROR READING MODEL')
	  ierr=getimodhead(xyscal,zscal,xofs,yofs,zofs,ifflip)
	  ierr=getimodscales(ximscale,yimscale,zimscale)
c	  print *,'Offsets:',xofs,yofs,zofs
	  do i=1,n_point
	    p_coord(1,i)=(p_coord(1,i)-xofs)/ximscale
	    p_coord(2,i)=(p_coord(2,i)-yofs)/yimscale
	    p_coord(3,i)=(p_coord(3,i)-zofs)/zimscale
	  enddo
	  indy=2
	  indz=3
	  if (ifflip.ne.0)then
	    indy=3
	    indz=2
	  endif
	  indcur=0
	  do iobj=1,max_mod_obj
	    if(npt_in_obj(iobj).ge.3)then
	      ncont=ncont+1
	      if(ncont.gt.idim)call errorexit(
     &		  'TOO MANY CONTOURS IN MODEL')
	      nvert(ncont)=npt_in_obj(iobj)
	      if(indcur+nvert(ncont).gt.limvert)call errorexit(
     &		  'TOO MANY POINTS IN CONTOURS')
	      do ip=1,nvert(ncont)
		ipt=abs(object(ibase_obj(iobj)+ip))
		xvert(ip+indcur)=p_coord(1,ipt)
		yvert(ip+indcur)=p_coord(indy,ipt)
	      enddo
	      zcont(ncont)=p_coord(indz,ipt)
	      indvert(ncont)=indcur+1
	      indcur=indcur+nvert(ncont)
	    endif
	  enddo
	  print *,ncont,
     &	      ' contours available for deciding which patches to use'
	endif
c	    
	nxtot=npatx
	nytot=npaty
	nztot=npatz
	nofsx=0
	nofsy=0
	nofsz=0
	ifsubset=0
	ifdoy=0
	nfityin=nytot
	ratmin=4.0
	ratmax=20.0
	fracdrop=0.1
	crit=0.01
	critabs=0.002
	elimmin=0.5
c	aspectmax=3.
c
8	write(*,'(1x,a,$)')'1 to find best warping automatically, 0 '//
     &	    'to proceed interactively: '
	read(5,*)ifauto
	nauto=0
	if(ifauto.ne.0)then
	  write(*,'(1x,a,$)')'Mean residual to achieve: '
	  read(5,*)targetres
	  write(*,'(1x,a,/,a,2f5.1,a,$)') 'Minimum and maximum ratio of '//
     &	      'measurements to unknowns to test','  (/ for'
     &	      ,ratmin,ratmax,'): '
	  read(5,*)ratmin,ratmax
c	  write(*,'(1x,a,f5.0,a,$)')
c     &	      'Maximum aspect ratio to allow in an area to be fit (/ for'
c     &	      ,aspectmax,'): '
c	  read(5,*)aspectmax
c	    
c	    set up parameters for automatic finding: make list of possible
c	    nxfit and nzfit values
c
	endif
c
	write(*,'(1x,a,$)')'0 to include all positions, or 1 to '//
     &	    'exclude rows or columns of patches: '
	read(5,*)ifsubset
c
	if(ifsubset.ne.0)then
10	  nexclxhi = npatx-nofsx-nxtot
	  write(*,'(1x,a,2i3,a,$)')'# of columns to exclude'
     &	      //' on the left and right in X (/ for',nofsx,
     &	      nexclxhi,'): '
	  read(5,*)nofsx,nexclxhi
	  nxtot=npatx-nofsx-nexclxhi
	  if(nxtot+nofsx.gt.npatx.or.nxtot.lt.2)then
	    if (ifauto .ne. 0) call errorexit(
     &		'ILLEGAL ENTRY FOR NUMBER OF COLUMNS TO EXCLUDE IN X')
	    print *,'Illegal entry'
	    nofsx=0
	    nxtot=npatx
	    go to 10
	  endif

12	  nexclyhi = npaty-nofsy-nytot
	  write(*,'(1x,a,2i3,a,$)')'# of slabs to exclude'
     &	      //' on the bottom and top of section in Y (/ for',nofsy,
     &	      nexclyhi,'): '
	  read(5,*)nofsy,nexclyhi
	  nytot=npaty-nofsy-nexclyhi
	  if(nytot+nofsy.gt.npaty.or.nytot.lt.1 )then
	    if (ifauto .ne. 0) call errorexit(
     &		'ILLEGAL ENTRY FOR NUMBER OF SLABS TO EXCLUDE IN Y')
	    print *,'Illegal entry'
	    nofsy=0
	    nytot=npaty
	    go to 12
	  endif

14	  nexclzhi = npatz-nofsz-nztot
	  write(*,'(1x,a,2i3,a,$)')'# of rows to exclude'
     &	      //' on the bottom and top in Z (/ for',nofsz,
     &	      nexclzhi,'): '
	  read(5,*)nofsz,nexclzhi
	  nztot=npatz-nofsz-nexclzhi
	  if(nztot+nofsz.gt.npatz.or.nztot.lt.2)then
	    if (ifauto .ne. 0) call errorexit(
     &		'ILLEGAL ENTRY FOR NUMBER OF ROWS TO EXCLUDE IN Z')
	    print *,'Illegal entry'
	    nofsz=0
	    nztot=npatz
	    go to 14
	  endif
	  print *,'Remaining # of patches in X, Y and Z is:',
     &	      nxtot,nytot,nztot
	else
	  nxtot=npatx
	  nytot=npaty
	  nztot=npatz
	  nofsx=0
	  nofsy=0
	  nofsz=0
	endif
c
	if(ifauto.ne.0)then
	  niny=nytot
	  if(ifdoy.ne.0)niny=nfity
	  do ix=nxtot,2,-1
	    do iz=nztot,2,-1
	      ratio=ix*iz*niny/4.0
c	      aspect = float(ix)/iz
c	      if(aspect.lt.1.)aspect=1./aspect
	      if((ix.ne.nxtot.or.iz.ne.nztot).and.ratio.ge.ratmin.and.
     &		  ratio.le.ratmax)then
c     &		  aspect.le.aspectmax)then
		nauto=nauto+1
		nfxauto(nauto)=ix
		nfzauto(nauto)=iz
	      endif
	    enddo
	  enddo
	  if(nauto.eq.0)then
	    print *
	    print *,'ERROR: FINDWARP - NO FITTING PARAMETERS GIVE THE',
     &		' REQUIRED RATIO OF',' MEASUREMENTS TO UNKNOWNS - ',
     &		'THERE ARE PROBABLY TOO FEW PATCHES'
	    call exit(1)
	  endif
c	    
c	    sort the list by size of area in inverted order
c	    
	  do i=1,nauto-1
	    do j=i+1,nauto
	      if(nfxauto(i)*nfzauto(i).lt.nfxauto(j)*nfzauto(j))then
		itmp=nfxauto(i)
		nfxauto(i)=nfxauto(j)
		nfxauto(j)=itmp
		itmp=nfzauto(i)
		nfzauto(i)=nfzauto(j)
		nfzauto(j)=itmp
	      endif
	    enddo
	  enddo
c	  write(*,'(3i5)')(i,nfxauto(i),nfzauto(i),i=1,nauto)
	  iauto = 1
	  nfitx=-100
	  nfity=-100
	  nfitz=-100
	  nlocdone=0
	  devavmin=10000.
	  go to 20
	endif

	ifdoy=1
	if(nytot.le.2)ifdoy=0
	if(ifdoy.gt.0)then
	  write(*,'(1x,a,$)')'0 to fit to all patches in Y, or 1 to '
     &	      //'fit to subsets in Y: '
	  read(5,*)ifdoy
	endif
c
	write(*,'(1x,a,$)')'1 to enter parameters to control outlier '
     &	    //'elimination, 0 not to: '
	read(5,*)ifdiddle
	if(ifdiddle.ne.0)then
	  write(*,'(1x,a,f5.2,a,$)')'Maximum fraction of patches to '//
     &	      'eliminate (/ for',fracdrop,'): '
	  read(5,*)fracdrop
	  write(*,'(1x,a,f5.2,a,$)')'Minimum residual needed to do any'
     &	      //' elimination (/ for',elimmin,'): '
	  read(5,*)elimmin
	  write(*,'(1x,a,f6.3,a,$)')'Criterion probability for ' //
     &	      'candidates for elimination (/ for',crit,'): '
	  read(5,*)crit
	  write(*,'(1x,a,f6.3,a,$)')'Criterion probability for enforced'
     &	      //' elimination (/ for',critabs,'): '
	  read(5,*)critabs
	endif
c
	nfitx=-100
	nfity=-100
	nfitz=-100
	nlocdone=0
	write(*,111)
111	format(/,' Enter 0 for the number of patches in X to loop',
     &	    ' back and find best fit ',/,
     &	    '  automatically, include a different subset of patches ',
     &	    /,'  or specify new outlier elimination parameters',/)
20	if(ifdoy.ne.0.and.ifauto.eq.0)then
	  if(nlocdone.eq.0)then
	    write(*,'(1x,a,$)')
     &		'Number of local patches for fit in X, Y and Z: '
	  else
	    write(*,'(1x,a,/,a,$)')
     &		'Number of local patches for fit in X, Y and Z,',
     &		'    or / to redo and save last result: '
	  endif
	  read(5,*)nfitxin,nfityin,nfitzin
	elseif(ifauto.eq.0)then
	  if(nlocdone.eq.0)then
	    write(*,'(1x,a,$)')
     &		'Number of local patches for fit in X and Z: '
	  else
	    write(*,'(1x,a,/,a,$)')
     &		'Number of local patches for fit in X and Z,',
     &		'    or / to redo and save last result: '
	  endif
	  read(5,*)nfitxin,nfitzin
	  nfityin=nytot
	else
	  nfitxin=nfxauto(iauto)
	  nfitzin=nfzauto(iauto)
	endif
c
	if(nfitxin.eq.0)go to 8
	if(nfitxin.ne.nfitx.or.nfityin.ne.nfity.or.nfitzin.ne.nfitz
     &	    .or.nlocdone.eq.0)then
	  nlocx=nxtot+1-nfitxin
	  nlocy=nytot+1-nfityin
	  nlocz=nztot+1-nfitzin
	  if(nfitxin.lt.2.or.nfitzin.lt.2.or.nlocx.lt.1.or.nlocz.lt.1
     &	      .or.nfityin.lt.1.or.nlocy.lt.1)then
	    if (ifauto .ne. 0) call errorexit(
     &		'IMPROPER NUMBER TO INCLUDE IN FIT')
	    print *,'Illegal entry, try again'
	    go to 20
	  endif
	  if(nfitxin*nfityin*nfitzin.gt.idim)then
	    if (ifauto .ne. 0) call errorexit(
     &		'TOO MANY PATCHES FOR ARRAY SIZES')
	    print *,'Too many patches for array sizes, try again'
	    go to 20
	  endif
	  nfitx=nfitxin
	  nfity=nfityin
	  nfitz=nfitzin
	else
	  if(nlocx.gt.1.or.nlocy.gt.1.or.nlocz.gt.1)then
	    if(ifauto.ne.0)write(*,*)
	    print *,'Enter name of file with initial '//
     &		'transformation, typically solve.xf',
     &		'   (Return if none)'
	    read(*,'(a)')filename
	    if(filename.ne.' ')then
	      call dopen(1,filename,'old','f')
	      read(1,*)((firstm(i,j),j=1,3),firstd(i),i=1,3)
	      close(1)
	    else
	      do i=1,3
		do j=1,3
		  firstm(i,j)=0.
		enddo
		firstm(i,i)=1.
		firstd(i)=0.
	      enddo
	    endif
c	    
	    print *,'Enter name of file to place warping ',
     &		'transformations in'
	    read(5,'(a)')filename
	    call dopen(1,filename,'new','f')
	    if(nlocy.eq.1)then
	      write(1,'(2i4)')nlocx,nlocz
	    else
	      write(1,'(3i4)')nlocx,nlocy,nlocz
	    endif
	    do locz=1,nlocz
	      do locy=1,nlocy
		do locx=1,nlocx
		  ind=indloc(locx,locy,locz)
		  induse=ind
c
c		    if this location was not solved, find nearest one that was
c		    
		  if(.not.solved(ind))then
		    distmin=1.e20
		    do i=1,nlocx*nlocy*nlocz
		      if(solved(i))then
			dist=(censave(1,i)-censave(1,ind))**2+
     &			    (censave(2,i)-censave(2,ind))**2+
     &			    (censave(3,i)-censave(3,ind))**2
			if(dist.lt.distmin)then
			  distmin=dist
			  induse=i
			endif
		      endif
		    enddo
c		    write(*,'(5i4,6f8.1,3f8.1)')locx,locy,locz,ind,induse,
c     &			(censave(j,ind),censave(j,induse),j=1,3)
		  endif
		  call xfmult3d(firstm,firstd,asave(1,1,induse),
     &		      dxyzsave(1,induse),atmp,dtmp)
		  call xfinv3d(atmp,dtmp,a,dxyz)
		  write(1,103)(censave(i,ind),i=1,3)
103		  format(3f9.1)
		  write(1,102)((a(i,j),j=1,3),dxyz(i),i=1,3)
102		  format(3f10.6,f10.3)
		enddo
	      enddo
	    enddo
	  else
	    print *,'Enter name of file in which to place single ',
     &		'refining transformation'
	    read(5,'(a)')filename
	    call dopen(1,filename,'new','f')
	    write(1,102)((a(i,j),j=1,3),dxyz(i),i=1,3)
	  endif
	  close(1)
	  call exit(0)
	endif
	devavsum=0.
	devmaxsum=0.
	devmaxmax=0.
	devavmax=0.
	nlistd=0
	ndroptot=0
	nlocdone=0
	if (max(nfitx,nfity,nfitz).gt.limaxis) call errorexit(
     &	    'TOO MANY POINTS IN FIT IN ONE DIMENSION FOR ARRAYS')

	do locz=1,nlocz
	  do locy=1,nlocy
	    do locx=1,nlocx
	      ndat=0
	      do i=1,3
		xyzsum(i)=0.
	      enddo
c		
c		count up number in each row in each dimension
c		
	      do i=1,max(nfitx,nfity,nfitz)
		inrowx(i)=0
		inrowy(i)=0
		inrowz(i)=0
	      enddo
	      do lz=locz+nofsz,locz+nofsz+nfitz-1
		do ly=locy+nofsy,locy+nofsy+nfity-1
		  do lx=locx+nofsx,locx+nofsx+nfitx-1
		    ifuse=0
		    ind=indpat(lx,ly,lz)
		    if(exists(ind))ifuse=1
		    do i=1,3
		      xyzsum(i)=xyzsum(i)+cx(ind,i)-0.5*nxyz(i)
		    enddo
		    if(ncont.gt.0.and.ifuse.gt.0)then
		      ifuse=0
c	      
c			find nearest contour in Z and see if patch is inside it
c
		      dzmin=100000.
		      do icont=1,ncont
			dz=abs(cx(ind,2)-zcont(icont))
			if(dz.lt.dzmin)then
			  dzmin=dz
			  icmin=icont
			endif
		      enddo
		      indv=indvert(icmin)
		      if(inside(xvert(indv),yvert(indv),nvert(icmin),
     &			  cx(ind,1),cx(ind,3))) ifuse=1
		    endif
c
		    if(ifuse.gt.0)then
		      ndat=ndat+1
		      do j=1,3
c	      
c	      the regression requires coordinates of second volume as
c	      independent variables (columns 1-3), those in first volume
c	      as dependent variables (stored in 5-7), to obtain transformation
c	      to get from second to first volume
c	      cx+dx in second volume matches cx in first volume
c
			xr(j+4,ndat)=cx(ind,j)-0.5*nxyz(j)
			xr(j,ndat)=xr(j+4,ndat)+dx(ind,j)
		      enddo
c			
c			Solve_wo_outliers uses columns 8-14; save indexi in 15
c
		      xr(15,ndat)=ind
		      inrowx(lx+1-locx-nofsx)=inrowx(lx+1-locx-nofsx)+1
		      inrowy(ly+1-locy-nofsy)=inrowy(ly+1-locy-nofsy)+1
		      inrowz(lz+1-locz-nofsz)=inrowz(lz+1-locz-nofsz)+1
		    endif
		  enddo
		enddo
	      enddo
	      indlc=indloc(locx,locy,locz)
c 
c		Need regular array of positions, so use the xyzsum to get
c		censave, not the cenloc values from the regression
c
	      do i=1,3
		censave(i,indlc)=xyzsum(i)/(nfitx*nfity*nfitz)
	      enddo
c		
c		solve for this location if there are at least half of the
c		normal number of patches present and if there are guaranteed
c		to be at least 3 patches in a different row from the dominant
c		one, even if the max are dropped from other rows
c		But treat Y differently: if there are not enough data
c		on another layer in Y, or if there is only one layer being fit,
c		then set the second column as fixed in the fits
c
	      solved(indlc)=ndat.ge.nfitx*nfity*nfitz/2
	      maxdrop=nint(fracdrop*ndat)
	      icolfix = 0
	      do i=1,max(nfitx,nfity,nfitz)
		if(inrowx(i).gt.ndat-3-maxdrop.or.
     &		    inrowz(i).gt.ndat-3-maxdrop)solved(indlc)=.false.
		if (inrowy(i).gt.ndat-3-maxdrop .or. nfity.eq.1)icolfix=2
	      enddo
	      if(solved(indlc))then
		call solve_wo_outliers(xr,ndat,3,icolfix,maxdrop,crit,
     &		    critabs, elimmin,idrop, ndrop, a,dxyz, cenloc,
     &		    devavg,devsd, devmax, ipntmax,devxyzmax)
c
c$$$		if (dxyz(1).gt.20.)then
c$$$		  do i=1,ndat
c$$$		    write(*,'(8f9.2)')(xr(j,i),j=1,7)
c$$$		  enddo
c$$$		  print *,(cenloc(i),i=1,3)
c$$$		  write(*,'(9f8.3)')((a(i,j),i=1,3),j=1,3)
c$$$		endif
		do i=1,ndrop
		  izd=(idrop(i)-1)/(nfitx*nfity)
		  left=idrop(i)-1-izd*nfitx*nfity
		  iyd=left/nfitx
		  ixd=left-iyd*nfitx
		  ixd=ixd+locx+nofsx
		  iyd=iyd+locy+nofsy
		  izd=izd+locz+nofsz
		  ifon=0
		  do j=1,nlistd
		    if(nint(xr(15,idrop(i))).eq.inddrop(j))then
c		    if(ixd.eq.ixdrop(j).and.iyd.eq.iydrop(j).and.
c     &			izd.eq.izdrop(j))then
		      ifon=1
		      ntimes(j)=ntimes(j)+1
		      dropsum(j)=dropsum(j)+xr(4,ndat+i-ndrop)
		    endif
		  enddo
		  if(ifon.eq.0.and.ifon.lt.idim)then
		    nlistd=nlistd+1
c		    ixdrop(nlistd)=ixd
c		    iydrop(nlistd)=iyd
c		    izdrop(nlistd)=izd
		    inddrop(nlistd)=nint(xr(15,idrop(i)))
		    ntimes(nlistd)=1
		    dropsum(nlistd)=xr(4,ndat+i-ndrop)
		  endif
		enddo
		ndroptot=ndroptot+ndrop
c		  
		devavsum=devavsum+devavg
		devmaxsum=devmaxsum+devmax
		devavmax=max(devavmax,devavg)
		devmaxmax=max(devmaxmax,devmax)
c
c		  mark this location as solved and save the solution. 
c
c		write(*,'(6i4,3f8.1)')indlc,locx,locy,locz,ndat,ndrop,(dxyz(i),i=1,3)
		do i=1,3
		  dxyzsave(i,indlc)=dxyz(i)
		  do j=1,3
		    asave(i,j,indlc)=a(i,j)
		  enddo
		enddo
		nlocdone=nlocdone+1
	      endif
	    enddo
	  enddo
	enddo
c	  
c	  check for auto control
c	  
	if(ifauto.ne.0)then
	  devavavg=10000.
	  if(nlocdone.gt.0)devavavg=devavsum/nlocdone
	  devavmin=min(devavmin,devavavg)
	  if(devavavg.le.targetres)then
c	      
c	      done: set nauto to zero to allow printing of results
c
	    nauto=0
	    write(*,107)nfitx,nfity,nfitz
107	    format(/,'Desired residual achieved with fits to',i3,',',i3,
     &		', and',i3,' patches in X, Y, Z',/)
	  else
	    iauto=iauto+1
c	    print *,iauto,' Did fit to',nfitx,nfity,nfitz
	    if(iauto.gt.nauto)then
	      write(*,108)devavmin
108	      format(//,'FINDWARP: Failed to find a warping with a ',
     &		  'mean residual less than',f9.3,/)
	      call exit(2)
	    endif
	  endif
	endif

	if(nlistd.gt.0.and.nauto.eq.0)then
	  write(*,105)nlistd,ndroptot
105	  format(i4,' separate patches eliminated as outliers a total'
     &	      ,' of',i5,' times',/,
     &	      ' patch position   # of times   mean residual')
	  do i=1,nlistd
c	    ixd=ixdrop(i)
c	    iyd=iydrop(i)
c	    izd=izdrop(i)
	    write(*,106)(cx(inddrop(i),j),j=1,3),ntimes(i),
     &		dropsum(i)/ntimes(i)
106	    format(3f6.0,i8,f12.2)
	  enddo
	  write(*,*)
	endif
c	  
	if(nlocdone.gt.0.and.nauto.eq.0)then
	  devavavg=devavsum/nlocdone
	  devmaxavg=devmaxsum/nlocdone
	  write(*,101)devavavg,devavmax,devmaxavg,devmaxmax
101	  format('Mean residual has an average of',f8.3,
     &	      ' and a maximum of',f8.3,/ ,
     &	      'Max  residual has an average of',f8.3,' and a maximum of'
     &	      ,f8.3)
	elseif(ifauto.eq.0)then
	  print *,'No locations could be solved for'
	endif
	go to 20
        end


	subroutine errorexit(message)
	character*(*) message
	print *
	print *,'ERROR: FINDWARP - ',message
	call exit(1)
	end
