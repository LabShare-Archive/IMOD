*	  * * * * * CORRSEARCH3D * * * * *
c	  
c	  CORRSEARCH3D will determine the 3D displacement between two image
c	  volumes at a regular array of positions.  At each position, it
c	  extracts a patch of a specified size from each volume, then
c	  searches for a local maximum in the cross-correlation between the
c	  two patches.  The starting point of the search is based upon the
c	  displacements of adjacent patches that have already been determined.
c	  If there are no such adjacent patches, or if no maximum is found for
c	  displacements within a specified range, the program runs the script
c	  onepatchcorr to obtain a displacement by FFT-based cross-
c	  correlation.  The program works from the center of the volume
c	  outward, analyzing patches in the X, then the Y, then the Z
c	  direction.  An optional model file can be entered with contours
c	  enclosing the area where patches should be analyzed; patches outside
c	  this area are excluded.  Also, information about the source volume
c	  for the file being transformed can be entered so that the program
c	  can analyze patches only from regions that are good in the second
c	  image file.
c	  
c	  Inputs to the program:
c	  
c	  Name of reference image file being aligned to
c	  
c	  Name of image file that will be transformed to match
c	  
c	  Name of output file for patch positions and displacements
c	  
c	  Name of a temporary file for output from onepatchcorr
c	  
c	  Name of a model file with contours that enclose the areas where
c	  patches should be analyzed, or Return to analyze patches at all
c	  locations
c	  
c	  Size of patches in X, Y and Z
c	  
c	  Number of patches in the X, Y and Z directions
c	  
c	  Size of borders to be excluded on the lower and upper sides in X,
c	  the lower and upper sides in Y, and the lower and upper sides in Z
c	  
c	  Number of pixels over which to taper the patches in X, Y and Z
c	  
c	  Maximum displacement to determine by searching
c	  
c	  Either the X, Y, and Z size or the name of the original source
c	  volume for the second image file.  Return to omit analysis of
c	  positions in this file.
c	  
c	  Name of the 3-D transformation file used to generate the second
c	  image file from its source volume, or Return to omit analysis of
c	  positions in the second file.
c	  
c	  Size of borders to be excluded on the lower and upper sides in X
c	  and the lower and upper sides in Z in the original source volume
c	  for the second image file.
c	  
c	  The program outputs an initial line with the total number of
c	  patches, then one line for each patch, containing the X, Y, and Z
c	  coordinates of the patch and the displacement between the two files
c	  in X, Y, and Z.
c
c	  The program runs onepatchcorr by telling tcsh to run the script in
c	  $IMOD_DIR/bin; therefore IMOD_DIR must be defined.  If tcsh is not
c	  available or you wish to run with a different shell, define the
c	  environment variable IMOD_CSHELL with the name of the alternate C
c	  shell, including any desired flags (the -f flag is added 
c	  automatically)
c
c	  David Mastronarde, 7/16/01
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.8  2003/12/24 19:05:08  mast
c	  Changed to fit new form of get_nxyz
c	
c	  Revision 3.7  2003/10/24 17:40:17  mast
c	  Removed -e flag from tcsh command
c	
c	  Revision 3.6  2003/10/24 03:48:13  mast
c	  Use IMOD_DIR and IMOD_CSHELL to run onepatchcorr explicitly for 
c	  Windows
c	
c	  Revision 3.5  2002/09/06 00:41:07  mast
c	  Needed to prevent negative spanning distances when transformed
c	  corners are very close to each other
c	
c	  Revision 3.4  2002/07/26 19:20:05  mast
c	  Was taking min of an int and a real - Intel compiler caught it.
c	
c	  Revision 3.3  2002/07/21 19:44:11  mast
c	  Added declaration of lnblnk
c	
c	  Revision 3.2  2002/07/20 23:56:52  mast
c	  Added analysis of regions to exclude based on their positions in the
c	  source file for the second volume.  Standardized error outputs and
c	  added declarations for implicit none.
c	
c	  
	implicit none
	include 'model.inc'
	integer idim,limvert,limcont,limpat,nadjlook
	parameter (idim=16000000)
	parameter (limvert=100000,limcont=1000,limpat=10000)
	parameter (nadjlook=6)
	integer*4 nx,ny,nz
	COMMON //NX,NY,NZ
C
	integer*4 NXYZ(3),MXYZ(3),nxyzbsrc(3)
	real*4 buf(idim)
C
	EQUIVALENCE (NX,NXYZ)
c
	logical inside,exist,readw_or_imod,found
	integer getimodhead,getimodscales
	real*4 xvert(limvert),yvert(limvert),zcont(limcont)
	integer*4 indvert(limcont),nvert(limcont)
	character*80 filea,fileb,filout,modelfile,tempfile,xffile
	real*4 dxpat(limpat),dypat(limpat),dzpat(limpat)
	integer*2 ifdone(limpat)
	character*160 imodpath, imodshell
	character*320 patchcom
	character*13 onepatchname/'onepatchcorr '/
	integer*4 idxadj(nadjlook)/-1,1,0,0,0,0/
	integer*4 idyadj(nadjlook)/0,0,-1,1,0,0/
	integer*4 idzadj(nadjlook)/0,0,0,0,-1,1/
	real*4 xvertbsrc(4),yvertbsrc(4)
c
	integer*4 indpat,ix,iy,iz,nxpatch,nypatch,nzpatch,maxshift
	integer*4 ifdebug,nonepatch,ncorrs,mode,numxpat,numypat,numzpat
	integer*4 nbxlo,nbxhi,nbylo,nbyhi,nbzlo,nbzhi,nxtap,nytap,nztap
	integer*4 ixstart,iystart,izstart,ixdelta,iydelta,izdelta
	integer*4 nbsrcxlo,nbsrcxhi,nbsrczlo,nbsrczhi,indv
	real*4 dmin,dmax,dmean,dum,a11,a12,a21,a22,dxsrc,dzsrc,xx,zz
	real*4 tmp,xyscal,zscal,xofs,yofs,zofs,ximscale,yimscale,zimscale
	integer*4 i,j,ixlo2,ixhi2,izlo2,izhi2,numx2,numz2,newxdelta
	integer*4 newzdelta,nxpad,nypad,nzpad,npixpatch,indpatcha
	integer*4 indpatchb,indloada,indloadb,maxload,maxxload,ncont
	integer*4 ierr,ifflip,indy,indz,indcur,iobj,ip,ipt,numtot
	real*4 xcen,ycen,zcen,xpatlo,xpathi,zpatlo,zpathi,dz,dzmin
	integer*4 indp,ifuse,icont,icmin,ixspan,izspan
	integer*4 izpstr,izpend,izdir,izpat,iz0,iz1,izcen
	integer*4 iypstr,iypend,iydir,iypat,iy0,iy1,iycen,ixpstr,ixpend
	integer*4 ixdir,ixpat,ix0,ix1,ixcen,nadj,ixadj,iyadj,izadj,inda
	integer*4 loady0,loady1,loadz0,loadz1,loadx0,loadx1,nmore
	integer*4 nxload,nyload,nzload
	real*4 dxsum,dysum,dzsum,err,perpos,dxadj,dyadj,dzadj
	integer*4 lnblnk,imodGetenv

	indpat(ix,iy,iz)=ix + (iy-1)*numxpat + (iz-1)*numxpat*numypat
c
c	  set IFDEBUG 1 to do both search and onepatchcorr at each postion and
c	  get an error output
c	  
c	  set IFDEBUG 2 to get dummy patch output for all positions
c
	ifdebug=0
	nonepatch=0
	ncorrs=0
C
C	Open image files.
C
	write(*,'(1x,a,$)')'Image file to align to: '
	read(5,50)filea
	write(*,'(1x,a,$)')'Image file being aligned: '
        read(5,50)fileb
	write(*,'(1x,a,$)')'Output file for displacements: '
	read(5,50)filout
	write(*,'(1x,a,$)')'Temporary file for onepatchcorr output: '
	read(5,50)tempfile
	print *,'Enter model file with contours enclosing areas to ',
     &	    'analyze, or Return for none'
	read(5,50)modelfile
50	format(A)
c
	CALL IMOPEN(1,filea,'RO')
	CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C
	CALL IMOPEN(2,fileb,'RO')
	CALL IRDHDR(2,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C
	write(*,'(1x,a,$)')'X, Y, and Z size of patches: '
	read(5,*)nxpatch,nypatch,nzpatch
	write(*,'(1x,a,$)')'Number of patches in X, Y and Z: '
	read(5,*)numxpat, numypat,numzpat
	write(*,'(1x,a,$)')'Border sizes on lower and upper sides in X,'
     &	    //' Y, and Z: '
	read(5,*)nbxlo,nbxhi,nbylo,nbyhi,nbzlo,nbzhi
	write(*,'(1x,a,$)')'Number of pixels to taper in X, Y, Z: '
	read(5,*)nxtap,nytap,nztap
	write(*,'(1x,a,$)')'Maximum shift to analyze for by searching: '
	read(5,*)maxshift
c
c	  get inputs for analyzing existence of data in B file
c
	nxyzbsrc(1)=0
	print *,'Enter name or NX, NY, NZ of the untransformed source',
     &	    ' for the image file being aligned, or Return for none'
	call get_nxyz(.false., ' ', ' ', 5,nxyzbsrc)
	print *,'Enter name of file used to transform the image file',
     &	    ' being aligned, or Return for none'
	read(5,50)xffile
	write(*,'(1x,a,/,a,$)')'Border sizes on lower and upper sides'
     &	    //' in X and in Z in the untransformed',
     &	    ' source for the image file being aligned: '
	read(5,*)nbsrcxlo,nbsrcxhi,nbsrczlo,nbsrczhi
c	  
c	  get patch starting positions and delta between patches
c
	call checkAndSetPatches(nx, nbxlo, nbxhi, nxpatch, numxpat,
     &	    ixstart, ixdelta, 'X AXIS')
	call checkAndSetPatches(ny, nbylo, nbyhi, nypatch, numypat,
     &	    iystart, iydelta, 'Y AXIS')
	call checkAndSetPatches(nz, nbzlo, nbzhi, nzpatch, numzpat,
     &	    izstart, izdelta, 'Z AXIS')
c	  
c	  compute transformed locations of corners of b source volume
c	  
	if (nxyzbsrc(1) .gt. 0 .and. xffile .ne. ' ') then
	  call dopen(1, xffile, 'ro', 'f')
	  read(1,*)a11,dum,a12,dxsrc
	  read(1,*)dum
	  read(1,*)a21,dum,a22,dzsrc
	  close(1)
	  xx = -nxyzbsrc(1) / 2 + nbsrcxlo
	  zz = -nxyzbsrc(3) / 2 + nbsrczlo
	  xvertbsrc(1) = a11 * xx + a12 * zz + nx / 2. + dxsrc
	  yvertbsrc(1) = a21 * xx + a22 * zz + nz / 2. + dzsrc
	  xx = nxyzbsrc(1) / 2 - nbsrcxhi
	  xvertbsrc(2) = a11 * xx + a12 * zz + nx / 2. + dxsrc
	  yvertbsrc(2) = a21 * xx + a22 * zz + nz / 2. + dzsrc
	  zz = nxyzbsrc(3) / 2 - nbsrczhi
	  xvertbsrc(3) = a11 * xx + a12 * zz + nx / 2. + dxsrc
	  yvertbsrc(3) = a21 * xx + a22 * zz + nz / 2. + dzsrc
	  xx = -nxyzbsrc(1) / 2 + nbsrcxlo
	  xvertbsrc(4) = a11 * xx + a12 * zz + nx / 2. + dxsrc
	  yvertbsrc(4) = a21 * xx + a22 * zz + nz / 2. + dzsrc
c	    
c	    now order the coordinates to find middle values that can be
c	    used to adjust the entered lower and upper limits, so that the
c	    basic grid can be set up to span between those limits
c	    
	  do i=1,4
	    xvert(i) = xvertbsrc(i)
	    yvert(i) = yvertbsrc(i)
	  enddo
	  do i=1,3
	    do j=i+1,4
	      if (xvert(i) .gt. xvert(j)) then
		tmp = xvert(i)
		xvert(i) = xvert(j)
		xvert(j) = tmp
	      endif
	      if (yvert(i) .gt. yvert(j)) then
		tmp = yvert(i)
		yvert(i) = yvert(j)
		yvert(j) = tmp
	      endif
	    enddo
	  enddo
c
c	    compute revised lower and upper limits for X
c	    
	  ixlo2 = max(nbxlo, nint(xvert(2)))
	  ixhi2 = min(nx - nbxhi, nint(xvert(3)))
	  if (numxpat .gt. 1) then
c	      
c	      get new number of intervals inside the limits, and a new delta
c	      to span the limits
c	      
	    ixspan = max(0, ixhi2 - ixlo2 - nxpatch)
	    numx2 = ixspan / ixdelta + 1
	    newxdelta = ixspan / numx2
	    if (newxdelta .lt. 0.6 * ixdelta) then
c		
c		but if delta is too small, drop the number of intervals
c
	      numx2=numx2-1
	      if (numx2 .gt. 0) then
		newxdelta = ixspan / numx2
	      else
		newxdelta = ixdelta
	      endif
	    endif
c	      
c	      now adjust ixlo2 up by half of remainder to center the patches
c	      in the span and find true start and end that fits inside the
c	      original low-hi limits
c	      
	    ixlo2 = ixlo2 + mod(ixspan, newxdelta) / 2
	    ixdelta = newxdelta
	    ixstart = ixlo2 - ixdelta * ((ixlo2 - nbxlo) / ixdelta)
	    numxpat = (nx - nbxhi - nxpatch - ixstart) / ixdelta + 1
	  else
c	      
c	      if only one patch, put it in new middle
c
	    ixstart=(ixlo2+ixhi2)/2 - nxpatch/2
	  endif
c
c	    compute revised lower and upper limits for Z
c	    
	  izlo2 = max(nbzlo, nint(yvert(2)))
	  izhi2 = min(nz - nbzhi, nint(yvert(3)))
	  if (numzpat .gt. 1) then
c
	    izspan = max(0, izhi2 - izlo2 - nzpatch)
	    numz2 = izspan / izdelta + 1
	    newzdelta = izspan / numz2
	    if (newzdelta .lt. 0.6 * izdelta) then
c
	      numz2=numz2-1
	      if (numz2 .gt. 0) then
		newzdelta = izspan / numz2
	      else
		newzdelta = izdelta
	      endif
	    endif
c
	    izlo2 = izlo2 + mod(izspan, newzdelta) / 2
	    izdelta = newzdelta
	    izstart = izlo2 - izdelta * ((izlo2 - nbzlo) / izdelta)
	    numzpat = (nz - nbzhi - nzpatch - izstart) / izdelta + 1
	  else
	    izstart=(izlo2+izhi2)/2 - nzpatch/2
	  endif
	else
	  nxyzbsrc(1) = 0
	endif
c	  
	if (ifdebug.eq.2) then
	  print *,'Scan limits in X:',ixstart,ixstart+(numxpat-1)*ixdelta
     &	      +nxpatch
	  print *,'Scan limits in Z:',izstart,izstart+(numzpat-1)*izdelta
     &	      +nzpatch
	endif
c	  
c	  set indexes at which to load data and compose patches
c	  
	nxpad=nxpatch+2*(maxshift+1)
	nypad=nypatch+2*(maxshift+1)
	nzpad=nzpatch+2*(maxshift+1)
	npixpatch=nxpatch*nypatch*nzpatch
	indpatcha=1
	indpatchb=indpatcha+npixpatch
	indloada=indpatchb+nxpad*nypad*nzpad
	if(indloada.gt.idim+1)call errorexit(
     &	    'PATCHES TOO LARGE FOR ARRAYS')
c	  
c	  get maximum load size: if less than size of one patch, set up to
c	  load into patch space directly
c
	maxload=(idim+1-indloada)/2
	if(maxload.lt.npixpatch)then
	  maxload=npixpatch
	  indloada=1
	endif
	indloadb=indloada+maxload
	maxxload=maxload/(nypatch*nzpatch)
c	print *,npixpatch,indpatcha,indpatchb,indloada,indloadb,maxload,
c     &	    maxxload
c	  
c	  process model if one exists
c
	ncont=0
	if(modelfile.ne.' ')then
	  exist=readw_or_imod(modelfile)
	  if(.not.exist)call errorexit(
     &	      'ERROR READING MODEL')
	  ierr=getimodhead(xyscal,zscal,xofs,yofs,zofs,ifflip)
	  ierr=getimodscales(ximscale,yimscale,zimscale)
c	  print *,'Offsets:',xofs,yofs,zofs
	  do i=1,n_point
	    p_coord(1,i)=(p_coord(1,i)-xofs) / ximscale
	    p_coord(2,i)=(p_coord(2,i)-yofs) / yimscale
	    p_coord(3,i)=(p_coord(3,i)-zofs) / zimscale
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
	      if(ncont.gt.limcont)call errorexit(
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
     &	      ' contours available for deciding which patches to analyze'
	endif
c
	if(numxpat*numypat*numzpat.gt.limpat)call errorexit(
     &	    'TOO MANY PATCHES FOR ARRAYS')
c	  
c	  prescan for patches inside boundaries, to get total count
c	  
	numtot=0
	do iz=1,numzpat
	  zcen=izstart+(iz-1)*izdelta+nzpatch/2
	  do iy=1,numypat
	    ycen=iystart+(iy-1)*iydelta+nypatch/2
	    do ix=1,numxpat
	      indp=indpat(ix,iy,iz)
	      xcen=ixstart+(ix-1)*ixdelta+nxpatch/2
	      ifuse=1
	      if(ncont.gt.0)then
		ifuse=0
c		  
c		  find nearest contour in Z and see if patch is inside it
c		  
		dzmin=100000.
		do icont=1,ncont
		  dz=abs(ycen-zcont(icont))
		  if(dz.lt.dzmin)then
		    dzmin=dz
		    icmin=icont
		  endif
		enddo
		indv=indvert(icmin)
		if(inside(xvert(indv),yvert(indv),nvert(icmin),xcen,zcen))
     &		    ifuse=1
	      endif
	      if(ifuse .eq. 1 .and. nxyzbsrc(1) .gt. 0)then
		ifuse=0
c		  
c		  if b source volume was given, make sure all corners of
c		  the patch are inside transformed area
c		  
		xpatlo = xcen - (nxpatch - nxtap) / 2
		xpathi = xcen + (nxpatch - nxtap) / 2
		zpatlo = zcen - (nzpatch - nztap) / 2
		zpathi = zcen + (nzpatch - nztap) / 2
		if (inside(xvertbsrc,yvertbsrc,4,xpatlo,zpatlo) .and.
     &		    inside(xvertbsrc,yvertbsrc,4,xpatlo,zpathi) .and.
     &		    inside(xvertbsrc,yvertbsrc,4,xpathi,zpatlo) .and.
     &		    inside(xvertbsrc,yvertbsrc,4,xpathi,zpathi)) ifuse = 1
	      endif
	      if(ifuse.eq.0)then
		ifdone(indp)=-1
	      else
		ifdone(indp)=0
		numtot=numtot+1
	      endif
	    enddo
	  enddo
	enddo
c	  
	if (numtot .lt. 1) call errorexit(
     &	    'NO PATCHES FIT WITHIN ALL OF THE CONSTRAINTS')
	call dopen(1,filout,'new','f')
	write(1,'(i7,a)')numtot,' positions'
c	  
c	  loop from center out in all directions, X inner, then Y, then Z
c	  
	loadx1=-1
	izpstr=(numzpat+1)/2
	izpend=numzpat
	do izdir=1,-1,-2
	  do izpat=izpstr,izpend,izdir
	    iz0=izstart+(izpat-1)*izdelta
	    iz1=iz0+nzpatch-1
	    izcen=iz0+nzpatch/2
	    iypstr=(numypat+1)/2
	    iypend=numypat
	    do iydir=1,-1,-2
	      do iypat=iypstr,iypend,iydir
		iy0=iystart+(iypat-1)*iydelta
		iy1=iy0+nypatch-1
		iycen=iy0+nypatch/2
		ixpstr=(numxpat+1)/2
		ixpend=numxpat
		do ixdir=1,-1,-2
		  do ixpat=ixpstr,ixpend,ixdir
		    ix0=ixstart+(ixpat-1)*ixdelta
		    ix1=ix0+nxpatch-1
		    ixcen=ix0+nxpatch/2
c		    print *,'doing',ixcen,iycen,izcen
		    indp=indpat(ixpat,iypat,izpat)
		    if(ifdebug.eq.2.and.ifdone(indp).eq.0)then
		      write(1,105)ixcen,iycen,izcen,2.,2.,2.
		      ifdone(indp)=1
		    endif

		    if(ifdone(indp).eq.0)then
c		      
c			find and average adjacent patches
c			
		      nadj=0
		      dxsum=0.
		      dysum=0.
		      dzsum=0.
		      do i=1,nadjlook
			ixadj=ixpat+idxadj(i)
			iyadj=iypat+idyadj(i)
			izadj=izpat+idzadj(i)
			if(ixadj.gt.0.and.ixadj.le.numxpat.and.
     &			    iyadj.gt.0.and.iyadj.le.numypat.and.
     &			    izadj.gt.0.and.izadj.le.numzpat)then
			  inda=indpat(ixadj,iyadj,izadj)
			  if(ifdone(inda).gt.0)then
			    nadj=nadj+1
			    dxsum=dxsum+dxpat(inda)
			    dysum=dysum+dypat(inda)
			    dzsum=dzsum+dzpat(inda)
			  endif
			endif
		      enddo
c		      nadj=max(1,nadj)
		      if(nadj.gt.0)then
			dxadj=dxsum/nadj
			dyadj=dysum/nadj
			dzadj=dzsum/nadj
			if(abs(dxadj).le.maxshift.and.abs(dyadj).le.maxshift
     &			    .and.abs(dzadj).le.maxshift)then
			  if(ix0.lt.loadx0.or.ix1.gt.loadx1.or.
     &			      iy0.lt.loady0.or.iy1.gt.loady1.or.
     &			      iz0.lt.loadz0.or.iz1.gt.loadz1)then
c			    print *,'loading data'
c			      
c			      need to load new data
c
			    loady0=iy0
			    loady1=iy1
			    loadz0=iz0
			    loadz1=iz1
c			      
c			      compute limits in X, loading as much as possible
c			      but limiting to edge of data and then truncating
c			      to the end of a patch
c
			    if(ixdir.gt.0)then
			      loadx0=ix0
			      loadx1=min(nx-1,ix0+maxxload-1)
			      nmore=(loadx1-ix1)/ixdelta
			      loadx1=ix1+ixdelta*nmore
			    else
			      loadx1=ix1
			      loadx0=max(0,ix1+1-maxxload)
			      nmore=(ix0-loadx0)/ixdelta
			      loadx0=ix0-ixdelta*nmore
			    endif
			    nxload=loadx1+1-loadx0
			    nyload=loady1+1-loady0
			    nzload=loadz1+1-loadz0
			    call loadvol(1,buf(indloada),nxload,
     &				nyload,loadx0, loadx1, loady0,
     &				loady1,loadz0,loadz1)
			    call loadvol(2,buf(indloadb),nxload,
     &				nyload,loadx0, loadx1, loady0,
     &				loady1,loadz0,loadz1)
			  endif
c			    
c			    get the a patch from the loaded data; taper inside
c			    and shift mean to zero
c
			  call extract_patch(buf(indloada),nxload,
     &			      nyload,nzload, loadx0,loady0,loadz0,ix0,iy0,
     &                        iz0,buf(indpatcha),nxpatch,nypatch,
     &			      nzpatch)
			  call taperinvol(buf(indpatcha),nxpatch,
     &			      nypatch,nzpatch, buf(indpatcha),nxpatch,nxpatch,
     &			      nypatch,nzpatch, nxtap,nytap,nztap)
			  call volmeanzero(buf(indpatcha),nxpatch,
     &			      nypatch,nzpatch)
c			    
c			    get the b patch from loaded data, taper inside and
c			    pad to the maximum shift; set mean to zero also
c
			  call extract_patch(buf(indloadb),nxload,
     &			      nyload,nzload, loadx0,loady0,loadz0,ix0,iy0,
     &                        iz0,buf(indpatchb),nxpatch,nypatch,
     &			      nzpatch)
			  call taperinvol(buf(indpatchb),nxpatch,
     &			      nypatch,nzpatch, buf(indpatchb),nxpad,nxpad,
     &			      nypad,nzpad, nxtap,nytap,nztap)
			  call volmeanzero(buf(indpatchb),nxpad,
     &			      nypad,nzpad)
c
			  call find_best_corr(buf(indpatcha),nxpatch,
     &			      nypatch,nzpatch, ix0,iy0,iz0,buf(indpatchb),
     &			      nxpad,nypad,nzpad,ix0-maxshift-1,iy0-maxshift-1,
     &			      iz0-maxshift-1, ix0,ix1,iy0,iy1,
     &			      iz0,iz1,dxadj,dyadj,dzadj,maxshift,found,ncorrs)
			  if(found)then
			    ifdone(indp)=1
			    dxpat(indp)=dxadj
			    dypat(indp)=dyadj
			    dzpat(indp)=dzadj
			  endif		
			endif
		      endif
c			
c			If there are no adjacent patches, or we were too close
c			to the shift limit, do a full cross corr
c			
		      if(ifdone(indp).eq.0.or.ifdebug.ne.0)then
			if (imodGetenv('IMOD_DIR', imodpath) .ne. 0)
     &			    call errorexit('FAILED TO GET IMOD_DIR '//
     &			    'ENVIRONMENT VARIABLE')
			if (imodGetenv('IMOD_CSHELL', imodshell) .ne. 0)
     &			    imodshell = 'tcsh'
			write(patchcom,
     &			    '(a,1x,a,1x,a,a,a,3i4,3i5,1x,a,1x,a,1x,a)')
     &			    imodshell(1:lnblnk(imodshell)),'-f',
     &			    imodpath(1:lnblnk(imodpath)),'/bin/',
     &			    onepatchname,nxpatch,nypatch,
     &			    nzpatch,ixcen,iycen,izcen,filea(1:lnblnk(filea)),
     &			    fileb(1:lnblnk(fileb)),tempfile(1:lnblnk(tempfile))
c			print *,patchcom(1:lnblnk(patchcom))
			call system(patchcom)
			inquire(file=tempfile,exist=exist)
			if(.not.exist)then
			  print *,'ERROR: CORRSEARCH3D - ONEPATCHCORR ',
     &			      'FAILED TO RETURN RESULTS AT',ixcen,iycen,izcen
			  call exit(1)
			endif
			open(2,file=tempfile,status='old')
			read(2,*)dxpat(indp),dypat(indp),dzpat(indp)
			close(2)
			if(ifdone(indp).eq.0) nonepatch=nonepatch+1
		      endif
		      if(ifdebug.ne.0)then
			if(ifdone(indp).eq.0)then
			  write(1,105)ixcen,iycen,izcen,dxpat(indp),
     &			      dypat(indp),dzpat(indp),dxpat(indp),
     &			      dypat(indp),dzpat(indp)
			else
			  err=sqrt((dxadj-dxpat(indp))**2+
     &			      (dyadj-dypat(indp))**2+(dzadj-dzpat(indp))**2)
			  write(1,105)ixcen,iycen,izcen, dxpat(indp),
     &			      dypat(indp),dzpat(indp),dxadj,dyadj,dzadj,err
			endif
		      else
			write(1,105)ixcen,iycen,izcen,dxpat(indp),
     &			    dypat(indp),dzpat(indp)
		      endif
105		      format(3i5,6f8.2,f8.3)
		      ifdone(indp)=1
		      call flush(1)
		    endif
		  enddo
		  ixpstr=ixpstr-1
		  ixpend=1
		enddo
	      enddo
	      iypstr=iypstr-1
	      iypend=1
	    enddo
	  enddo
	  izpstr=izpstr-1
	  izpend=1
	enddo
	close(1)
	perpos=(3.*ncorrs)/(numtot-nonepatch)
	write(*,'(f8.2,a,i5,a)')perpos,' correlations per position,'//
     &	    ' onepatchcorr run',nonepatch,' times' 
	call exit(0)
	end


c	  FIND_BEST_CORR will search for the best 3-D displacement between
c	  two volumes in ARRAY and BRRAY.  ARRAY is dimensioned to NXA by NYA
c	  by NZA, and its starting index coordinates are LOADAX0, LOADAY0,
c	  LOADAZ0.  BRRAY is dimensioned to NXB by NYB by NZB, and its
c	  starting index coordinates are LOADBX0, LOADBY0, LOADBZ0.  The volume
c	  to be correlated is specified by starting and ending index
c	  coordinates IX0, IX1, IY0, IY1, IZ0, IZ1.  DXADJ, DYADJ, DZADJ
c	  should contain a starting shift upon entry, and will return the
c	  shift with the best correlation.  MAXSHIFT specifies the maximum
c	  shift that is allowed.  FOUND is returned as TRUE if a correlation
c	  peak is found within the maximum shift.  NCORRS is a variable that
c	  allows the calling program to maintain a count of the total number
c	  of correlations computed.
c
c	  This was originally written to work with patches embedded in larger
c	  loaded volumes, with the two volumes potentially loaded separately,
c	  hence the unnecessary complexity for the program at hand.
c
	subroutine find_best_corr(array,nxa,nya,nza,
     &	    loadax0,loaday0,loadaz0, brray,nxb,nyb,nzb,
     &	    loadbx0,loadby0,loadbz0,ix0,ix1,iy0,iy1, iz0,iz1,dxadj,
     &	    dyadj,dzadj,maxshift, found,ncorrs)
	implicit none

	integer*4 nxa,nxb,nya,nyb,nza,nzb
	real*4 array(nxa,nya,nza),brray(nxb,nyb,nzb)
	real*8 corrs(-1:1,-1:1,-1:1),corrtmp(-2:2,-2:2,-2:2)
	real*8 corrmax
	logical done(-1:1,-1:1,-1:1),donetmp(-2:2,-2:2,-2:2)
	integer*4 idyseq(9)/0,-1,1,0,0,-1,1,-1,1/
	integer*4 idzseq(9)/0,0,0,1,-1,-1,-1,1,1/
	logical found
	integer*4 ix0,ix1,iy0,iy1, iz0,iz1,maxshift,ncorrs
	integer*4 loadax0,loadbx0,loaday0,loadby0,loadaz0,loadbz0
	real*4 dxadj,dyadj,dzadj
c	  
	integer*4 idxglb,idyglb,idzglb,ix,iy,iz,iseq,idy,idz
	integer*4 idycor,idzcor,ix0cor,ix1cor,iy0cor,iy1cor,iz0cor,iz1cor
	integer*4 indmax
	real*4 cx,y1,y2,y3,denom,cy,cz
c	  
c	  get global displacement of b, including the load offset
c
c	print *,'findbest',nxa,nya,nza,
c     &	    loadax0,loaday0,loadaz0, nxb,nyb,nzb,
c     &	    loadbx0,loadby0,loadbz0,ix0,ix1,iy0,iy1, iz0,iz1,dxadj,
c     &	    dyadj,dzadj,maxshift
	idxglb=nint(dxadj)+loadax0-loadbx0
	idyglb=nint(dyadj)+loaday0-loadby0
	idzglb=nint(dzadj)+loadaz0-loadbz0
c	  
c	  clear flags for existence of corr
c
	do iz=-1,1
	  do iy=-1,1
	    do ix=-1,1
	      done(ix,iy,iz)=.false.
	    enddo
	  enddo
	enddo

	corrmax=-1.e30
	iseq=1
	do while(iseq.le.9)
	  idy=idyseq(iseq)
	  idz=idzseq(iseq)
c	  print *,iseq,idy,idz
	  if(.not.(done(-1,idy,idz).and.done(0,idy,idz).and.
     &	      done(1,idy,idz)))then
c	      
c	      if the whole row does not exist, do the correlations
c	      limit the extent if b is displaced and near an edge
c
	    idycor=idyglb+idy
	    idzcor=idzglb+idz
	    ix0cor=max(ix0-loadax0,-(idxglb-1))
	    ix1cor=min(ix1-loadax0,nxb-1-(idxglb+1))
	    iy0cor=max(iy0-loaday0,-idyglb)
	    iy1cor=min(iy1-loaday0,nyb-1-idyglb)
	    iz0cor=max(iz0-loadaz0,-idzglb)
	    iz1cor=min(iz1-loadaz0,nzb-1-idzglb)
	    ncorrs=ncorrs+1
	    call threecorrs(array,nxa,nya,brray,nxb,nyb,ix0cor,ix1cor,
     &		iy0cor,iy1cor,iz0cor,iz1cor,idxglb,idycor,idzcor,
     &		corrs(-1,idy,idz),corrs(0,idy,idz),corrs(1,idy,idz))
	    done(-1,idy,idz)=.true.
	    done(0,idy,idz)=.true.
	    done(1,idy,idz)=.true.
	  endif
	  if(corrs(0,idy,idz).gt.corrs(-1,idy,idz).and.
     &	      corrs(0,idy,idz).gt.corrs(1,idy,idz))then
	    indmax=0
	  elseif(corrs(-1,idy,idz).gt.corrs(0,idy,idz).and.
     &		corrs(-1,idy,idz).gt.corrs(1,idy,idz))then
	    indmax=-1
	  else
	    indmax=1
	  endif
	  if(corrs(indmax,idy,idz).gt.corrmax)then
c	    print *,'moving by',indmax,idy,idz
	    corrmax=corrs(indmax,idy,idz)
c	      
c	      if there is a new maximum, shift the done flags and the existing
c	      correlations, and reset the sequence
c
	    idxglb=idxglb+indmax
	    idyglb=idyglb+idy
	    idzglb=idzglb+idz
c	      
c	      but if beyond the limit, return failure
c
	    if(max(abs(idxglb+loadbx0-loadax0),abs(idyglb+loadby0-loaday0),
     &		abs(idzglb+loadbz0-loadaz0)).gt.maxshift)then
	      found=.false.
	      return
	    endif
	    do iz=-1,1
	      do iy=-1,1
		do ix=-1,1
		  donetmp(ix,iy,iz)=.false.
		enddo
	      enddo
	    enddo
	    do iz=-1,1
	      do iy=-1,1
		do ix=-1,1
		  donetmp(ix-indmax,iy-idy,iz-idz)=done(ix,iy,iz)
		  corrtmp(ix-indmax,iy-idy,iz-idz)=corrs(ix,iy,iz)
		enddo
	      enddo
	    enddo
	    do iz=-1,1
	      do iy=-1,1
		do ix=-1,1
		  done(ix,iy,iz)=donetmp(ix,iy,iz)
		  corrs(ix,iy,iz)=corrtmp(ix,iy,iz)
		enddo
	      enddo
	    enddo
	    if(indmax.ne.0.or.idy.ne.0.or.idz.ne.0)iseq=0
	  endif
	  iseq=iseq+1
	enddo
c	  
c	  do independent parabolic fits in 3 dimensions
c	  
	cx=0.
	y1=corrs(-1,0,0)
	y2=corrs(0,0,0)
	y3=corrs(1,0,0)
	denom=2.*(y1+y3-2.*y2)
	if(abs(denom).gt.1.e-6)cx=(y1-y3)/denom
	if(abs(cx).gt.0.5)cx=sign(0.5,cx)
c	  
	cy=0.
	y1=corrs(0,-1,0)
	y3=corrs(0,1,0)
	denom=2.*(y1+y3-2.*y2)
	if(abs(denom).gt.1.e-6)cy=(y1-y3)/denom
	if(abs(cy).gt.0.5)cy=sign(0.5,cy)
c	  
	cz=0.
	y1=corrs(0,0,-1)
	y3=corrs(0,0,1)
	denom=2.*(y1+y3-2.*y2)
	if(abs(denom).gt.1.e-6)cz=(y1-y3)/denom
	if(abs(cz).gt.0.5)cz=sign(0.5,cz)
c	  
	dxadj=idxglb+cx+loadbx0-loadax0
	dyadj=idyglb+cy+loadby0-loaday0
	dzadj=idzglb+cz+loadbz0-loadaz0
	found=.true.
c	print *,'returning a peak'
	return
	end


c	  THREECORRS computes three correlations between volumes in ARRAY
c	  and BRRAY.  The volume in ARRAY is dimensioned to NXA by NYA, that
c	  in BRRAY is dimensioned to NXB by NYB.  The starting and ending
c	  index coordinates (numbered from 0) in ARRAY over which the 
c	  correlations are to be computed are IX0, IX1, IY0, IY1, IZ0, IZ1.
c	  The shift between coordinates in ARRAY and coordinates in B is given
c	  by IDX, IDY, IDZ.  The three correlations are returned in CORR1 (for
c	  IDX-1), CORR2 (for IDX), and CORR3 (for IDX+1).
c	  
c	  On both the SGI and the PC, three correlations can be computed at
c	  once almost as fast as one can.  This technique thus speeds up the
c	  overall search by almost a factor of 3.
c
	subroutine threecorrs(array,nxa,nya,brray,nxb,nyb,ix0,ix1,
     &	    iy0,iy1,iz0,iz1,idx,idy,idz,corr1,corr2,corr3)
	implicit none
	real*4 array(*),brray(*)
	real*8 sum1,sum2,sum3,corr1,corr2,corr3
	integer*4 ix0,ix1,iy0,iy1, iz0,iz1,idx,idy,idz,nxa,nxb,nya,nyb
	integer*4 iz,izb,iy,iyb,indbasea,inddelb,ix,ixb,nsum
	sum1=0.
	sum2=0.
	sum3=0.
	do iz=iz0,iz1
	  izb=iz+idz
	  do iy=iy0,iy1
	    iyb=iy+idy
	    indbasea=1+iy*nxa+iz*nxa*nya
	    inddelb=1+iyb*nxb+izb*nxb*nyb+idx-indbasea
	    
	    do ix=indbasea+ix0,indbasea+ix1
	      ixb=ix+inddelb
	      sum1=sum1+array(ix)*brray(ixb-1)
	      sum2=sum2+array(ix)*brray(ixb)
	      sum3=sum3+array(ix)*brray(ixb+1)
	    enddo
	  enddo
	enddo	
	nsum=(iz1+1-iz0)*(iy1+1-iy0)*(ix1+1-ix0)
	corr1=sum1/nsum
	corr2=sum2/nsum
	corr3=sum3/nsum
c	print *,idx,idy,idz,corr1,corr2,corr3
	return
	end


c	  This was a more straightforward version for testing THREECORRS
c
c$$$	subroutine safecorrs(array,nxa,nya,brray,nxb,nyb,ix0,ix1,
c$$$     &	    iy0,iy1,iz0,iz1,idx,idy,idz,corr1,corr2,corr3)
c$$$	real*4 array(nxa,nya,*),brray(nxb,nyb,*)
c$$$	real*8 sum1,sum2,sum3,corr1,corr2,corr3
c$$$c	  
c$$$c	print *,nxa,nya,nxb,nyb,ix0,ix1, iy0,iy1,iz0,iz1
c$$$	sum1=0.
c$$$	sum2=0.
c$$$	sum3=0.
c$$$	do iz=iz0+1,iz1+1
c$$$	  izb=iz+idz
c$$$	  do iy=iy0+1,iy1+1
c$$$	    iyb=iy+idy
c$$$c	      print *,array(ix0+1,iy,iz),brray(ix0+idx+1,iyb,izb)
c$$$	    do ix=ix0+1,ix1+1
c$$$	      ixb=ix+idx
c$$$c	      print *,array(ix,iy,iz),brray(ixb,iyb,izb)
c$$$	      sum1=sum1+array(ix,iy,iz)*brray(ixb-1,iyb,izb)
c$$$	      sum2=sum2+array(ix,iy,iz)*brray(ixb,iyb,izb)
c$$$	      sum3=sum3+array(ix,iy,iz)*brray(ixb+1,iyb,izb)
c$$$	    enddo
c$$$	  enddo
c$$$	enddo	
c$$$	nsum=(iz1+1-iz0)*(iy1+1-iy0)*(ix1+1-ix0)
c$$$	corr1=sum1/nsum
c$$$	corr2=sum2/nsum
c$$$	corr3=sum3/nsum
c$$$c	print *,idx,idy,idz,corr1,corr2,corr3
c$$$	return
c$$$	end


c	  LOADVOL loads a subset of the volume from unit IUNIT, into ARRAY
c	  assuming dimensions of NXDIM by NYDIM, from index coordinates
c	  IX0, IX1, IY0, IY1, IZ0, IZ1.
c
	subroutine loadvol(iunit,array,nxdim,nydim,ix0,ix1,iy0,iy1,iz0,iz1)
	implicit none
	integer*4 nxdim,nydim,ix0,ix1,iy0,iy1,iz0,iz1,iunit,indz,iz
	real*4 array(nxdim,nydim,*)
c	  
c	print *,iunit,nxdim,nydim,ix0,ix1,iy0,iy1,iz0,iz1
	indz=0
	do iz=iz0,iz1
	  indz=indz+1
	  call imposn(iunit,iz,0)
	  call irdpas(iunit,array(1,1,indz),nxdim,nydim,ix0,ix1,iy0,iy1,*99)
	enddo
	return
99	call errorexit('ERROR READING FILE')
	end


c	  EXTRACT_PATCH extracts a patch of dimensions NXPATCH by NYPATCH by
c	  NZPATCH into ARRAY from the loaded volume in BUF, whose dimensions
c	  are NXLOAD by NYLOAD by NZLOAD.  BUF is loaded from starting index
c	  coordinates LOADX0, LOADY0, LOADZ0, and the starting index
c	  coordinates of the patch are IX0, IY0, IZ0.
c
	subroutine extract_patch(buf,nxload,nyload,nzload,
     &	    loadx0,loady0,loadz0,ix0,iy0,
     &	    iz0,array,nxpatch,nypatch,nzpatch)
	implicit none
	integer*4 nxload,nyload,nzload,loadx0,loady0,loadz0,ix0,iy0
	integer*4 iz0,nxpatch,nypatch,nzpatch
	real*4 buf(nxload,nyload,nzload),array(nxpatch,nypatch,nzpatch)
	integer*4 iz, izfrom,iy,iyfrom,ix
c	  
	do iz=1,nzpatch
	  izfrom=iz+iz0-loadz0
	  do iy=1,nypatch
	  iyfrom=iy+iy0-loady0
	    do ix=1,nxpatch
	      array(ix,iy,iz)=buf(ix+ix0-loadx0,iyfrom,izfrom)
	    enddo
	  enddo
	enddo
	return
	end

	
c	  VOLMEANZERO shifts the mean to zero of the volume in ARRAY
c	  dimensioned NX by NY by NZ
c
	subroutine volmeanzero(array,nx,ny,nz)
	implicit none
	real*4 array(*)
	real*8 sum
	integer*4 nx,ny,nz
	integer*4 i
	real*4 dmean
	sum=0.
	do i=1,nx*ny*nz
	  sum=sum+array(i)
	enddo
	dmean=sum/(nx*ny*nz)
	do i=1,nx*ny*nz
	  array(i)=array(i)-dmean
	enddo
	return
	end


c	  checkAndSetPatches does error checks and sets the basic start
c	  and delta for the patches
c
	subroutine checkAndSetPatches(nx, nbxlo, nbxhi, nxpatch, numxpat,
     &	    ixstart, ixdelta, axis)
	implicit none
	integer*4 nx, nbxlo, nbxhi, nxpatch, numxpat, ixstart, ixdelta
	character*6 axis
	character*80 concat
c	  
c	  check basic input properties
c
	if (nbxlo .lt. 0 .or. nbxhi .lt. 0) call errorexit(
     &	    'A NEGATIVE BORDER WAS ENTERED FOR THE '//axis)
	if (nxpatch .le. 4) call errorexit(
     &	    'PATCH SIZE NEGATIVE OR TOO SMALL FOR THE '//axis)
	if (numxpat .le. 0) call errorexit(
     &	    'NUMBER OF PATCHES MUST BE POSITIVE FOR THE '//axis)
	if (nxpatch .gt. nx-(nbxlo+nbxhi)) call errorexit(
     &	    'PATCH SIZE IS TOO LARGE TO FIT WITHIN BORDERS FOR THE '//
     &	    axis)
c	  
c	  If multiple patches, compute the delta and then adjust the number
c	  of patches down to require a delta of at least 2
c
	if(numxpat.gt.1)then
	  ixstart=nbxlo
	  ixdelta=(nx-(nbxlo+nbxhi+nxpatch))/(numxpat-1)
	  do while (numxpat .gt. 1 .and. ixdelta .lt. 2)
	    numxpat = numxpat - 1
	    if (numxpat .gt. 1) ixdelta=(nx-(nbxlo+nbxhi+nxpatch))/(numxpat-1)
	  enddo
	endif
c	  
c	  If only one patch originally or now, center it in range
c
	if (numxpat .eq. 1) then
	  ixstart=(nbxlo+nx-nbxhi)/2 - nxpatch/2
	  ixdelta=1
	endif
	return
	end


	subroutine errorexit(message)
	character*(*) message
	integer*4 lnblnk
	print *
	print *,'ERROR: CORRSEARCH3D - ',message(1:lnblnk(message))
	call exit(1)
	end

