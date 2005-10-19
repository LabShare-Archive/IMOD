* * * * * * REFINEMATCH * * * * * *
c
c	  REFINEMATCH will solve for a general 3-dimensional linear
c	  transformation to align two volumes to each other.  It performs
c	  multiple linear regression on the  displacements between the volumes
c	  determined at a matrix of positions.  The displacements must be
c	  contained in a file with the following form:
c	  
c	  Number of displacements
c	  One line for each displacement consisting of the X, Y, and Z
c	  .  coordinates in the first volume, then the displacements in X, Y
c	  .  and Z involved in moving from the first to the second volume
c	  
c	  The program will automatically eliminate "outliers", patch
c	  displacements that are likely to be incorrect because they are so
c	  extreme, when compared to the rest of the displacements.  This
c	  elimination is conservative, but if for some reason it operates
c	  incorrectly, use FINDWARP, where you can control the parameters of
c	  elimination or stop the elimination from occurring.  The program will
c	  eliminate up to 10% of the patches.  If more than this number are
c	  bad, either get a new set of patches that do not approach so close to
c	  the edge of the volume, make a model in the tomogram with contours
c	  enclosing the patches to use, eliminate the bad ones from the file by
c	  hand, or use FINDWARP to eliminate a whole row or column of patches.
c	  When the mean residual exceeds a value that you specify, the program
c	  will exit with an error.
c
c	  A model specifying which patches to include in the fit can be quite
c	  simple, consisting of just a single contour enclosing the region 
c	  where patches are generally good.  This contour can be drawn in any 
c	  Z plane of the flipped tomogram.  However, if the good region
c	  changes through the depth of the tomogram, you can draw contours at
c	  several Z levels.  If you have two layers of patches, draw two
c	  contours, one near the top and one near the bottom of the tomogram;
c	  if you have three layers, add another contour in the middle, etc.
c	  For a given patch, the program will find the contour at the nearest
c	  Z level and use that one to determine whether to include the patch.
c
c	  If there is only one layer of patches in one dimension, there is
c	  insufficient information to solve for the full transformation, so
c	  the program will solve for only two of the three columns of the
c	  transformation matrix.  This typically occurs in the Y dimension,
c	  in which case the second column of the matrix is fixed at 0, 1, 0.
c
c	  
c	  Inputs to the program:
c	  
c	  Name of file with positions and displacements
c	  
c	  Either the file name of one of the volumes, or the X, Y, and
c	  .  Z dimensions of the volumes.
c	  
c	  Name of an Imod model file with contours enclosing the patches to
c	  .  be included in the fit, or Return to use all patches.
c
c	  Limiting value for the mean residual; above this value, the program
c	  .  will exit with an error.
c
c	  Name of output file in which to place the transformation, or Return
c	  .  for no output to a file
c
c	  David Mastronarde, 1995
c	  12/24/98: added outlier elimination; 6/6/99: added error exit
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.5  2004/06/11 00:13:39  mast
c	  Added error exit if fewer than 4 data points for fit
c	
c	  Revision 3.4  2003/12/24 19:03:53  mast
c	  Changed to fit new form of get_nxyz
c	
c	  Revision 3.3  2002/10/23 15:40:22  mast
c	  Added ability to get a solution with only one layer of patches in
c	  one of the dimensions, but fixing the column of the matrix for that
c	  dimension.
c	
c	  Revision 3.2  2002/09/09 21:36:00  mast
c	  Eliminate stat_source: and nimp_source: from all includes
c	
c	  Revision 3.1  2002/07/21 00:06:46  mast
c	  Standardized error outputs, added declarations for implicit none, and
c	  added model scaling in case image file has a non-unitary scale.
c	
c	  
	implicit none
        include 'statsize.inc'
	include 'model.inc'
	integer idim,limvert
        parameter (idim=40000,limvert=100000)
        real*4 xr(msiz,idim)
        real*4 cx(idim),dx(idim),a(3,3),dxyz(3),devxyz(3)
	real*4 devxyzmax(3),cenloc(3),cxlast(3)
        integer*4 nxyz(3),idrop(idim)
	logical inside,exist,readw_or_imod,onelayer(3)
	integer getimodhead,getimodscales
	real*4 xvert(limvert),yvert(limvert),zcont(idim)
	integer*4 indvert(idim),nvert(idim)
        character*80 filename
c	  
	integer*4 ncont,ierr,ifflip,i,indy,indz,indcur,iobj,ndat,nfill
	real*4 ximscale,yimscale,zimscale,xyscal,zscal,xofs,yofs,zofs
	integer*4 ifuse,icont,icmin,j,ind,maxdrop,ndrop,ipntmax,ip,ipt
	real*4 dzmin,crit,elimmin,critabs,devavg,devsd,devmax,stoplim,dz
	integer*4 icolfix

        write(*,'(1x,a,$)')
     &      'Name of file with correlation positions and results: '
        read(*,'(a)')filename
        call dopen(1,filename,'old','f')
	print *,'Enter NX, NY, NZ of tomogram',
     &	      ', or name of either tomogram file'
	call get_nxyz(.false., ' ', 'REFINEMATCH', 1,nxyz)
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
        read(1,*)ndat
	if(ndat.gt.idim)call errorexit(
     &	    'TOO MANY POINTS FOR DATA ARRAYS')

	do j=1,3
	  onelayer(j)=.true.
	enddo
	nfill=0
        do i=1,ndat
c	    
c	    these are center coordinates and location of the second volume
c	    relative to the first volume
c
          read(1,*)(cx(j),j=1,3),(dx(j),j=1,3)
	  do j = 1,3
	    if (i.gt.1 .and. cx(j).ne.cxlast(j)) onelayer(j)=.false.
	    cxlast(j) = cx(j)
	  enddo

	  ifuse=1
	  if(ncont.gt.0)then
	    ifuse=0
c	      
c	      find nearest contour in Z and see if patch is inside it
c
	    dzmin=100000.
	    do icont=1,ncont
	      dz=abs(cx(2)-zcont(icont))
	      if(dz.lt.dzmin)then
		dzmin=dz
		icmin=icont
	      endif
	    enddo
	    ind=indvert(icmin)
	    if(inside(xvert(ind),yvert(ind),nvert(icmin),cx(1),cx(3)))
     &		ifuse=1
	  endif
c
	  if(ifuse.gt.0)then
	    nfill=nfill+1
c	    write(*,'(3f6.0)')cx(1),cx(3),cx(2)
	    do j=1,3
c	      
c		the regression requires coordinates of second volume as
c		independent variables (columns 1-3), those in first volume
c		as dependent variables (stored in 5-7), to obtain
c		transformation to get from second to first volume
c		cx+dx in second volume matches cx in first volume
c
	      xr(j+4,nfill)=cx(j)-0.5*nxyz(j)
	      xr(j,nfill)=xr(j+4,nfill)+dx(j)
	    enddo
	  endif
        enddo

	close(1)
	if (ndat .lt. 4) call errorexit('TOO FEW DATA POINTS FOR FITTING')
	ndat=nfill
        print *,ndat,' data points will be used for fit'
c	  
	write(*,'(1x,a,$)')'Mean residual above which to STOP and '//
     &	    'exit with an error: '
	read(5,*)stoplim
c
	icolfix = 0
	do i = 1,3
	  if (icolfix.ne.0 .and. onelayer(i)) call errorexit(
     &	      'CANNOT FIT TO PATCHES THAT EXTEND IN ONLY ONE DIMENSION')
	  if (onelayer(i)) icolfix = i
	enddo
	if (icolfix.gt.0)print *,'There is only one layer of patches',
     &	    ' in the ',char(ichar('W')+icolfix),' dimension'
c
	maxdrop=nint(0.1*ndat)
	crit=0.01
	elimmin=0.5
	critabs=0.002
	call solve_wo_outliers(xr,ndat,3,icolfix,maxdrop,crit,critabs,
     &	    elimmin, idrop,ndrop, a,dxyz,cenloc, devavg,devsd,devmax,
     &	    ipntmax, devxyzmax)
c
	if(ndrop.ne.0)then
	  write(*,104)ndrop,devavg,devsd,(idrop(i),i=1,ndrop)
	  write(*,105)(xr(4,i),i=ndat+1-ndrop,ndat)
104	  format(i3,' points dropped by outlier elimination; ',
     &	      'residual mean =',f7.2,', SD =',f7.2,/,
     &	      ' point-pairs:',(11i6))
105	  format(' deviations :',(11f6.2))
	endif
c
        write(*,101)devavg,devmax,ipntmax,(devxyzmax(i),i=1,3)
101     format(/,' Mean residual',f8.3,',  maximum',f8.3,
     &      ' at point-pair',i4,/,'  Deviations:',3f8.3,/)
c
	print *,'Refining transformation:'
        write(*,102)((a(i,j),j=1,3),dxyz(i),i=1,3)
102     format(3f10.6,f10.3)
	print *,'Enter name of file to place transformation in, or ',
     &	    'Return for none'
	read(5,'(a)')filename
	if(filename.ne.' ')then
	  call dopen(1,filename,'new','f')
	  write(1,102)((a(i,j),j=1,3),dxyz(i),i=1,3)
	  close(1)
	endif
	if(devavg.gt.stoplim)then
	  print *
	  print *,'REFINEMATCH: THE MEAN RESIDUAL IS ABOVE THE '//
     &	      'SPECIFIED LIMIT;',
     &	      '  EITHER RAISE THE LIMIT OR USE WARPING'
	  call exit(2)
	endif
	call exit(0)
        end

	subroutine errorexit(message)
	character*(*) message
	print *
	print *,'ERROR: REFINEMATCH - ',message
	call exit(1)
	end
