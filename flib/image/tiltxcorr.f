*	  * * * * * * TILTXCORR * * * * * *
*	  
*	  TILTXCORR uses cross-correlation to find an initial translational
c	  alignment between successive images of a tilt series.  For a given
c	  pair of images, it stretches the image with the larger tilt angle
c	  perpendicular to the tilt axis, by an amount equal to the ratio of
c	  the cosines of the two tilt angles (cosine stretch).  The stretched
c	  image is correlated with the other image, and the position of the
c	  peak of the correlation indicates the relative shift between the
c	  images.  There are options to use only a subset of the image, to
c	  pad the image with a border before correlating, and to taper the
c	  image intensities down to the average level over some boundary
c	  region.  The latter feature is particularly important for getting
c	  reliable correlation peaks.  The program also has on option to
c	  ignore a central peak caused by fixed pattern noise in the images,
c	  which can be a serious problem unless digital camera images are well
c	  gain-normalized.
c
c	  Some notes about some of the options:
c	  
c	  Filtering: Some high pass filtering, using a small value of Sigma1
c	  such as 0.03, may be helpful to keep the program from being misled
c	  by very large scale features in the images.  If the images are
c	  noisy, some high pass filtering with Sigma2 and Radius2 is
c	  appropriate (e.g. 0.05 for Sigma2, 0.25 for Radius2).
c	  
c	  The exclusion of a central peak can be essential when there is
c	  fixed noise in the images.  Because one image is stretched, this
c	  spurious peak can actually occur anywhere in an elongated region
c	  perpendicular to the tilt axis.  If the real alignment happens to
c	  fall in that streak, then it will be ignored as well, and an
c	  incorrect alignment will be found.  For this reason, this option 
c	  should be used only when necessary.
c	  
c	  Trimming some area off the edges of the images may be helpful if
c	  those areas are particularly out of focus or contain material with
c	  no useful features in it.
c	  
c	  Padding is customarily done to reduce the contribution to the
c	  correlation from wrapped around features, which occurs when
c	  correlation is done with Fourier transforms.  Extensive padding does
c	  not help with typical biological specimens but may be needed for
c	  specimens with periodic structures, in which case one should pad each
c	  edge by half the image size.
c	  
c	  In contrast, tapering the images down to a mean intensity at their
c	  edges is very important.  Tapering over as few as 20 pixels may be
c	  adequate, but fewer artifacts will appear in the correlation with
c	  longer tapers (say, 50 to 100 pixels).
c	  
c	  Entries to the program in order are:
c	  
C	  Image input file
c
C	  Piece list file for reordering the Z values in the stack, or Return
c	  if none
c
C	  Output file for F transforms
C	  
c	  -1 to enter individual tilt angle for each view, 1 to specify a
c	  starting and increment tilt, or 0 to read tilt angles from a file
c	  
c	  IF you entered 1, next enter the starting and incremental tilt angles
c	  IF you entered -1, enter the tilt angle of each view.
c	  IF you entered 0, enter name of file with tilt angles
c	  
c	  Angle of rotation of the tilt axis in the images; specifically, the
c	  angle from the vertical to the tilt axis (counterclockwise
c	  positive).
c
C	  Filter parameters to filter the correlation, or / for no filter
C	  (Enter values of Sigma1, Sigma2, Radius1, Radius2 just as for
c	  ENHANCE.  
c
c	  1 to exclude a central correlation peak due to fixed pattern noise
c	  in the images, or 0 not to
c	  
c	  Amounts to trim off each side in the X and Y dimensions, or / to use
c	  the whole image area
c	  
C	  Borders with which to pad images in the X and Y dimensions, or / for
c	  the default, which is 5% of the image dimensions up to 20 pixels
c	  
c	  Distances over which to taper image intensities down to the mean at
c	  the edges, in the X and Y dimensions.  Enter / for the default, which
c	  is 10% of the image dimensions up to 100 pixels.
c
C	  Starting and ending view #'s (first is 1), or / for all views
C	  
c	  
c	  David Mastronarde 10/6/98
*
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
	parameter (idim=2300*2300,limview=720)
	COMMON //NX,NY,NZ,nxs,nys,nzs
C
	DIMENSION NXYZ(3),MXYZ(3),nxyzs(3),mxyzs(3),title(20)
     &	    ,label(20,20)
	real*4 ctfa(8193),ctfb(8193),ctfp(8193)
	complex array(idim/2),brray(idim/2),crray(idim/2)
C
	EQUIVALENCE (NX,NXYZ),(nxs,nxyzs)
c
	character*80 filin,plfile,imfilout
        real*4 f(2,3,limview),fs(2,3),fsinv(2,3)
	character*9 dat
	character*8 tim
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech
	character*70 titstr
	character*7 fltrda/' '/,fltrdb/' '/,fltrdp/' '/

	integer*4 ixpclist(limview),iypclist(limview),izpclist(limview)
	integer*4 listz(limview)
	real*4 tilt(limview)

        write(*,'(1x,a,$)')'Image input file: '
	READ(5,101)FILIN
101     format(a)
        CALL IMOPEN(1,FILIN,'RO')
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
	IF (((NX+2)*NY.GT.idim)) then
	  print *,'IMAGE TOO LARGE FOR ARRAYS'
	  call exit(1)
	endif
	if (nz.gt.limview) then
	  print *,'TOO MANY VIEWS FOR ARRAYS'
	  call exit(1)
	endif
C   
	write(*,'(1x,a,$)')'Piece list file if there is one,'//
     &	    ' otherwise Return: '
	read(*,101)plfile
	call read_piece_list(plfile,ixpclist,iypclist,izpclist,npclist)
c	  
c	    if no pieces, set up mocklist
c
	if(npclist.eq.0)then
	  do i=1,nz
	    ixpclist(i)=0
	    iypclist(i)=0
	    izpclist(i)=i-1
	  enddo
	  npclist=nz
	endif
	if(npclist.ne.nz)then
	  print *,'Piece list should have an entry for each image; nz =',
     &	      nz,', # in piece list =',npclist
	  call exit(1)
	endif
	call fill_listz(izpclist,npclist,listz,nview)
	call checklist(ixpclist,npclist,1,nx,minxpiece
     &	    ,nxpieces,nxoverlap)
	call checklist(iypclist,npclist,1,ny,minypiece
     &	    ,nypieces,nyoverlap)
	if(nxpieces*nypieces.gt.1)then
	  print *,'Program will not work with montages: ',
     &	      'blend images into single frames first'
	  call exit(1)
	endif
	if(nview.ne.nz.or.listz(1).ne.0.or.listz(nview).ne.nz-1)then
	  print *,'The piece list should specify all Z values from 0 to'
     &	      ,nz-1
	  call exit(1)
	endif
c
        write(*,'(1x,a,$)')'Output file for transforms: '
	READ(5,101)FILIN
	call dopen(1,filin,'new','f')
c	  
	imfilout=' '
c	write(*,'(1x,a,$)')'Correlation output file, Return for none: '
c	read(5,101)imfilout
c
	ifimout=0
	if(imfilout.ne.' ')then
	  CALL IMOPEN(3,imfilout,'NEW')
	  CALL ITRHDR(3,1)
	  ifimout=1
	endif
c	    
	call get_tilt_angles(nview,3,tilt)
	if(nview.ne.nz)then
	  print *,'There must be a tilt angle for each image: nz =',
     &	      nz,', but there are',nview,' tilt angles'
	  call exit(1)
	endif
c
	write(*,'(1x,a,$)')
     &	    'Rotation angle FROM vertical TO the tilt axis: '
	read(5,*)rotangle
c
	print *,'Enter filter parameters to filter the correlation,'
     &	    //' or / for no filter'
	call setctf(ctfp,nxpad,nypad,deltap)
c
	iexclude=0
	write(*,'(1x,a,$)')'1 to exclude central correlation peak due'
     &	    //' to fixed pattern noise, 0 not to: '
	read(5,*)ifexclude
	radexcl=0.
	if(ifexclude.eq.1) radexcl=1.1
c
7	nxtrim=0
	nytrim=0
	write(*,'(1x,a,$)')
     &	    'Amounts to trim off each side in X and Y (/ for 0,0):'
	read(5,*)nxtrim,nytrim
	if(nxtrim.lt.0.or.nytrim.lt.0.or.nxtrim.gt.nx/2-16.or.
     &	    nytrim.gt.ny/2-16)then
	  print *,'Impossible amount to trim by, try again'
	  go to 7
	endif
	nxuse=nx-2*nxtrim
	nyuse=ny-2*nytrim
c
	nxbord = max(5,min(20,nint(0.05*nxuse)))
	nybord = max(5,min(20,nint(0.05*nyuse)))
8	write(*,'(1x,a,2i4,a,$)') 'Amounts to pad images on each side '
     &	    //'in X and Y (/ for',nxbord,nybord,'): '
	read(*,*)nxbord,nybord
	nxpad=niceframe(nxuse+2*nxbord,2,19)
	nypad=niceframe(nyuse+2*nybord,2,19)
	if((nxpad+2)*nypad.gt.idim)then
	  print *,'Padded image too big, try smaller borders'
	  go to 8
	endif
	write(*,'(a,i5,a,i5)')' Padded size is',nxpad,' by',nypad
c
	nxtap = max(5,min(100,nint(0.1*nxuse)))
	nytap = max(5,min(100,nint(0.1*nyuse)))
	write(*,'(1x,a,2i4,a,$)') 'Widths over which to taper images'
     &	    //' in X and Y (/ for',nxtap,nytap,'): '
	read(*,*)nxtap,nytap

	ifrot180=ifrotshift/2
	ifshift=mod(ifrotshift,2)
	if(ifrot180.ne.0)ifshift=-ifshift
15	izst=1
	iznd=nz
	write(*,'(1x,a,$)') 'Starting and ending views'
     &	    //' to do (first is 1), or / for all: '
	read(*,*)izst,iznd
	if(izst.lt.1.or.iznd.gt.nz.or.izst.ge.iznd)go to 15
c	  
	do kk=1,nz
	  call xfunit(f(1,1,kk),1.0)
	enddo
c
	if(imfilout.ne.' ')then
	  nout=iznd-izst
	  if(ifimout.ne.0)nout=nout*3
	  call ialsiz_sam_cel(3,nxpad,nypad,nout)
	  dmsum=0.
	  dmax=-1.e10
	  dmin=1.e10
	endif
c
        DO iview=izst+1,iznd

	  do i=1,nz
	    if(izpclist(i)+1.eq.iview-1)izlast=i-1
	    if(izpclist(i)+1.eq.iview)izcur=i-1
	  enddo
c	    
c	    get the stretch - if its less than 1., invert everything
c
	  idir=1
	  stretch=cosd(tilt(iview-1))/cosd(tilt(iview))
	  if(stretch.lt.1.)then
	    idir=-1
	    iztmp=izcur
	    izcur=izlast
	    izlast=iztmp
	    stretch=1./stretch
	  endif
	  call rotmagstr_to_amat(0.,1.,stretch,rotangle,fs)
	  fs(1,3)=0.
	  fs(2,3)=0.
	  call xfinvert(fs,fsinv)
c	  print *,izlast,izcur,stretch
c	 
c	    get "current" into array, stretch into brray, pad it
C
	  call imposn(1,izcur,0)
	  if(nxtrim.eq.0.and.nytrim.eq.0)then
	    call irdsec(1,array,*99)
	  else
	    call irdpas(1,array,nxuse,nyuse,nxtrim,nx-nxtrim-1,nytrim,
     &		ny-nytrim-1,*99)
	  endif
	  call cubinterp(array,brray,nxuse,nyuse,nxuse,nyuse,fs,
     &	      nxuse/2., nyuse/2.,0.,0. ,1.,dmean2)
	  call taperinpad(brray,nxuse,nyuse,brray,nxpad+2,nxpad,nypad,
     &	      nxtap, nytap)
c
	  call meanzero(brray,nxpad+2,nxpad,nypad)
c	    
c	    get "last" into array, just pad it there
c
	  call imposn(1,izlast,0)
	  if(nxtrim.eq.0.and.nytrim.eq.0)then
	    call irdsec(1,array,*99)
	  else
	    call irdpas(1,array,nxuse,nyuse,nxtrim,nx-nxtrim-1,nytrim,
     &		ny-nytrim-1,*99)
	  endif
	  call taperinpad(array,nxuse,nyuse,array,nxpad+2,nxpad,nypad,
     &	      nxtap, nytap)
	  if(ifimout.ne.0)then
	    do isout=1,2
	      if(isout.eq.1)then
		call irepak(crray,array,nxpad+2,nypad,
     &		    0,nxpad-1,0,nypad-1)
	      else
		call irepak(crray,brray,nxpad+2,nypad,
     &		    0,nxpad-1,0,nypad-1)
	      endif
	      if(mode.ne.2)then
		CALL IsetDN(crray,NXpad,NYpad,MODE,1,NXpad,1,NYpad,DMIN2,
     &		    DMAX2,DMEAN2)
	      else
		CALL IclDeN(crray,NXpad,NYpad,1,NXpad,1,NYpad,DMIN2,DMAX2,
     &		    DMEAN2)
	      endif
	      CALL IWRSEC(3,crray)
C	      
	      DMAX = max(dmax,dmax2)
	      DMIN = min(dmin,dmin2)
	      DMsum = dmsum + dmean2
	    enddo
	  endif
	  call meanzero(array,nxpad+2,nxpad,nypad)

C	    
c	  print *,'taking fft'
	  call todfft(array,nxpad,nypad,0)
	  call todfft(brray,nxpad,nypad,0)
c	    
c	    multiply array by complex conjugate of brray, put back in array
c	    
c	  print *,'multiplying arrays'
	  do jx=1,nypad*(nxpad+2)/2
	    array(jx)=array(jx)*conjg(brray(jx))
	  enddo
c
	  if(deltap.ne.0.)call filterpart(array,array,nxpad,nypad,ctfp,
     &	      deltap)
c	  print *,'taking back fft'
	  call todfft(array,nxpad,nypad,1)
c
c	    the factor here determines the balance between accepting a spurious
c	    peak and rejecting a real peak because it falls in the streak.
c	    The spurious peak could be as far as 0.5 out but is much more
c	    likely to be within 0.25 of the maximum displacement
c	    
	  streak=0.25*(stretch-1.0)*nxuse
	  call peakfind(array,nxpad+2,nypad,xpeak,ypeak,radexcl,
     &	      rotangle, streak)
	  call xfapply(fsinv,0.,0.,xpeak,ypeak,usdx,usdy)
	  print *,'View',iview,', shifts',usdx,usdy
	  call flush(6)
	  f(1,3,iview)=idir*usdx
	  f(2,3,iview)=idir*usdy
	  if(imfilout.ne.' ')then
	    call packcorr(crray,array,nxpad+2,nypad)
	    if(mode.ne.2)then
	      CALL IsetDN(crray,NXpad,NYpad,MODE,1,NXpad,1,NYpad,DMIN2,
     &		  DMAX2,DMEAN2)
	    else
	      CALL IclDeN(crray,NXpad,NYpad,1,NXpad,1,NYpad,DMIN2,DMAX2,
     &		  DMEAN2)
	    endif
	    CALL IWRSEC(3,crray)
C	      
	    DMAX = max(dmax,dmax2)
	    DMIN = min(dmin,dmin2)
	    DMsum = dmsum + dmean2
	  endif
	enddo
c
	if(imfilout.ne.' ')then
	  dmean=dmsum/nout
	  CALL DATE(DAT)
	  CALL TIME(TIM)
c	
	  if(deltap.ne.0.)fltrdp=', filtered'
	  titstr='TILTXCORR: stack cosine stretch/correlated '//
     &	      fltrdp
c	   
c 7/7/00 CER: remove the encodes
c
C         ENCODE(80,1500,TITLE) titstr,DAT,TIM
          write(titlech,1500) titstr,DAT,TIM
          read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
1500	  FORMAT(A54,2x,A9,2X,A8)
	  
	  CALL IWRHDR(3,TITLE,1,DMIN,DMAX,DMEAN)
	  call imclose(3)
	endif
c
	do iv=1,nz
	  call xfwrite(1,f(1,1,iv),96)
	enddo
	close(1)
	call imclose(1)
C
	WRITE(6,500)
500	FORMAT(' PROGRAM EXECUTED TO END.')
	call exit(0)
99	WRITE(6,450)
450	FORMAT(' END OF IMAGE WHILE READING')
	call exit(1)
94	WRITE (6,660)
660	FORMAT(' Input image too big .')
	call exit(1)
96	print *,'ERROR WRITING TRANSFORMS TO FILE'
	call exit(1)
	END



c	  PEAKFIND finds the coordinates of the the absolute peak, XPEAK, YPEAK
c	  in the array ARRAY, which is dimensioned to nx+2 by ny.  It fits
c	  a paravola in to the three points around the peak in X or Y and gets
c	  a much better estimate of peak location.
c
	subroutine peakfind(array,nxplus,nyrot,xpeak,ypeak,radexcl,
     &	    rotangle,streak)
	real*4 array(nxplus,nyrot)
	nxrot=nxplus-2
c	  
c	  find peak
c	  
	costh=cosd(-rotangle)
	sinth=sind(-rotangle)
	peak=-1.e30
	do iy=1,nyrot
	  do ix=1,nxrot
	    if(array(ix,iy).gt.peak)then
c		
c		first check if outside exclusion streak
c
	      idx=ix-1
	      idy=iy-1
	      if(idx.gt.nxrot/2)idx=idx-nxrot
	      if(idy.gt.nyrot/2)idy=idy-nyrot
	      xrot=idx*costh-idy*sinth
	      yrot=idx*sinth+idy*costh
	      if(abs(yrot).ge.radexcl.or.abs(xrot).ge.streak+radexcl)then
c		  
c		  next check that point is actually a local peak
c		  
		lower=0
		do idx =-1,1
		  do idy=-1,1
		    if(array(ix,iy).lt.array(indmap(ix+idx,nxrot),
     &			indmap(iy+idy,nyrot)))lower=1
		  enddo
		enddo
		if(lower.eq.0)then
		  peak=array(ix,iy)
		  ixpeak=ix
		  iypeak=iy
		endif
	      endif
	    endif
	  enddo
	enddo
c	print *,ixpeak,iypeak
c	  
c	  simply fit a parabola to the two adjacent points in X or Y
c
	cx=0.
	y1=array(indmap(ixpeak-1,nxrot),iypeak)
	y2=peak
	y3=array(indmap(ixpeak+1,nxrot),iypeak)
	denom=2.*(y1+y3-2.*y2)
	if(abs(denom).gt.-1.e6)cx=(y1-y3)/denom
	if(abs(cx).gt.0.5)cx=sign(0.5,cx)
c	print *,'X',y1,y2,y3,cx
	cy=0.
	y1=array(ixpeak,indmap(iypeak-1,nyrot))
	y3=array(ixpeak,indmap(iypeak+1,nyrot))
	denom=2.*(y1+y3-2.*y2)
	if(abs(denom).gt.-1.e6)cy=(y1-y3)/denom
	if(abs(cy).gt.0.5)cy=sign(0.5,cy)
c	print *,'Y',y1,y2,y3,cy
c	  
c	  return adjusted pixel coordinate minus 1
c
	xpeak=ixpeak+cx-1.
	ypeak=iypeak+cy-1.
c	print *,xpeak,ypeak
	if(xpeak.gt.nxrot/2)xpeak=xpeak-nxrot
	if(ypeak.gt.nyrot/2)ypeak=ypeak-nyrot
	return
	end



	subroutine packcorr(crray,array,nxpadpl,nypad)
	real*4 array(nxpadpl,nypad),crray(*)
	nxpad=nxpadpl-2
	iout=1
	ixin=nxpad/2+1
	iyin=nypad/2+1
	do iy=1,nypad
	  do ix=1,nxpad
	    crray(iout)=array(ixin,iyin)
	    iout=iout+1
	    ixin=ixin+1
	    if(ixin.gt.nxpad)ixin=1
	  enddo
	  iyin=iyin+1
	  if(iyin.gt.nypad)iyin=1
	enddo
	return
	end

