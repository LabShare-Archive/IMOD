*	  * * * * * * XCORR * * * * * *
*	  
*	  XCORR cross-correlates each of the sections in one image stack with
*	  a single section in a second image file.  A subset of sections may
*	  be done.  Either or both of the input sections may be filtered before
*	  the correlation, and the resulting correlation may also be filtered.
*	  (Note that if you are applying only a single filter, the result will
*	  be the same regardless of where the filter is applied.)
*	  The single image may be smaller than the images in the stack.  In
*	  this case, the single image will be placed in an array of the larger
c	  size, with the MIDDLE of the smaller image moved to the LOWER-LEFT
c	  corner of the larger array.  As a consequence of this shift,
c	  the coordinates of the peaks in the correlation will
c	  correspond to the coordinates of the centers of features in the image
c	  stack that best match the single image.  If the single image and
c	  stack have the same dimensions, then you have a choice as to whether
c	  to keep the single image as it is or to move its middle into the
c	  lower left corner.  Displacements between images would then be
c	  reflected by the positions of peaks in the correlogram relative to
c	  the (0,0) pixel (if the image is not shifted) or relative to the
c	  middle (NX/2,NY/2) pixel (if the image is shifted.)
c	  
c	  Entries in order are:
c	  
C	  Stack input file
C	  Single image input file
C	  Correlation output file
C	  
C	  Starting and ending section #'s (first is 0), or / for all sections
C	  
C	  Filter parameters to prefilter images in stack, or / for no filter
C	  Filter parameters to prefilter the single image, or / for no filter
C	  Filter parameters to postfilter the correlation, or / for no filter
C	  .    (Enter values of Sigma1, Sigma2, Radius1, Radius2 just as for
C	  .       ENHANCE or VIEWFILT)
c	  
c	  IF the single image is smaller than the images in the stack, then
c	  enter 1 to shift the center of the single image to the lower left
c	  corner, or 0 not to.
*	  
c	  David Mastronarde 4/26/89
c
*
	parameter (idim=2100*2100)
	COMMON //NX,NY,NZ,nxs,nys,nzs
C
	DIMENSION NXYZ(3),MXYZ(3),nxyzs(3),mxyzs(3),title(20)
     &	    ,label(20,20)
	real*4 ctfa(8193),ctfb(8193),ctfp(8193)
	complex array(idim/2),brray(idim/2)
C
	EQUIVALENCE (NX,NXYZ),(nxs,nxyzs)
c
	character*80 filbig,filsmall,filout
	character*9 dat
	character*8 tim
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech
	character*70 titstr
	character*7 fltrda/' '/,fltrdb/' '/,fltrdp/' '/
C
C	Open image files.
C
	write(*,'(1x,a,$)')'Stack input file: '
	read(5,50)filbig
	write(*,'(1x,a,$)')'Single image input file: '
        read(5,50)filsmall
	write(*,'(1x,a,$)')'Correlation output file: '
	read(5,50)filout
50	format(A)
c
	CALL IMOPEN(1,filbig,'RO')
	CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C
	IF (((NX+2)*NY.GT.idim)) GOTO 94
C
	CALL IMOPEN(2,filsmall,'RO')
	CALL IRDHDR(2,NXYZS,MXYZS,MODESM,DMINS,DMAXS,DMEANS)
C
	IF (NXs.gt.nx.or.nYs.gt.ny) GOTO 94
c
	CALL IMOPEN(3,filout,'NEW')
c
	CALL ITRHDR(3,1)
c	  
15	izst=0
	iznd=nz-1
	write(*,'(1x,a,$)') 'Starting and ending sections'
     &	    //' to do (first is 0), or / for all: '
	read(*,*)izst,iznd
	if(izst.lt.0.or.iznd.gt.nz-1.or.izst.gt.iznd)go to 15
	nzdo=iznd+1-izst
c	  
	call ialsiz_sam_cel(3,nx,ny,nzdo)
c
	print *,'Enter standard filter parameters to prefilter stack,'
     &	    //' or / for no filter'
	call setctf(ctfa,nx,ny,deltaa)
	print *,'Enter filter parameters to prefilter single image,'
     &	    //' or / for no filter'
	call setctf(ctfb,nx,ny,deltab)
	print *,'Enter filter parameters to filter product,'
     &	    //' or / for no filter'
	call setctf(ctfp,nx,ny,deltap)
c	  
c	  if single image is same size, just load into brray
c	  otherwise, load into array and pad and split it up in brray
c
	if(nxs.eq.nx.and.nys.eq.ny)then
	  ifsplit=0
	  write(*,'(1x,a,$)')'1 to shift middle of single image to'//
     &	      ' lower left corner, 0 not to: '
	  read(5,*)ifsplit
	else
	  ifsplit=1
	endif
	if(ifsplit.eq.0)then
	  call irdpas(2,brray,nx+2,ny,0,nx-1,0,ny-1,*99)
	else
	  call irdsec(2,array,*99)
	  call splitfill(array,nxs,nys,brray,nx+2,nx,ny)
	endif
C	  
c	  get fft of single image, apply filter if desired
c
	call todfft(brray,nx,ny,0)
	if(deltab.ne.0.)call filterpart(brray,brray,nx,ny,ctfb,deltab)
c
	dmsum=0.
	dmax=-1.e10
	dmin=1.e10
        DO KK=izst,iznd
C
c	    print *,'reading section',kk
	  call imposn(1,kk,0)
	  call irdpas(1,array,nx+2,ny,0,nx-1,0,ny-1,*99)
C	    
c	  print *,'taking fft'
	  call todfft(array,nx,ny,0)
	  if(deltaa.ne.0.)call filterpart(array,array,nx,ny,ctfa,deltaa)
c	    
c	    multiply array by complex conjugate of brray, put back in array
c	    
c	  print *,'multiplying arrays'
	  do jx=1,ny*(nx+2)/2
	    array(jx)=array(jx)*conjg(brray(jx))
	  enddo
c
	  if(deltap.ne.0.)call filterpart(array,array,nx,ny,ctfp,deltap)
c	  print *,'taking back fft'
	  call todfft(array,nx,ny,1)
	  
c	  print *,'repack, set density, write'
	  call irepak(array,array,nx+2,ny,0,nx-1,0,ny-1)
	  CALL IsetDN(array,NX,NY,MODE,1,NX,1,NY,DMIN2,DMAX2,DMEAN2)
C
C	    Write out lattice.
C	    
	  CALL IWRSEC(3,array)
C
	  DMAX = max(dmax,dmax2)
	  DMIN = min(dmin,dmin2)
	  DMsum = dmsum + dmean2
	enddo
c
	dmean=dmsum/nzdo
c	  
c	  concatenate labels from both files, limit to 9, add one
c
	CALL IRTLAB(1,LABEL(1,1),NL1)
	CALL IRTLAB(2,LABEL(1,NL1+1),NL2)
	CALL IALLAB(3,LABEL,MIN(9,NL1+NL2))
	CALL DATE(DAT)
	CALL TIME(TIM)
c	
	if(deltaa.ne.0.)fltrda=' fltrd '
	if(deltab.ne.0.)fltrdb=' fltrd '
	if(deltap.ne.0.)fltrdp=', fltrd'
	titstr='XCORR:'//fltrda//'stack correlated with'//fltrdb//
     &	    'image'//fltrdp
c
c 7/7/00 CER: remove the encodes
c
c       ENCODE(80,1500,TITLE) titstr,DAT,TIM
        write(titlech,1500) titstr,dat,tim
        read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
1500	FORMAT(A54,2x,A9,2X,A8)
	CALL IWRHDR(3,TITLE,1,DMIN,DMAX,DMEAN)
        call imclose(3)
	CALL IMCLOSE(2)
	CALL IMCLOSE(1)
C
	WRITE(6,500)
500	FORMAT(' PROGRAM EXECUTED TO END.')
	call exit(0)
99	WRITE(6,450)
450	FORMAT(' END OF IMAGE WHILE READING')
	STOP
94	WRITE (6,660)
660	FORMAT(' Input image too big .')
	STOP
	END




c	  SPLITFILL splits a small image, dimensions NXBOX by NYBOX in ARRAY,
c	  into the 4 corners of a larger array.  The padded image in BRRAY will
c	  have size NX by NY while the BRRAY will be dimensioned NXDIM by NY
c
	subroutine splitfill(array,nxbox,nybox,brray,nxdim,nx,ny)
	real*4 array(nxbox,nybox),brray(nxdim,ny)
c
	if(nxbox.ne.nx.or.nybox.ne.ny)then
c
c	    find mean of edge of box
c
	  sum=0.
	  do ix=1,nxbox
	    sum=sum+array(ix,1)+array(ix,nybox)
	  enddo
	  do iy=2,nybox-1
	    sum=sum+array(1,iy)+array(nxbox,iy)
	  enddo
	  edge=sum/(2*nxbox+2*(nybox-1))
	  call iclden(array,nxbox,nybox,1,nxbox,1,nybox,dmin,dmax,dmean)
c	
	  write(*,'(a,4f10.2)')' min, max, mean, edge:'
     &	      ,dmin,dmax,dmean,edge
c	    
c	    default fill with edge value and subtract a bias that would produce
c	    a mean of zero
c	    
	  fill=edge
	  bias=edge+(dmean-edge)*float(nxbox*nybox)/float(nx*ny)
c	    
	  write(*,'(1x,a,f10.3,a,$)')
     &	      'value to fill rest of array[ / for default=' ,fill,']: '
	  read(*,*)fill
	  write(*,'(1x,a,f10.3,a,$)') 'offset to subtract from whole'//
     &	      ' image[ / for default=',bias,']: '
	  read(*,*)bias
c	    
c	    fill whole brray with fill-bias 
c	  
	  do iy=1,ny
	    do ix=1,nx
	      brray(ix,iy)=fill-bias
	    enddo
	  enddo
	endif
c
c	  move array into brray, splitting it into the 4 corners of brray
c
	ixlo=-nxbox/2
	iylo=-nybox/2
	do iy=1,nybox
	  do ix=1,nxbox
	    ixnew=ix+ixlo
	    iynew=iy+iylo
	    if(ixnew.le.0)ixnew=ixnew+nx
	    if(iynew.le.0)iynew=iynew+ny
	    brray(ixnew,iynew)=array(ix,iy)-bias
	  enddo
	enddo
	return
	end
