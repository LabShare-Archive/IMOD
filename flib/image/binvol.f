c	  BINVOL  bins an image file down by equal amounts in X, Y, and Z
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
*	  
	implicit none
	integer idim
	parameter (idim=4200*4200)
	integer*4 NX,NY,NZ,nxs,nys,nzs
C
	integer*4 NXYZ(3),MXYZ(3),nxyzs(3),nxyzst(3),mode
	real*4 array(idim),brray(idim/4),dmin,dmax,dmean,dmsum,cell(6)
	real*4 dmin2, dmax2, dmean2
	integer*4 izout,iredfac,i,inz, ierr
	data nxyzst/0,0,0/
C
	EQUIVALENCE (NX,NXYZ),(nxs,nxyzs)
	COMMON //NX,NY,NZ,nxs,nys,nzs
c
	character*120 filbig,filout
	character*9 dat
	character*8 tim
	character*70 titstr
	character*80 titlech
	common /bigarr/array,brray
c
	integer*4 numOptArg, numNonOptArg
	integer*4 PipGetInteger
	integer*4 PipGetInOutFile
c	  
c	  fallbacks from ../../manpages/autodoc2man -2 2  binvol
c	  
	integer numOptions
	parameter (numOptions = 4)
	character*(40 * numOptions) options(1)
	options(1) =
     &      'input:InputFile:FN:@output:OutputFile:FN:@'//
     &      'binning:BinningFactor:I:@help:usage:B:'
c
	iredfac = 2
c
	call PipReadOrParseOptions(options, numOptions, 'binvol',
     &	    'ERROR: BINVOL - ', .false., 2, 1, 1, numOptArg,
     &	    numNonOptArg)
	if (PipGetInOutFile('InputFile', 1, ' ', filbig)
     &	    .ne. 0) call errorexit('NO INPUT FILE SPECIFIED')
	if (PipGetInOutFile('InputFile', 2, ' ', filout)
     &	    .ne. 0) call errorexit('NO OUTPUT FILE SPECIFIED')

C
C	Open image files.
C
	CALL IMOPEN(1,filbig,'RO')
	CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C
	IF (NX*NY.GT.idim) call errorexit('INPUT IMAGES TOO LARGE FOR ARRAYS')
C
	CALL IMOPEN(3,filout,'NEW')
c
	CALL ITRHDR(3,1)
c	  
	ierr = PipGetInteger('BinningFactor', iredfac)
	if (iredfac.lt.2) call errorexit('Binning factor must be at least 2')
c	  
	nxs = nx/iredfac
	nys = ny/iredfac
	nzs = nz/iredfac
	call irtcel(1, cell)
	do i = 1,3
	  cell(i) = iredfac*nxyzs(i)*(cell(i)/mxyz(i))
	enddo
	call ialsiz(3,nxs,nxyzst)
	call ialsam(3,nxs)
	call ialcel(3,cell)

	dmsum=0.
	dmax=-1.e10
	dmin=1.e10
	call imposn(1,0,0)
	do izout=1,nzs
	  do i=1,nxs*nys
	    brray(i)=0.
	  enddo
	  do inz=1,iredfac
	    call irdsec(1,array,*99)
	    call add_into_array(array,nx,ny,brray,nxs,nys,iredfac)
	  enddo

	  CALL IclDeN(brray,NXs,NYs,1,NXs,1,NYs,DMIN2,DMAX2,DMEAN2)
	  CALL IWRSEC(3,brray)
C
	  DMAX = max(dmax,dmax2)
	  DMIN = min(dmin,dmin2)
	  DMsum = dmsum + dmean2
	enddo
c
	dmean=dmsum/nzs
	CALL DATE(DAT)
	CALL TIME(TIM)
c	
	titstr='BINVOL: Volume binned down'
c
	write(titlech, 1500) titstr,dat,tim
1500	FORMAT(A54,2x,A9,2X,A8)

	CALL IWRHDRc(3,TITLEch,1,DMIN,DMAX,DMEAN)
        call imclose(3)
	CALL IMCLOSE(1)
C
	WRITE(6,500)
500	FORMAT(' PROGRAM EXECUTED TO END.')
	call exit(0)
99	call errorexit('READING IMAGE')
	END


	subroutine add_into_array(array,nx,ny,brray,nxs,nys,iredfac)
	real*4 array(nx,ny), brray(nxs,nys)
	denom=iredfac**3
	do iys=1,nys
	  do ixs=1,nxs
	    do ix=(ixs-1)*iredfac+1,ixs*iredfac
	      do iy=(iys-1)*iredfac+1,iys*iredfac
		brray(ixs,iys)=brray(ixs,iys)+array(ix,iy)/denom
	      enddo
	    enddo
	  enddo
	enddo
	return
	end


	subroutine errorexit(message)
	character*(*) message
	print *
	print *,'ERROR: BINVOL - ',message
	call exit(1)
	end
