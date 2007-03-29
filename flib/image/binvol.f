c	  BINVOL  bins an image file down by equal amounts in X, Y, and Z
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.5  2007/02/02 18:49:33  mast
c	  Fixed option specification for output file; it was overwriting input
c	
c	  Revision 3.4  2006/07/11 19:28:18  mast
c	  Set up to write in chunks too to work on really big images
c	
c	  Revision 3.3  2005/04/13 23:50:06  mast
c	  Set up to read in chunks, so it only needs one large array
c	
c	  Revision 3.2  2005/04/13 05:31:02  mast
c	  Made binning anisotropic
c	
c	  Revision 3.1  2004/10/28 22:22:49  mast
c	  Added to package
c	
*	  
	implicit none
	integer idim
	parameter (idim=3500*3500)
	integer*4 NX,NY,NZ,nxs,nys,nzs
C
	integer*4 NXYZ(3),MXYZ(3),nxyzs(3),nxyzst(3),mode
	real*4 array(idim),dmin,dmax,dmean,cell(6)
	real*4 dmin2, dmax2, dmean2
        real*8 dmsum, pixSum
	integer*4 izout,iredfac(3),iredall,i,inz, ierr, isec, maxLines, inbase
	integer*4 numChunks,iy,numLines, nReduced,nBinX,nBinY,nBinZ,iChunk
        integer*4 limit, ifLimSet
	data nxyzst/0,0,0/
C
	EQUIVALENCE (NX,NXYZ),(nxs,nxyzs),(nBinX,iredfac)
	COMMON //NX,NY,NZ,nxs,nys,nzs,nBinX,nBinY,nBinZ
c
	character*160 filbig,filout
	character*9 dat
	character*8 tim
	character*80 titlech
	common /bigarr/array
c
	integer*4 numOptArg, numNonOptArg
	integer*4 PipGetInteger
	integer*4 PipGetInOutFile
c	  
c	  fallbacks from ../../manpages/autodoc2man -2 2  binvol
c	  
        integer numOptions
        parameter (numOptions = 8)
        character*(40 * numOptions) options(1)
        options(1) =
     &      'input:InputFile:FN:@output:OutputFile:FN:@'//
     &      'binning:BinningFactor:I:@xbinning:XBinningFactor:I:@'//
     &      'ybinning:YBinningFactor:I:@zbinning:ZBinningFactor:I:@'//
     &      'test:TestLimit:I:@help:usage:B:'
c
	iredall = 2
        limit = idim
c
	call PipReadOrParseOptions(options, numOptions, 'binvol',
     &	    'ERROR: BINVOL - ', .false., 2, 1, 1, numOptArg,
     &	    numNonOptArg)
	if (PipGetInOutFile('InputFile', 1, ' ', filbig)
     &	    .ne. 0) call exitError('NO INPUT FILE SPECIFIED')
	if (PipGetInOutFile('OutputFile', 2, ' ', filout)
     &	    .ne. 0) call exitError('NO OUTPUT FILE SPECIFIED')

c	  
	ierr = PipGetInteger('BinningFactor', iredall)
	nBinX = iredall
	nBinY = iredall
	nBinZ = iredall
	ierr = PipGetInteger('XBinningFactor', nBinX)
	ierr = PipGetInteger('YBinningFactor', nBinY)
	ierr = PipGetInteger('ZBinningFactor', nBinZ)
        ifLimSet = 1 - PipGetInteger('TestLimit', limit)
        limit = min(limit, idim)
C
C	Open image files.
C
	CALL IMOPEN(1,filbig,'RO')
	CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
        if (nz .eq. 1) nBinZ = 1
C
	CALL IMOPEN(3,filout,'NEW')
c
	CALL ITRHDR(3,1)
c	  
	call irtcel(1, cell)
	do i = 1,3
	  if (iredfac(i).lt.1) call exitError(
     &	      'Binning factor must be at least 1')
	  nxyzs(i) = nxyz(i) / iredfac(i)
	  if (nxyzs(i) .lt. 1) call exitError('Binning factor too large')
	  cell(i) = iredfac(i)*nxyzs(i)*(cell(i)/mxyz(i))
	enddo
C
	maxLines = nBinY * ((limit / (nx + (nxs + nBinY - 1)/ nBinY))
     &      / nBinY - 1)
	IF (maxLines .lt. 1) call exitError('INPUT'
     &	    //' IMAGES TOO LARGE FOR ARRAYS WITH GIVEN BINNING FACTORS')
	call ialsiz(3,nxyzs,nxyzst)
	call ialsam(3,nxyzs)
	call ialcel(3,cell)

	dmsum=0.
	dmax=-1.e30
	dmin=1.e30
        pixSum = 0.

        inbase = nxs * (maxLines / nBinY)
	numChunks = (ny + maxLines - 1) / maxLines
        if (ifLimSet .ne. 0) print *,numChunks,' chunks, maximum lines',
     &      maxLines,inbase
	do izout=0, nzs - 1
          iy = 0
          do iChunk = 1, numChunks
            numLines = min(maxLines, ny - (iChunk - 1) * maxLines)
            nReduced = numLines / nBinY
            do i=1,inbase
              array(i)=0.
            enddo
            isec = izout * nBinZ
            do inz=1,nBinZ
              call imposn(1,isec,iy)
              isec = isec + 1
	      call irdsecl(1,array(inbase + 1), numLines, *99) 

	      call add_into_array(array(inbase + 1),nx,numLines,
     &		  array,nxs,nReduced,iredfac)
	    enddo
            iy = iy + numLines

            CALL IclDeN(array,NXs,nReduced,1,NXs,1,nReduced,DMIN2,DMAX2,DMEAN2)
            CALL IWRSECL(3,array, nReduced)
C             
            DMAX = max(dmax,dmax2)
            DMIN = min(dmin,dmin2)
            DMsum = dmsum + dmean2 * nReduced * nxs
            pixSum = pixSum + nReduced * nxs
	  enddo
	enddo
c
	dmean=dmsum/pixSum
	CALL DATE(DAT)
	CALL TIME(TIM)
c	
	write(titlech, 1500) nBinX, nBinY, nBinZ, dat,tim
1500	FORMAT('BINVOL: Volume binned down by factors', 3i4, t57,A9,2X,A8)

	CALL IWRHDRc(3,TITLEch,1,DMIN,DMAX,DMEAN)
        call imclose(3)
	CALL IMCLOSE(1)
C
	WRITE(6,500)
500	FORMAT(' PROGRAM EXECUTED TO END.')
	call exit(0)
99	call exitError('READING IMAGE')
	END


	subroutine add_into_array(array,nx,ny,brray,nxs,nys,iredfac)
	implicit none
	integer*4 iredfac(3), ixs, iys, ix, iy, nx, ny, nxs, nys
	real*4 array(nx,ny), brray(nxs,nys), denom
	denom=iredfac(1) * iredfac(2) * iredfac(3)
	do iys=1,nys
	  do ixs=1,nxs
	    do ix=(ixs-1)*iredfac(1)+1,ixs*iredfac(1)
	      do iy=(iys-1)*iredfac(2)+1,iys*iredfac(2)
		brray(ixs,iys)=brray(ixs,iys)+array(ix,iy)/denom
	      enddo
	    enddo
	  enddo
	enddo
	return
	end
