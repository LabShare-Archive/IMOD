c*mrtfft.for**************************************************************
c
c	this is a set of mixed-radix 2D FFT routines based on Lynn Ten Eyck's
c	1-D routines that will either pack
c	the nyquist data so that both input/output sizes are NX,NY
c	uses FPS packed FFT format
c    or
c	will allow the nyquist data to be stored in a separate array
c	NOTE: the data is NOT scaled - need to use 1./(nx*ny)
c
c	      both ARRAY and ANYQUIST are overwritten during operation
c	
c  calls are:
c
c	mr_tfft(array,nx,ny,idim)
c	mr_tfft_nyq(array,anyquist,nx,ny,idim)
c
c	ARRAY:		nx,ny  data array for either image or FT
c	NX,NY:		sizes of image and arrays
c	IDIM:		0 for forward transform, 1 for inverse
c	ANYQUIST:	ny+2 long vector to contain nyquist strip (x=.5)
c			y runs from 0 to .5 as complex variables
c
c
c	version 1.00	18.march.87		daa for VAX/FPSx64
c****************************************************************************
c
        subroutine mr_tfft_nyq(array,anyquist,nx,ny,idir)
        dimension idim(5),array(nx,ny)
        dimension anyquist(ny+2)
c
        nxo2 = nx/2
        nyo2 = ny/2
	nyp2 = ny + 2
        if (idir .eq. 1) goto 50
c
c  foward transform comes here
c
        idim(1) = nx*ny
        idim(2) = 2
        idim(3) = idim(1)
        idim(4) = idim(1)
        idim(5) = nx
c
        call real_ft(array(1,1),array(2,1),nxo2,idim)
c
        do 100 iy = 1,ny
          anyquist(iy) = array(2,iy)
          array(2,iy) = 0.0
100     continue
c
        idim(1) = nx*ny
        idim(2) = nx
        idim(3) = idim(1)
        idim(4) = idim(2)
        idim(5) = 2
        call cmplft(array(1,1),array(2,1),ny,idim)
        do 300 iy = 1,ny
          do 200 ix = 2,nx,2
            array(ix,iy) = -array(ix,iy)
200       continue
300     continue
c
        idim(1) = nyp2
        idim(2) = 2
        idim(3) = idim(1)
        idim(4) = idim(1)
        idim(5) = nyp2
        call realft(anyquist,anyquist(2),nyo2,idim)
c
        do 400 iy = 4,ny,2
          anyquist(iy) = -anyquist(iy)
400     continue
        anyquist(ny+1) = anyquist(2)
	anyquist(2) = 0.0
	anyquist(nyp2) = 0.0
c
        return
c
c  inverse transform comes here
c
50      idim(1) = ny
        idim(2) = 2
        idim(3) = idim(1)
        idim(4) = idim(1)
        idim(5) = ny
c
        anyquist(2) = anyquist(ny+1)
        call hermft(anyquist(1),anyquist(2),nyo2,idim)
c
        idim(1) = nx*ny
        idim(2) = nx
        idim(3) = idim(1)
        idim(4) = idim(2)
        idim(5) = 2
        call cmplft(array(1,1),array(2,1),ny,idim)
c
        do 500 iy = 1,ny
          array(2,iy) = anyquist(iy)
500     continue
c
        idim(1) = nx*ny
        idim(2) = 2
        idim(3) = idim(1)
        idim(4) = idim(1)
        idim(5) = nx
        call hermft(array(1,1),array(2,1),nxo2,idim)
c
        return
        end
