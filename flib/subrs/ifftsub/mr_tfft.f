        subroutine mr_tfft(array,nx,ny,idir)
        dimension idim(5),array(nx,ny)
        dimension anyquist(2050)
c
        nxo2 = nx/2
        nyo2 = ny/2
        if (idir .eq. -1) goto 50
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
          anyquist(iy) = array(1,iy)
          anyquist(iy + ny) = array(2,iy)
100     continue
c
        idim(1) = nx*ny
        idim(2) = nx
        idim(3) = idim(1)
        idim(4) = idim(2)
        idim(5) = 2
        call cmplft(array(1,1),array(2,1),ny,idim)
c
        idim(1) = ny*2
        idim(2) = 2
        idim(3) = idim(1)
        idim(4) = idim(1)
        idim(5) = ny
        call real_ft(anyquist,anyquist(2),nyo2,idim)
c
        do 400 iy = 1,ny
          array(1,iy) = anyquist(iy)
          array(2,iy) = anyquist(iy+ny)
400     continue
c
        return
c
c  inverse transform comes here
c
50	do 500 iy = 1,ny
	  do 500 ix = 4,nx,2
	    array(ix,iy) = -array(ix,iy)
500     continue
        do 550 iy = 1,ny
          anyquist(iy) = array(1,iy)
          anyquist(iy + ny) = array(2,iy)
550     continue
	do 600 iy = 4,ny,2
	  anyquist(iy) = -anyquist(iy)
	  anyquist(iy+ny) = -anyquist(iy+ny)
600     continue
c
        idim(1) = ny*2
        idim(2) = 2
        idim(3) = idim(1)
        idim(4) = idim(1)
        idim(5) = ny
        call hermft(anyquist(1),anyquist(2),nyo2,idim)
c
        idim(1) = nx*ny
        idim(2) = nx
        idim(3) = idim(1)
        idim(4) = idim(2)
        idim(5) = 2
        call cmplft(array(1,1),array(2,1),ny,idim)
c
        do 700 iy = 1,ny
          array(1,iy) = anyquist(iy)
          array(2,iy) = anyquist(iy + ny)
700     continue
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
