	subroutine iclscl(obs,nx,ny,ix1,ix2,iy1,iy2,scl,off,omin,
     .	omax,dmin,dmax,dmean)
	dimension obs(nx,ny)
	dmin = 1.e20
	dmax = -1.e20
	dme4an = 0.0
	do iy = iy1,iy2
	  do ix = ix1,ix2
	    obs(ix,iy) = obs(ix,iy)*scl + off
	    if (obs(ix,iy) .lt. omin) obs(ix,iy) = omin
	    if (obs(ix,iy) .gt. omax) obs(ix,iy) = omax
	    if (obs(ix,iy) .lt. dmin) dmin = obs(ix,iy)
	    if (obs(ix,iy) .gt. dmax) dmax = obs(ix,iy)
	    dmean = dmean + obs(ix,iy)
	  enddo
	enddo
	dmean = dmean/((ix2-ix1+1)*(iy2-iy1+1))
	return
	end	  	   
