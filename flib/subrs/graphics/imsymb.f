c	  Symbol drawing functions, now relying on postscript capabilities
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c
	subroutine imsymb(x,y,ityp)
	common /imparm/ nthick,width,upi,safe,xcur,ycur
     &	    ,udlen,exlen,hafthk,symscl,ifgks
	dimension index(21),vec(3,44)
	real*4 ringFrac /.45/
	data index /1,1,5,5,9,9,12,16,-1,-1,20,20,23,29,43,-2,-1,-1,41,43,41/
	integer*4 iffill(21), ifcircle(21)
	real*4 xvec(4),yvec(4)
	data iffill /0,1,0,1,0,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0/
	data ifcircle /0,0,0,0,0,0,0,0,1,1,0,0,0,0,1,0,1,0,0,0,1/
c	  square, diamond, triangle, X, cross, up tri, U, S, -, |
	data vec /-.5,-.5,-1.,.5,-.5,0.,.5,.5,0.,-.5,.5,1.
     1, -.5,0.,-1., 0.,-.625,0., .5,0.,0., 0.,.625,1.
     2, -.5,-.289,-1., .5,-.289,0., 0.,.577,1.
     3, -.4,-.4,-1., .4,.4,0., -.4,.4,-1., .4,-.4,1.
     4, -.5,0.,-1., .5,0.,0., 0.,-.5,-1., 0.,.5,1.
     5, -.5,.289,-1., .5,.289,0., 0.,-.577,1.
     6, -.38,.5,-1., -.38,-.38,0., -.26,-.5,0., .26,-.5,0., .38,-.38,0.,
     7 .38,.5,1.
     8, .38,.38,-1., .26,.5,0., -.26,.5,0., -.38,.38,0., -.38,.12,0.
     9, -.26,0.,0., .26,0.,0., .38,-.12,0., .38,-.38,0., .26,-.5,0.
     8, -.26,-.5,0., -.38,-.38,1.
     &	    ,-.5,0,-1., .5,0,1.,  0,-.5,-1., 0,.5,1./
	character*4 dummy,dumm2
	if(ityp.eq.0.or.ityp.gt.21)return
	if(ityp.lt.0)then
	  iscal=nint(2**15*0.87*symscl/width)
	  write(dummy,'(i4)')-ityp
	  nchar=alog10(float(-ityp))+1.0001
	  dumm2=' '
	  dumm2(1:nchar)=dummy(5-nchar:4)
	  call wtstr(x+.016,y-.018,dumm2(1:nchar),iscal,0,0)
	  return
	endif
	ind=index(ityp)
	sclvec = symscl
	if (iffill(ityp) .ne. 0) sclvec = symscl + (nthick - 1) / upi
	indvec = 0
	go to (20,20,20,20,20,20,10,10,30,30,20,20,10,10,10,50,40,40
     &	    ,10,10,10),ityp
c
c	  arbitrary path
c
10	xx=x+symscl*vec(1,ind)
	yy=y+symscl*vec(2,ind)
	instr=nint(vec(3,ind))
	if(instr.lt.0)call imma(xx,yy)
	if(instr.ge.0)call imva(xx,yy)
	if(instr.gt.0)then
	  if (ifcircle(ityp) .ne. 0) go to 30
	  return
	endif
	ind=ind+1
	go to 10
c	  
c	  closed path
c	  
20	indvec = indvec + 1
	xvec(indvec) = x + sclvec*vec(1,ind)
	yvec(indvec) = y + sclvec*vec(2,ind)
	instr=nint(vec(3,ind))
	ind=ind+1
	if (instr .le. 0) go to 20
	if (indvec .eq. 3) call pstriangle(xvec, yvec, iffill(ityp))
	if (indvec .eq. 4) call psquadrangle(xvec, yvec, iffill(ityp))
	if (ifcircle(ityp) .ne. 0) go to 30
	return
c	  
c	  circle, open or filled
c
30	call pscircle(x, y, sclvec / 2., iffill(ityp))
	return
c	  
c	  Dot
c
40	call pscircle(x, y, max(2, nthick) / upi, 1)
	if (ifcircle(ityp) .ne. 0) go to 30
	return
c	  
c	  Ring, select a thickness to cover given fraction
c
50	nthsav=nthick
	rscl = 0.5*sclvec
	nthick = nint(ringFrac * rscl * upi) + 1
	call imset(nthick,c1,c2,c3,0)
	call pscircle(x,y,(1-ringFrac / 2.) * rscl, 0)
	call imset(nthsav,c1,c2,c3,0)
	return
	end

	subroutine symsiz(size)
	common /imparm/ nthick,width,upi,safe,xcur,ycur
     &	    ,udlen,exlen,hafthk,symscl,ifgks
	symscl=size
	return
	end
