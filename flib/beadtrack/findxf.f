	subroutine findxf(xr,npnts,iftrans,ifrotrans,ifdev,f,devavg,
     &	    devsd,devmax,ipntmax)
	include 'statsize.inc'
	real*4 xr(msiz,*), sx(msiz), xm(msiz), sd(msiz)
     1	    , ss(msiz,msiz), ssd(msiz,msiz), d(msiz,msiz), r(msiz,msiz)
     2	    , b(msiz), b1(msiz)
	real*4 f(2,3)
c	  
	call xfunit(f,1.)
	if(ifrotrans.eq.0)then
	  do isol=1,2
	    if(iftrans.eq.0)then
c		
c		move 4th or 5th column into 3rd for regression
c		
	      do i=1,npnts
		xr(3,i)=xr(3+isol,i)
	      enddo
	      call multr(xr,3,npnts,sx,ss,ssd,d,r,xm,sd,b,b1,
     &		  const, rsq ,fra)
	      f(isol,1)=b1(1)
	      f(isol,2)=b1(2)
	    else
c		
c		or, if getting translations only, find difference in
c		means for X or Y
c		
	      const=0.
	      do i=1,npnts
		const=const+xr(3+isol,i)-xr(isol,i)
	      enddo
	      const=const/npnts
	    endif
	    f(isol,3)=const
	  enddo
	else
c	    
c	    or find rotations and translations only by getting means
c	    and sums of squares and cross-products of deviations
c	    
	  call correl(xr,5,npnts,sx,ss,ssd,d,r,xm,sd,1)
	  theta=atan(-(ssd(2,4)-ssd(1,5))/(ssd(1,4)+ssd(2,5)))
	  sinth=sin(theta)
	  costh=cos(theta)
	  if(ifrotrans.eq.2)then
	    gmag=((ssd(1,4)+ssd(2,5))*costh
     &		-(ssd(2,4)-ssd(1,5))*sinth)/(ssd(1,1)+ssd(2,2))
	    sinth=sinth*gmag
	    costh=costh*gmag
	  endif
	  f(1,1)=costh
	  f(1,2)=-sinth
	  f(2,1)=sinth
	  f(2,2)=costh
	  f(1,3)=xm(4)-xm(1)*costh+xm(2)*sinth
	  f(2,3)=xm(5)-xm(1)*sinth-xm(2)*costh
	endif
	if(ifdev.eq.0)return
c	  
c	  compute mean and max deviation between points in adjacent
c	  sections after the transformation is applied
c	  
	devsum=0.
	devsumsq=0.
	devmax=-1.
	do ipnt=1,npnts
	  call xfapply(f,0.,0.,xr(1,ipnt),xr(2,ipnt),xx,
     &	      yy)
	  xdev=xr(4,ipnt)-xx
	  ydev=xr(5,ipnt)-yy
	  devpnt=sqrt(xdev**2+ydev**2)
c	  xr(8,ipnt)=xr(4,ipnt)+xcen
c	  xr(9,ipnt)=xr(5,ipnt)+ycen
	  xr(10,ipnt)=xdev
	  xr(11,ipnt)=ydev
c	  if(abs(xdev).gt.1.e-6.and.abs(ydev).gt.1.e-6)then
c	    xr(12,ipnt)=atan2d(ydev,xdev)
c	  else
c	    xr(12,ipnt)=0.
c	  endif
	  xr(13,ipnt)=devpnt
	  devsum=devsum+devpnt
	  devsumsq=devsumsq+devpnt**2
	  if(devpnt.gt.devmax)then
	    devmax=devpnt
	    ipntmax=ipnt
	  endif
	enddo
	call sums_to_avgsd(devsum,devsumsq,npnts,devavg,devsd)
	return
	end
