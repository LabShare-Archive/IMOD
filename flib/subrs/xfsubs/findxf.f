c	  FINDXF finds a transformation from a set of corresponding points
c	  that have been loaded into the matrix XR, independent variable values
c	  of X and Y in columns 1 and 2, dependent variable values of X and Y
c	  in columns 4 and 5 (column 3 will be used for each in turn.)
c	  NPNTS is the number of points
c	  XCEN, YCEN should contain the center coordinates that have been
c	  subtracted from the X and Y point data.
c	  IFTRANS and IFROTRANS should be 0 to get a general linear transform
c	  IFTRANS should be 1 to solve for translation only
c	  IFROTRANS should be 1 to solve for rotations and translations, or
c	  2 to solve for magnification also
c	  IFDEV should be 0 for no deviation information, 1 or 2 for deviations
c	  F(2,3) is returned with the transform
c	  If IFDEV is non-zero, DEVAVG, DEVSD, DEVMAX, and IPNTMAX are returned
c	  with the mean, SD, and maximum deviation and the point number at
c	  which the maximum occurs, and columns 10 and 11, and 13 are filled
c	  with the deviation in X and Y and total deviation.
c	  If IFDEV is 2, columns 8 and 9 are filled with the coordinates with
c	  XCEN and YCEN added back, and column 12 has the angle of the
c	  deviation
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 1.1  2004/06/21 23:03:44  mast
c	  Took out of beadtrack, added to library, to avoid duplication code
c	  with xfmodel
c	
c
	subroutine findxf(xr,npnts,xcen,ycen,iftrans,ifrotrans,ifdev,f,devavg,
     &	    devsd,devmax,ipntmax)
	implicit none
	include 'statsize.inc'
	integer*4 npnts,iftrans,ifrotrans,ifdev,ipntmax
	real*4 f(2,3), devavg,devsd,devmax,xcen,ycen
	real*4 xr(msiz,*), sx(msiz), xm(msiz), sd(msiz), ss(msiz,msiz)
	real*4 ssd(msiz,msiz), d(msiz,msiz), r(msiz,msiz), b(msiz), b1(msiz)
	integer*4 isol,i,ipnt
	real*4 const, rsq, fra, theta,sinth,costh,gmag,devsum,devsumsq,xdev
	real*4 ydev,devpnt,xx,yy
	real*4 atan2d
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
	  xr(10,ipnt)=xdev
	  xr(11,ipnt)=ydev
	  xr(13,ipnt)=devpnt
	  devsum=devsum+devpnt
	  devsumsq=devsumsq+devpnt**2
	  if(devpnt.gt.devmax)then
	    devmax=devpnt
	    ipntmax=ipnt
	  endif
c	    
c	    save true, not-centered coordinate and the angle of deviation if
c	    ifdev > 1
c
	  if (ifdev .gt. 1) then
	    xr(8,ipnt)=xr(4,ipnt)+xcen
	    xr(9,ipnt)=xr(5,ipnt)+ycen
	    if(abs(xdev).gt.1.e-6.and.abs(ydev).gt.1.e-6)then
	      xr(12,ipnt)=atan2d(ydev,xdev)
	    else
	      xr(12,ipnt)=0.
	    endif
	  endif
	enddo
	call sums_to_avgsd(devsum,devsumsq,npnts,devavg,devsd)
	return
	end
