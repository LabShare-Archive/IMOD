c****	  SOLVE_XYZD obtains estimates for the underlying (real) x,y,z
c	  coordinates of each point and for the delta X and Y of each view,
c	  for the given values of tilt, rotation, mag and compression.
c	  It takes advantage of the linear relationship between the projection
c	  x and y coordinates and the x,y,z and dx and dy.  The routine takes
c	  in incoming values of dx and dy, and, for each real point, uses
c	  regression equations to solve for the x,y,z coordinates of that
c	  point.  Then it uses the collection of x,y,z coordinates to compute
c	  the dx and dy values for each section.  This process is repeated up
c	  to NSOLVE times, or until the biggest change in dx or dy becomes
c	  tiny.  The procedure is not guaranteed to converge for large data
c	  sets and is useful only for quickly getting good initial estimates of
c	  the (x,y,z).
c
c	  The equations relating the parameters are:
c	  _    xproj = a*x + b*y + c*z + dx(view)
c	  _    xproj = d*x + e*y + f*z + dy(view)
c	  where a = mag * cos (tilt) * cos (rot)
c	  _     b = - mag * sin (rot)
c	  _     c = mag * comp * sin (tilt) * cos (rot)
c	  where d = mag * cos (tilt) * sin (rot)
c	  _     e = mag * cos (rot)
c	  _     f = mag * comp * sin (tilt) * sin (rot)
c
	subroutine solve_xyzd(xx,yy,isecview,irealstr, nview,nrealpt,
     &	    tilt,rot,gmag,comp,xyz,dxy,nsolve,error,erlist,isolve)
c	
	real*4 xx(*),yy(*),tilt(*),rot(*),gmag(*),comp(*)
     &	    ,xyz(3,*),dxy(2,*)
	integer*4 isecview(*),irealstr(*)
	double precision error
c
	parameter (maxview=490,ms=maxview)
	real*4 bvec(3)
	real*4 a(ms),b(ms),c(ms),asq(ms),bsq(ms),csq(ms)
     &	    ,axb(ms),axc(ms),bxc(ms),d(ms),e(ms),f(ms)
c	
	real*4 erlist(*),dxsum(maxview),dysum(maxview)
     &	    ,dxsqsum(maxview),dysqsum(maxview)
	integer*4 nsum(maxview)
	real*4 deldxylas(2,ms),dxylas(2,ms)
c	
c	  precompute the a-f and relevant cross-products for the regression
c
	iwatch =0
c	if(iwatch.eq.0)iwatch=1
	do i=1,nview
	  costhet=cos(tilt(i))
	  csinthet=comp(i)*sin(tilt(i))
	  gcosphi=gmag(i)*cos(rot(i))
	  gsinphi=gmag(i)*sin(rot(i))
c
	  a(i)=costhet*gcosphi
	  b(i)=-gsinphi
	  c(i)=csinthet*gcosphi
	  d(i)=costhet*gsinphi
	  e(i)=gcosphi
	  f(i)=csinthet*gsinphi
c
c	    the square and cross product terms for the x and y paired
c	    observations are always added together - so just combine them here
c
	  asq(i)=a(i)**2+d(i)**2
	  bsq(i)=b(i)**2+e(i)**2
	  csq(i)=c(i)**2+f(i)**2
	  axb(i)=a(i)*b(i)+d(i)*e(i)
	  axc(i)=a(i)*c(i)+d(i)*f(i)
	  bxc(i)=b(i)*c(i)+e(i)*f(i)
	enddo
c	
c	  start looping until nsolve times, or diffmax gets small, or error
c	  really blows up a lot from first iteration
c
	isolve=1
	diffmax=1.
	jumpfast=0
	jumpbase=3
	ncyskplim=5
	do while(isolve.le.nsolve.and.diffmax.gt.1.e-7.and.
     &	    (isolve.le.3.or.erlist(max(1,isolve-1)).le.1000.*erlist(1)))
c
c	    zero arrays for sums of xd,dy and squares for each view
c
	  do iv=1,nview
	    dxsum(iv)=0.
	    dysum(iv)=0.
	    dxsqsum(iv)=0.
	    dysqsum(iv)=0.
	    nsum(iv)=0
	  enddo
c	  
c	    loop on each real point
c
	  do jpt=1,nrealpt
	    if(jpt.lt.nrealpt)then
c
c		zero the matrix elements for sums of squares and cross-products
c
	    am11=0.
	    am12=0.
	    am13=0.
	    am22=0.
	    am23=0.
	    am33=0.
	    bv1=0.
	    bv2=0.
	    bv3=0.
c
c	      for each projection of the real point, add terms to matrix
c
	    do i=irealstr(jpt),irealstr(jpt+1)-1
	      iv=isecview(i)
	      xpr=xx(i)-dxy(1,iv)
	      ypr=yy(i)-dxy(2,iv)
	      am11=am11+asq(iv)
	      am22=am22+bsq(iv)
	      am33=am33+csq(iv)
	      am12=am12+axb(iv)
	      am13=am13+axc(iv)
	      am23=am23+bxc(iv)
	      bv1=bv1+a(iv)*xpr+d(iv)*ypr
	      bv2=bv2+b(iv)*xpr+e(iv)*ypr
	      bv3=bv3+c(iv)*xpr+f(iv)*ypr
	    enddo
c
c	      fill out lower triangle of matrix
c
	    am31=am13
	    am21=am12
	    am32=am23
c
c	      solve it
c
	    det=am11*(am22*am33-am23*am32)-am12*(am21*am33-am23*am31)
     &		+am13*(am21*am32-am22*am31)
	    bvec(1)=(bv1*(am22*am33-am23*am32)-am12*(bv2*am33-am23*bv3)
     &		+am13*(bv1*am32-am22*bv3))/det
	    bvec(2)=(am11*(bv2*am33-am23*bv3)-bv1*(am21*am33-am23*am31)
     &		+am13*(am21*bv3-bv2*am31))/det
	    bvec(3)=(am11*(am22*bv3-bv2*am32)-am12*(am21*bv3-bv2*am31)
     &		+bv1*(am21*am32-am22*am31))/det
	  else
c	      
c	      get coordinates of last point as minus sum of all other points
c
	    do i=1,3
	      bsum=0.
	      do kpt=1,nrealpt-1
		bsum=bsum-xyz(i,kpt)
	      enddo
	      bvec(i)=bsum
	    enddo
	  endif
c
c	    store new x,y,z
c
	    do i=1,3
	      xyz(i,jpt)=bvec(i)
	    enddo
c
c	      for each view that point is projected into, add its contribution
c	      to the dx and dy differences in that view
c
	    do i=irealstr(jpt),irealstr(jpt+1)-1
	      iv=isecview(i)
	      dxtmp=xx(i)-(a(iv)*bvec(1)+b(iv)*bvec(2)+c(iv)*bvec(3))
	      dytmp=yy(i)-(d(iv)*bvec(1)+e(iv)*bvec(2)+f(iv)*bvec(3))
	      dxsum(iv)=dxsum(iv)+dxtmp
	      dxsqsum(iv)=dxsqsum(iv)+dxtmp**2
	      dysum(iv)=dysum(iv)+dytmp
	      dysqsum(iv)=dysqsum(iv)+dytmp**2
	      nsum(iv)=nsum(iv)+1
	    enddo
c
	  enddo
c
c	    set new dx and dy equal to the average dx and dy for each view,
c	    and compute total error using the sums of squares gotten above
c
	  error=0.
	  diffmax=0.
	  do iv=1,nview
	    dxnew=dxsum(iv)/nsum(iv)
	    dynew=dysum(iv)/nsum(iv)
	    deldx=dxnew-dxy(1,iv)
	    deldy=dynew-dxy(2,iv)
	    diffmax=max(diffmax,abs(deldx),abs(deldy))
	    dxy(1,iv)=dxnew
	    dxy(2,iv)=dynew
C	      
c	      save info to make possible big jumps in dx and dy next time round
c
	    if(jumpfast.eq.1)then
	      deldxylas(1,iv)=deldx
	      deldxylas(2,iv)=deldy
	      dxylas(1,iv)=dxy(1,iv)
	      dxylas(2,iv)=dxy(2,iv)
	    endif
	    error=error+dxsqsum(iv)+dysqsum(iv)
     &		-nsum(iv)*(dxy(1,iv)**2+dxy(2,iv)**2)
	  enddo
c
c	    stick error on list, calculate difference from last error
c
	  erlist(isolve)=error
	  if(isolve.gt.1)errdiff=abs(erlist(isolve)-erlist(isolve-1))
     &	      /max(erlist(isolve),erlist(isolve-1))
	  isolve=isolve+1
c	    
c	    try to make big jumps in dx or dy; even this stuff doesn't make big
c	    data sets converge adequately
c
	  if(jumpfast.eq.2)then
	    do iv=1,nview
	      do i=1,2
		ncyskip=1
		del2=dxy(i,iv)-dxylas(i,iv)
		del1=deldxylas(i,iv)
		if(del2*del1.gt.0.and.abs(del2).lt.abs(del1))then
		  ncyskip=ncyskplim
		  if(abs(del1-del2).gt.abs(del1)/
     &		      ncyskplim)ncyskip=del1/(del1-del2)
		endif
		delnew=ncyskip*del1+(del2-del1)*ncyskip*(ncyskip+1)/2
		dxy(i,iv)=dxylas(i,iv)+delnew
	      enddo
	    enddo
	  endif
c
	  if(isolve.gt.jumpbase)jumpfast=mod(jumpfast+1,3)
c
	  if(iwatch.gt.0)then
	    deldx=dxy(1,iwatch)-dxlas
	    deldy=dxy(2,iwatch)-dylas
	    write(*,'(2i4,4f10.4,f20.5)')iwatch,isolve,dxy(1,iwatch)
     &		,deldx,dxy(2,iwatch),deldy,error
	    dxlas=dxy(1,iwatch)
	    dylas=dxy(2,iwatch)
	  endif
	enddo
c
c	  after convergence, recompute error de novo: this may give more
c	  accurate double precision result, or may be superfluous

	error=0.
	do jpt=1,nrealpt
	  do i=irealstr(jpt),irealstr(jpt+1)-1
	    iv=isecview(i)
	    error=error+(a(iv)*xyz(1,jpt)+b(iv)*xyz(2,jpt)
     &		+c(iv)*xyz(3,jpt)+dxy(1,iv)-xx(i))**2
     &		+(d(iv)*xyz(1,jpt)+e(iv)*xyz(2,jpt)
     &		+f(iv)*xyz(3,jpt)+dxy(2,iv)-yy(i))**2
	  enddo
	enddo
c
	erlist(isolve)=error
c	write(*,'(4f20.5)')(erlist(i),i=1,isolve)
	if(iwatch.gt.0)iwatch=mod(iwatch,nview)+1
	return
	end



c***	  INIT_DXY sets the initial values of dx and dy for each view:
c	  it sets dx and dy to 0 for the view with minimum tilt (IMINTILT),
c	  then sets the dx and dy of each view so that the points shared
c	  between adjacent views have the same center of gravity.
c
	subroutine init_dxy(xx,yy,isecview,irealstr,
     &	    nview,nrealpt,imintilt,dxy)
	real*4 xx(*),yy(*),dxy(2,*)
	integer*4 isecview(*),irealstr(*)
c
c	  consider each pair of views from the one with min tilt out to ends
c
	dxy(1,imintilt)=0.
	dxy(2,imintilt)=0.
	nvstr=imintilt
	nvend=2
	do idir=-1,1,2
	  do iv=nvstr,nvend,idir
c
c	      find each real point in both views, add up disparities
c
	    dxsum=0.
	    dysum=0.
	    nsum=0
	    do ipt=1,nrealpt
	      inone=0
	      intwo=0
	      do i=irealstr(ipt),irealstr(ipt+1)-1
		if(isecview(i).eq.iv+idir)inone=i
		if(isecview(i).eq.iv)intwo=i
	      enddo
	      if(inone*intwo.gt.0)then
		dxsum=dxsum+xx(intwo)-xx(inone)
		dysum=dysum+yy(intwo)-yy(inone)
		nsum=nsum+1
	      endif
	    enddo
c
c	      adjust this section's dx,dy by the disparity
c
	    dxy(1,iv+idir)=dxy(1,iv)+dxsum/nsum
	    dxy(2,iv+idir)=dxy(2,iv)+dysum/nsum
	  enddo
	  nvend=nview-1
	enddo
	return
	end
