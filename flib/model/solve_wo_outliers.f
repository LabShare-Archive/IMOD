c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.1  2002/07/27 16:18:31  mast
c	  Increased dimension maxerr to 10000 so that it will work
c	  on large sets with Refinematch
c	

	subroutine solve_wo_outliers(xr,ndat,ncol,icolfix,maxdrop,
     &	    critprob, critabs, elimmin, idrop,ndrop,a,dxyz,cenloc,
     &	    devavg,devsd, devmax, ipntmax, devxyzmax)
	implicit none
        include 'statsize.inc'
	integer maxerr
	parameter (maxerr = 10000)
	integer*4 idrop(*)
        real*4 xr(msiz,*)
        real*4 a(3,*),dxyz(3),devxyz(3),devxyzmax(3),cenloc(3),centmp(3)
	integer*4 index(maxerr)
	integer*4 ndat,ncol,maxdrop,ndrop,ipntmax,icolfix
	real*4 critprob,critabs,elimmin,devavg,devsd,devmax
	integer*4 i,j,lastdrop,itmp,nkeep,jdrop
	real*4 probperpt,absperpt,sigfromavg,sigfromsd,sigma,z,prob
	real*4 erfcc,gprob
	gprob(z)=1.-0.5*erfcc(z/1.414214)
c	  
c	  get probability per single point from the overall criterion prob
c
	probperpt=(1.-critprob)**(1./ndat)
	absperpt=(1.-critabs)**(1./ndat)
c	  
c	  copy the data into columns 8-14
c
	do i=1,ndat
	  do j=1,ncol+4
	    xr(j+ncol+4,i)=xr(j,i)
	  enddo
	enddo

	call do3multr(xr,ndat,ncol,ndat,icolfix,a,dxyz,cenloc,devavg,
     &	    devsd, devmax, ipntmax, devxyzmax)
	ndrop = 0
	if(maxdrop.eq.0.or.devmax.lt.elimmin) return
c	  
c	  order the residuals
c
	lastdrop=0
	if (ndat.gt.maxerr) then
	  print *, 'CANNOT FIND OUTLIERS: ARRAYS NOT LARGE ENOUGH'
	  return
	endif
c	  
c	  Sort the residuals and keep index back to initial values
c	  
	do i=1,ndat
	  index(i)=i
	enddo
	do i=1,ndat-1
	  do j=i+1,ndat
	    if(xr(ncol+1,index(i)).gt.xr(ncol+1,index(j)))then
	      itmp=index(i)
	      index(i)=index(j)
	      index(j)=itmp
	    endif
	  enddo
	enddo
c	  
c	  load the data in this order
c
	do i=1,ndat
	  do j=1,ncol+4
	    xr(j,i)=xr(j+ncol+4,index(i))
	  enddo
	enddo
c	  
c	  Drop successively more points: get mean and S.D. of the remaining
c	  points and check how many of the points pass the criterion 
c	  for outliers.  
c	  
	do jdrop = 1, maxdrop+1
c	  
c	  reload the data if there is a fixed column
c	    
	  if (jdrop.gt.1.and.icolfix.gt.0)then
	    do i=1,ndat
	      do j=1,ncol+4
		xr(j,i)=xr(j+ncol+4,index(i))
	      enddo
	    enddo
	  endif
c
	  call do3multr(xr,ndat,ncol,ndat-jdrop,icolfix,a,dxyz,centmp,
     &	      devavg, devsd, devmax, ipntmax, devxyzmax)

c	    
c	    estimate the sigma for the error distribution as the maximum of
c	    the values implied by the mean and the SD of the deviations
c	    
	  sigfromavg=devavg/sqrt(8./3.14159)
	  sigfromsd=devsd/sqrt(3.-8./3.14159)
	  sigma=max(sigfromavg,sigfromsd)

	  nkeep=0
	  do j=ndat-jdrop+1,ndat
	    z=xr(ncol+1,j)/sigma
	    prob=2*(gprob(z)-0.5)-sqrt(2./3.14159)*z*exp(-z**2/2.)
	    if(prob.lt.probperpt)nkeep=nkeep+1
	    if(prob.ge.absperpt) ndrop=min(maxdrop,max(ndrop,ndat+1-j))
	  enddo
c	    
c	    If all points are outliers, this is a candidate for a set to drop
c	    When only the first point is kept, and all the rest of the points
c	    were outliers on the previous round, then this is a safe place to
c	    draw the line between good data and outliers.  In this case, set
c	    ndrop; and at end take the biggest ndrop that fits these criteria
c	    
	  if(nkeep.eq.0)lastdrop=jdrop
	  if(nkeep.eq.1.and.lastdrop.eq.jdrop-1.and.lastdrop.gt.0)
     &	      ndrop=lastdrop
c	  print *,'drop',jdrop,', keep',nkeep,', lastdrop',lastdrop,
c     &	      ',  ndrop =',ndrop
	enddo
c	  
c	  when finish loop, need to redo with right amount of data
c	  and reload the data if there is a fixed column
c	    
	if (icolfix.gt.0)then
	  do i=1,ndat
	    do j=1,ncol+4
	      xr(j,i)=xr(j+ncol+4,index(i))
	    enddo
	  enddo
	endif
	do i=1,ndrop
	  idrop(i)=index(ndat+i-ndrop)
	enddo
	call do3multr(xr,ndat,ncol,ndat-ndrop,icolfix,a,dxyz,centmp,
     &	    devavg, devsd, devmax, ipntmax, devxyzmax)
	ipntmax=index(ipntmax)
	return
	end



	subroutine do3multr(xr,ndat,ncolin,ndo,icolfix,a,dxyz,cenloc,
     &	    devavg,devsd, devmax, ipntmax, devxyzmax)
	implicit none
        include 'statsize.inc'
        real*4 xr(msiz,*), sx(msiz), xm(msiz), sd(msiz)
     1      , ss(msiz,msiz), ssd(msiz,msiz), d(msiz,msiz), r(msiz,msiz)
     2      , b(msiz), b1(msiz)
        real*4 a(3,*),dxyz(3),devxyz(3),devxyzmax(3),cenloc(3)
	integer*4 ndat,ncolin,ndo,ipntmax,icolfix
	real*4 devavg,devsd,devmax
	integer*4 ixyz,i,j,ipnt,ncoldo
	real*4 const,rsq,fra,devsum,devsq,devpnt

c	  
c	  if one column is fixed, decrement the number of columns to do
c	  subtract that column's independent var from its dependent var
c	  and pack the dependent vars into the smaller number of columns
c
	ncoldo = ncolin
	if (icolfix.ne.0) then
	  ncoldo = ncolin - 1
	  do i = 1,ndat
	    xr(ncolin + 1 + icolfix, i) = xr(ncolin + 1 + icolfix, i) -
     &		xr(icolfix,i)
	    do j = icolfix,ncoldo
	      xr(j,i) = xr(j+1, i)
	    enddo
	  enddo
	endif

c	  
c	  do the three multr's, moving the appropriate column of independent
c	  var data into the one past the dependent vars
c
        do ixyz=1,3
c	  print *,'FIT #',ixyz
          do i=1,ndat
            xr(ncoldo+1,i)=xr(ncolin+1+ixyz,i)
c	    if (mod(i,10).eq.1)write(*,'(i4,4f9.2)')i,(xr(j,i),j=1,ncoldo+1)
          enddo
          call multr(xr,ncoldo+1,ndo,sx,ss,ssd,d,r,xm,sd,b,b1,const,rsq,
     &	      fra)
          do j=1,ncoldo
            a(ixyz,j)=b1(j)
          enddo
c	  print *,'solution:',(b1(j),j=1,ncoldo),const
          dxyz(ixyz)=const
	  cenloc(ixyz)=xm(ncoldo+1)
        enddo
c       
        devsum=0.
        devmax=-1.
	devsq=0.
        do ipnt=1,ndat
          do ixyz=1,3
            devxyz(ixyz)=dxyz(ixyz)-xr(ncolin+1+ixyz,ipnt)
            do j=1,ncoldo
              devxyz(ixyz)=devxyz(ixyz)+a(ixyz,j)*xr(j,ipnt)
            enddo
          enddo
          devpnt=sqrt(devxyz(1)**2+devxyz(2)**2+devxyz(3)**2)
	  xr(ncolin+1,ipnt)=devpnt
	  if(ipnt.le.ndo)then
	    devsum=devsum+devpnt
	    devsq=devsq+devpnt**2
	    if(devpnt.gt.devmax)then
	      devmax=devpnt
	      ipntmax=ipnt
	      do i=1,3
		devxyzmax(i)=devxyz(i)
	      enddo
	    endif
	  endif
        enddo
	call sums_to_avgsd(devsum,devsq,ndo,devavg,devsd)

	if (icolfix.ne.0)then
c	  
c	    open up the a matrix and insert the fixed solution values
c	    
	  do ixyz = 1,3
	    do j = ncoldo,icolfix,-1
	      a(ixyz,j+1) = a(ixyz,j)
	    enddo
	    a(ixyz,icolfix) = 0.
	  enddo
	  a(icolfix,icolfix) = 1.
	endif

	return
	end

	real*4 function erfcc(x)
c	  returns the complementary error function erfc(x) with fractional
c	  error everywhere less than 1.2e-7
	implicit none
	real*4 x,z,t
c
	z=abs(x)
	t=1./(1.+0.5*Z)
	erfcc=t*exp(-z*z-1.26551223+T*(1.00002368+T*(.37409196+
     &	    t*(.09678418+t*(-.18628806+t*(.27886807+t*(-1.13520398+
     &	    t*(1.48851587+t*(-.82215223+t*.17087277)))))))))
	if(x.lt.0) erfcc=2.-erfcc
	return
	end
