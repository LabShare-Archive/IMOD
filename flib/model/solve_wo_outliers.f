c       $Id$
c       Log at end
c       
c       SOLVE_WO_OUTLIERS solves for a fit between sets of positions in 3D
c       and eliminates outlying position-pairs from the solution
c       
c       XR is the data matrix, NDAT is the full amount of data
c       NCOL is the number of columns of independent variables
c       ICOLFIX specifies which column has insufficient variance (is fixed)
c       and should be negative to indicate inversion in that dimension
c       MAXDROP is the maximum number of points to drop as outliers
c       CRITPROB and CRITABS are the criterion probabilities for considering
c       a point an outlier
c       If the maximum residual is below ELIMMIN nothing will be eliminated
c       NDROP is number of points dropped, point numbers returned in IDROP
c       A is the 3x3 matrix computed
c       DXYZ has the displacements
c       CENLOC is the mean location in the dependent variables
c       DEVAVG, DEVSD, DEVMAX are mean, SD, and maximum deviations
c       IPNTMAX and DEVXYZMAX given the point number and deviation in X,y,Z
c       at which the maximum occurred
c       
c       The routine copies original data to columns 8 to 14 (or ncol + 5 to
c       2*(ncol + 4)), puts a cross index from the ordered data to the
c       original row in column 5 (or ncol + 2), returns a mean residual in
c       column 4 (ncol + 1) for the ordered data, and puts a residual vector
c       components for the ordered data in columns 15 to 17 (2*(ncol + 4) + 
c       1,2,3).
c       
      subroutine solve_wo_outliers(xr,ndat,ncol,icolfix,maxdrop,
     &    critprob, critabs, elimmin, idrop,ndrop,a,dxyz,cenloc,
     &    devavg,devsd, devmax, ipntmax, devxyzmax)
      implicit none
      include 'statsize.inc'
      integer maxerr
      parameter (maxerr = 100000)
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
c       get probability per single point from the overall criterion prob
c       
      probperpt=(1.-critprob)**(1./ndat)
      absperpt=(1.-critabs)**(1./ndat)
c       
c       copy the data into columns 8-14 (for ncol = 3)
c       
      do i=1,ndat
        do j=1,ncol+4
          xr(j+ncol+4,i)=xr(j,i)
        enddo
      enddo

      call do3multr(xr,ndat,ncol,ndat,icolfix,a,dxyz,cenloc,devavg,
     &    devsd, devmax, ipntmax, devxyzmax)
      ndrop = 0
c       
c       If returning right away, load cross-indexes into col 5
      if(maxdrop.eq.0.or.devmax.lt.elimmin) then 
        do i=1,ndat
          xr(ncol+2,i)=i
        enddo
        return
      endif
c       
c       order the residuals
c       
      lastdrop=0
      if (ndat.gt.maxerr) then
        print *, 'CANNOT FIND OUTLIERS: ARRAYS NOT LARGE ENOUGH'
        return
      endif
c       
c       Sort the residuals and keep index back to initial values
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
c       load the data in this order
c       
      do i=1,ndat
        do j=1,ncol+4
          xr(j,i)=xr(j+ncol+4,index(i))
        enddo
      enddo
c       
c       Drop successively more points: get mean and S.D. of the remaining
c       points and check how many of the points pass the criterion 
c       for outliers.  
c       
      do jdrop = 1, maxdrop+1
        call do3multr(xr,ndat,ncol,ndat-jdrop,icolfix,a,dxyz,centmp,
     &      devavg, devsd, devmax, ipntmax, devxyzmax)
c         
c         estimate the sigma for the error distribution as the maximum of
c         the values implied by the mean and the SD of the deviations
c         
        sigfromavg=devavg/sqrt(8./3.14159)
        sigfromsd=devsd/sqrt(3.-8./3.14159)
        sigma=max(sigfromavg,sigfromsd)

        nkeep=0
        do j=ndat-jdrop+1,ndat
          if (sigma .lt. 0.1 * abs(xr(ncol+1,j)) .or. sigma .lt. 1.e-5) then
            z = sign(10., xr(ncol+1,j))
          else
            z = xr(ncol+1,j) / sigma
          endif
          prob=2*(gprob(z)-0.5)-sqrt(2./3.14159)*z*exp(-z**2/2.)
          if(prob.lt.probperpt)nkeep=nkeep+1
          if(prob.ge.absperpt) ndrop=min(maxdrop,max(ndrop,ndat+1-j))
        enddo
c         
c         If all points are outliers, this is a candidate for a set to drop
c         When only the first point is kept, and all the rest of the points
c         were outliers on the previous round, then this is a safe place to
c         draw the line between good data and outliers.  In this case, set
c         ndrop; and at end take the biggest ndrop that fits these criteria
c         
        if(nkeep.eq.0)lastdrop=jdrop
        if(nkeep.eq.1.and.lastdrop.eq.jdrop-1.and.lastdrop.gt.0)
     &      ndrop=lastdrop
c	  print *,'drop',jdrop,', keep',nkeep,', lastdrop',lastdrop,
c         &	      ',  ndrop =',ndrop
      enddo
c       
c       when finish loop, need to redo with right amount of data and save
c       indices in column past the residuals
c       
      do i=1,ndrop
        idrop(i)=index(ndat+i-ndrop)
      enddo
      call do3multr(xr,ndat,ncol,ndat-ndrop,icolfix,a,dxyz,centmp,
     &    devavg, devsd, devmax, ipntmax, devxyzmax)
      ipntmax=index(ipntmax)
      do i=1,ndat
        xr(ncol+2,i) = index(i)
      enddo
      return
      end


c       DO3MULTR does the three regressions to determine a transformation
c       matrix, or it does a search for a reduced set of parameters if
c       ICOLFIXIN is non-zero.
c       XR is the data matrix, NDAT is the full amount of data
c       NCOLIN is the number of columns of independent variables
c       NDO is the number of data points to use
c       ICOLFIXIN specifies which column has insufficient variance (is fixed)
c       and should be negative to indicate inversion in that dimension
c       A is the 3x3 matrix computed
c       DXYZ has the displacements
c       CENLOC is the mean location in the dependent variables
c       DEVAVG, DEVSD, DEVMAX are mean, SD, and maximum deviations
c       IPNTMAX and DEVXYZMAX given the point number and deviation in X,y,Z
c       at which the maximum occurred
c       
      subroutine do3multr(xr,ndat,ncolin,ndo,icolfixin,a,dxyz,cenloc,
     &    devavg,devsd, devmax, ipntmax, devxyzmax)
      implicit none
      include 'statsize.inc'
      real*4 xr(msiz,*), xm(msiz), sd(msiz)
      real*4 ssd(msiz,msiz), b1(msiz)
      real*4 a(3,*),dxyz(3),devxyz(3),devxyzmax(3),cenloc(3)
      integer*4 ndat,ncolin,ndo,ipntmax,icolfix,icolfixin
      real*4 devavg,devsd,devmax,xmsav(6)
      integer*4 ixyz,i,j,ipnt,ncoldo,k,km
      real*4 const,devsum,devsq,devpnt, amat(2,2), funcErr
      real*8 smsq(4,6), aa(3,3), errMin
      integer*4 nullAxis, mSign, ifTrace,nTrial
      common /funccom/nullAxis, mSign, ifTrace, nTrial, errMin, smsq, aa
      real*8 sum
      real*4 pp(7,7),yy(7),ptmp(6),ptol(6), da(6), ptol1, ftol1,delfac,var(6)
      real*4 ftol2, ptol2
      data da/2.,2.,.02,.02,2.,2./
      integer*4 jmin, iter, icol
      logical firstTime
      data firstTime/.true./
      save var, firstTime
      
      external func
c       
c       values for simplex fit
c       
      ptol2 = 5.e-4
      ftol2 = 5.e-4
      ptol1 = 1.e-5
      ftol1 = 1.e-5
      delfac = 2.
      errMin = 1.e30
      nTrial = 0
c       set to 1 for results of each fit, 2 for trace of new minima, 3 for
c       full trace of simplex search
      ifTrace = 0

      ncoldo = ncolin
      icolfix = abs(icolfixin)
      if (icolfix.ne.0) then
        nullAxis = icolfix
        mSign = sign(1, icolfixin)
c	  
c         if one column is fixed first get sums of squares and cross products
c         
        do icol = 1, 6
          j = icol
          if (icol .gt. 3) j = icol + 1
          sum = 0.
          do i = 1, ndo
            sum = sum + xr(j, i)
          enddo
          xmsav(icol) = sum / ndo
        enddo
        do icol = 1, 6
          j = icol
          if (icol .gt. 3) j = icol + 1
          do ixyz=1,4
            k = ixyz
            km = ixyz
            if (ixyz .eq. 4) then
              k = j
              km = icol
            endif
            sum = 0.
            do i = 1, ndo
              sum = sum + (xr(j, i) - xmsav(icol)) * (xr(k, i) - xmsav(km))
            enddo
            smsq(ixyz, icol) = sum
          enddo
        enddo
c	  print *,'xmsav',(xmsav(i), i=1,6)
c	  print *,'smsq'
c	  write(*,'(6f12.1)')((smsq(i,j),j=1,6),i=1,4)
c         
c	  now decrement the number of columns to do
c	  subtract that column's independent var from its dependent var
c	  and pack the dependent vars into the smaller number of columns
c         
        ncoldo = ncolin - 1
        do i = 1,ndo
          xr(ncolin + 1 + icolfix, i) = xr(ncolin + 1 + icolfix, i) -
     &        xr(icolfix,i)
          xr(ncolin + 1, i) = xr(icolfix, i)
          do j = icolfix,ncoldo
            xr(j,i) = xr(j+1, i)
          enddo
        enddo
      endif
c       
c       do the three multr's, moving the appropriate column of independent
c       var data into the one past the dependent vars
c       
      icol = 1
      do ixyz=1,3
c	  print *,'FIT #',ixyz
        if (ixyz .ne. icolfix) then
          do i=1,ndo
            xr(ncoldo+1,i)=xr(ncolin+1+ixyz,i)
c             if (mod(i,10).eq.1)write(*,'(i4,5f9.2)')i,(xr(j,i),j=1,
c             &		  ncoldo+1)
          enddo
          call multRegress(xr,msiz,1,ncoldo,ndo,1,0,b1,msiz,const,xm,sd,ssd)
c	    print *,'ss:'
c	    write(*,'(4f12.1)')((ssd(i,j),j=1,4),i=1,4)
          do j=1,ncoldo
            a(icol,j)=b1(j)
          enddo
c           print *,'solution:',(b1(j),j=1,ncoldo),const
          dxyz(ixyz)=const
          cenloc(ixyz)=xm(ncoldo+1)
          icol = icol + 1
        endif
      enddo
c       
      if (icolfix .ne. 0) then
c         
c         restore the data
c         
        do i = 1,ndo
          do j = ncoldo,icolfix,-1
            xr(j+1,i) = xr(j, i)
          enddo
          xr(icolfix, i) = xr(ncolin + 1, i)
          xr(ncolin + 1 + icolfix, i) = xr(ncolin + 1 + icolfix, i) +
     &        xr(icolfix,i)
        enddo
c         
c         get starting variables for minimization from matrix the first time,
c         or just restart from previous run
c         
        amat(1,1) = a(1,1)
        amat(1,2) = a(1,2)
        amat(2,1) = a(2,1)
        amat(2,2) = a(2,2)
        if (icolfix .eq. 2) then
          amat(1,2) = -a(1,2)
          amat(2,1) = -a(2,1)
        endif

        if (firstTime) then
          call amat_to_rotmag(amat, var(1), var(2), var(3), var(4))
          var(5) = 0.
          var(6) = 0.
        endif
        firstTime = .false.
        call amoebaInit(pp, yy, 7, 6, delfac, ptol2, var, da, func, ptol)
        call amoeba(pp,yy,7,6,ftol2,func,iter,ptol,jmin)
c         
c         per Press et al. recommendation, just restart at current location
c         
        do i=1,6
          var(i) = pp(jmin, i)
        enddo
        call amoebaInit(pp, yy, 7, 6, delfac, ptol1, var, da, func, ptol)
        call amoeba(pp,yy,7,6,ftol1,func,iter,ptol,jmin)
c         
c         recover result, get aa matrix from best result, compute dxyz
c         
        do i = 1, 6
          var(i) = pp(jmin, i)
        enddo
        funcErr = sqrt(max(0.,yy(jmin) / ndo))
        if (iftrace .ne. 0) write(*,'(i5,f10.4, 2f9.2,2f9.4,2f9.2)')
     &      iter,funcErr, (var(i), i = 1,6)
        do i = 1, 3
          cenloc(i) = xmsav(i+3)
          dxyz(i) = xmsav(i+3)
          do j = 1, 3
            a(i, j) = aa(i, j)
            dxyz(i) = dxyz(i) - a(i,j)*xmsav(j)
          enddo
          if(ifTrace .ne. 0)write(*,102)(a(i,j),j=1,3),dxyz(i)
102       format(3f10.6,f10.3)
        enddo
      endif
c       
c       compute deviations for all the data (ndat), keep track of max
c       for the ones in the fit (ndo)
c       return residual vector components in columns 15 to 17
c       
      devsum=0.
      devmax=-1.
      devsq=0.
      do ipnt=1,ndat
        do ixyz=1,3
          devxyz(ixyz)=dxyz(ixyz)-xr(ncolin+1+ixyz,ipnt)
          do j=1,ncolin
            devxyz(ixyz)=devxyz(ixyz)+a(ixyz,j)*xr(j,ipnt)
          enddo
          xr(2 * (ncolin + 4) + ixyz, ipnt) = devxyz(ixyz)
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

      return
      end


c       
c       FUNC is called by the simplex routine to compute the sum squared
c       error for the solution vector in P
c       
      subroutine func(p, funcErr)
      implicit none
      real*4 p(*), funcErr
      real*8 smsq(4,6), aa(3,3), errMin
      integer*4 nullAxis, mSign, ifTrace,nTrial
      common /funccom/nullAxis, mSign, ifTrace, nTrial, errMin, smsq, aa
      real*8 b(2,2), c(2,2), d(2,2), smag(3), all(2,2,3)
      equivalence (all,b), (all(1,1,2), c), (all(1,1,3), d)
      real*8 cosang1, cosang2, sinang1, sinang2, dtor
      real*8 a11, a12, a13, a21, a22, a23, a31, a32, a33
      real*4 r4mat(2,2)
      integer*4 ind5, ind6, i
      equivalence (aa(1,1), a11), (aa(1,2), a12), (aa(1,3), a13)
      equivalence (aa(2,1), a21), (aa(2,2), a22), (aa(2,3), a23)
      equivalence (aa(3,1), a31), (aa(3,2), a32), (aa(3,3), a33)
      character*1 starout
c       
      dtor = 3.1415926536 / 180.
      nTrial = nTrial + 1
      cosang1 = cos(p(5) * dtor)
      sinang1 = sin(p(5) * dtor)
      cosang2 = cos(p(6) * dtor)
      sinang2 = sin(p(6) * dtor)
c       
c       set up the X, Y, and Z rotation matrices
c       
      if (nullAxis .eq. 1) then
        ind5 = 2
        ind6 = 3
      else if (nullAxis .eq. 2) then
        ind5 = 1
        ind6 = 3
      else
        ind5 = 1
        ind6 = 2
      endif
c       
      call rotmag_to_amat(p(1), p(2), p(3), p(4), r4mat)
      all(1,1,nullAxis) = r4mat(1,1)
      all(1,2,nullAxis) = r4mat(1,2)
      all(2,1,nullAxis) = r4mat(2,1)
      all(2,2,nullAxis) = r4mat(2,2)
      smag(nullAxis) = mSign * p(3)
      smag(ind5) = 1.
      smag(ind6) = 1.
      all(1,1,ind5) = cosang1
      all(1,2,ind5) = -sinang1
      all(2,1,ind5) = sinang1
      all(2,2,ind5) = cosang1
      all(1,1,ind6) = cosang2
      all(1,2,ind6) = -sinang2
      all(2,1,ind6) = sinang2
      all(2,2,ind6) = cosang2
      c(1,2) = -c(1,2)
      c(2,1) = -c(2,1)
c       
c       get products for full a matrix.  If Y is the null axis, put it
c       first to avoid Z and X rotations playing off against each other
c       with a 90 degree rotation around Y
c       
      if (nullAxis .eq. 2) then
c         
c	  Product C * B * D:
c	  
        a11 = b(2,1) * c(1,2) * d(2,1) + smag(1) * c(1,1) * d(1,1)
        a12 = b(2,1) * c(1,2) * d(2,2) + smag(1) * c(1,1) * d(1,2)
        a13 = smag(3) * b(2,2) * c(1,2)
        a21 = smag(2) * b(1,1) * d(2,1)
        a22 = smag(2) * b(1,1) * d(2,2)
        a23 = smag(2) * b(1,2) * smag(3)
        a31 = b(2,1) * c(2,2) * d(2,1) + smag(1) * c(2,1) * d(1,1)
        a32 = b(2,1) * c(2,2) * d(2,2) + smag(1) * c(2,1) * d(1,2)
        a33 = smag(3) * b(2,2) * c(2,2)
      else
c	  
c         Product B * C * D:
c         
        a11 = smag(1) * c(1,1) * d(1,1)
        a12 = smag(1) * c(1,1) * d(1,2)
        a13 = smag(1) * c(1,2) * smag(3)
        a21 = b(1,2) * c(2,1) * d(1,1) + smag(2) * b(1,1) * d(2,1)
        a22 = b(1,2) * c(2,1) * d(1,2) + smag(2) * b(1,1) * d(2,2)
        a23 = smag(3) * b(1,2) * c(2,2)
        a31 = b(2,2) * c(2,1) * d(1,1) + smag(2) * b(2,1) * d(2,1)
        a32 = b(2,2) * c(2,1) * d(1,2) + smag(2) * b(2,1) * d(2,2)
        a33 = smag(3) * b(2,2) * c(2,2)
      endif
c       
c       get error sum
c       
      funcErr = (a11**2 + a21**2 + a31**2) * smsq(1,1) +
     &    2. * (a11*a12 + a21*a22 + a31*a32) * smsq(1,2) +
     &    2. * (a11*a13 + a21*a23 + a31*a33) * smsq(1,3) +
     &    (a12**2 + a22**2 + a32**2) * smsq(2,2) +
     &    2. * (a12*a13 + a22*a23 + a32*a33) * smsq(2,3) +
     &    (a13**2 + a23**2 + a33**2) * smsq(3,3) -
     &    2. * (a11 * smsq(1,4) + a21 * smsq(1,5) + a31 * smsq(1,6)) -
     &    2. * (a12 * smsq(2,4) + a22 * smsq(2,5) + a32 * smsq(2,6)) -
     &    2. * (a13 * smsq(3,4) + a23 * smsq(3,5) + a33 * smsq(3,6)) +
     &    smsq(4,4) + smsq(4,5) + smsq(4,6)
      
      if (ifTrace .gt. 1) then
        starout=' '
        if(funcErr.lt.errMin)then
          starout='*'
          errMin=funcErr
        endif
        if(iftrace.gt.2.or.funcErr.lt.errMin)
     &      write(*,72)starout,nTrial,funcErr, (p(i), i = 1,6)
72      format(1x,a1,i4,f15.5, 2f9.3,2f9.5,2f9.3)
      endif
      
      return
      end

c       $Log$
c       Revision 3.9  2008/06/25 22:08:44  mast
c       Load cross-indexes if returning right away on first fit
c
c       Revision 3.8  2006/08/21 16:43:01  mast
c       Placed index to ordered data in column after the residual, increased
c       dimension to 100000
c
c       Revision 3.7  2006/06/18 19:37:48  mast
c       Changed to use new C function for amoeba
c	
c       Revision 3.6  2005/10/19 16:44:57  mast
c       Increased maxerr to 40000 to match patch limits
c	
c       Revision 3.5  2005/03/27 18:57:00  mast
c       Moved erfcc to library
c	
c       Revision 3.4  2004/06/10 00:36:15  mast
c       Restarted amoeba for second round as recommended
c	
c       Revision 3.3  2003/12/24 18:05:18  mast
c       Implemented constrained solution with in-plane linear
c       transformation and two rotation angles, using a simplex search,
c       when one column is "fixed"
c	
c       Revision 3.2  2002/10/23 15:38:57  mast
c       Added the ability to fix the solution for a single column and solve
c       only for the dependence on two variables.
c	
c       Revision 3.1  2002/07/27 16:18:31  mast
c       Increased dimension maxerr to 10000 so that it will work
c       on large sets with Refinematch
c	
