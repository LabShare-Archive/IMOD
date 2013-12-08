c       PLANEFIT will find the plane that fits a set of 3-D points,
c       minimizing the sum of squared distances from the points to the plane.
c       It returns the centroid of the original point in, a rotation matrix,
c       and the points rotated so that the best-fitting plane is the X-Y
c       plane.

      subroutine planefit(ptin,npt,centroid,rmat,ptrot)
      real*4 ptin(3,*),ptrot(3,*),rmat(3,3),centroid(3)
      parameter (limpt=1000)
      real*4 xx(limpt),yy(limpt),zz(limpt),rfit(3,3),rtmp(3,3)
      real*4 angles(3)
      real*8 err,errlas
      equivalence (alpha,angles(1)),(beta,angles(2))
      iterlim=50
      errmin=1.e-5
      diffmin=1.e-6
c       
c       get centroid, initialize matrix and copy points
c       
      do i=1,3
        sum=0.
        do j=1,npt
          sum=sum+ptin(i,j)
        enddo
        centroid(i)=sum/npt
        do k=1,3
          rmat(i,k)=0.
        enddo
        rmat(i,i)=1.
      enddo
c       
      do j=1,npt
        do i=1,3
          ptrot(i,j)=ptin(i,j)-centroid(i)
        enddo
      enddo
c       
      errlas=1.e10
      err=1.e9
      niter=0
      do while(niter.lt.iterlim.and.err.gt.errmin.and.
     &    abs(errlas-err)/max(err,errlas).gt.diffmin)
c         
c         copy data and fit to it
c         
        do j=1,npt
          xx(j)=ptrot(1,j)
          yy(j)=ptrot(2,j)
          zz(j)=ptrot(3,j)
        enddo
        ifrotate=0
        if(niter.eq.0)then
c           
c           first time, see if need to rotate points by 90 degrees: assess
c           by sd of the x and z coordinates
c           
          call avgsd(xx,npt,xavg,xsd,xsem)
          call avgsd(zz,npt,zavg,zsd,zsem)
          if(zsd.gt.xsd)then
            alpha=0.
            beta=90.
            ifrotate=1
          endif
        endif
        if(ifrotate.eq.0)then
          call lsfit2(xx,yy,zz,npt,a,b,d)
c           
c           get angles, rotation matrix from them
c           
          a=-a
          b=-b
          beta=-atand(a)
          cp=cosd(beta)-a*sind(beta)
          alpha=atand(b/cp)
        endif
        angles(3)=0.
        call icalc_matrix(angles,rfit)
c         
c         get product matrix
c         
        
        do i=1,3
          do j=1,3
            rtmp(i,j)=0.
            do k=1,3
              rtmp(i,j)=rtmp(i,j)+rfit(i,k)*rmat(k,j)
            enddo
          enddo
        enddo
        do i=1,3
          do j=1,3
            rmat(i,j)=rtmp(i,j)
          enddo
        enddo
c         
c         rotate points and add up error
c         
        errlas=err
        err=0.
        do j=1,npt
          call rotpnt(ptin(1,j),centroid,rmat,ptrot(1,j))
          err=err+ptrot(3,j)**2
        enddo
        niter=niter+1
      enddo
      if(niter.eq.iterlim)then
        write(*,101)npt,(centroid(i),i=1,3)
101     format('Planefit: Iteration limit for',i4,' points at',3f7.1)
      endif
      return
      end


      subroutine rotpnt(pt,cen,r,ptr)
      real*4 pt(3),cen(3),r(3,3),ptr(3)
      do i=1,3
        ptr(i)=0.
        do j=1,3
          ptr(i)=ptr(i)+(pt(j)-cen(j))*r(i,j)
        enddo
      enddo
      return
      end
