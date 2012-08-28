c       
c       $Id$
c       
c       SMOOTHGRID smooths the 2 dimension arrays in DXGRID, DYGRID and
c       DDENGRID, based on values in D[XY]GRID and the array SDGRID.  IXGDIM,
c       IYGDIM are the dimensions of the arrays.  NXGRID, NYGRID are the
c       number of values in the X (1st) and Y (2nd) dimensions.  At each grid
c       position, it computes the differences between SDGRID and the local
c       average of SDGRID, and between the displacement specified by
c       D[XY]GRID and the local average of such displacements.  It computes
c       the mean and standard deviation of these two differences over all
c       grid positions.  Then, every position with a difference between
c       SDGRID and the local mean greater than SDCRIT standard deviations
c       more than the mean such difference, or with the difference between
c       the displacement and the local mean greater than DEVCRIT standard
c       deviations more than the mean such difference, will have their
c       DXGRID, DYGRID, and DDENGRID replaced by the local mean.  Next, it
c       fits a polynomial in X and Y to series of local parts of the DXGRID
c       and DYGRID arrays.  NORDER is the polynomial order (e.g. 2 means use
c       terms in X, Y, X**2, Y**2 and XY), NXFIT and NYFIT are the number of
c       X and Y positions to include in each fit, and N[XY]SKIP specifies the
c       number of X and Y positions to skip between adjacent polynomial fits.
c       Each point is closest to the center of some polynomial fitting area;
c       DXGRID, DYGRID, and DDENGRID for that point are replaced by the value
c       calculated from that polynomial for that position.
c       
      subroutine smoothgrid(dxgrid,dygrid,sdgrid,ddengrid,ixgdim, iygdim,nxgrid,nygrid,
     &    sdcrit, devcrit,nxfit,nyfit,norder, nxskip,nyskip)
      implicit none
      integer idim,msiz
      integer*4 ixgdim,iygdim,nxgrid,nygrid,nxfit,nyfit,norder,nxskip,nyskip
c       
      real*4 dxgrid(ixgdim,iygdim),dygrid(ixgdim,iygdim)
      real*4 sdgrid(ixgdim,iygdim),ddengrid(ixgdim,iygdim)
      real*4 sdcrit, devcrit
      parameter (idim=100,msiz=50)
      real*4 xr(msiz,idim), xm(msiz), sd(msiz) , ssd(msiz, msiz)
      real*4 bb(msiz,3), vect(msiz), c(3), b(msiz), b1(msiz),b2(msiz),b3(msiz)
      real*4 dxnew(nxgrid,nygrid), dynew(nxgrid,nygrid), denew(nxgrid,nygrid)
      real*4 sdmax, sdsum,sdsq,devsum,devsqsum, dxmean ,dymean,denmean,dev, devvary, bint
      integer*4 ix, iy, nnear, length, nfitpt, ixy, ihi, ilo, nfit, iord, i
      integer*4 nxftl, nyftl, ixsolvsub, ixsolvadd, ixsolvstr, ixsolvend, ixsolspan
      integer*4 nxsolve, ixfitsub,ixfitadd,iysolvsub,iysolvadd,iysolvstr,iysolvend
      integer*4 iysolspan, nysolve, iyfitsub, iyfitadd, nindep, idepen1, idepen2, idepen3
      integer*4 ixfitend, ixsol, ixcen, ixfitstr,ixst, ixnd, iyfitend, iysol
      integer*4 iycen, iyfitstr, iyst, iynd, npnts, ind,nsum
      real*4 xsum, ysum, dsum, devsq, devsd, fra, devmean, rsq, sdmean, sdsd
      real*4 sdvary
c       
c       intercept the case of no overlap
c       
      if(sdgrid(1,1).lt.0.)return
c       
c       take mean and sd of the sd's and of the deviations from local means
      sdmax=0.
      sdsum=0.
      sdsq=0.
      devsum=0.
      devsqsum=0.
c       
      do ix=1,nxgrid
        do iy=1,nygrid
          sdsum=sdsum+sdgrid(ix,iy)
          sdsq=sdsq+sdgrid(ix,iy)**2
          sdmax=max(sdmax,sdgrid(ix,iy))
          call localmean(dxgrid,dygrid,sdgrid,ddengrid,ixgdim,iygdim,
     &        nxgrid,nygrid,ix,iy, dxmean ,dymean,denmean,nnear)
          devsq=(dxmean-dxgrid(ix,iy))**2+(dymean-dygrid(ix,iy))**2
          devsqsum=devsqsum+devsq
          devsum=devsum+sqrt(devsq)
        enddo
      enddo
c       
      nsum=nxgrid*nygrid
      sdmean=sdsum/nsum
      sdsd=sqrt((sdsq-nsum*sdmean**2)/(nsum-1.))
      devmean=devsum/nsum
      devsd=sqrt((devsqsum-nsum*devmean**2)/(nsum-1.))
c       
c       write(*,'(a,3f8.4)')' mean, sd, max of sd''s is:',sdmean,sdsd,
c       &           sdmax
c       write(*,'(a,2f8.4)')
c       &           ' mean and sd of deviations from local means is:'
c       &           ,devmean,devsd
c       
      do ix=1,nxgrid
        do iy=1,nygrid
          sdvary=(sdgrid(ix,iy)-sdmean)/sdsd
          call localmean(dxgrid,dygrid,sdgrid,ddengrid,ixgdim,iygdim,
     &        nxgrid,nygrid,ix,iy, dxmean ,dymean,denmean,nnear)
          dev=sqrt((dxmean-dxgrid(ix,iy))**2+(dymean-dygrid(ix,iy))**2)
          devvary=(dev-devmean)/devsd
          if(sdvary.gt.sdcrit.or.devvary.gt.devcrit)then
c             write(*,'(a,2i4,5f8.2)')' replacing',ix,iy,dxgrid(ix,iy),
c             &           dygrid(ix,iy), sdgrid(ix,iy),dxmean,dymean
            dxgrid(ix,iy)=dxmean
            dygrid(ix,iy)=dymean
            ddengrid(ix,iy)=denmean
          endif
        enddo
      enddo

      if(nxgrid.eq.1.or.nygrid.eq.1) then
        length = max(nxgrid,nygrid)
        nfitpt = min(max(nxfit,nyfit), length)
        do ixy=1,length
          ihi=min(ixy+nfitpt/2,length)
          ilo=max(1,ihi+1-nfitpt)
          ihi=min(length,ilo+nfitpt-1)
          nfit=ihi+1-ilo
          iord=norder
          if(nfit.lt.7)iord=min(norder,2)
          if(nfit.lt.5)iord=min(norder,1)
          ind=1
          do i=ilo,ihi
            if(nxgrid.eq.1)then
              ix=1
              iy=i
            else
              ix=i
              iy=1
            endif
            b(ind)=i-ixy
            b1(ind)=dxgrid(ix,iy)
            b2(ind)=dygrid(ix,iy)
            b3(ind)=ddengrid(ix,iy)
            ind=ind+1
          enddo
          if(nxgrid.eq.1)then
            iy=ixy
          else
            ix=ixy
          endif
          call polyfit(b,b1,nfit,iord,vect,bint)
          ind = ix + (iy-1) * nxgrid
          dxnew(ix,iy)=bint
          call polyfit(b,b2,nfit,iord,vect,bint)
          dynew(ix,iy)=bint
          call polyfit(b,b3,nfit,iord,vect,bint)
          denew(ix,iy)=bint
        enddo

      else
        nxftl=min(nxfit,nxgrid)
        nyftl=min(nyfit,nygrid)
c         now set up sub-grid on which to do regressions
        ixsolvsub=nxftl/2                       !to get starting, ending x to
        ixsolvadd=nxftl-1-ixsolvsub             !include in solution
        ixsolvstr=ixsolvsub+1                   !first position
        ixsolvend=nxgrid-ixsolvadd              !last position
        ixsolspan=ixsolvend+1-ixsolvstr         !span of solution points
        nxsolve=(ixsolspan+nxskip-1)/nxskip     !# of positions to solve
        ixfitsub=nxskip/2                       !to get starting, ending x
        ixfitadd=nxskip-1-ixfitsub              !values to replace with fit
c         
        iysolvsub=nyftl/2                       !to get starting, ending y to
        iysolvadd=nyftl-1-iysolvsub             !include in solution
        iysolvstr=iysolvsub+1                   !first position
        iysolvend=nygrid-iysolvadd              !last position
        iysolspan=iysolvend+1-iysolvstr         !span of solution points
        nysolve=(iysolspan+nyskip-1)/nyskip     !# of positions to solve
        iyfitsub=nyskip/2                       !to get starting, ending y
        iyfitadd=nyskip-1-iyfitsub              !values to replace with fit
c         
        nindep=norder*(norder+3)/2              !# of independent variables
        idepen1=nindep+1                        !index of independent var 1
        idepen2=nindep+2                        !index of independent var 2
        idepen3=nindep+3
        ixfitend=0
        do ixsol=1,nxsolve
          ixcen=min((ixsol-1)*nxskip+ixsolvstr,ixsolvend) !center position
c           take starting position to replace with fits 1 past last end
          ixfitstr=ixfitend+1
c           limit ending position by last solution point
          ixfitend=min(ixcen+ixfitadd,ixsolvend-1)
c           but if this is last point, go to edge
          if(ixsol.eq.nxsolve)ixfitend=nxgrid
          ixst=ixcen-ixsolvsub
          ixnd=ixcen+ixsolvadd
c           
          iyfitend=0
          do iysol=1,nysolve
            iycen=min((iysol-1)*nyskip+iysolvstr,iysolvend) !center position
c             take starting position to replace with fits 1 past last end
            iyfitstr=iyfitend+1
c             limit ending position by last solution point
            iyfitend=min(iycen+iyfitadd,iysolvend-1)
c             but if this is last point, go to edge
            if(iysol.eq.nysolve)iyfitend=nygrid
            iyst=iycen-iysolvsub
            iynd=iycen+iysolvadd
c             
            npnts=0
            
            do ix=ixst,ixnd
              do iy=iyst,iynd
                npnts=npnts+1
                call polyterm(ix-ixcen,iy-iycen,norder,xr(1,npnts))
                xr(idepen1,npnts)=dxgrid(ix,iy)
                xr(idepen2,npnts)=dygrid(ix,iy)
                xr(idepen3,npnts)=ddengrid(ix,iy)
                if(npnts.gt.1)then
                  if(xr(idepen1,npnts).eq.xr(idepen1,npnts-1))
     &                xr(idepen1,npnts)=dxgrid(ix,iy)+0.01
                  if(xr(idepen2,npnts).eq.xr(idepen2,npnts-1))
     &                xr(idepen2,npnts)=dygrid(ix,iy)+0.01
                  if(xr(idepen3,npnts).eq.xr(idepen3,npnts-1))
     &                xr(idepen3,npnts)=ddengrid(ix,iy)+0.001
                endif
              enddo
            enddo
c             
c             Solve for dx, dy, den as function of terms and compute fitted values
            call multRegress(xr,msiz,1,nindep,npnts,3,0,bb,msiz,c,xm,sd,ssd)
            do ix=ixfitstr,ixfitend
              do iy=iyfitstr,iyfitend
                call polyterm(ix-ixcen,iy-iycen,norder,vect)
                xsum=c(1)
                ysum=c(2)
                dsum=c(3)
                do i=1,nindep
                  xsum=xsum+bb(i,1)*vect(i)
                  ysum=ysum+bb(i,2)*vect(i)
                  dsum=dsum+bb(i,3)*vect(i)
                enddo
                dxnew(ix,iy)=xsum
                dynew(ix,iy)=ysum
                denew(ix,iy)=dsum
              enddo
            enddo
          enddo
        enddo
      endif
c       
c       replace new values into dxgrid and dygrid arrays
c       
      dxgrid(1:nxgrid,1:nygrid)=dxnew(1:nxgrid,1:nygrid)
      dygrid(1:nxgrid,1:nygrid)=dynew(1:nxgrid,1:nygrid)
      ddengrid(1:nxgrid,1:nygrid)=denew(1:nxgrid,1:nygrid)
c       
      return
      end
