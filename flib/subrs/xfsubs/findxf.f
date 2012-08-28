c       FINDXF finds a transformation from a set of corresponding points
c       that have been loaded into the matrix XR, independent variable values
c       of X and Y in columns 1 and 2, dependent variable values of X and Y
c       in either columns 4 and 5, or 3 and 4, depending on whether IXCOL is 4 or 3.
c       MSIZE is the first dimension (total number of columns) of XR
c       NPNTS is the number of points
c       XCEN, YCEN should contain the center coordinates that have been
c       subtracted from the X and Y point data.
c       IFTRANS and IFROTRANS should be 0 to get a general linear transform
c       IFTRANS should be 1 to solve for translation only
c       IFROTRANS should be 1 to solve for rotations and translations, or
c       2 to solve for magnification also
c       IFDEV should be 0 for no deviation information, 1 or 2 for deviations
c       F(2,3) is returned with the transform
c       If IFDEV is non-zero, DEVAVG, DEVSD, DEVMAX, and IPNTMAX are returned
c       with the mean, SD, and maximum deviation and the point number at
c       which the maximum occurs, and columns 10 and 11, and 13 are filled
c       with the deviation in X and Y and total deviation.
c       If IFDEV is 2, columns 8 and 9 are filled with the coordinates with
c       XCEN and YCEN added back, and column 12 has the angle of the deviation
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
      subroutine findxf(xr,msize,icolx,npnts,xcen,ycen,iftrans,ifrotrans,ifdev,f,devavg,
     &    devsd,devmax,ipntmax)
      implicit none
      integer*4 npnts,iftrans,ifrotrans,ifdev,ipntmax,msize,icolx,icoly
      real*4 f(2,3), devavg,devsd,devmax,xcen,ycen
      real*4 xr(msize,*), sx(icolx+1), xm(icolx+1), sd(icolx+1), ss(icolx+1,icolx+1)
      real*4 ssd(icolx+1,icolx+1), d(icolx+1,icolx+1), r(icolx+1,icolx+1), b(icolx+1, 2)
      integer*4 isol,i,ipnt
      real*4 const, c(2), theta,sinth,costh,gmag,devsum,devsumsq,xdev
      real*4 ydev,devpnt,xx,yy
      real*4 atan2d
c       
      icoly = icolx + 1
      call xfunit(f,1.)
      if(ifrotrans.eq.0 .and. iftrans .eq. 0)then
c             
c         move 4th or 5th column into 3rd for regression
c             
        if (icolx .gt. 3) then
          do i=1,npnts
            xr(3,i)=xr(4,i)
            xr(4,i)=xr(5,i)
          enddo
        endif
        call multRegress(xr,msize,1,2,npnts,2,0,b,icoly,c,xm,sd,ss)
        do isol = 1, 2
          f(isol,1) = b(1,isol)
          f(isol,2) = b(2,isol)
          f(isol,3) = c(isol)
        enddo
c         
c         shift points back out for consistency in getting residuals
        if (icolx .gt. 3) then
          do i=1,npnts
            xr(5,i)=xr(4,i)
            xr(4,i)=xr(3,i)
          enddo
        endif
      elseif (ifrotrans .eq. 0) then
        do isol=1,2
c             
c           or, if getting translations only, find difference in
c           means for X or Y
c           
          const=0.
          do i=1,npnts
            const=const+xr(icolx+isol-1,i)-xr(isol,i)
          enddo
          const=const/npnts
          f(isol,3)=const
        enddo
      else
c         
c         or find rotations and translations only by getting means
c         and sums of squares and cross-products of deviations
c         
        call statMatrices(xr,msize,1,icoly,icoly,npnts,sx,ss,ssd,d,r,xm,sd,1)
        theta=atan(-(ssd(2,icolx)-ssd(1,icoly))/(ssd(1,icolx)+ssd(2,icoly)))
        sinth=sin(theta)
        costh=cos(theta)
        if(ifrotrans.eq.2)then
          gmag=((ssd(1,icolx)+ssd(2,icoly))*costh
     &        -(ssd(2,icolx)-ssd(1,icoly))*sinth)/(ssd(1,1)+ssd(2,2))
          sinth=sinth*gmag
          costh=costh*gmag
        endif
        f(1,1)=costh
        f(1,2)=-sinth
        f(2,1)=sinth
        f(2,2)=costh
        f(1,3)=xm(icolx)-xm(1)*costh+xm(2)*sinth
        f(2,3)=xm(icoly)-xm(1)*sinth-xm(2)*costh
      endif
      if(ifdev.eq.0)return
c       
c       compute mean and max deviation between points in adjacent
c       sections after the transformation is applied
c       
      devsum=0.
      devsumsq=0.
      devmax=-1.
      do ipnt=1,npnts
        call xfapply(f,0.,0.,xr(1,ipnt),xr(2,ipnt),xx, yy)
        xdev=xr(icolx,ipnt)-xx
        ydev=xr(icoly,ipnt)-yy
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
c         save true, not-centered coordinate and the angle of deviation if
c         ifdev > 1
c         
        if (ifdev .gt. 1) then
          xr(8,ipnt)=xr(icolx,ipnt)+xcen
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
