C       !
c       This subroutine will correct for a distortion field and also apply 
c       coordinate transformations using cubic or linear interpolation.
C       
C       ^ [array]   - The input image array
C       ^ [bray]    - The output image array
C       ^ [nxa,nya] - The dimensions of ARRAY
C       ^ [nxb,nyb] - The dimensions of BRAY
C       ^ [amat]    - A 2x2 matrix to specify rotation,scaling,skewing
C       ^ [xc,yc]   - The coordinates of the center of ARRAY
C       ^ [xt,yt]   - The translation to add to the final image.
C       ^ [scale]   - A multiplicative scale factor for the intensities
C       ^ [dmean]   - Mean intensity of image, or value to fill edges with
C       ^ [linear]  - Set non-zero to do linear interpolation
c       ^ [dxGrid, dyGrid] - Grid of distortion displacements in X & Y;
c       each value is the shift to get from a desired position in the
c       undistorted image to the corresponding position in the original image
c       ^ [ixgDim]  - The first dimension of [dxGrid] and [dyGrid]
c       ^ [ixGridStrt, iyGridStrt] - Coordinates at which grid starts
c       ^ [xGridIntrv, yGridIntrv] - Spacing between grid points in X & Y
c       ^ [nxGrid, nyGrid] - Number of grid points in X & Y
C       
c       After the image is undistorted the transformation is:
C       ^ Xo = a11(Xi - Xc) + a12(Yi - Yc) + NXB/2. + XT
C       ^ Yo = a21(Xi - Xc) + a22(Yi - Yc) + NYB/2. + YT
c       ^  where X coordinates run from 0 at the left edge of the first pixel
c       to [nxa] at the right edge of the last pixel, etc.
C       !         
c       written by David Mastronarde, December 2003.
c       
c       $Id$
c       
c       $Log$
c       Revision 3.1  2005/02/28 15:10:12  mast
c       Moved to libhvem
c       
c       Revision 3.1  2005/02/27 15:58:11  mast
c       Added to library
c       
c       
      subroutine undistInterp(array,bray,nxa,nya,nxb,nyb,amat,
     &    xc,yc,xt,yt,scale,dmean, linear, dxGrid, dyGrid, ixgDim,
     &    nxGrid, ixGridStrt, xGridIntrv, nyGrid, iyGridStrt,
     &    yGridIntrv)
      implicit none
      integer*4 NXA,NYA,NXB,NYB,linear
      real*4 ARRAY(NXA,NYA),BRAY(NXB,NYB),AMAT(2,2)
      real*4 XC,YC,XT,YT,SCALE,dmean
      real*4 xcen,ycen,xco,yco,denom,a11,a12,a22,a21,dyo,xbase,ybase
      real*4 xst,xnd,xlft,xrt,xp,yp,dennew,dx,dy,v2,v4,v6,v8,v5,a,b,c,d
      real*4 dxm1,dxdxm1,fx1,fx2,fx3,fx4,dym1,dydym1,v1,v3,vmin,vmax
      integer*4 iy,ix,ixp,ixpp1,iyp,iypp1,iypp2,ixpp2,ixpm1,iypm1,linefb
      integer*4 ixnd,ixst,ixfbst,ixfbnd,iqst,iqnd,ifall
      integer*4 ixgDim, nxGrid, nyGrid, ixGridStrt, iyGridStrt
      real*4 dxGrid(ixgDim, *), dyGrid(ixgDim, *)
      real*4 xGridIntrv, yGridIntrv
C       
C       Calc inverse transformation
C       
      XCEN = NXB/2. + XT + 0.5
      YCEN = NYB/2. + YT + 0.5
      XCO = XC + 0.5
      YCO = YC + 0.5
      DENOM = AMAT(1,1)*AMAT(2,2) - AMAT(1,2)*AMAT(2,1)
      A11 =  AMAT(2,2)/DENOM
      A12 = -AMAT(1,2)/DENOM
      A21 = -AMAT(2,1)/DENOM
      A22 =  AMAT(1,1)/DENOM
C       
C       Loop over output image
C       
      DO IY = 1,NYB
        DYO = IY - YCEN
        xbase = a12*dyo +xco - a11*xcen
        ybase = a22*dyo + yco - a21*xcen

        if (linear .ne. 0) then
c           
c           linear interpolation
c           
          do ix=1,nxb
            xp = a11*ix + xbase
            yp = a21*ix + ybase
            call interpolateGrid(xp, yp, dxGrid, dyGrid, ixgDim, nxGrid,
     &          nyGrid, ixGridstrt, xGridIntrv, iyGridStrt,
     &          yGridIntrv, dx, dy)
            xp = xp + dx
            yp = yp + dy
            IXP = XP
            IYP = YP
            bray(ix,iy)=dmean
            IF (IXP .ge. 1 .and. IXP .lt. NXA .and. IYP .ge. 1 .and.
     &          IYP .lt. NYA) then
              DX = XP - IXP
              DY = YP - IYP
              IXPP1 = IXP + 1
              IYPP1 = IYP + 1
              bray(ix, iy) = (1. - dy) * ((1. - dx) * array(ixp, iyp) +
     &            dx * array(ixpp1, iyp)) +
     &            dy * ((1. - dx) * array(ixp, iypp1) +
     &            dx * array(ixpp1, iypp1))
            endif
          enddo
        else
c           
c           cubic interpolation
c           
          do ix=1,nxb
            xp = a11*ix + xbase
            yp = a21*ix + ybase
            call interpolateGrid(xp, yp, dxGrid, dyGrid, ixgDim, nxGrid,
     &          nyGrid, ixGridstrt, xGridIntrv, iyGridStrt,
     &          yGridIntrv, dx, dy)
            xp = xp + dx
            yp = yp + dy
            IXP = XP
            IYP = YP
            bray(ix,iy)=dmean
            IF (IXP .ge. 2 .and. IXP .lt. NXA - 1 .and. IYP .ge. 2 .and.
     &          IYP .lt. NYA - 1) then

              DX = XP - IXP
              DY = YP - IYP
              IXPP1 = IXP + 1
              IXPM1 = IXP - 1
              IYPP1 = IYP + 1
              IYPM1 = IYP - 1
              ixpp2 = ixp + 2
              iypp2 = iyp + 2
              
              dxm1 = dx-1.
              dxdxm1=dx*dxm1
              fx1=-dxm1*dxdxm1
              fx4=dx*dxdxm1
              fx2=1+dx**2*(dx-2.)
              fx3=dx*(1.-dxdxm1)
              
              dym1 = dy-1.
              dydym1=dy*dym1
              
              v1=fx1*array(ixpm1,iypm1)+fx2*array(ixp,iypm1)+
     &            fx3*array(ixpp1,iypm1)+fx4*array(ixpp2,iypm1)
              v2=fx1*array(ixpm1,iyp)+fx2*array(ixp,iyp)+
     &            fx3*array(ixpp1,iyp)+fx4*array(ixpp2,iyp)
              v3=fx1*array(ixpm1,iypp1)+fx2*array(ixp,iypp1)+
     &            fx3*array(ixpp1,iypp1)+fx4*array(ixpp2,iypp1)
              v4=fx1*array(ixpm1,iypp2)+fx2*array(ixp,iypp2)+
     &            fx3*array(ixpp1,iypp2)+fx4*array(ixpp2,iypp2)
              bray(ix,iy)=-dym1*dydym1*v1+(1.+dy**2*(dy-2.))*v2+
     &            dy*(1.-dydym1)*v3 +dy*dydym1*v4
c               
            endif
          enddo
        endif
        
      enddo
      RETURN
      END
