c       
c       $Id$
c       
c       SDCALC compares positions in two image arrays ARRAY and BRRAY (each
c       dimensioned to NX,NY, the image dimensions as well), and within a
c       defined box, calculates the mean DDEN and standard deviation SD of
c       the pixel-by-pixel difference (B minus A).  I[XY]BOX[01] define the
c       array index limits of the box in ARRAY, and DX and DY define the
c       displacement between that position and the desired position in BRRAY.
c       If DX and DY are integers, the computation is done rapidly between
c       corresponding pixels; otherwise, quadratic interpolation is used to
c       find the pixel value in BRRAY for a given pixel in ARRAY.
c       
      subroutine sdcalc(array,brray,nx,ny,ixbox0,iybox0,ixbox1,iybox1
     &    ,dx,dy,sd,dden)
c       
      real*4 array(nx,ny),brray(nx,ny)
c       
      nsum=0
      sum=0.
      sumsq=0.
      sd=9999.
c       convert dx and dy into integer idx,idy and fractions dxint, dyint
c       use high end of box to be sure to get right dxint and dyint values
      yp=iybox1+dy
      iyp=nint(yp)
      dyint=yp-iyp
      idy=dy-dyint
      xp=ixbox1+dx
      ixp=nint(xp)
      dxint=xp-ixp
      idx=dx-dxint
      if(dxint.eq.0..and.dyint.eq.0.)then
c         if dx, dy are integers, don't need interpolation
c         compute limits for box in brray
        ixb0=max(1,ixbox0+idx)
        ixb1=min(nx,ixbox1+idx)
        iyb0=max(1,iybox0+idy)
        iyb1=min(ny,iybox1+idy)
        if(ixb0.le.ixb1.and.ixb0.le.nx.and.ixb1.ge.1
     &      .and.iyb0.le.iyb1.and.iyb0.le.ny.and.iyb1.ge.1)then
          do iyb=iyb0,iyb1
            iya=iyb-idy
            do ixb=ixb0,ixb1
              diff=brray(ixb,iyb)-array(ixb-idx,iya)
              sum=sum+diff
              sumsq=sumsq+diff**2
            enddo
          enddo
          nsum=(ixb1+1-ixb0)*(iyb1+1-iyb0)
        endif
      else
c         compute the coefficients for quadratic interpolation
        dysq=dyint**2
        c8=0.5*(dysq+dyint)
        c2=0.5*(dysq-dyint)
        dxsq=dxint**2
        c6=0.5*(dxsq+dxint)
        c4=0.5*(dxsq-dxint)
        c5=1.-dxsq-dysq
c         
        ixb0=max(2,ixbox0+idx)
        ixb1=min(nx-1,ixbox1+idx)
        iyb0=max(2,iybox0+idy)
        iyb1=min(ny-1,iybox1+idy)
        if(ixb0.le.ixb1.and.ixb0.lt.nx.and.ixb1.gt.1
     &      .and.iyb0.le.iyb1.and.iyb0.lt.ny.and.iyb1.gt.1)then
          do iyp=iyb0,iyb1
            iya=iyp-idy
            iypp1=iyp+1
            iypm1=iyp-1
            do ixp=ixb0,ixb1
              binterp=c5*brray(ixp,iyp)
     &            + c4*brray(ixp-1,iyp) + c6*brray(ixp+1,iyp)
     &            + c2*brray(ixp,iypm1) + c8*brray(ixp,iypp1)
              diff=binterp-array(ixp-idx,iya)
              sum=sum+diff
              sumsq=sumsq+diff**2
            enddo
          enddo
          nsum=(ixb1+1-ixb0)*(iyb1+1-iyb0)
        endif
      endif
      if(nsum.gt.0)dden=sum/nsum
      if(nsum.gt.1)sd=sqrt((sumsq-sum**2/nsum)/(nsum-1.))
      return
      end
