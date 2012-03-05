c       AMAT_TO_ROTMAG converts a transformations matrix into
c       axial rotation and stretch specifications.  Variables are as above.
      subroutine amat_to_rotmag(amat,theta,ydtheta,smag,ydmag)
      real*4 amat(2,2)
      xmag=sqrt(amat(1,1)**2+amat(2,1)**2)
      xtheta=atan2d(amat(2,1),amat(1,1))
      ymag=sqrt(amat(1,2)**2+amat(2,2)**2)
      ytheta=atan2d(-amat(1,2),amat(2,2))
      smag=0.5*(xmag+ymag)
      ydmag=ymag-xmag
      ydtheta=ytheta-xtheta
      if(abs(ydtheta).gt.180.)ydtheta=ydtheta-sign(360.,ydtheta)
      theta=xtheta+0.5*ydtheta
      if(abs(theta).gt.180.)theta=theta-sign(360.,theta)
      return
      end
