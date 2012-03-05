c       ROTMAG_TO_AMAT converts axial rotation and stretch specifications
c       into an A matrix for transformations, AMAT
c       THETA is global rotation (average rotation of X&Y axes)
c       YDTHETA is difference between rotations of Y- and X-axis
c       SMAG is global magnification (average stretch along X&Y axes)
c       YDMAG is difference between stretch along Y- and X-axis
      subroutine rotmag_to_amat(theta,ydtheta,smag,ydmag,amat)
      real*4 amat(2,2)
      xmag=smag-ydmag/2.
      ymag=smag+ydmag/2.
      xtheta=theta-ydtheta/2.
      ytheta=theta+ydtheta/2.
      amat(1,1)=xmag*cosd(xtheta)
      amat(2,1)=xmag*sind(xtheta)
      amat(1,2)=-ymag*sind(ytheta)
      amat(2,2)=ymag*cosd(ytheta)
      return
      end
