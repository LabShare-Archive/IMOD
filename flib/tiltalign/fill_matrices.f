c       fill_matrices has routines for setting up a matrix for each of the
c       individual changes.  It also contains a routine for converting alpha,
c       tilt, and rotation for beam tilt.
c       
c       $Id$
c       Log at end of file
c       
c
c       ZERO_MATRIX fills a matrix with zeros
c       
      subroutine zero_matrix(rmat, n)
      implicit none
      real*4 rmat(*)
      integer*4 n,i
      do i=1,n
        rmat(i) = 0.
      enddo
      return
      end

c       MAT_PRODUCT takes the product of RMAT, a NMROWS x NMCOLS matrix,
c       and PROD, a NPROWS x NPCOLS matrix applied first, and places the
c       resulting NMROWS x NPCOLS matrix back into PROD
c       Matrices must be organized to progress across rows
c       
      subroutine mat_product(prod, npRows, npCols, rmat, nmRows, nmCols)
      implicit none
      integer*4 npRows, npCols, nmRows, nmCols
      real*8 prod(npCols, npRows)
      real*4 rmat(nmCols, nmRows)
      real*8 tmp(3,3)
      integer*4 irow, icol, i
      do irow = 1, nmRows
        do icol = 1, npCols
          tmp(icol, irow) = 0.
          do i = 1, nmCols
            tmp(icol, irow) = tmp(icol, irow) + rmat(i, irow) * prod(icol, i)
          enddo
        enddo
      enddo
      do irow = 1, nmRows
        do icol = 1, npCols
          prod(icol, irow) = tmp(icol, irow)
        enddo
      enddo
      return
      end

c       CONVERT_FOR_BEAMTILT converts the given alpha, tilt angle, and
c       rotation for the beam tilt angle.  alf, tilt, and rot are assumed to
c       be in degrees while beamtilt must be in radians

      subroutine convert_for_beamtilt(alf, tilt, rot, beamTilt, ifanyalf)
      implicit none
      real*4 alf, tilt, rot, beamTilt
      integer*4 ifanyalf, i, j
      real*4 beamMat(9), beamInv(9),dmat(9),xtmat(9),ytmat(9),rmat(9),angles(3)
      real*4 cosalf, sinalf, cosbet, sinbet, cosBeam, sinBeam,costmp,sintmp
      real*8 pmat(9)
      real*4 dtor/0.0174532/
c
c       Get a matrix that includes X tilt, beam tilt, tilt, and rotation, then
c       derive 3 rotation angles from that
      call fill_beam_matrices(beamTilt, beamInv, beamMat, cosBeam, sinBeam)
      beamMat(7) = 0.
      beamMat(8) = sinBeam
      beamMat(9) = cosBeam
      call fill_xtilt_matrix(alf*dtor, ifanyalf, xtmat, cosalf,
     &    sinalf)
      call fill_ytilt_matrix(tilt*dtor, ytmat, cosbet, sinbet)
      call zero_matrix(rmat, 9)
      call fill_rot_matrix(rot*dtor, rmat, costmp, sintmp)
      rmat(3) = 0.
      rmat(4) = sintmp
      rmat(5) = costmp
      rmat(9) = 1.
      do i = 1, 9
        pmat(i) = xtmat(i)
      enddo
      call mat_product(pmat, 3, 3, beamInv, 3, 3)
      call mat_product(pmat, 3, 3, ytmat, 3, 3)
      call mat_product(pmat, 3, 3, beamMat, 3, 3)
      call mat_product(pmat, 3, 3, rmat, 3, 3)
c       
c       transpose the matrix to go into the stock routines
c       
      do i = 1, 3
        do j = 1, 3
          xtmat(i + 3 * (j - 1)) = pmat(j + 3 * (i - 1))
        enddo
      enddo
      call inv_matrix(xtmat, ytmat)
      call icalc_angles(angles, ytmat)
c       write(*,'(i4,3(2f9.2,3x))')iv, rot, -angles(3), tilt,
c       &          -angles(2), alf, -angles(1)
c       
c       replace the angles - then assume beam tilt 0 below
c       
      rot = -angles(3)
      tilt = -angles(2)
      alf = -angles(1)
      return
      end


c       ROUTINES FOR FILLING MATRICES: they all fill a matrix of a defined
c       geometry for the particular transformation, take angles in radians,
c       and return cosine and sine of the relevant angle

c       FILL_DIST_MATRIX fills a 3x3 matrix DMAT for in-plane distortions,
c       given mag GMAG, x-stretch DMAG, X-axis rotation angle SKEW,
c       compression COMP, and stretch type istrType
c       
      subroutine fill_dist_matrix(gmag, dmag, skew, comp, istrType,
     &    dmat, cdel, sdel)
      implicit none
      real*4 gmag, dmag, skew, comp, dmat(*), cdel, sdel,xmag
      integer*4 istrType

      xmag = gmag + dmag
      cdel = cos(skew)
      sdel = sin(skew)
      call zero_matrix(dmat, 9)
      if (istrType .eq. 1)then
        dmat(1)=xmag*cdel
        dmat(4)=xmag*sdel
        dmat(5)=gmag
      else if (istrType .eq. 2) then
        dmat(1)=xmag*cdel
        dmat(2)=-xmag*sdel
        dmat(4)=-gmag*sdel
        dmat(5)=gmag*cdel
      else
        dmat(1)=(gmag-dmag)*cdel
        dmat(2)=-xmag*sdel
        dmat(4)=-(gmag-dmag)*sdel
        dmat(5)=xmag*cdel
      endif
      dmat(9)=gmag*comp
      return
      end

c       FILL_XTILT_MATRIX fills 3x3 matrix XTMAT given X-axis tilt angle ALF,
c       if IFANYALF is not zero; otherwise it assumes alpha = 0
c       
      subroutine fill_xtilt_matrix(alf, ifanyalf, xtmat, calf, salf)
      implicit none
      real*4 alf, xtmat(*), calf, salf
      integer*4 ifanyalf
c       
      call zero_matrix(xtmat, 9)
      xtmat(1)=1.
      xtmat(5)=1.
      xtmat(9)=1.
      if(ifanyalf.ne.0)then
        calf=cos(alf)
        salf=sin(alf)
        xtmat(5)=calf
        xtmat(6)=-salf
        xtmat(8)=salf
        xtmat(9)=calf
      else
        calf = 1.
        salf = 0.
      endif
      return
      end

c       FILL_BEAM_MATRICES fills 3x3 matrix beamInv and 2x3 matrix beamMat
c       given beam inclination angle beamTilt
c       
      subroutine fill_beam_matrices(beamTilt, beamInv, beamMat, cbeam, sbeam)
      implicit none
      real*4 beamMat(*), beamInv(*), cbeam, sbeam, beamTilt
c       
      call zero_matrix(beamInv, 9)
      call zero_matrix(beamMat, 6)
      
      beamInv(1)=1.
      beamMat(1)=1.
      cbeam=cos(beamTilt)
      sbeam=sin(beamTilt)
      beamInv(5)=cbeam
      beamInv(6)=sbeam
      beamInv(8)=-sbeam
      beamInv(9)=cbeam
      beamMat(5)=cbeam
      beamMat(6)=-sbeam
      return
      end

c       FILL_YTILT_MATRIX fills 3x3 matrix YTMAT given tilt angle TILT
c       
      subroutine fill_ytilt_matrix(tilt, ytmat, cbeta, sbeta)
      implicit none
      real*4 tilt, ytmat(*), cbeta, sbeta
c       
      call zero_matrix(ytmat, 9)
      cbeta=cos(tilt)
      sbeta=sin(tilt)
      ytmat(1)=cbeta
      ytmat(3)=sbeta
      ytmat(5)=1.
      ytmat(7)=-sbeta
      ytmat(9)=cbeta
      return
      end

c       FILL_PROJ_MATRIX fills 2x2 matrix PROJMAT for projection skew
c       given skew PROJSKEW and the approximate rotation angle of the axes,
c       projStrRot
c       
      subroutine fill_proj_matrix(projStrRot, projSkew, projMat,
     &    cosPSkew, sinPSkew, cos2rot, sin2rot)
      implicit none
      real *4 projStrRot, projSkew, projMat(*), cosPSkew, sinPSkew, cos2rot
      real*4 sin2rot
      cosPSkew = cos(projSkew)
      sinPSkew = sin(projSkew)
      cos2rot = cos(2 * projStrRot)
      sin2rot = sin(2 * projStrRot)
      projMat(1) = cosPSkew + sinPskew * sin2rot
      projMat(2) = -sinPSkew * cos2rot
      projMat(3) = projMat(2)
      projMat(4) = cosPSkew - sinPskew * sin2rot
      return
      end

c       FILL_ROT_MATRIX fills 2x2 matrix RMAT given rotation angle ROT
c       
      subroutine fill_rot_matrix(rot, rmat, cgam, sgam)
      implicit none
      real*4 rot, rmat(*), cgam, sgam
      cgam=cos(rot)
      sgam=sin(rot)
      rmat(1)=cgam
      rmat(2)=-sgam
      rmat(3)=sgam
      rmat(4)=cgam
      return
      end

      
c       $Log$
