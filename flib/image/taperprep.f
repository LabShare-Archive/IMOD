c       $Id$
c
c       $Log$
c
c       TAPERPREP does common work of getting subvolume parameters and
c       computing the real image box size for taperoutvol and combinefft
c
c       PIPINPUT indicates whether PIP input is being used
c       noFFT is a flag that sizes are not to be increased for FFTs
c       NXYZ(3) has the input file sizes
c       IXLO, IXHI, IYLO, IYHI, IZLO, IZHI are returned with the index 
c       coordinates of the subvolume in the file
c       NXBOX, NYBOX, NZBOX are returned with the size of the box being read in
c       NX3, NY3, NZ3 are returned with the size of the padded volume
c       NXYZ2, MXYZ2 are filled with the values of nx3, ny3, nz3
c       CELL2 is returned with the correct cell values
c       ORIGX, ORIGY, ORIGZ are returned with the subvolume origin values
c
      subroutine taperprep(pipinput, noFFT, nxyz, ixlo, ixhi, iylo, iyhi, izlo,
     &    izhi, nxbox, nybox, nzbox, nx3, ny3, nz3, nxyz2, mxyz2, cell2,
     &    origx, origy, origz)
      implicit none
      logical pipinput, noFFT
      integer*4 nxyz(3), ixlo, ixhi, iylo, iyhi, izlo,izhi, nxbox, nybox, nzbox
      integer*4 nx3, ny3, nz3, nxyz2(3), mxyz2(3)
      real*4 cell2(6), origx, origy, origz
      integer*4 npadx, npady, npadz, ierr
      real*4 delta(3)
      integer*4 niceframe, PipGetTwoIntegers, PipGetThreeIntegers

      ixlo=0
      iylo=0
      izlo=0
      ixhi=nxyz(1)-1
      iyhi=nxyz(2)-1
      izhi=nxyz(3)-1
      npadx = 0
      npady = 0
      npadz = 0
      if (pipinput) then
        ierr = PipGetTwoIntegers('XMinAndMax', ixlo, ixhi)
        ierr = PipGetTwoIntegers('YMinAndMax', iylo, iyhi)
        ierr = PipGetTwoIntegers('ZMinAndMax', izlo, izhi)
        ierr = PipGetThreeIntegers('TaperPadsInXYZ', npadx,npady,npadz)
      else
        write(*,'(1x,a,/,a,$)')'Starting and ending X, then Y, then '//
     &      'Z index coordinates to extract',' (/ for whole volume): '
        read(5,*)ixlo,ixhi,iylo,iyhi,izlo,izhi
        write(*,'(1x,a,$)')'Width of pad/taper borders in X, Y, and Z: '
        read(5,*)npadx,npady,npadz
      endif
c       
      if(ixlo.lt.0.or.ixhi.ge.nxyz(1).or.iylo.lt.0.or.iyhi.ge.nxyz(2)
     &    .or.izlo.lt.0.or.izhi.ge.nxyz(3))call exitError(
     &    'BLOCK NOT ALL INSIDE VOLUME')
      nxbox=ixhi+1-ixlo
      nybox=iyhi+1-iylo
      nzbox=izhi+1-izlo
c       
      if (noFFT) then
        nx3 = nxbox + 2 * npadx
        ny3 = nybox + 2 * npady
        nz3 = nzbox + 2 * npadz
      else
        nx3=niceframe(2*((nxbox+1)/2+npadx),2,19)
        ny3=niceframe(2*((nybox+1)/2+npady),2,19)
        nz3 = nzbox
        if (nz3 .gt. 1 .or. npadz .gt. 0)
     &      nz3=niceframe(2*((nzbox+1)/2+npadz),2,19)
      endif

      call irtdel(1,delta)
      call irtorg(1, origx, origy, origz)
      MXYZ2(1)=NX3
      MXYZ2(2)=NY3
      MXYZ2(3)=nz3
      nxyz2 = mxyz2
      cell2(1:3) = MXYZ2 * delta
      CELL2(4:6) = 90.
      origx = origx - delta(1) * (ixlo - (nx3 - nxbox) / 2)
      origy = origy - delta(2) * (iylo - (ny3 - nybox) / 2)
      origz = origz - delta(3) * (izlo - (nz3 - nzbox) / 2)
      return
      end
