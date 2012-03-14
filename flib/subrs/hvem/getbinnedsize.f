c       ! Given an original image dimension [nx] and a binning [nbin],
C       computes a binned size [nxbin] and an offset [ixOffset] for binned
c       pixels that will contain all of the original data and is as centered
c       as possible.  The offset is the negative of the number of
c       non-existent pixels in the first binned pixel.
c       !
c       $Author$
c       
c       $Date$
c       
c       $Revision$

      subroutine getBinnedSize(nx, nbin, nxbin, ixOffset)
      implicit none
      integer*4 nx, nxbin, nbin, ixOffset
      integer*4 irem
c       
c       get rounded down binned size and remainder
c       
      nxbin = nx / nbin
      irem = nx - nbin * nxbin
c       
c       if binned size is same even/odd state as original size, increase
c       by 2 pixels if there is more than 1 remainder pixel
c       
      if (mod(nx, 2) .eq. mod(nxbin, 2)) then
        if (irem .gt. 1) nxbin = nxbin + 2
      else
c         
c         if there is change in even/odd state, must increase by 1 pixel
c         and add 1 binned pixel's worth of pixels to remainder to divide
c         between left and right
c         
        nxbin = nxbin + 1
        irem = irem + nbin
      endif
c       
c       The offset is the negative of the number of non-existent pixels in
c       the first binned pixel
c       
      ixOffset = 0
      if (irem .gt. 1) ixOffset = -(nbin - irem/2)
      return
      end
