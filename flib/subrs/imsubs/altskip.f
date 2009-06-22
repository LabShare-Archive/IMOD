c*BITSKIP implements a skip for n-bit data: skip npixel pixels on unit iunit
c
c       $Id$
c       
c       $Log$
c
      subroutine altskip(iunit,npix1, npix2,*)
      use imsubs
      implicit none
      integer*4 iunit,npix1, npix2
      if(npix1.le.0 .or. npix2 .le. 0)return
      if(mode(iunit).le.8)then
        call qskip(iunit,npix1,npix2)         !regular mode: call regular skip
        return
      endif                                     !otherwise call bitskip
      call bitskip(iunit, npix1 * npix2, 99)
      return
99    return 1
      end

      subroutine bitskip(iunit,npixel,*)
      use imsubs
      implicit none
      integer*4 iunit,npixel,nbitskip,nbytes,nleft,ier
      nbitskip=mode(iunit)*npixel    !total bits to skip
      nbitskip=nbitskip-ibleft(iunit) !minus the ones in current byte
      nbytes=nbitskip/8              !bytes to simply skip
      call qskip(iunit,nbytes,1)
      nleft=mod(nbitskip,8)          !# of bits left to skip
      if(nleft.eq.0)then             !if 0, then done, with none left over
        ibleft(iunit)=0
      else                           !otherwise get next byte into bytcur
        ibleft(iunit)=8-nleft
        call qread(iunit,bytcur(iunit),1,ier)
        if(ier.ne.0)return 1
      endif
      return
      end
