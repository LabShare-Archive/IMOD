c*BITSKIP implements a skip for n-bit data: skip npixel pixels on unit iunit
c
        subroutine altskip(iunit,npixel,*)
	include 'imsubs.inc'
        if(npixel.le.0)return
        if(mode(iunit).le.8)then
          call qskip(iunit,npixel)     !regular mode: call regular skip
          return
        endif                          !otherwise fall into bitskip
        entry bitskip(iunit,npixel,*)
        nbitskip=mode(iunit)*npixel    !total bits to skip
        nbitskip=nbitskip-ibleft(iunit) !minus the ones in current byte
        nbytes=nbitskip/8              !bytes to simply skip
        call qskip(iunit,nbytes)
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
