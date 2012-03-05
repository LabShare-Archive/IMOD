c       A function to determine whether "nint" and "nreal" from the
c       MRC header represent the number of bytes per section, and the
c       flags for what type of items are stored per section, as defined
c       by SerialEM on the Boulder HVEM.
c       It simply tests for whether the total number of bytes implied
c       by interpreting nreal as a set of flags equals nint.
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       
      logical function nbytes_and_flags(nint,nreal)
      implicit none
      integer*4 nint,nreal,nflags
      integer*4 nbytes_per_item(32)
      integer*4 i,nbyte_total,ntmp
c       
      call b3dHeaderItemBytes(nflags, nbytes_per_item)
      nbyte_total=0
      ntmp=nreal
      do i=1,nflags
        if(mod(ntmp,2).ne.0)nbyte_total=nbyte_total+nbytes_per_item(i)
        ntmp=ntmp/2
      enddo
      nbytes_and_flags=ntmp.eq.0.and.nbyte_total.eq.nint
      return
      end

