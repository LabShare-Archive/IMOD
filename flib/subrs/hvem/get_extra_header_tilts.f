c       $Id$
c       Log at end of file
c       !
c       Returns tilt angles from an extra header written by SerialEM or in the
c       Agard format.  It relies on the Z values of the sections in the file
c       as listed in [izpiece], which should be obtained first with 
c       @get_extra_header_pieces .
c       ^  [array] = array of extra header data
c       ^  [nbsym] = number of bytes of data there
c       ^  [nbyte] = number of bytes per section
c       ^  [iflags] = flags for type of data present
c       ^  [nz] = number of sections or pieces
c       ^  [tilt] = array for tilt angles
c       ^  [ntilt] = # of tilt angles returned (or highest # if there are gaps)
c       ^  [maxtilts] = size of TILT array
c       ^  [izpiece] = Z value of each section in file
c       !
      subroutine get_extra_header_tilts(array,nbsym,nbyte,iflags,nz,tilt,
     &    ntilt,maxtilts,izpiece)
      implicit none
      integer*1 array(*)
      real*4 tilt(*)
      integer*4 izpiece(*)
      integer*4 nbsym,nbyte,iflags,nz,ntilt,maxtilts

      call get_extra_header_items(array, nbsym, nbyte,iflags,nz,1,tilt,
     &    tilt,ntilt,maxtilts,izpiece)
      return
      end

c       !
c       Returns values of a defined type from an extra header written by 
c       SerialEM; will also return tilt angles from a header in the
c       Agard format.  It relies on the Z values of the sections in the file
c       as listed in [izpiece], which should be obtained first with 
c       @get_extra_header_pieces .
c       ^  [array] = array of extra header data
c       ^  [nbsym] = number of bytes of data there
c       ^  [nbyte] = number of bytes per section
c       ^  [iflags] = flags for type of data present
c       ^  [nz] = number of sections or pieces
c       ^  [itype] = type of data to retrieve: 1 for tilt angle, 3 for stage
c       position, 4 for magnification, 5 for intensity, 6 for exposure dose
c       ^  [val1], [val2] = arrays for one or two values to be returned
c       ^  [nvals] = # of values returned (or highest # if there are gaps)
c       ^  [maxvals] = size of [val1] and [val2] arrays
c       ^  [izpiece] = Z value of each section in file
c       !
      subroutine get_extra_header_items(array,nbsym,nbyte,iflags,nz,
     &    itype,val1,val2,nvals,maxvals,izpiece)
      implicit none
      integer*1 array(*)
      real*4 val1(*), val2(*)
      integer*4 izpiece(*)
      integer*4 nbsym,nbyte,iflags,nz,nvals,maxvals,itype
      integer*2 temp, temp2
      logical nbytes_and_flags,shorts
      integer*4 i,ind,nbskip,ival,nflags
      integer*4 nbytes_per_item(32)
      real*4 SEMshortsToFloat
c       
      nvals =0
      if (nbsym .eq. 0) return
c       
      call b3dHeaderItemBytes(nflags, nbytes_per_item)
c       
      shorts=nbytes_and_flags(nbyte,iflags)
      if(shorts) then
c         
c         if data are packed as shorts, then test for the bit corresponding
c         to itype.  Skip nbyte between sections and advance starting index
c         for each entry prior to the desired one
c         
        if (mod(iflags/2**(itype-1), 2).eq.0 .or. nbyte.eq.0) return
        nbskip=nbyte
        ind=1
        do i = 1, itype - 1
          if (mod(iflags/2**(i-1), 2) .ne. 0) ind = ind + nbytes_per_item(i)
        enddo
      else
c         
c         otherwise, tilt angle is the first float; need to skip over ints
c         
        if(iflags.eq.0 .or. itype.gt.1)return
        nbskip=4*(nbyte+iflags)
        ind=1+4*nbyte
      endif
c       
      do i=1,nz
        ival=izpiece(i)+1
        if(ival.lt.1)then
          write(*,'(/,a,a)')'ERROR: GET_EXTRA_HEADER_ITEMS',
     &        ' - VALUE ARRAY NOT DESIGNED FOR NEGATIVE Z VALUES'
          call exit(1)
        endif
        if(ival.gt.maxvals)then
          write(*,'(/,a,a)')'ERROR: GET_EXTRA_HEADER_ITEMS',
     &        ' - ARRAY NOT BIG ENOUGH FOR DATA'
          call exit(1)
        endif
        nvals=max(nvals,ival)
        if(shorts)then
          call move(temp,array(ind),2)
          if (itype .eq. 1) then
            val1(ival)=temp/100.                ! Tilt angle * 100
          elseif (itype .eq. 3) then
            val1(ival) = temp / 25.             !Stage X and Y * 25.
            call move(temp, array(ind + 2), 2)
            val2(ival) = temp / 25.
          elseif (itype .eq. 4) then
            val1(ival) = temp * 100.            ! Magnification / 100
          elseif (itype .eq. 5) then
            val1(ival) = temp / 25000.          ! Intensity * 25000.
          elseif (itype .eq. 6) then
            call move(temp2, array(ind + 2), 2)
            val1(ival) = SEMshortsToFloat(temp, temp2) ! Exposure dose
          endif
        else
          call move(val1(ival),array(ind),4)
        endif
        ind=ind+nbskip
        if(ind.gt.nbsym)return
      enddo
      return
      end

c       !
c       Converts the two short integers [low] and [ihigh] stored by SerialEM
c       for a floating point number back into the number
c       !
      real*4 function SEMshortsToFloat(low, ihigh)
      implicit none
      integer*2 low, ihigh
      integer*4 iexp, ival, ivalsign, iexpsign
      iexpsign = 1
      ivalsign = 1
      if (low .lt. 0) then
        low = -low
        ivalsign = -1
      endif
      if (ihigh .lt. 0) then
        ihigh = -ihigh
        iexpsign = -1
      endif
      ival = low * 100 + mod(ihigh, 100)
      iexp = ihigh / 100
      SEMshortsToFloat = ivalsign * (ival * 10.**(iexp * iexpsign))
      return
      end

c       
c       $Log$
c       Revision 3.4  2005/12/09 04:39:44  mast
c       gfortran: .xor., continuation, byte, or open fixes
c       
c       Revision 3.3  2004/03/18 17:56:17  mast
c       Changed to calling central routine with byte counts
c       
c       Revision 3.2  2003/06/05 00:11:38  mast
c       Change STOP to standardized ERROR exit
c       
c       Revision 3.1  2002/02/26 23:09:22  mast
c       Added test for whether tilt angles are packed as shorts or reals,
c       and ability to retrieve them if they are the first real
c       
