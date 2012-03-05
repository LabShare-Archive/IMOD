c       $Id$
c       Log at end of file
c       !
c       Returns piece coordinates from the extra header written by SerialEM
c       ^  [array] = array of extra header data
c       ^  [nbsym] = number of bytes of data there
c       ^  [nbyte] = number of bytes per section
c       ^  [iflags] = flags for type of data present
c       ^  [nz] = number of pieces in the file
c       ^  [ixpiece], [iypiece], [izpiece] = arrays in which coordinates are
c       returned
c       ^  [npiece] = number of coordinates returned (should equal [nz])
c       ^  [maxpiece] = size of [piece] arrays
c       !
      subroutine get_extra_header_pieces (array,nbsym,nbyte,iflags,nz,
     &    ixpiece,iypiece,izpiece,npiece,maxpiece)
      implicit none
      include 'endian.inc'
      integer*1 array(*)
      integer*4 ixpiece(*),iypiece(*),izpiece(*)
      integer*4 nbsym,nbyte,iflags,nz,npiece,maxpiece
      integer*4 i4temp
      integer*2 i2temp(2),iztemp
      equivalence (i2temp,i4temp)
      logical nbytes_and_flags,shorts,allzero
      integer*4 i,ind,nbskip,ind_piece
      real*4 xtemp,ytemp,ztemp,crit
      
      crit = 1.e-5
      npiece =0
      if(nbsym.eq.0) return
      if (nz.gt.maxpiece) then
        write(*,'(/,a,a)')'ERROR: GET_EXTRA_HEADER_PIECES ',
     &      '- ARRAYS NOT LARGE ENOUGH FOR PIECE LISTS'
        call exit(1)
      endif

      shorts=nbytes_and_flags(nbyte,iflags)
      if(shorts) then
c         
c         if data are packed as shorts, see if the montage flag is set then
c         set starting index based on whether there are tilt angles too
c         
        if(mod(iflags/2,2).eq.0.or.nbyte.eq.0) return
        ind=1
        if(mod(iflags,2).ne.0)ind=3
        i4temp = 0
        do i=1,nz
          if(ind.gt.nbsym)return
          call move(i2temp(lowbyte),array(ind),2)
          ixpiece(i)=i4temp
          call move(i2temp(lowbyte),array(ind+2),2)
          iypiece(i)=i4temp
          call move(iztemp,array(ind+4),2)
          izpiece(i)=iztemp
          ind=ind+nbyte
          npiece=i
        enddo
      else
c         
c         otherwise the coordinates MIGHT be in the reals, at the position
c         given by ind_piece
c         make sure there are enough reals in header, set up to skip ints
c         and reals
c         The code is ready to go, but disable it for now
c         
        ind_piece=999
        if(iflags.lt.ind_piece+2)return
        nbskip=4*(nbyte+iflags)
        ind=1+4*(nbyte+ind_piece-1)
        allzero=.true.
        do i=1,nz
          if(ind.gt.nbsym)then
            if(allzero)npiece=0
            return
          endif
          call move(xtemp,array(ind),4)
          ixpiece(i)=nint(xtemp)
          call move(ytemp,array(ind+4),4)
          iypiece(i)=nint(ytemp)
          call move(ztemp,array(ind+8),4)
          izpiece(i)=nint(ztemp)
c           
c           keep track of whether anything is nonzero
c           
          if(ixpiece(i).ne.0.or.iypiece(i).ne.0.or.izpiece(i).ne.0)
     &        allzero=.false.
c           
c           if any number is not close enough to being an integer, it
c           cannot be piece coordinates
c           
          if(abs(ixpiece(i)-xtemp).gt.crit*abs(xtemp).or.
     &        abs(iypiece(i)-ytemp).gt.crit*abs(ytemp).or.
     &        abs(izpiece(i)-ztemp).gt.crit*abs(ztemp)) then
            npiece=0
            return
          endif
          ind=ind+nbskip
          npiece=i
        enddo
c         
c         if no numbers were non-zero, just return zero pieces
c         
        if(allzero)npiece=0
      endif
      return
      end

c       !
c       Returns piece coordinates from a metadata autodoc file written by
c       SerialEM.  The file should be opened first with 
c       @autodoc.html#AdocOpenImageMetadata .
c       ^  [index] = index of autodoc
c       ^  [itype] = type of metadata file: 1 for one file, 2 for image series
c       ^  [nz] = number of pieces in the file
c       ^  [ixpiece], [iypiece], [izpiece] = arrays in which coordinates are
c       returned
c       ^  [npiece] = number of coordinates returned (should equal [nz])
c       ^  [maxpiece] = size of [piece] arrays
c       ^  [nfound] = number of sections piece coordinates were found for
c       !
      subroutine get_metadata_pieces(index, itype, nz, ixpiece,iypiece,izpiece,
     &    maxpiece, nfound)
      implicit none
      integer*4 ixpiece(*),iypiece(*),izpiece(*)
      integer*4 maxpiece, index, itype, nz, nfound
      integer*4 i,ind
      character*6 sectNames(2) /'ZValue', 'Image'/
      integer*4 AdocSetCurrent, AdocGetThreeIntegers
      
      if (nz.gt.maxpiece) then
        write(*,'(/,a,a)')'ERROR: GET_METADATA_PIECES ',
     &      '- ARRAYS NOT LARGE ENOUGH FOR PIECE LISTS'
        call exit(1)
      endif
      if (AdocSetCurrent(index) .ne. 0) then
        write(*,'(/,a,a)')'ERROR: GET_METADATA_PIECES ',
     &      '- FAILED TO SET AUTODOC INDEX'
        call exit(1)
      endif
      do i = 1, nz
        if (AdocGetThreeIntegers(sectNames(itype), i, 'PieceCoordinates',
     &      ixpiece(i), iypiece(i), izpiece(i)) .eq. 0) nfound = nfound + 1
      enddo
      return
      end

c       $Log$
c       Revision 3.9  2007/05/19 00:03:49  mast
c       Formatted documentation, imporved error printout
c
c       Revision 3.8  2005/12/21 15:26:46  mast
c       But don't use same int*2 for moving Z and X/Y around
c       
c       Revision 3.7  2005/12/20 15:55:35  mast
c       But treat z as signed still!
c       
c       Revision 3.6  2005/12/20 15:54:13  mast
c       Had to account for endianness in moving unsigned ints
c       
c       Revision 3.5  2005/12/20 01:40:04  mast
c       Changed piece coordinates to unsigned integers
c       
c       Revision 3.4  2005/12/09 04:39:44  mast
c       gfortran: .xor., continuation, byte, or open fixes
c       
c       Revision 3.3  2003/06/05 00:11:47  mast
c       Change STOP to standardized ERROR exit
c       
c       Revision 3.2  2002/02/26 23:08:19  mast
c       *** empty log message ***
c       
c       Revision 3.1  2002/02/26 23:07:14  mast
c       Added test for whether piece coordinates are packed as shorts or
c       reals, and ability to retrieve them if they are reals
c       
