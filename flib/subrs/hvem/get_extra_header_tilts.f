c	  GET_EXTRA_HEADER_TILTS will return tilt angles from extra header,
c	  based on the Z values of the pieces as listed in IZPIECE.
c	  ARRAY = array of extra header data
c	  NBSYM = number of bytes of data there
c	  NBYTE = number of bytes per section
c	  IFLAGS = flags for type of data present
c	  NZ = number of sections or pieces
c	  TILT = array for tilt angles
c	  NTILT = # of tilt angles returned (or highest # if there are gaps)
c	  MAXTILTS = size of TILT array
c	  IZPIECE = Z value of each section in file
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.2  2003/06/05 00:11:38  mast
c	  Change STOP to standardized ERROR exit
c	
c	  Revision 3.1  2002/02/26 23:09:22  mast
c	  Added test for whether tilt angles are packed as shorts or reals,
c	  and ability to retrieve them if they are the first real
c	
c
	subroutine get_extra_header_tilts
     &	    (array,nbsym,nbyte,iflags,nz,tilt,ntilt,maxtilts,izpiece)
	implicit none
	byte array(*)
	real*4 tilt(*)
	integer*4 izpiece(*)
	integer*4 nbsym,nbyte,iflags,nz,ntilt,maxtilts

	call get_extra_header_items(array, nbsym, nbyte,iflags,nz,1,tilt,
     &	    tilt,ntilt,maxtilts,izpiece)
	return
	end

	subroutine get_extra_header_items(array,nbsym,nbyte,iflags,nz,
     &	    itype,val1,val2,nvals,maxvals,izpiece)
	implicit none
	byte array(*)
	real*4 val1(*), val2(*)
	integer*4 izpiece(*)
	integer*4 nbsym,nbyte,iflags,nz,nvals,maxvals,itype
	integer*2 temp
	logical nbytes_and_flags,shorts
	integer*4 i,ind,nbskip,ival,nflags
	integer*4 nbytes_per_item(32)
c
	nvals =0
	if (nbsym .eq. 0) return
c	  
	call b3dHeaderItemBytes(nflags, nbytes_per_item)
c
	shorts=nbytes_and_flags(nbyte,iflags)
	if(shorts) then
c	    
c	    if data are packed as shorts, then test for the bit corresponding
c	    to itype.  Skip nbyte between sections and advance starting index
c	    for each entry prior to the desired one
c
	  if (mod(iflags/2**(itype-1), 2).eq.0 .or. nbyte.eq.0) return
	  nbskip=nbyte
	  ind=1
	  do i = 1, itype - 1
	    if (mod(iflags/2**(i-1), 2) .ne. 0) ind = ind + nbytes_per_item(i)
	  enddo
	else
c	    
c	    otherwise, tilt angle is the first float; need to skip over ints
c
	  if(iflags.eq.0 .or. itype.gt.1)return
	  nbskip=4*(nbyte+iflags)
	  ind=1+4*nbyte
	endif
c
	do i=1,nz
	  ival=izpiece(i)+1
	  if(ival.lt.1)then
	    print *
	    print *,'ERROR: GET_EXTRA_HEADER_ITEMS',
     &		' - VALUE ARRAY NOT DESIGNED FOR NEGATIVE Z VALUES'
	    call exit(1)
	  endif
	  if(ival.gt.maxvals)then
	    print *
	    print *,'ERROR: GET_EXTRA_HEADER_ITEMS',
     &	      ' - ARRAY NOT BIG ENOUGH FOR DATA'
	    call exit(1)
	  endif
	  nvals=max(nvals,ival)
	  if(shorts)then
	    call move(temp,array(ind),2)
	    if (itype .eq. 1) then
	      val1(ival)=temp/100.		! Tilt angle * 100
	    elseif (itype .eq. 3) then
	      val1(ival) = temp / 25.		!Stage X and Y * 25.
	      call move(temp, array(ind + 2), 2)
	      val2(ival) = temp / 25.
	    elseif (itype .eq. 4) then
	      val1(ival) = temp * 100.		! Magnification / 100
	    elseif (itype .eq. 5) then
	      val1(ival) = temp / 25000.	! Intensity * 25000.
	    endif
	  else
	    call move(val1(ival),array(ind),4)
	  endif
	  ind=ind+nbskip
	  if(ind.gt.nbsym)return
	enddo
	return
	end
