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
	integer*2 temp
	logical nbytes_and_flags,shorts
	integer*4 i,ind,nbskip,itilt
c
	ntilt =0
	if(nbsym.eq.0) return
	shorts=nbytes_and_flags(nbyte,iflags)
	if(shorts) then
c	    
c	    if data are packed as shorts, then the tilt angle is the first
c	    set of data
c
	  if(mod(iflags,2).eq.0.or.nbyte.eq.0) return
	  nbskip=nbyte
	  ind=1
	else
c	    
c	    otherwise, tilt angle is the first float; need to skip over ints
c
	  if(iflags.eq.0)return
	  nbskip=4*(nbyte+iflags)
	  ind=1+4*nbyte
	endif
c
	do i=1,nz
	  itilt=izpiece(i)+1
	  if(itilt.lt.1)then
	    print *
	    print *,'ERROR: GET_EXTRA_HEADER_TILTS',
     &		' - TILT ARRAY NOT DESIGNED FOR NEGATIVE Z VALUES'
	    call exit(1)
	  endif
	  if(itilt.gt.maxtilts)then
	    print *
	    print *,'ERROR: GET_EXTRA_HEADER_TILTS',
     &	      ' - ARRAY NOT BIG ENOUGH FOR TILT DATA'
	    call exit(1)
	  endif
	  ntilt=max(ntilt,itilt)
	  if(shorts)then
	    call move(temp,array(ind),2)
	    tilt(itilt)=temp/100.
	  else
	    call move(tilt(itilt),array(ind),4)
	  endif
	  ind=ind+nbskip
	  if(ind.gt.nbsym)return
	enddo
	return
	end

