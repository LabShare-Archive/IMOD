c	  GET_EXTRA_HEADER_PIECES will return piece coordinates from the extra
c	  header.
c	  ARRAY = array of extra header data
c	  NBSYM = number of bytes of data there
c	  NBYTE = number of bytes per section
c	  IFLAGS = flags for type of data present
c	  NZ = number of pieces
c	  IXPIECE, IYPIECE, IZPIECE = arrays in which coordinates are returned
c	  NPIECE = number of coordinates returned (= NZ)
c	  MAXPIECE = size of PIECE arrays
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.2  2002/02/26 23:08:19  mast
c	  *** empty log message ***
c	
c	  Revision 3.1  2002/02/26 23:07:14  mast
c	  Added test for whether piece coordinates are packed as shorts or
c	  reals, and ability to retrieve them if they are reals
c	
c
	subroutine get_extra_header_pieces (array,nbsym,nbyte,iflags,nz,
     &	    ixpiece,iypiece,izpiece,npiece,maxpiece)
	implicit none
	byte array(*)
	integer*4 ixpiece(*),iypiece(*),izpiece(*)
	integer*4 nbsym,nbyte,iflags,nz,npiece,maxpiece
	integer*2 temp
	logical nbytes_and_flags,shorts,allzero
	integer*4 i,ind,nbskip,ind_piece
	real*4 xtemp,ytemp,ztemp,crit
	
	npiece =0
	if(nbsym.eq.0) return
	if (nz.gt.maxpiece) then
	  print *
	  print *,'ERROR: GET_EXTRA_HEADER_PIECES ',
     &	      '- ARRAYS NOT LARGE ENOUGH FOR PIECE LISTS'
	  call exit(1)
	endif

	shorts=nbytes_and_flags(nbyte,iflags)
	if(shorts) then
c	    
c	    if data are packed as shorts, see if the montage flag is set then
c	    set starting index based on whether there are tilt angles too
c
	  if(mod(iflags/2,2).eq.0.or.nbyte.eq.0) return
	  ind=1
	  if(mod(iflags,2).ne.0)ind=3
	  do i=1,nz
	    if(ind.gt.nbsym)return
	    call move(temp,array(ind),2)
	    ixpiece(i)=temp
	    call move(temp,array(ind+2),2)
	    iypiece(i)=temp
	    call move(temp,array(ind+4),2)
	    izpiece(i)=temp
	    ind=ind+nbyte
	    npiece=i
	  enddo
	else
c	    
c	    otherwise the coordinates MIGHT be in the reals, at the position
c	    given by ind_piece
c	    make sure there are enough reals in header, set up to skip ints
c	    and reals
c	    The code is ready to go, but disable it for now
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
c	      keep track of whether anything is nonzero
c	      
	    if(ixpiece(i).ne.0.or.iypiece(i).ne.0.or.izpiece(i).ne.0)
     &		allzero=.false.
c	      
c	      if any number is not close enough to being an integer, it
c	      cannot be piece coordinates
c
	    if(abs(ixpiece(i)-xtemp).gt.crit*abs(xtemp).or.
     &		abs(iypiece(i)-ytemp).gt.crit*abs(ytemp).or.
     &		abs(izpiece(i)-ztemp).gt.crit*abs(ztemp)) then
	      npiece=0
	      return
	    endif
	    ind=ind+nbskip
	    npiece=i
	  enddo
c	    
c	    if no numbers were non-zero, just return zero pieces
c
	  if(allzero)npiece=0
	endif
	return
	end
