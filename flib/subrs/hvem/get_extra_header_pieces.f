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
	subroutine get_extra_header_pieces (array,nbsym,nbyte,iflags,nz,
     &	    ixpiece,iypiece,izpiece,npiece,maxpiece)
	byte array(*)
	integer*4 ixpiece(*),iypiece(*),izpiece(*)
	integer*2 temp
	npiece =0
	if(mod(iflags/2,2).eq.0.or.nbyte.eq.0.or.nbsym.eq.0) return
	ind=1
	if(mod(iflags,2).ne.0)ind=3
	if (nz.gt.maxpiece) stop
     &	    '- ARRAYS NOT LARGE ENOUGH FOR PIECE LISTS'
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
	return
	end
