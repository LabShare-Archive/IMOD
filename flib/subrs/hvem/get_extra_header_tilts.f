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
	subroutine get_extra_header_tilts
     &	    (array,nbsym,nbyte,iflags,nz,tilt,ntilt,maxtilts,izpiece)
	byte array(*)
	real*4 tilt(*)
	integer*4 izpiece(*)
	integer*2 temp
	ntilt =0
	if(mod(iflags,2).eq.0.or.nbyte.eq.0.or.nbsym.eq.0) return
	ind=1
	do i=1,nz
	  itilt=izpiece(i)+1
	  if(itilt.lt.1)stop
     &	      ' - TILT ARRAY NOT DESIGNED FOR NEGATIVE Z VALUES'
	  if(itilt.gt.maxtilts)stop
     &	      ' - ARRAY NOT BIG ENOUGH FOR TILT DATA'
	  ntilt=max(ntilt,itilt)
	  call move(temp,array(ind),2)
	  tilt(itilt)=temp/100.
	  ind=ind+nbyte
	  if(ind.gt.nbsym)return
	enddo
	return
	end

