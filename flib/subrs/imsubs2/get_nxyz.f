c	  GET_NXYZ accepts a line of input and attempts to read it as 3
c	  integers.  If this succeeds, the values are returned as NXYZ.
c	  If this generates an error, it then uses the line as a filename and
c	  tries to open an MRC file on unit IUNIT.  NXYZ is then fetched from
c	  the header.
c
	subroutine get_nxyz(iunit,nxyz)
	integer*4 nxyz(3),mxyz(3)
	character*80 line
	read(5,'(a)')line
	if(line(1:1).eq.'/')go to 10
	read(line,*,err=10)(nxyz(i),i=1,3)
	return
10	call ialprt(.false.)
	call imopen(iunit,line,'ro')
	call irdhdr(iunit,nxyz,mxyz,mode,dmin,dmax,dmean)
	call imclose(iunit)
	call ialprt(.true.)
	return
	end
