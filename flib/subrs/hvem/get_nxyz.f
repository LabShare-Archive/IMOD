c	  GET_NXYZ accepts a line of input and attempts to read it as 3
c	  integers.  If this succeeds, the values are returned as NXYZ.
c	  If this generates an error, it then uses the line as a filename and
c	  tries to open an MRC file on unit IUNIT.  NXYZ is then fetched from
c	  the header.
c
c	  If PIPINPUT is TRUE, then the string is fetched with the given
c	  OPTION.  Set PROGNAME to the name of the program if the option is
c	  mandatory.  If this entry is optional, pass PROGNAME as a blank
c	  string.
c	  
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.1  2002/07/21 19:36:30  mast
c	  Fixed treatment of / to just return and added ability for blank line
c	  to do the same
c	

	subroutine get_nxyz(pipinput,option,progname,iunit,nxyz)
	implicit none
	integer*4 nxyz(3),mxyz(3),iunit,mode,i, PipGetString
	character*(*) option, progname
	character*80 line
	logical pipinput
	real*4 dmin,dmax,dmean

	if (.not.pipinput) then
	  read(5,'(a)')line
	else
	  if (PipGetString(option, line) .gt. 0) then
	    if (progname .eq. ' ') return
	    print *
	    print *,'ERROR: ',progname,' - you must enter option ',option,
     &		' to specify file size or file name'
	    call exit(1)
	  endif
	endif
	if(line(1:1).eq.'/'.or.line.eq.' ')return
	read(line,*,err=10)(nxyz(i),i=1,3)
	return
10	call ialprt(.false.)
	call imopen(iunit,line,'ro')
	call irdhdr(iunit,nxyz,mxyz,mode,dmin,dmax,dmean)
	call imclose(iunit)
	call ialprt(.true.)
	return
	end
