*	  XFPRODUCT  is used to concatenate two lists of transforms, or to
c	  multiply all of the transforms in one file by another transform.
c
c	  See man page for details.
c
c	  David Mastronarde 1989
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.1  2003/08/02 22:32:14  mast
c	  Standardize error output and exit
c	
c	  
	implicit none
	integer nflimit
	parameter (nflimit=100000)
	real*4 f(2,3,nflimit),g(2,3,nflimit),prod(2,3)
	character*120 gfile
c	  
	integer*4 nfirst, nsecond, nout, ierr, i
	real*4 scale1, scale2
c
	logical pipinput
	integer*4 numOptArg, numNonOptArg
	integer*4 PipGetTwoFloats
	integer*4 PipGetInOutFile
c	  
c	  
c         fallbacks from ../../manpages/autodoc2man -2 2  xfproduct
c	  
	integer numOptions
	parameter (numOptions = 5)
	character*(40 * numOptions) options(1)
	options(1) =
     &      'in1:InputFile1:FN:@in2:InputFile2:FN:@'//
     &      'output:OutputFile:FN:@scale:ScaleShifts:FP:@help:usage:B:'
c
	scale1 = 1.
	scale2 = 1.
c
	call PipReadOrParseOptions(options, numOptions, 'xfproduct',
     &	    'ERROR: XFPRODUCT - ', .true., 3, 2, 1, numOptArg,
     &	    numNonOptArg)
	pipinput = numOptArg + numNonOptArg .gt. 0

	if (PipGetInOutFile('InputFile1', 1,
     &	    'File of transforms applied first', gfile)
     &	    .ne. 0) call errorexit('NO FIRST INPUT FILE SPECIFIED')
	call dopen(1,gfile,'ro','f')
	call xfrdall(1,f,nfirst,*92)
	print *,nfirst,' transforms in first file'
c
	if (PipGetInOutFile('InputFile2', 2,
     &	    'File of transforms applied second', gfile)
     &	    .ne. 0) call errorexit('NO SECOND INPUT FILE SPECIFIED')
	call dopen(2,gfile,'ro','f')
	call xfrdall(2,g,nsecond,*92)
	print *,nsecond,' transforms in second file'
c	  
	if (nfirst.gt.nflimit.or.nsecond.gt.nflimit)
     &	    call errorexit('too many transforms for array size')
c
	if (PipGetInOutFile('OutputFile', 3,
     &	    'New file for product transforms', gfile)
     &	    .ne. 0) call errorexit('NO OUTPUT FILE SPECIFIED')
	call dopen(3,gfile,'new','f')

	if (pipinput) ierr = PipGetTwoFloats('ScaleShifts', scale1, scale2)
	call PipDone()
c
	if (nsecond.ne.nfirst)then
	  if(nsecond.eq.1)then
	    do i=2,nfirst
	      call xfcopy(g(1,1,1),g(1,1,i))
	    enddo
	    nsecond=nfirst
	    print *,'Single second transform applied to all',
     &		' first transforms'
	  elseif(nfirst.eq.1)then
	    do i=2,nsecond
	      call xfcopy(f(1,1,1),f(1,1,i))
	    enddo
	    nfirst=nsecond
	    print *,'Single first transform applied to all',
     &		' second transforms'
	  else
	    print *,'Number of transforms does not match'
	  endif
	endif
c	  
	nout=min(nfirst,nsecond)
	do i=1,nout
	  f(1,3,i) = f(1,3,i) * scale1
	  f(2,3,i) = f(2,3,i) * scale1
	  g(1,3,i) = g(1,3,i) * scale2
	  g(2,3,i) = g(2,3,i) * scale2
	  call xfmult(f(1,1,i),g(1,1,i),prod)
	  call xfwrite(3,prod,*94)
	enddo
	print *,nout,' new transforms written'
	call exit(0)
c
92	call errorexit('reading old f/g file')
94	call errorexit('writing out g file')
	end

	subroutine errorexit(message)
	character*(*) message
	print *
	print *,'ERROR: XFPRODUCT - ',message
	call exit(1)
	end
