*	  XFPRODUCT  is used to concatenate two lists of transforms, or to
c	  multiply all of the transforms in one file by another transform.
c
c	  In the first case, for each line in the input files, it multiplies
c	  the transform in the first input file by the transform in the second
c	  file, and writes the product in the output file.  This can thus be
c	  used to implement two stage alignment: If the first set of
c	  transforms were obtained by a first stage of aligning raw images,
c	  and the second set are obtained by a second stage of alignment
c	  applied to images that have been aligned by the first set, then the
c	  products produced by this program can be applied to the raw images,
c	  in one step, to produce an alignment that incorporates both stages.
c	  
c	  To multiply all of the transforms in one file by a single transform,
c	  make the latter transform be the only one in a file.  The file with
c	  the single transform can be either the first or the second file.
c	  in the other file.
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
c
	parameter (nflimit=1000)
	real*4 f(2,3,nflimit),g(2,3,nflimit),prod(2,3)
	character*80 gfile
c
	write(*,'(1x,a,$)')
     &	    'file of transforms applied first: '
	read(*,'(a)')gfile
	call dopen(1,gfile,'ro','f')
	call xfrdall(1,f,nfirst,*92)
	print *,nfirst,' transforms'
c
	write(*,'(1x,a,$)')
     &	    'file of transforms applied second: '
	read(*,'(a)')gfile
	call dopen(2,gfile,'ro','f')
	call xfrdall(2,g,nsecond,*92)
	print *,nsecond,' transforms'
c	  
	if(nfirst.gt.nflimit.or.nsecond.gt.nflimit)then
	  print *,'too many transforms for array size'
	  stop
	endif
c
	write(*,'(1x,a,$)')
     &	    'new file for product transforms: '
	read(*,'(a)')gfile
	call dopen(3,gfile,'new','f')
c
	if(nsecond.ne.nfirst)then
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
	  call xfmult(f(1,1,i),g(1,1,i),prod)
	  call xfwrite(3,prod,*94)
	enddo
	print *,nout,' new transforms written'
	call exit(0)
c
92	print *,'ERROR: XFPRODUCT - reading old f/g file'
	call exit(1)
94	print *,'ERROR: XFPRODUCT - writing out g file'
	call exit(1)
	end
