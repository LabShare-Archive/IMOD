*	  XFINVERSE will take the inverse of each transformation in a file.
c	  The only option is whether to set the translations to zero or retain
c	  them in the output.  The latter would give the true inverse, but the
c	  former has been useful in arcane realignment procedures.
c	  
c	  Inputs to the program:
c	  
c	  Input file with list of transformations
c	  
c	  Output file for inverse transformations
c	  
c	  0 to set the translations (shifts) to zero, or one to retain them
c	  and output the true inverse transformation
c	  
c	  David Mastronarde, added to IMOD 4/21/00
c
	parameter (nflimit=1000)
	real*4 f(2,3,nflimit),g(2,3,nflimit),prod(2,3)
	character*80 gfile
c
	write(*,'(1x,a,$)')
     &	    'Input file of transforms: '
	read(*,'(a)')gfile
	call dopen(1,gfile,'ro','f')
	call xfrdall(1,f,nout,*92)
c
	write(*,'(1x,a,$)')
     &	    'Output file for inverse transforms: '
	read(*,'(a)')gfile
	call dopen(3,gfile,'new','f')
c	  
	write(*,'(1x,a,$)')
     &	    '0 to set shifts to zero, or 1 to retain them: '
	read(5,*)ifretain
c
	do i=1,nout
	  call xfinvert(f(1,1,i),prod)
	  if(ifretain.eq.0)then
	    prod(1,3)=0.
	    prod(2,3)=0.
	  endif
	  call xfwrite(3,prod,*94)
	enddo
	print *,nout,' inverse transforms written'
	call exit(0)
c
92	print *,'error reading old f/g file'
	stop
94	print *,'error writing out g file'
	stop
	end
