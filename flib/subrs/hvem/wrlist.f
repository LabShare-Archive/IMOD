c	  WRLIST writes out the list of NLISTZ values in array LISTZ as a
c	  series of ranges separated by commas
c
	subroutine wrlist(listz,nlistz)
	integer*4 listz(*)
	parameter (limran=200)
	integer*4 numstr(limran),numend(limran),numout(limran)
	character*1 charout(limran)
c	integer*4 numstr(200),numend(200),numout(200)
c        character*1 charout(200)

	nranno=1
	numstr(1)=listz(1)
	do il=2,nlistz
	  if(listz(il).ne.listz(il-1)+1)then
	    numend(nranno)=listz(il-1)
	    nranno=nranno+1
	    numstr(nranno)=listz(il)
	  endif
	enddo
	numend(nranno)=listz(nlistz)
c	  
c	  turn the list of ranges into list of outputs
c
	nout=0
	do iran=1,nranno
	  nout=nout+1
	  numout(nout)=numstr(iran)
	  if(numstr(iran).lt.numend(iran))then
	    charout(nout)='-'
	    if(numstr(iran)+1.eq.numend(iran))charout(nout)=','
	    nout=nout+1
	    numout(nout)=numend(iran)
	  endif
	  charout(nout)=','
	enddo
	charout(nout)=' '
	write(*,'(16(i4,a1))')(numout(i),charout(i),i=1,nout)
	return
	end
