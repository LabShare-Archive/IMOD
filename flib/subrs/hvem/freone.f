c	  FREONE gets a single value via FREFOR and converts to an integer
c	  FRETWO gets two values.
c
	subroutine freone(ivalue)
c
	character*50 dummy
	real*4 freinp(10)
	integer*4 ivalue,ival2
c
	nval=1
	go to 10
c
	entry fretwo(ivalue,ival2)
	nval=2
c
10	read(*,'(a)')dummy
	call frefor(dummy,freinp,ninp)
	if(ninp.gt.0)ivalue=nint(freinp(1))
	if(nval.gt.1.and.ninp.gt.1)ival2=nint(freinp(2))
	return
	end

