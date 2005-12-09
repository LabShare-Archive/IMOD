	subroutine date(dat)
	implicit none
	character*(*) dat
	character*8 packdate
	integer*4 ivals(8)
	character*3 months(12)
	data months /'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &	    'Sep','Oct','Nov','Dec'/
	call date_and_time(DATE=packdate, VALUES=ivals)
	write(dat,101)ivals(3),months(ivals(2)),packdate(3:4)
101	format(i2,'-',a3,'-',a2)
	return
	end

	subroutine time(tim)
	implicit none
	character*(*) tim
	character*10 packtime
	call date_and_time(TIME=packtime)
	write(tim,101)packtime(1:2),packtime(3:4),packtime(5:6)
101	format(a2,':',a2,':',a2)
	return
	end
