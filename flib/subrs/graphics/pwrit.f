c	  This file contains modules to adapt from NCAR graphics to Postscript
c	  graphics subroutines

	subroutine pwrit(x,y,string,nchar,jsize,jor,jctr)
	character*(*) string
	call wtstr(x,y,string(1:nchar),jsize/32,jor,jctr)
	return
	end

	subroutine pwritx(x,y,string,nchar,jsize,jor,jctr)
	character*(*) string
	call wtstr(x,y,string(1:nchar),jsize,jor,jctr)
	return
	end


	subroutine pwritx_parse(ascstr,pwrstr,ichout)
	character*(*) ascstr,pwrstr
	pwrstr=ascstr
	ichout=lnblnk(ascstr)
	return
	end


	subroutine setpwrmode
	return
	end
