c	  FLNAM reads a file name.  It can provide the prompt for the name,
c	  and it can supply an extension if desired.
c
	subroutine flnam (name,ifmes,ext)
	character*(*) name,ext
	character*320 concat
	if(ifmes.ne.0)then
	  if(ext.eq.'0')then
	    write(*,'(1x,a,$)')'File name: '
	  else
	    write(*,'(1x,a,a,a$)')'File name (.',ext,' assumed): '
	  endif
	endif
	read(5,10)name
10	format(a)
	if(ext.eq.'0'.or.index(name,'.').ne.0)return
	name=concat(name,'.')
        name=concat(name,ext)
c	m=lnblnk(name)+1
c	name(m:m)='.'
c	name(m+1:m+4)=ext
	return
	end

