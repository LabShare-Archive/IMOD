c       FLNAM reads a file name.  It can provide the prompt for the name,
c       and it can supply an extension if desired.
c       
      subroutine flnam (name,ifmes,ext)
      character*(*) name,ext
      if(ifmes.ne.0)then
        if(ext.eq.'0')then
          write(*,'(1x,a,$)')'File name: '
        else
          write(*,'(1x,a,a,a$)')'File name (.',ext,' assumed): '
        endif
      endif
      read(5,10)name
10    format(a)
      if(ext.eq.'0'.or.index(name,'.').ne.0)return
      name=trim(name)//'.'//trim(ext)
      return
      end

