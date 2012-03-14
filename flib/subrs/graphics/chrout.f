c       CHROUT output a single character whose ascii value is given by the
c       integer argument
c       
      subroutine chrout(int)
c       VMS version
c       write(*,'(a)')char(0)//char(int)
c       unix version
c       ierr=putc(lshift(int,24))
c       version that will work for any system that doesn't swallow 
c       leading char
      write(*,'(a,$)')char(int)
      return
      end
