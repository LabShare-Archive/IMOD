      subroutine mypause(message)
      character*(*) message
      character ichar
      write(*,101)message
101   format(' PAUSE: ',a,'   (return to continue)',$)
      read(5,'(a)')ichar
      return
      end

