      subroutine date(dat)
      implicit none
      character*(*) dat
      call b3ddate(dat)
      return
      end

      subroutine time(tim)
      implicit none
      character*(*) tim
      character*10 packtime
      call date_and_time(TIME=packtime)
      write(tim,101)packtime(1:2),packtime(3:4),packtime(5:6)
101   format(a2,':',a2,':',a2)
      return
      end
