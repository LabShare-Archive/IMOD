c       $Id$
c       
c       !
c       Composes a filename from an optional temporary directory [tempdir], a
c       filename in [filein] with leading directories stripped off, and an
c       extension in [tempext]
c       
      character*320 function temp_filename(filein,tempdir,tempext)
      implicit none
      character*(*) filein,tempdir,tempext
      integer*4 inend, instr, i
c       
c       find last / in filein
c       
      inend = len_trim(filein)
      instr = 1
      do i = 1, inend
        if (filein(i:i) == '/') instr = i + 1
      enddo
c       
      if (tempdir == ' ') then
        temp_filename = trim(filein(instr:inend))//'.'//trim(tempext)
      else
        temp_filename = trim(tempdir)//'/'//trim(filein(instr:inend))//'.'//trim(tempext)
      endif
      return
      end
