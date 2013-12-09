c       COPY_TO_BACKUP copies the file to a backup with ~ added to the name
      subroutine copy_to_backup(filin)
      character*(*) filin
      character*640 comline
      comline = 'cp '//trim(filin)//' '//trim(filin)//'~'
      print *,'Copying file . . .'
      call system(comline)
      return
      end
