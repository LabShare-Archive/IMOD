c	  COPY_TO_BACKUP is a system-dependent routine.  On SGI/Unix, it
c	  copies the file to a backup with ~ added to the name
	subroutine copy_to_backup(filin)
	character*(*) filin
	character*80 comline
	namelen=lnblnk(filin)
	comline='cp '//filin(1:namelen)//' '//filin(1:namelen)//'~'
	print *,'Copying file . . .'
	call system(comline)
	return
	end

