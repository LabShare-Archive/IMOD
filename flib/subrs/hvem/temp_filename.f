c	  function TEMP_FILENAME will compose a filename from an optional
c	  temporary directory TEMPDIR, a filename in FILEIN with leading
c	  directories stripped off, and an extension in TEMPEXT
c
	character*80 function temp_filename(filein,tempdir,tempext)
	character*(*) filein,tempdir,tempext
	character*80 concat
c	  
c	  find last / in filein
c	  
	inend=lnblnk(filein)
	instr=1
	do i=1,inend
	  if(filein(i:i).eq.'/')instr=i+1
	enddo
c
	if(tempdir.eq.' ')then
	temp_filename=concat(concat(filein(instr:inend),'.'),
     &	      tempext)
	else
	  temp_filename=concat(concat(concat(concat(tempdir,'/'),
     &	      filein(instr:inend)),'.'),tempext)
	endif
	return
	end
