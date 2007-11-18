c       $Id$
c       $Log$
c       
c       !
c       Composes a filename from an optional temporary directory [tempdir], a
c       filename in [filein] with leading directories stripped off, and an
c       extension in [tempext]
c       
      character*320 function temp_filename(filein,tempdir,tempext)
      character*(*) filein,tempdir,tempext
      character*320 concat
c       
c       find last / in filein
c       
      inend=lnblnk(filein)
      instr=1
      do i=1,inend
        if(filein(i:i).eq.'/')instr=i+1
      enddo
c       
      if(tempdir.eq.' ')then
	temp_filename=concat(concat(filein(instr:inend),'.'),
     &      tempext)
      else
        temp_filename=concat(concat(concat(concat(tempdir,'/'),
     &      filein(instr:inend)),'.'),tempext)
      endif
      return
      end
