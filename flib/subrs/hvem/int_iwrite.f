c       INT_IWRITE writes the integer NUM left-justified into STRING and
c       reports the number of characters in NCHAR
c       
      subroutine int_iwrite(string,num,nchar)
      character*(*) string
      character*12 temp
      string = ' '
      write(temp,'(i12)')num
      nchar=0
      do i=1,12
        if(temp(i:i).ne.' ')then
          nchar=nchar+1
          string(nchar:nchar)=temp(i:i)
        endif
      enddo
      return
      end
