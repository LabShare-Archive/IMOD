c       CONVERTMOD  will read in a model file (WIMP ASCII or IMOD) and
c       output a WIMP-style ASCII model file

      include 'model.inc'

      character*80 oldfile,newfile
      character*6 binasc1,binasc2
      logical readw_or_imod

c       get specifications
91    write(*,'(1x,a,$)')'Old WIMP or IMOD model file: '
      read(*,'(a)')oldfile
      if(.not.readw_or_imod(oldfile))then
        print *,'Error, try again'
        go to 91
      endif
      write(*,'(1x,a,$)')'New WIMP'//
     &    ' model file: '
      read(*,'(a)')newfile
c       
      close(20)
C       7/14/00 CER remove carriagecontrol for LINUX
      open(20,file=newfile,status='new',
     &    form='formatted')
      call store_mod(newfile)
      close(20)
      end

