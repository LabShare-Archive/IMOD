c       GETINOUT will retrieve input and output filenames from the command
c       line or ask for them from user if no argumants are present.
c       Specifically, when the caller requests NARG names (1 or 2), 
c       the routine gets the first command line argument or requests an
c       input file name and returns it in FILIN; then, if NARG is 2, it gets
c       the second command line argument if any, or requests an output file
c       name, and returns it in FILOUT.
c       
      subroutine getinout(narg,filin,filout)
      character*(*) filin,filout
      if(iargc().eq.0)then
        write(*,'(1x,a,$)')'Name of input file: '
        read(5,100)filin
100     format(a)
      else
        call getarg(1,filin)
      endif
      if(narg.gt.1)then
        if(iargc().lt.2)then
          write(*,'(1x,a,$)')'Name of output file: '
          read(5,100)filout
        else
          call getarg(2,filout)
        endif
      endif
      return
      end

