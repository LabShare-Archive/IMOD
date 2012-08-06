c       !
c       GET_NXYZ takes a line of input and attempts to read it as 3 integers.
c       If this succeeds, the values are returned in [nxyz].  If this generates
c       an error, or the line contains letters, it then uses the line as a
c       filename and tries to open an MR file on unit [iunit].  [nxyz] is
c       then fetched from the header.
c       
c       If [pipinput] is TRUE, then the string is fetched with the given
c       [option].  Set [progname] to the name of the program if the option is
c       mandatory.  If this entry is optional, pass [progname] as a blank
c       string.
c       !
c       $Id$
c
      subroutine get_nxyz(pipinput,option,progname,iunit,nxyz)
      implicit none
      integer*4 nxyz(3),mxyz(3),iunit,mode,i, PipGetString
      character*(*) option, progname
      character*320 line
      logical pipinput
      real*4 dmin,dmax,dmean
      logical line_is_filename

      if (.not.pipinput) then
        read(5,'(a)')line
      else
        if (PipGetString(option, line) .gt. 0) then
          if (progname .eq. ' ') return
          write(*,'(/,a,a,a,a,a)') 'ERROR: ',trim(progname), ' - you must enter option ',
     &        trim(option), ' to specify file size or file name'
          call exit(1)
        endif
      endif
      if(line(1:1).eq.'/'.or.line.eq.' ')return
      if (line_is_filename(line)) go to 10
      read(line,*,err=10,end=10)(nxyz(i),i=1,3)
      return
10    call ialprt(.false.)
      call imopen(iunit,line,'ro')
      call irdhdr(iunit,nxyz,mxyz,mode,dmin,dmax,dmean)
      call imclose(iunit)
      call ialprt(.true.)
      return
      end


c       LINE_IS_FILENAME tests for whether a line could be a valid numeric
c       input and returns TRUE if not.  It is to replace reliance on
c       trying to read from an internal string, which fails mysteriously
c       on some Windows installations.
c       
c       String must contain spaces, tabs, period, comma, plus, minus, digits
c       
      logical function line_is_filename(line)
      implicit none
      character*(*) line
      integer*4 i, ich
      line_is_filename = .true.
      do i = 1, len(line)
        ich = ichar(line(i:i))
        if (ich .ne. 32 .and. ich .ne. 11 .and.
     &      (ich .lt. 43 .or. ich .gt. 57 .or. ich .eq. 47)) return
      enddo
      line_is_filename = .false.
      return
      end
