*       XFPRODUCT  is used to concatenate two lists of transforms, or to
c       multiply all of the transforms in one file by another transform.
c       
c       See man page for details.
c       
c       David Mastronarde 1989
c       
c       $Id$
c       Log at end of file
c       
      implicit none
      integer nflimit
      parameter (nflimit=100000)
      real*4 f(2,3,nflimit),g(2,3,nflimit),prod(2,3)
      character*320 gfile
c       
      integer*4 nfirst, nsecond, nout, ierr, i, nsingle
      real*4 scale1, scale2
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetTwoFloats, PipGetInteger
      integer*4 PipGetInOutFile
c       
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  xfproduct
c       
      integer numOptions
      parameter (numOptions = 6)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'in1:InputFile1:FN:@in2:InputFile2:FN:@output:OutputFile:FN:@'//
     &    'scale:ScaleShifts:FP:@one:OneXformToMultiply:I:@help:usage:B:'
c       
      scale1 = 1.
      scale2 = 1.
      nsingle = -1
c       
      call PipReadOrParseOptions(options, numOptions, 'xfproduct',
     &    'ERROR: XFPRODUCT - ', .true., 3, 2, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0

      if (PipGetInOutFile('InputFile1', 1,
     &    'File of transforms applied first', gfile)
     &    .ne. 0) call errorexit('NO FIRST INPUT FILE SPECIFIED')
      call dopen(1,gfile,'ro','f')
      call xfrdall(1,f,nfirst,*92)
      close(1)
      if (nfirst .eq. 0) call errorexit('NO TRANSFORMS IN FIRST INPUT FILE')
      print *,nfirst,' transforms in first file'
c       
      if (PipGetInOutFile('InputFile2', 2,
     &    'File of transforms applied second', gfile)
     &    .ne. 0) call errorexit('NO SECOND INPUT FILE SPECIFIED')
      call dopen(2,gfile,'ro','f')
      call xfrdall(2,g,nsecond,*92)
      close(2)
      if (nsecond .eq. 0)
     &    call errorexit('NO TRANSFORMS IN SECOND INPUT FILE')
      print *,nsecond,' transforms in second file'
c       
      if (nfirst.gt.nflimit.or.nsecond.gt.nflimit)
     &    call errorexit('too many transforms for array size')
c       
      if (PipGetInOutFile('OutputFile', 3,
     &    'New file for product transforms', gfile)
     &    .ne. 0) call errorexit('NO OUTPUT FILE SPECIFIED')
      call dopen(3,gfile,'new','f')

      if (pipinput) then
        ierr = PipGetTwoFloats('ScaleShifts', scale1, scale2)
        ierr = PipGetInteger('OneXformToMultiply', nsingle)
      endif
      call PipDone()
c       
      if (nsecond.ne.nfirst)then
        if(nsecond.eq.1)then
          if (nsingle .ge. 0 .and. nsingle .lt. nfirst) then
            call xfcopy(g(1,1,1),g(1,1,nsingle+1))
            do i=1,nfirst
              if (i .ne. nsingle+1) call xfunit(g(1,1,i), 1.)
            enddo
            print *,'Single second transform applied to first transform #',
     &          nsingle
          else
            do i=2,nfirst
              call xfcopy(g(1,1,1),g(1,1,i))
            enddo
            print *,'Single second transform applied to all first transforms'
          endif
          nsecond=nfirst
        elseif(nfirst.eq.1)then
          if (nsingle .ge. 0 .and. nsingle .lt. nsecond) then
            call xfcopy(f(1,1,1),f(1,1,nsingle+1))
            do i=1,nsecond
              if (i .ne. nsingle+1) call xfunit(f(1,1,i), 1.)
            enddo
            print *,'Single first transform applied to second transform #',
     &          nsingle
          else
            do i=2,nsecond
              call xfcopy(f(1,1,1),f(1,1,i))
            enddo
            print *,'Single first transform applied to all second transforms'
          endif
          nfirst=nsecond
        else
          print *,'WARNING: XFPRODUCT - Number of transforms does not match'
        endif
      endif
c       
      nout=min(nfirst,nsecond)
      do i=1,nout
        f(1,3,i) = f(1,3,i) * scale1
        f(2,3,i) = f(2,3,i) * scale1
        g(1,3,i) = g(1,3,i) * scale2
        g(2,3,i) = g(2,3,i) * scale2
        call xfmult(f(1,1,i),g(1,1,i),prod)
        call xfwrite(3,prod,*94)
      enddo
      print *,nout,' new transforms written'
      call exit(0)
c       
92    call errorexit('reading old f/g file')
94    call errorexit('writing out g file')
      end

      subroutine errorexit(message)
      character*(*) message
      print *
      print *,'ERROR: XFPRODUCT - ',message
      call exit(1)
      end

c       
c       $Log$
c       Revision 3.3  2005/10/13 00:54:03  mast
c       Generate error when files are empty and WARNING if they don't match
c	
c       Revision 3.2  2004/01/27 03:34:04  mast
c       Convert to PIP input and add option for scaling translations
c	
c       Revision 3.1  2003/08/02 22:32:14  mast
c       Standardize error output and exit
c	
