c       NUMERICDIFF compares numerical output between two text files
c       
c       $Id$
c       
      implicit none
      character*120 afile,bfile,stripStr
      character*10240 linea,lineb,line,stripa,stripb
      real*4 diffLimits(1000), diffMax(1000),avals(1000),bvals(1000)
      integer*4 ierr, numMaxes,numericSect,numError,i, j,lenStrip
      integer*4 ival,numLimits,maxComp,numComp,numAvals,numBvals
      logical*4 inSection,aIsNumeric,bIsNumeric,general,bigOutput,didBig,dump
      logical numericLine

      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInOutFile,PipNumberOfEntries
      integer*4 PipGetFloatArray,PipExitOnError,PipGetLogical,PipGetBoolean
      integer*4 PipGetString
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  numericdiff
c       
      integer numOptions
      parameter (numOptions = 7)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'ainput:AInputFile:FN:@binput:BInputFile:FN:@'//
     &    'max:MaxDifferences:FAM:@general:GeneralFormat:B:@'//
     &    'strip:StripLinesWith:B:@big:BigDifferenceOutput:B:@'//
     &    'help:usage:B:'
c       
      general = .false.
      call PipReadOrParseOptions(options, numOptions, 'numericdiff',
     &    'ERROR: NUMERICDIFF - ', .false., 1, 2, 0, numOptArg,
     &    numNonOptArg)
      inSection = .false.
      numericSect = 0
      numLimits = 0
      numError = 0
      stripStr = ' '
      bigOutput = .false.
      lenStrip = 0
c       
      if (PipGetInOutFile('AInputFile', 1, ' ', afile) .ne. 0)
     &    call errorexit('NO INPUT FILES SPECIFIED')
c       
      if (PipGetInOutFile('BInputFile', 2, ' ', bfile) .ne. 0)
     &    bfile = trim(afile)//'~'
c       
      open(1, file=afile, status='OLD',form='formatted', err=98)
      open(2, file=bfile, status='OLD',form='formatted', err=99)
c       
      ierr = PipNumberOfEntries('MaxDifferences', numMaxes)
      ierr = PipGetLogical('GeneralFormat', general)
      if (PipGetString('StripLinesWith', stripStr) .eq. 0)
     &    lenStrip = len_trim(stripStr)
      ierr = PipGetLogical('BigDifferenceOutput', bigOutput)

      do while(.true.)
c         
c         read lines and check for numeric text
c         
        read(1, '(a)', err=96, end=50)linea
        read(2, '(a)', err=97, end=50)lineb
        aIsNumeric = numericLine(linea, stripa, stripStr, lenStrip)
        bIsNumeric = numericLine(lineb, stripb, stripStr, lenStrip)
        if (inSection .and.  .not.(aIsNumeric .and. bIsNumeric)) then
c           
c           If in a section and either is non-numeric, end section and
c           consume all numeric text
c           
          call outputDiffs(diffMax,maxComp, diffLimits,numLimits,
     &        numericSect, numError, general)
          inSection = .false.
          do while (aIsNumeric)
            read(1, '(a)', err=96, end=50)linea
            aIsNumeric = numericLine(linea, stripb, stripStr, lenStrip)
          enddo
          do while (bIsNumeric)
            read(2, '(a)', err=97, end=50)lineb
            bIsNumeric = numericLine(lineb, stripa, stripStr, lenStrip)
          enddo
c           
        else if (.not.inSection .and. (aIsNumeric .or. bIsNumeric)) then
c           
c           If not in a section and either is numeric, start a section,
c           consume any non-numerics, and get limits if any
          numericSect = numericSect + 1
          if (numericSect .le. numMaxes) then
            numLimits = 0
            ierr = PipGetFloatArray('MaxDifferences', diffLimits,
     &          numLimits, 1000)
          endif
          do i = 1, 1000
            diffMax(i) = 0.
          enddo
          maxComp = 0
c           
          do while (.not.aIsNumeric)
            read(1, '(a)', err=96, end=50)linea
            aIsNumeric = numericLine(linea, stripa, stripStr, lenStrip)
          enddo
          do while (.not.bIsNumeric)
            read(2, '(a)', err=97, end=50)lineb
            bIsNumeric = numericLine(lineb, stripb, stripStr, lenStrip)
          enddo
          inSection = .true.
          didBig = .false.
        endif
c         
c         If in a section, evaluate the differences
c         
        if (inSection) then
          call frefor(stripa, avals, numAvals)
          call frefor(stripb, bvals, numBvals)
          numComp = min(numAvals, numBvals)
          maxComp = max(maxComp, numComp)
          dump = .false.
          do i = 1, numComp
            diffMax(i) = max(diffMax(i), abs(avals(i) - bvals(i)))
            if (bigOutput .and. i .le. numLimits .and.
     &          abs(avals(i) - bvals(i)) .gt. diffLimits(i)) dump = .true.
          enddo

          if (dump) then
            print *
            if (.not.didBig) write(*,'(a,i3,a)')'Section',numericSect,
     &          ' lines with big differences:'
            didBig = .true.
            print *,trim(linea)
            print *,trim(lineb)
          endif
        endif
      enddo

50    close(1)
      close(2)
      if (inSection) call outputDiffs(diffMax,maxComp,
     &    diffLimits,numLimits, numericSect, numError, general)
      
      call exit(numError)
96    call errorexit('Reading first input file')
97    call errorexit('Reading second input file')
98    call errorexit('Opening first input file')
99    call errorexit('Opening second input file')
      end


      subroutine outputDiffs(diffMax,maxComp,diffLimits,numLimits,
     &    numericSect, numError, general)
      implicit none
      real*4 diffMax(*), diffLimits(*)
      logical*4 general
      integer*4 maxComp,numLimits, numericSect, numError, numAbove, k
c       
      write(*,'(a,i3,a)')'Section',numericSect,' differences:'
      if (general) then
        write(*,'(6g13.5)')(diffMax(k),k=1,maxComp)
      else
        write(*,'(f9.4,7f10.4)')(diffMax(k),k=1,maxComp)
      endif
      numAbove = 0
      do k = 1, numLimits
        if (diffMax(k) .gt. diffLimits(k)) numAbove = numAbove + 1
      enddo
      if (numAbove .gt. 0) then
        print *,numAbove,' differences above limit'
        numError = numError + 1
      endif
      return
      end
      
      logical function numericLine(inline, line, stripStr, lenStrip)
      implicit none
      character*(*) line, inline, Stripstr
      integer*4 lenStrip,i,ival,length
c       
      line = inline
      numericLine = .false.
      length = len_trim(line)
      if (length .eq. 0) return
c       
c       If the strip string matches, convert the line to all numeric
c       
      if (lenStrip .gt.0 .and. index(line, stripStr(1:max(1,lenStrip)))
     &    .gt. 0) then
        do i = 1,length
          ival = ichar(line(i:i))
c           
c           If numeric, convert comma to space; convert an E or e to
c           space unless followed by +, - or digit; convert all else
c           
          if (ival.eq.32 .or. (ival.ge.43 .and. ival.le.46) .or.
     &        (ival.ge.48 .and. ival.le.57)) then
            if (ival .eq. 44) line(i:i) = ' '
          else if ((ival.eq.69 .or. ival.eq.101) .and. i .lt. length) then
            ival = ichar(line(i+1:i+1))
            if (.not.(ival .eq. 43 .or. ival .eq. 45 .or. 
     &          (ival.ge.48 .and. ival.le.57))) line(i:i) = ' '
          else
            line(i:i) = ' '
          endif
        enddo
        length = len_trim(line)
        if (length .eq. 0) return
        numericLine = .true.
        return
      endif

      do i = 1,length
        ival = ichar(line(i:i))
        if (.not.(ival.eq.32 .or. (ival.ge.43 .and. ival.le.46) .or.
     &      (ival.ge.48 .and. ival.le.57) .or. ((ival.eq.69 .or.
     &      ival.eq.101) .and. i .lt. length))) return
        if (ival.eq.69 .or. ival.eq.101) then
          ival = ichar(line(i+1:i+1))
          if (.not.(ival .eq. 43 .or. ival .eq. 45 .or. 
     &        (ival.ge.48 .and. ival.le.57))) return
        endif
      enddo
      numericLine = .true.
      return
      end

      subroutine errorexit(message)
      character*(*) message
      print *
      print *,'ERROR: NUMERICDIFF - ',message
      call exit(1)
      end
