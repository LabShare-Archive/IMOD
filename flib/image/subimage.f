c       SUBIMAGE
c
c       -----------------------------------------------------
c       --- This program subtracts one image from another ---
c       --- Written: 8-feb-1989 Sam Mitchell              ---
c       -----------------------------------------------------
c
c       $Id$
c       
      implicit none
      integer maxarr,maxlist
      parameter (maxarr=2100)
      parameter (maxlist=20000)

      real*4    array(maxarr*maxarr), brray(maxarr*maxarr), title(20), cell(6)
      common /bigarr/ array,brray
      integer   nxyz(3), mxyz(3), nxyzst(3), asec, bsec, nxyz2(3)
      integer*4 listasec(maxlist),listbsec(maxlist),nx,ny,nz,nbsec,isec,kti
      equivalence (nxyz,nx)
      common//nx,ny,nz
      character*160 afile, bfile, cfile
      character dat*9, tim*8
      character*10000 listString
c       
c       7/7/00 CER: remove the encode's; titlech is the temp space
c       
      character*80 titlech
      data nxyzst / 0,0,0 /
      integer*4 limarr,mode,i,nasec,maxLines,numChunks, iChunk,numLines,ierr
      real*4 dmin,dmax,dmean,dsum,tmin,tmax,realMin, realMax, realMean
      real*4 cmin,cmax,tmean,sd,lowThresh, highThresh, sdLimit, diffLimit
      real*8 totpix,tsum,tsumsq,sdsum,sdsumsq,sum,sumsq,diffMean,realSum
      integer*4 modeOut,ifZeroMean, ifLowThresh, ifHighThresh
      integer*4 minXstat, minYstat, maxXstat, maxYstat, lineStart, iyst, iynd

      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetString,PipGetInteger,PipGetFloat,PipGetTwoIntegers
      integer*4 PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  subimage
c       
      integer numOptions
      parameter (numOptions = 15)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'afile:AFileSubtractFrom:FN:@bfile:BFileSubtractOff:FN:@'//
     &    'output:OutputFile:FN:@mode:ModeOfOutput:I:@asections:ASectionList:LI:@'//
     &    'bsections:BSectionList:LI:@zero:ZeroMeanOutput:B:@lower:LowerThreshold:F:@'//
     &    'upper:UpperThreshold:F:@xstats:StatisticsXminAndMax:IP:@'//
     &    'ystats:StatisticsYminAndMax:IP:@minmax:ErrorMinMaxLimit:F:@'//
     &    'sdlimit:ErrorSDLimit:F:@param:ParameterFile:PF:@help:usage:B:'

      limarr = maxarr**2
      cfile = ' '
      bfile = ' '
      ifZeroMean = 0
      ifLowThresh = 0
      ifHighThresh = 0
      sdLimit = 0.
      diffLimit = 0.
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'subimage',
     &    'ERROR: SUBIMAGE - ', .true., 1, 2, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0

      if (.not.pipinput) then
        write ( *, '( a )' ) ' This program will subtract sections of'
        write ( *, '( a )' ) ' file B from sections of file A.  The '
        write ( *, '( a )' ) ' resulting file (C) contains the difference'
        write ( *, '( a )' ) ' images of (A-B).'
      endif

      if (PipGetInOutFile('AFileSubtractFrom', 1, 'Name of file A', afile)
     &    .ne. 0) call exitError('NO INPUT FILE A SPECIFIED')

      call ialprt(.false.)
      call imopen(1,afile,'ro')
      call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean) 
      modeOut = mode

      nasec = nz
      do i = 1, nasec
        listasec(i) = i - 1
        listbsec(i) = i - 1
      enddo
      if (pipinput) then
        if (PipGetString('ASectionList', listString) .eq. 0)
     &      call parselist(listString, listasec, nasec)
c         ierr = PipGetInteger('TestLimit', limarr)
        limarr = min(limarr, maxarr**2)
        ierr = PipGetInteger('ModeOfOutput', modeOut)
        ierr = PipGetInteger('ZeroMeanOutput', ifZeroMean)
        ifLowThresh = 1 - PipGetFloat('LowerThreshold', lowThresh)
        ifHighThresh = 1 - PipGetFloat('UpperThreshold', highThresh)
      else
        print *,'Enter list of section numbers from file A ',
     &      '(ranges OK, / for all)'
        call rdlist(5,listasec,nasec)
      endif
      if (nasec.gt.maxlist) call exitError('TOO MANY SECTIONS FOR ARRAYS')

      if (PipGetInOutFile('BFileSubtractOff', 2,
     &    'Name of file B, or return for A~', bfile) .ne. 0)
     &    bfile = trim(afile)//'~'
      nbsec = nasec

      if (pipinput) then
        if (PipGetString('BSectionList', listString) .eq. 0)
     &      call parselist(listString, listbsec, nbsec)
      else
        write ( *, '( a,i5,a,i5,a )' ) ' Enter list of',nasec,
     &      ' corresponding sections from file B (/ for 0-',
     &      nasec-1,')'
        call rdlist(5,listbsec,nbsec)
      endif
      if (nasec.ne.nbsec)call exitError('NUMBER OF SECTIONS DOES NOT MATCH')
      ierr = PipGetInOutFile('OutputFile', 3,
     &    'Name of file C, or return for statistics only', cfile)

      minXstat = 0
      minYstat = 0
      maxXstat = nx - 1
      maxYstat = ny - 1
      if (pipinput) then
        ierr = PipGetTwoIntegers('StatisticsXminAndMax', minXstat, maxXstat)
        ierr = PipGetTwoIntegers('StatisticsYminAndMax', minYstat, maxYstat)
        if (minXstat < 0 .or. maxXstat .ge. nx .or. minXstat .ge. maxXstat .or.
     &      minYstat < 0 .or. maxYstat .ge. ny .or. minYstat .ge. maxYstat) call exitError
     &      ('COORDINATES FOR GETTING STATISTICS ARE OUT OF RANGE')
        ierr = PipGetFloat('ErrorMinMaxLimit', diffLimit)
        ierr = PipGetFloat('ErrorSDLimit', sdLimit)
      endif

      call ialprt(.true.)
      call imopen(2,bfile,'ro')

      call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean) 
      call irdhdr(2,nxyz2,mxyz,mode,dmin,dmax,dmean)

      if(nxyz2(1).ne.nx.or.nxyz(2).ne.ny)
     &    call exitError( 'IMAGE SIZES DO NOT MATCH')
      call PipDone()

      do i=1,nasec
        asec=listasec(i)
        bsec=listbsec(i)
        if(asec.lt.0.or.asec.ge.nz.or.bsec.lt.0.or.bsec.ge.nxyz2(3))
     &      call exitError( 'ILLEGAL SECTION NUMBER')
      enddo

      if(nx.gt.limarr)call exitError( 'IMAGES TOO LARGE FOR ARRAYS')

      if(cfile.ne.' ')then
        call imopen(3,cfile,'new')
        call itrhdr(3,1)
        call ialmod(3,modeOut)
      endif
      dsum=0.
      sdsum=0.
      sdsumsq=0.
      realSum = 0.
      realMin = 1.e37
      realMax = -realMin
      print *,'Section      Min            Max            Mean'//
     &    '           S.D.'
      maxLines = limarr / nx
      numChunks = (ny + maxLines - 1) / maxLines
      do isec=1,nasec
        asec=listasec(isec)
        bsec=listbsec(isec)

c         
c         Get mean difference if needed
        diffMean = 0.
        if (ifZeroMean .ne. 0) then 
          call imposn(1,asec,0)
          call imposn(2,bsec,0)
          sum = 0.
          do iChunk = 1, numChunks
            numLines = min(maxLines, ny - (iChunk - 1) * maxLines)
            call irdsecl(1,array, numLines, *100) 
            call irdsecl(2,brray, numLines, *100) 
            call iclden(array,nx,numLines,1,nx,1,numLines,cmin,cmax,tmean)
            call iclden(brray,nx,numLines,1,nx,1,numLines,cmin,cmax,tmin)
            sum = sum + (tmean - tmin) * numLines
          enddo
          diffMean = sum / ny
        endif

        call imposn(1,asec,0)
        call imposn(2,bsec,0)

        sum = 0.
        sumsq = 0.
        tmin = 1.e37
        tmax = -tmin
        do iChunk = 1, numChunks
          lineStart = (iChunk - 1) * maxLines
          numLines = min(maxLines, ny - lineStart)
          call irdsecl(1,array, numLines, *100) 
          call irdsecl(2,brray, numLines, *100) 

c           -----------------------------------------
c           --- Subtract section B from section A and apply thresholds ---

          do i = 1 , nx*numLines
            array(i) = array(i) - brray(i) - diffMean
          enddo
          if (ifLowThresh .ne. 0) then
            do i = 1 , nx*numLines
              array(i) = max(array(i), lowThresh)
            enddo
          endif
          if (ifHighThresh .ne. 0) then
            do i = 1 , nx*numLines
              array(i) = min(array(i), highThresh)
            enddo
          endif

c           ---------------------------------
c           --- Write out the difference  ---
          if(cfile.ne.' ') call iwrsecl(3,array, numLines)
c           call iclden(array,nx,ny,1,nx,1,ny,tmin,tmax,tmean)
          call iclavgsd(array,nx,numLines,1,nx,1,numLines,cmin,cmax,
     &        tsum,tsumsq,tmean,sd)
          realSum = realSum + tsum
          realMin = min(realMin, cmin)
          realMax = max(realMax, cmax)
          if (lineStart .le. maxYstat .and. lineStart + numLines - 1 .ge. minYstat) then
c             
c             Add to stats if within Y range, replace temporary stats if it is subarea
            if (minXstat .gt. 0 .or. maxXstat .lt. nx - 1 .or. minYstat .gt. 0 .or.
     &          maxYstat .lt. ny - 1) then
              iyst = max(minYstat - lineStart, 0) + 1
              iynd = min(maxYstat - lineStart, numLines - 1) + 1
              call iclavgsd(array,nx,numLines,minXstat+1,maxXstat+1,iyst,iynd,cmin,cmax,
     &            tsum,tsumsq,tmean,sd)
            endif
            sum = sum + tsum
            sumsq = sumsq + tsumsq
            tmin = min(tmin,cmin)
            tmax = max(tmax,cmax)
          endif
        enddo
        call sums_to_avgsd8(sum, sumsq, maxXstat + 1 - minXstat, maxYstat + 1 - minYstat,
     &      tmean, sd)
        write(6,4000)asec,tmin,tmax,tmean,sd
        sdsum=sdsum+sum
        sdsumsq=sdsumsq+sumsq
        if(isec.eq.1)then
          dmin=tmin
          dmax=tmax
        else
          dmin=min(dmin,tmin)
          dmax=max(dmax,tmax)
        endif
        dsum=dsum+tmean
      enddo

      call imclose(1)
      call imclose(2)
      dmean=dsum/nasec
c       
      totpix=float(nasec)
      totpix=(maxXstat + 1 - minXstat) * (maxYstat + 1 - minYstat) * totpix
      sd=sqrt(max(0.,(sdsumsq-sdsum**2/totpix)/(totpix-1.)))
      if(nasec.gt.1)write(6,5000)dmin,dmax,dmean,sd

      ierr = 0
      if (diffLimit .gt. 0. .and. max(abs(dmin), abs(dmax)) .gt. diffLimit) then
        write(*, '(a,f14.4)')'THE MAXIMUM DIFFERENCE EXCEEDS ',diffLimit
        ierr = 1
      endif
      if (sdLimit .gt. 0. .and. sd .gt. sdLimit) then
        write(*, '(a,f14.4)')'THE STANDARD DEVIATION OF THE DIFFERENCE EXCEEDS ', sdLimit
        ierr = 1
      endif
      if(cfile.eq.' ')call exit(ierr)
      totpix=float(nasec)
      totpix=nx*ny*totpix
      realMean = realSum / totpix
      call irtcel(1, cell)
      nxyz(3) = nasec
      mxyz(3) = nasec
      cell(3) = (cell(3) * nasec) / nz
      call ialsam(3,mxyz)
      call ialcel(3,cell)
      call ialsiz(3,nxyz,nxyzst)

      call date(dat)
      call time(tim)
      write(titlech,3000) dat,tim
      read(titlech,'(20a4)')(title(kti),kti=1,20)

      call iwrhdr(3,title,1,realMin,realMax,realMean)

      call imclose(3)
      call exit(ierr)

1000  format ( 1x, a6, ' file : ', $ )
2000  format ( a30 )
3000  format ( 'SUBIMAGE: Subtract section B from section A.',
     &    t57, a9, 2x, a8 )
4000  format(i5,4f15.4)
5000  format(' all ',4f15.4)

100   call exitError('READING FILE')
      end

c       Some ancient history:
c       --- Updates: 31-may-1989, sjm                     ---
c       --- DNM 11/4/00: added multiple-section capability---
c       --- DNM 11/6/01: added ability to get statistics only
c       
