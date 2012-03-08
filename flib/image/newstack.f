*************NEWSTACK.FOR**********************************************
*       
c       Newstack is a general stack editor to move images into, out of, or
c       between stacks.  It can float the images to a common range or mean of
c       density. It can apply a general linear transform specified as a line
c       in a file. It can put the output into a smaller or larger array and
c       independently recenter each image separately from the transform.
c       Images can be taken from multiple input files and placed into multiple
c       output files.
*       
*       For all details see the man page.
*       
c       $Id$
c       
      implicit none
      integer maxtemp,lmsec,maxchunks,lmGradSec
      parameter (lmsec=1000000,maxchunks=250,lmGradSec=10000)
      parameter (maxtemp=1000000)
      integer*4 nx,ny,nz

      real*4, allocatable :: array(:)
C       
      integer*4 NXYZ(3),MXYZ(3),NXYZST(3), NXYZ2(3),MXYZ2(3), maxExtraIn,maxExtraOut
      real*4 CELL2(6),cell(6), TITLE(20), delt(3), xorig, yorig, zorig, deltafirst(3)
C       
      CHARACTER*320 xffil,filistin,filistout
      CHARACTER*320, allocatable :: FILIN(:),FILOUT(:)
      character*320 idfFile, magGradFile
      character*320 tempname,temp_filename, seriesExt
      character*100000 listString
      character*6 convfmt
      character*10 convnum
      EQUIVALENCE (NX,NXYZ(1)), (ny,nxyz(2)), (nz,nxyz(3))
C       
      DATA NXYZST/0,0,0/
      character*20 floatxt/' '/,xftext/' '/,trunctxt/' '/
      real*4 frot(2,3), fexp(2,3), fprod(2,3)
      integer*4 inlist(lmsec)
      integer*4, allocatable :: nlist(:),listind(:),nsecout(:),linetmp(:)
      real*4 optmax(16)
      integer*4 lineOutSt(maxchunks+1),nLinesOut(maxchunks)
      integer*4 lineInSt(maxchunks+1),nLinesIn(maxchunks)
      real*4, allocatable :: scaleFacs(:), scaleConsts(:)
      integer*1, allocatable :: extrain(:),extraout(:)
      data optmax/255.,32767.,255.,32767.,255.,255.,65535.,255.,255.,
     &    511.,1023.,2047.,4095.,8191., 16383.,32767./
c       
      integer(kind=8) idim, limdim, i8, npix,ibchunk,ibbase,istart, limIfFail
      integer(kind=8) nmove,noff
      integer*4 ifDistort, idfBinning, iBinning, idfNx, idfNy, iWarpFlags
      integer*4 nxGrid, nyGrid, numFields, numIdfUse
      real*4 xGridIntrv, yGridIntrv, pixelSize, xGridStrt, yGridStrt, warpScale
      real*4 xnbig, ynbig
c       
      integer*4 ifMagGrad, numMagGrad, magUse
      real*4 pixelMagGrad, axisRot
      integer*4, allocatable :: lineuse(:),listReplace(:), idfUse(:), nControl(:)
      real*4, allocatable :: xcen(:),ycen(:),secmean(:),f(:,:,:)
      real*4, allocatable :: tmpDx(:,:), tmpDy(:,:), fieldDx(:,:), fieldDy(:,:)
            
      real*4 tiltAngles(lmgradsec), dmagPerUm(lmgradsec), rotPerUm(lmgradsec)
c       
      logical rescale,blankOutput,adjustOrigin, hasWarp, fillTmp, fillNeeded
      character dat*9,tim*8,tempext*9
      logical nbytes_and_flags
      character*80 titlech
      integer*4 inunit,nfilein,listot,noutot,nfileout,nxout,nyout,lmGrid,ifWarping
      integer*4 newmode,ifoffset,ifxform,nxforms,nlineuse,ifmean,iffloat
      integer*4 nsum,ilis,ifil,nsecred,loadyst,loadynd,isec,isecout
      real*4 xofsall,yofsall,fraczero,dminspec,dmaxspec,conlo,conhi
      real*4 zmin,zmax,diffmin,diffmax,grandsum,sdsec
      real*4 grandmean,shiftmin,shiftmax,shiftmean,dminin,dmaxin,dmeanin
      integer*4 ifileout,ntrunclo,ntrunchi,ifheaderout,iftempopen,nbsymin
      integer*4 nbytexin,iflagxin,mode,nbytexout,nbsymout,indxout,iVerbose
      real*4 dmin,dmax,dmean,dmin2,dmax2,dmean2,optin,optout,bottomin
      real*4 bottomout,xci,yci,dx,dy,fieldMaxX, ystart
      integer*4 linesleft,nchunk,nextline,ichunk,ifOutChunk,iscan,iytest
      integer*4 iybase,iy1,iy2,lnu,maxin,numScaleFacs,maxFieldX,needYfirst, needYlast
      real*4 dmeansec,tmpmin,tmpmax,val,tsum2,sclfac
      integer*4 needyst,needynd,nload,nyload,nych,iseriesBase, nyNeeded
      integer*4 ix1,ix2,nbcopy,nbclear,ifLinear, limEntered,insideTaper
      real*4 const,denoutmin,den, tmin2,tmax2,tmean2,avgsec
      integer*4 numInputFiles, numSecLists, numOutputFiles, numToGet, maxNxg, maxNyg
      integer*4 numOutValues, numOutEntries, ierr, ierr2, i, kti, iy
      integer*4 maxFieldY, inputBinning, nxFirst, nyFirst, nxBin, nyBin
      integer*4 ixOffset, iyOffset, lenTemp, ierr3, applyFirst,numTaper
      integer*4 ifOnePerFile,ifUseFill,listIncrement,indout
      integer*4 ixOriginOff,iyOriginOff, numReplace, isecReplace, modeOld
      integer*4 indFilter,linesShrink, numAllSec, maxNumXF, nxmax, nymax, ifControl
      real*4 fieldMaxY, rotateAngle, expandFactor, fillVal, shrinkFactor
      real*8 dsum,dsumsq,tsum,tsumsq, wallstart,walltime,loadtime,savetime
      real*8 rottime
      real*4 cosd, sind
      integer*4 taperAtFill, selectZoomFilter, zoomFiltInterp
      integer*4 readCheckWarpFile
      integer*4 getLinearTransform, findMaxGridSize, getSizeAdjustedGrid
      character*320 concat
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetBoolean, PipGetLogical
      integer*4 PipGetString,PipGetTwoIntegers, PipGetFloatArray, PipGetFloat
      integer*4 PipGetIntegerArray, PipGetNonOptionArg, PipGetTwoFloats
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  newstack
c       
      integer numOptions
      parameter (numOptions = 40)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FNM:@output:OutputFile:FNM:@'//
     &    'fileinlist:FileOfInputs:FN:@fileoutlist:FileOfOutputs:FN:@'//
     &    'split:SplitStartingNumber:I:@append:AppendExtension:CH:@'//
     &    'secs:SectionsToRead:LIM:@skip:SkipSectionIncrement:I:@'//
     &    'numout:NumberToOutput:IAM:@replace:ReplaceSections:LI:@'//
     &    'blank:BlankOutput:B:@size:SizeToOutputInXandY:IP:@'//
     &    'mode:ModeToOutput:I:@offset:OffsetsInXandY:FAM:@'//
     &    'applyfirst:ApplyOffsetsFirst:B:@xform:TransformFile:FN:@'//
     &    'uselines:UseTransformLines:LIM:@onexform:OneTransformPerFile:B:@'//
     &    'rotate:RotateByAngle:F:@expand:ExpandByFactor:F:@'//
     &    'shrink:ShrinkByFactor:F:@antialias:AntialiasFilter:I:@'//
     &    'bin:BinByFactor:I:@origin:AdjustOrigin:B:@'//
     &    'linear:LinearInterpolation:B:@float:FloatDensities:I:@'//
     &    'contrast:ContrastBlackWhite:IP:@scale:ScaleMinAndMax:FP:@'//
     &    'fill:FillValue:F:@multadd:MultiplyAndAdd:FPM:@'//
     &    'taper:TaperAtFill:IP:@distort:DistortionField:FN:@'//
     &    'imagebinned:ImagesAreBinned:I:@fields:UseFields:LIM:@'//
     &    'gradient:GradientFile:FN:@memory:MemoryLimit:I:@'//
     &    'test:TestLimits:IP:@verbose:VerboseOutput:I:@'//
     &    'param:ParameterFile:PF:@help:usage:B:'
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'newstack',
     &    'ERROR: NEWSTACK - ', .true., 2, 2, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0
c       
c       defaults
c       
      filistin = ' '
      filistout = ' '
      numInputFiles = 0
      numOutputFiles = 0
      numSecLists = 0
      ifOnePerFile = 0
      ifDistort = 0
      ifMagGrad = 0
      maxFieldY = 0
      idfFile = ' '
      magGradFile = ' '
      rotateAngle = 0.
      expandFactor = 0.
      iBinning = 1
      inputBinning = 1
      limdim = 4 * lmsec
      lenTemp = maxtemp
      idim = limdim - 1
      limEntered = 0
      applyFirst = 0
      ifLinear = 0
      numScaleFacs = 0
      ifUseFill = 0
      blankOutput = .false.
      adjustOrigin = .false.
      listIncrement = 1
      numReplace = 0
      iVerbose = 0
      limIfFail = 1950000000 / 4
      numTaper = 0
      insideTaper = 0
      loadtime=0.
      savetime=0.
      rottime = 0.
      maxextraIn = 0
      maxextraOut = 0
      indFilter = 5
      linesShrink = 0
      iseriesBase = -1
      seriesExt = ' '
      maxNumXF = 40000
      ifWarping = 0
      ifControl = 0
      lmGrid = 200
c       
c       Preliminary allocation of array
      allocate(array(limdim), stat = ierr)
      call memoryError(ierr, 'ALLOCATING MAIN ARRAY')
C       
C       Read in list of input files
C       
      call ialprt(.false.)
      inunit=5
c       
c       get number of input files
c       
      if (pipinput) then
        ierr = PipGetInteger('VerboseOutput', iVerbose)
        ierr = PipGetString('FileOfInputs', filistin)
        call PipNumberOfEntries('InputFile', numInputFiles)
        nfilein = numInputFiles + max(0, numNonOptArg - 1)
        if (nfilein .gt. 0 .and. filistin .ne. ' ') call exitError(
     &      'YOU CANNOT ENTER BOTH INPUT FILES AND AN INPUT LIST FILE')
        if (filistin .ne. ' ')nfilein = -1
        call PipNumberOfEntries('SectionsToRead', numSecLists)
        ierr = PipGetInteger('SkipSectionIncrement', listIncrement)
        ierr = PipGetLogical('BlankOutput', blankOutput)
        if (PipGetInteger('BytesSignedInOutput', i) .eq. 0) call overrideWriteBytes(i)
      else
        write(*,'(1x,a,$)')'# of input files (or -1 to read list'//
     &      ' of input files from file): '
        read(5,*)nfilein
      endif
c       
c       if it is negative, open a list file, set up input from 7
c       
      if (nfilein .eq. 0) call exitError('NO INPUT FILE SPECIFIED')
      if(nfilein.lt.0)then
        inunit=7
        if (.not.pipinput) then
          write(*,'(1x,a,$)')'Name of input list file: '
          read(5,101)filistin
        endif
        call dopen(7,filistin,'ro','f')
        read(inunit,*)nfilein
      endif
      listot=0
      numAllSec = 0
      allocate(filin(nfilein), nlist(nfilein), listind(nfilein),  linetmp(nfilein), 
     &    scaleFacs(nfilein), scaleConsts(nfilein),  stat = ierr)
      call memoryError(ierr, 'ARRAYS FOR INPUT FILES')
c       
      nxmax = 0
      nymax = 0
      do i=1,nfilein
c         
c         get the next filename
c         
        if (pipinput .and. inunit.ne.7) then
          if (i .le. numInputFiles) then
            ierr = PipGetString('InputFile', filin(i))
          else
            ierr = PipGetNonOptionArg(i - numInputFiles, filin(i))
          endif
        else
          if(inunit.ne.7)then
            if(nfilein.eq.1)then
              write(*,'(1x,a,$)')'Name of input file: '
            else
              write(*,'(1x,a,i3,a,$)')'Name of input file #',i,': '
            endif
          endif
          READ(inunit,101)FILIN(i)
        endif
c         
c         open file to make sure it exists and get default section list
c         
        CALL IMOPEN(1,FILIN(i),'RO')
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
        if (mode .eq. 16) call exitError('CANNOT WORK WITH COLOR DATA'//
     &      ' (MODE 16); SEE MAN PAGE FOR ALTERNATIVES')
        call imclose(1)
        if (i .eq. 1) then
          nxFirst = nx
          nyFirst = ny
          call irtdel(1, deltaFirst)
        endif
        nxmax = max(nx, nxmax)
        nymax = max(ny, nymax)
        nlist(i) = nz
        do isec = 1, nz
          inlist(min(listot + isec, lmsec)) = isec - 1
        enddo
        numAllSec = numAllSec + nz
c         
c         get section list
c         
        if (.not.pipinput .or. inunit .eq. 7) then
          if(inunit.ne.7)print *,'Enter list of sections to read from'
     &        //' file (/ for all, 1st sec is 0; ranges OK)'
          call rdlist(inunit,inlist(listot+1),nlist(i))
        elseif (i .le. numSecLists) then
          ierr = PipGetString('SectionsToRead', listString)
          call parselist(listString, inlist(listot+1),nlist(i))
        endif
c         
c         check list legality
c         
        if (listot + nlist(i) .gt. lmsec) call exitError('TOO MANY SECTIONS FOR ARRAYS')
        listind(i)=listot+1
        indout = listind(i)
        do isec = listot + 1, listot + nlist(i), max(1, listIncrement)
          if (.not.blankOutput .and.
     &        (inlist(isec) .lt. 0 .or. inlist(isec) .ge. nz)) then
            write(*,'(/,a,i7,a,a)')'ERROR: NEWSTACK -',inlist(isec),
     &          ' IS AN ILLEGAL SECTION NUMBER FOR ',
     &          trim(filin(i))
            call exit(1)
          endif
          inlist(indout) = inlist(isec)
          indout = indout + 1
        enddo
        nlist(i) = indout - listind(i)
        listot=listot+nlist(i)
      enddo
      close(7)
101   FORMAT(A)
c       
      maxNumXF = max(maxNumXF, numAllSec)
      allocate(lineuse(listot),listReplace(listot), idfUse(listot), xcen(listot),
     &    ycen(listot),secmean(listot),f(2,3,maxNumXF), stat=ierr)
      call memoryError(ierr, 'ARRAYS FOR INPUT FILES')
C       
C       Read in list of output files
C       
      inunit=5
      noutot=0
c       
c       get number of output files
c       
      if (pipinput) then
        ierr = PipGetString('FileOfOutputs', filistout)
        call PipNumberOfEntries('OutputFile', numOutputFiles)
        nfileout = numOutputFiles + min(1, numNonOptArg)
        if (nfileout .gt. 0 .and. filistout .ne. ' ') call exitError(
     &      'YOU CANNOT ENTER BOTH OUTPUT FILES AND AN OUTPUT'//
     &      ' LIST FILE')
        if (filistout .ne. ' ') nfileout = -1
        ierr = PipGetInteger('SplitStartingNumber', iseriesBase)
        if (iseriesBase .ge. 0 .and. nfileout .ne. 1) call exitError('THERE'//
     &      ' MUST BE ONLY ONE OUTPUT FILE NAME FOR SERIES OF NUMBERED FILES')
        if (iseriesBase .ge. 0) nfileout = listot
        ierr = PipGetString('AppendExtension', seriesExt)
      else
        write(*,'(1x,a,$)')'# of output files (or -1 to read list'//
     &      ' of output files from file): '
        read(5,*)nfileout
      endif
      if (nfileout .eq. 0) call exitError('NO OUTPUT FILE SPECIFIED')
c       
      if (nfileout .gt. 0) then
        allocate(filout(nfileout), nsecout(nfileout), stat = ierr)
        call memoryError(ierr, 'ARRAYS FOR OUTPUT FILES')
      endif
c
c       get list input
c       
      if(nfileout.lt.0)then
        inunit=7
        if (.not.pipinput) then
          write(*,'(1x,a,$)')'Name of output list file: '
          read(5,101)filistout
        endif
        call dopen(7,filistout,'ro','f')
        read(inunit,*)nfileout
        if (nfileout .le. 0) call exitError('THE OUTPUT LIST FILE MUST START'//
     &      ' WITH A POSITIVE NUMBER OF FILES TO OUTPUT')
        allocate(filout(nfileout), nsecout(nfileout), stat = ierr)
        call memoryError(ierr, 'ARRAYS FOR OUTPUT FILES')
      elseif(nfileout.eq.1 .and. .not.pipinput)then
c         
c         get single output file
c         
        write(*,'(1x,a,$)')'Name of output file: '
        read(5,101)filout(1)
        nsecout(1)=listot
        noutot=listot
      endif
c       
c       or get all the output files and the number of sections
c       
      if (noutot.eq.0)then
        if (pipinput .and. inunit .ne. 7) then
          if (nfileout .eq. 1) then
            nsecout(1)=listot
          elseif (nfileout .eq. listot) then
            do i = 1, nfileout
              nsecout(i) = 1
            enddo
          else
            call PipNumberOfEntries('NumberToOutput', numOutEntries)
            if (numOutEntries .eq. 0)
     &          call exitError('YOU MUST SPECIFY NUMBER OF SECTIONS '//
     &          'TO WRITE TO EACH OUTPUT FILE')
            
            numOutValues = 0
            do i = 1, numOutEntries
              numToGet = 0
              ierr = PipGetIntegerArray('NumberToOutput',
     &            nsecout(numOutValues + 1), numToGet, nfileout - numOutValues)
              numOutValues = numOutValues + numToGet
            enddo
            if (numOutValues .ne. nfileout) call exitError(
     &          'THE NUMBER OF VALUES FOR SECTIONS TO OUTPUT DOES'
     &          //' NOT EQUAL THE NUMBER OF OUTPUT FILES')
          endif
          do i=1,nfileout
            if (i .le. numOutputFiles) then
              ierr = PipGetString('OutputFile', filout(i))
            else
              ierr = PipGetNonOptionArg(numNonOptArg, filout(i))
            endif
            noutot=noutot+nsecout(i)
          enddo
        else
          do i=1,nfileout
            if(inunit.ne.7)write(*,'(1x,a,i3,a,$)')
     &          'Name of output file #',i,': '
            READ(inunit,101)FILOUT(i)
            if(inunit.ne.7)write(*,'(1x,a,$)')
     &          'Number of sections to store in that file: '
            read(inunit,*)nsecout(i)
            noutot=noutot+nsecout(i)
          enddo
        endif
      endif
c       
c       If series, now take the one filename as root name and make filenames
      if (iseriesBase .ge. 0) then
        ierr = alog10(10. * (iseriesBase + listot - 1))
        tempname = FILOUT(1)
        write(convfmt, 132) ierr,ierr
132     format('(i',i1,'.',i1,')')
        do i = 1, listot
          nsecout(i) = 1
          write(convnum, convfmt) i + iseriesBase - 1
          if (seriesExt .eq. ' ') then
            filout(i) = concat(concat(tempname, '.'), convnum)
          else
            filout(i) = concat(concat(concat(tempname, convnum), '.'),
     &          seriesExt)
          endif
        enddo
        noutot=listot
      endif

      if(noutot.ne.listot)call exitError(
     &    'Number of input and output sections does not match')
c       
c       get new size and mode and offsets
c       
      nxout=-1
      nyout=-1
      newmode=-1
      xofsall=0.
      yofsall=0.
      ifoffset = 0
      if (pipinput) then
        ierr = PipGetTwoIntegers('SizeToOutputInXandY', nxout, nyout)
        ierr = PipGetInteger('ModeToOutput', newmode)
        call PipNumberOfEntries('OffsetsInXandY', numOutEntries)
        if (numOutEntries .gt. 0) then
          ifoffset = 1
          numOutValues = 0
          do i = 1, numOutEntries
            numToGet = 0
            ierr = PipGetFloatArray('OffsetsInXandY',
     &          array(numOutValues + 1), numToGet, lmsec * 2 - numOutValues)
            numOutValues = numOutValues + numToGet
          enddo
          if (numOutValues .ne. 2 .and. numOutValues .ne. 2 * listot)
     &        call exitError('THERE MUST BE EITHER ONE OFFSET OR AN'
     &        //' OFFSET FOR EACH SECTION')
          do i = 1, numOutValues / 2
            xcen(i) = array(2 * i - 1)
            ycen(i) = array(2 * i)
          enddo
          if (numOutValues .eq. 2) ifoffset = -1
          xofsall = xcen(1)
          yofsall = ycen(1)
        endif
      else
        write(*,'(1x,a,$)')'Output file X and Y dimensions'//
     &      ' (/ for same as first input file): '
        read(5,*)nxout,nyout
        write(*,'(1x,a,$)')'Output file data mode (/ for same as first input file): '
        read(5,*)newmode
c         
c         get list of x,y coordinate offsets
c         
        write(*,'(1x,a,/,a,$)')'1 to offset centers of individual '//
     &      'images,','  -1 to apply same offset to all sections,'//
     &      ' or 0 for no offsets: '
        read(5,*)ifoffset
        if(ifoffset.gt.0)then
          print *,'Enter X and Y center offsets for each section'
          read(5,*)(xcen(i),ycen(i),i=1,listot)
        elseif(ifoffset.lt.0)then
          write(*,'(1x,a,$)')'X and Y center offsets for all sections: '
          read(5,*)xofsall,yofsall
        endif
      ENDIF
c       
c       fill offset list if only one or none
c       
      if (ifoffset .le. 0) then
        do i=1,listot
          xcen(i)=xofsall
          ycen(i)=yofsall
        enddo
      endif
C       
C       Get list of transforms
C       
      ifxform = 0
      if (pipinput) then
        ierr = PipGetBoolean('LinearInterpolation', ifLinear)
        if (PipGetString('TransformFile', xffil) .eq. 0) ifxform = 1
        ierr = PipGetBoolean('OneTransformPerFile', ifOnePerFile)
      else
        write(*,'(1x,a,$)')'1 or 2 to transform images with cubic or'
     &      //' linear interpolation, 0 not to: '
        read(5,*)ifxform
        if(ifxform.ne.0)then
          write(*,'(1x,a,$)')'Name of transform file: '
          read(5,101)xffil
        endif
        if (ifxform .gt. 1) ifLinear = 1
      endif
      if(ifxform.ne.0)then
c         
c         read transforms, set default to section list unless there is just
c         one line in file
c         
        ierr = readCheckWarpFile(xffil, 0, 1, idfNx, idfNy, nxforms,
     &      idfBinning, pixelSize,  iWarpFlags, listString)
        if (ierr .lt. -1) call exitError(listString)
        if (ierr .ge. 0) then
          write(*,'(a,a)')'Warping file opened: ',trim(xffil)
          ifWarping = 1
          if (mod(iWarpFlags / 2, 2) .ne. 0) ifControl = 1
          if (nxforms .gt. maxNumXF) call exitError(
     &        'TOO MANY SECTIONS IN WARPING FILE FOR TRANSFORM ARRAY')
          warpScale = pixelSize / deltaFirst(1)
          do i = 1, nxforms
            if (getLinearTransform(i, f(1,1,i)) .ne. 0)
     &          call exitError('GETTING LINEAR TRANSFORM FROM WARP FILE')
            f(1,3,i) = f(1,3,i) * warpScale
            f(2,3,i) = f(2,3,i) * warpScale
          enddo
          numFields = nxforms
        else
          call dopen(3,xffil,'ro','f')
          call xfrdall2(3,f,nxforms,maxNumXF,ierr)
          if (ierr .eq. 2) call exitError('READING TRANSFORM FILE')
          if (ierr .eq. 1) call exitError(
     &        'TOO MANY TRANSFORMS IN FILE FOR TRANSFORM ARRAY')
          close(3)
        endif
        if (nxforms .eq. 0) call exitError('THE TRANSFORM FILE CONTAINS NO TRANSFORMS')

        call getItemsToUse(nxforms, listot, inlist, 'UseTransformLines',
     &      listString, pipinput, 'TRANSFORM LINE', ifOnePerFile, nfilein,
     &      lineUse, nLineUse, listot)

        if (ifOnePerFile .gt. 0) then
          if (nLineUse .lt. nfilein) call exitError(
     &        'NOT ENOUGH TRANSFORMS SPECIFIED FOR THE INPUT FILES')
c           
c           Copy list to temp array and build list with line for each sec
c           
          do iy = 1, nfilein
            linetmp(iy) = lineUse(iy)
          enddo
          nLineUse = 0
          do iy = 1, nfilein
            do i = 1, nlist(iy)
              nLineUse = nLineUse + 1
              lineUse(nLineUse) = linetmp(iy)
            enddo
          enddo
        endif
C         
C         use single number for all sections
C         
        xftext=', transformed'
        if (ifWarping .ne. 0) xftext=', warped'
        if(nlineuse.eq.1)then
          do i=2,listot
            lineuse(i)=lineuse(1)
          enddo
          nlineuse=listot
        endif
        if(nlineuse.ne.listot)call exitError(
     &      'Specified # of transform lines does not match # of sections')
      endif
c       
c       find out if root beer float or what
c       
      fraczero=0.
      ifmean=0
      iffloat = 0
      dminspec=0
      dmaxspec=0
      conlo=0
      conhi=255
      if (pipinput) then
        ierr = 1 - PipGetTwoFloats('ContrastBlackWhite', conlo, conhi)
        ierr2 = 1 - PipGetTwoFloats('ScaleMinAndMax', dminspec, dmaxspec)
        ierr3 = 1 - PipGetInteger('FloatDensities', iffloat)
        call PipNumberOfEntries('MultiplyAndAdd', numScaleFacs)
        ifUseFill = 1 - PipGetFloat('FillValue', fillVal)
        if (iffloat .ge. 4) then
          if (ierr2 .eq. 0) call exitError
     &        ('You must enter -scale with -float 4')
        else
          if (ierr + ierr2 + ierr3 + min(numScaleFacs,1) .gt. 1)
     &        call exitError('The -scale, -contrast, -multadd, and -float '//
     &        'options are mutually exclusive except with -float 4')
          if (iffloat .lt. 0)call exitError('You must use -contrast or '
     &        //'-scale instead of a negative -float entry')
          if (ierr .ne. 0) iffloat = -2
          if (ierr2 .ne. 0 .or. numScaleFacs .ne. 0) iffloat = -1
c           
c           get scale factors, make sure there are right number
c           
          if (numScaleFacs .gt. 0) then
            if (numScaleFacs .ne. 1 .and. numScaleFacs .ne. nfilein)
     &          call exitError('You must enter -multadd either once '//
     &          'or once per input file')
            do i = 1, numScaleFacs
              ierr = PipGetTwoFloats('MultiplyAndAdd', scaleFacs(i),
     &            scaleConsts(i))
            enddo
          endif
        endif	  
      else
        write(*,102)
102     format(' Enter 0 for no floating',/,8x,
     &      '-2 to scale to bytes based on black and white contrast ',
     &      'levels',/,8x,
     &      '-1 to specify a single rescaling of all sections'
     &      ,/,8x,' 1 to float all sections to same range',/,8x,' ',
     &      '2 to float all sections to same mean & standard deviation'
     &      ,/,8x,' 3 to shift sections to same mean without scaling',/
     &      ,6x,'or 4 to shift to same mean and specify a single',
     &      ' rescaling: ',$)
        read(5,*)iffloat
      endif
      if(iffloat.gt.1)ifmean=1
      if(iffloat.lt.0)then
        floatxt=', densities scaled'
        if(iffloat.eq.-1 .and. .not.pipinput)then
          write(*,'(1x,a,/,a,$)')'Values to scale input file''s'//
     &        ' min and max to,','   or / to scale to maximum range,'
     &        //' or 1,1 to override mode scaling: '
          read(5,*)dminspec,dmaxspec
        elseif (iffloat .lt. -1) then
          if (.not.pipinput) then
            write(*,'(1x,a,$)')'Contrast ramp black and white settings '
     &          //'(values between 0 and 255): '
            read(5,*)conlo,conhi
          endif
          conhi=max(conhi,conlo+1.)
          dminspec= -conlo*255/(conhi-conlo)
          dmaxspec=dminspec+65025/(conhi-conlo)
c	    print *,conlo, conhi,dminspec,dmaxspec
        endif
      endif
c       
c       get new options
c       
      if (pipinput) then
        ierr = PipGetBoolean('ApplyOffsetsFirst', applyFirst)
        ierr = PipGetFloat('RotateByAngle', rotateAngle)
        ierr = PipGetFloat('ExpandByFactor', expandFactor)
        ierr = PipGetInteger('BinByFactor', iBinning)
        ierr = PipGetString('DistortionField', idfFile)
        ierr = PipGetString('GradientFile', magGradFile)
        ierr = PipGetLogical('AdjustOrigin', adjustOrigin)
        ierr = PipGetTwoIntegers('TaperAtFill', numTaper, insideTaper)
        limEntered = 1 - PipGetTwoIntegers('TestLimits', ierr, lenTemp)
        if (limEntered .gt. 0) limdim = ierr
        if (PipGetInteger('MemoryLimit', ierr) .eq. 0) then
          limEntered = 1
          limdim = int(ierr, kind = 8) * 1024 * 256
        endif
        if (limEntered .gt. 0) then
          if (limdim .lt. 1000 .or. lenTemp .lt. 1 .or. lenTemp .gt.
     &        limdim / 2) call exitError('INAPPROPRIATE MEMORY LIMITS ENTERED')
          idim = limdim - lenTemp
          call reallocateArray()
        endif
        if (ifWarping .ne. 0 .and. (idfFile .ne. ' ' .or. magGradFile .ne. ' ')) call
     &      exitError('YOU CANNOT USE DISTORTION CORRECTIONS WITH WARPING TRANSFORMS')
c         
        if (ifWarping .ne. 0 .and. (rotateAngle .ne. 0 .or. expandFactor .ne. 0.)) call
     &      exitError('YOU CANNOT USE -expand or -rotate WITH WARPING TRANSFORMS')
        if (iBinning .le. 0) call exitError
     &      ('BINNING FACTOR MUST BE A POSITIVE NUMBER')
c         
c         Shrinkage
        if (PipGetFloat('ShrinkByFactor', shrinkFactor) .eq. 0) then
          if (ifxform .ne. 0 .or. rotateAngle .ne. 0. .or. expandFactor .ne. 0. .or.
     &        idfFile .ne. ' ' .or. magGradFile .ne. ' ') call exitError('YOU CANNOT'//
     &        ' USE -shrink WITH -xform, -rotate, -expand, -distort, or -gradient')
          if (shrinkFactor .le. 1.) call exitError(
     &        'FACTOR FOR -shrink MUST BE GREATER THAN 1')
          ierr = PipGetInteger('AntialiasFilter', indFilter)
          indFilter = max(0, indFilter - 1)
          expandFactor = 1. / shrinkFactor
          ierr = 1
          i = indFilter
          do while (ierr .eq. 1)
            ierr = selectZoomFilter(indFilter, expandFactor, linesShrink)
            if (ierr .eq. 1) indFilter = indFilter - 1 
            if (ierr .gt. 1) call exitError( 'SELECTING ANTIALIASING FILTER')
          enddo
          if (indFilter .lt. i) print *,'Using the last antialiasing filter, #',
     &        indFilter + 1
          linesShrink = linesShrink / 2 + 2
        endif
c         
c         Section replacement
        if (PipGetString('ReplaceSections', listString) .eq. 0) then
          call parselist(listString, listReplace, numReplace)
          if (numReplace .gt. 0) then
            print *,'replacing',(listReplace(i),i = 1, numReplace)
            if (nfileout .gt. 1) call exitError(
     &          'THERE MUST BE ONLY ONE OUTPUT FILE TO USE -replace')
            call ialprt(.true.)
            call imopen(2, filout(1), 'OLD')
            call irdhdr(2, nxyz2, mxyz2, modeold, dmin, dmax, dmean)
            call ialprt(.false.)
            do i = 1, numReplace
              if (listReplace(i) .lt. 0 .or. listReplace(i) .ge. nxyz2(3))
     &            call exitError('REPLACEMENT SECTION NUMBER OUT OF RANGE')
            enddo
          endif
        endif
c         
c         Distortion field
        if (idfFile .ne. ' ') then
          ifDistort = 1
          xftext=', undistorted'

          ierr = readCheckWarpFile(idfFile, 1, 1, idfNx, idfNy, numFields,
     &        idfBinning, pixelSize, iWarpFlags, listString)
        if (ierr .lt. 0) call exitError(listString)
c           
          if (PipGetInteger('ImagesAreBinned', inputBinning) .ne. 0) then
            if (nxFirst .le. idfNx * idfBinning / 2 .and.
     &          nyFirst .le. idfNy * idfBinning / 2) call exitError
     &		('YOU MUST SPECIFY BINNING OF IMAGES BECAUSE THEY '//
     &          'ARE NOT LARGER THAN HALF THE CAMERA SIZE')
          endif
          if (inputBinning .le. 0) call exitError
     &        ('IMAGE BINNING MUST BE A POSITIVE NUMBER')
          warpScale = float(idfBinning) / inputBinning
c           
c           Set up default field numbers to use then process use list if any
c           
          call getItemsToUse(numFields, listot, inlist, 'UseFields',
     &      listString, pipinput, 'FIELD', 0, 0, idfUse, numIdfUse, listot)
          if(numIdfUse.eq.1)then
            do i=2,listot
              idfUse(i)=idfUse(1)
            enddo
            numIdfUse=listot
          endif
          if(numIdfUse.ne.listot)call exitError(
     &        'Specified # of fields does not match # of sections')

        endif
c         
c         get mag gradient information; multiply pixel size by binning
c         
        if (magGradFile .ne. ' ') then
          ifMagGrad = 1
          xftext=', undistorted'
          call readMagGradients(magGradFile, lmGradSec, pixelMagGrad, axisRot,
     &        tiltAngles, dmagPerUm, rotPerUm, numMagGrad)
          pixelMagGrad = pixelMagGrad * iBinning
        endif
      endif
      call PipDone()
c       
c       if not transforming and distorting, rotating, or expanding, set up
c       a unit transform
c       
      if (ifxform .eq. 0 .and. (ifDistort .ne. 0 .or. ifMagGrad .ne. 0 .or.
     &    rotateAngle .ne. 0. .or. expandFactor .ne. 0.)) then
        ifxform = 1
        call xfunit(f(1,1,1), 1.)
        do i = 1, listot
          lineuse(i) = 0
        enddo
        nlineuse = listot
        nxforms = 1
      endif
c       
c       set up rotation and expansion transforms and multiply by transforms
c       
      if (rotateAngle .ne. 0. .or. expandFactor .ne. 0.) then
        call xfunit(frot, 1.)
        if (rotateAngle .ne. 0.) then
          frot(1,1) = cosd(rotateAngle)
          frot(1,2) = -sind(rotateAngle)
          frot(2,2) = frot(1,1)
          frot(2,1) = -frot(1,2)
c           This was needed to correct for cubinterp rotating off center,
c           changed 10/12/07
c          frot(1,3) = 0.5 * (frot(1,1) + frot(1,2)) - 0.5
c          frot(2,3) = 0.5 * (frot(2,1) + frot(2,2)) - 0.5
        endif
        if (expandFactor .eq. 0.) expandFactor = 1.
        call xfunit(fexp, expandFactor)
        call xfmult(frot, fexp, fprod)
c	  print *,'transform',((fprod(i, iy),i=1,2),iy=1,3)
        do i = 1, nxforms
          call xfmult(f(1,1,i), fprod, frot)
          call xfcopy(frot, f(1,1,i))
        enddo
      endif
      if (expandFactor .eq. 0.) expandFactor = 1.
c       
c       adjust xcen, ycen and transforms if binning and allocate temp space
c       
      if (iBinning .gt. 1) then
        do i = 1, listot
          xcen(i) = xcen(i) / iBinning
          ycen(i) = ycen(i) / iBinning
        enddo
        if (ifxform .ne. 0) then
          do i = 1, nxforms
            f(1,3,i) = f(1,3,i) / iBinning
            f(2,3,i) = f(2,3,i) / iBinning
          enddo
        endif
        idim = limdim - lenTemp
      else
        idim = limdim - 1
      endif
c	
      if(iffloat.gt.0)then
        floatxt=', floated to range'
        if(fraczero.ne.0.)
     &      write(trunctxt,'(a,f6.3)')', truncated by',fraczero
        if(ifmean.ne.0)then
c           
c           if means, need to read all sections to get means
c           
          if(iffloat.eq.2)then
            floatxt=', floated to means'
            zmin=1.e10
            zmax=-1.e10
          else
            diffmin=0.
            diffmax=0.
            grandsum=0.
            nsum=0.
            if(iffloat.eq.3)then
              floatxt=',  shifted to means'
            else
              floatxt=', mean shift&scaled'
              if (.not.pipinput) then
                write(*,'(1x,a,/,a,$)')'Values to scale the shifted'//
     &              ' min and max to,','   or / to scale to maximum'//
     &              ' range: '
                read(5,*)dminspec,dmaxspec
              endif
            endif
          endif
          do ifil=1,nfilein
            CALL IMOPEN(1,FILIN(ifil),'RO')
            CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
C             
c             get the binned size to read
c             
            call getBinnedSize(nx, iBinning, nxbin, ixOffset)
            call getBinnedSize(ny, iBinning, nybin, iyOffset)
c             
            nyNeeded = nybin
            call reallocateIfNeeded()
c                
            do ilis=1,nlist(ifil)
              nsecred=inlist(ilis+listind(ifil)-1)
              if (nsecred .ge. 0 .and. nsecred .lt. nz) then
c                 
                if (iVerbose .gt. 0) print *,'scanning for mean/sd',nsecred
                call scansection(array,idim,nxbin,nybin, 0, iBinning, ixOffset,
     &              iyOffset,nsecred,iffloat,dmin2, dmax2,dmean2,sdsec,
     &              loadyst,loadynd,array(idim + 1), lenTemp)
                secmean(ilis+listind(ifil)-1)=dmean2
c                 
                if(iffloat.eq.2)then
c                   
c                   find the min and max Z values ((density-mean)/sd) 
c                   
                  if (dmax2 .gt. dmin2 .and. sdsec .gt. 0.) then
                    zmin=min(zmin,(dmin2-dmean2)/sdsec)
                    zmax=max(zmax,(dmax2-dmean2)/sdsec)
                  endif
                else
c                   
c                   or, if shifting, get maximum range from mean
c                   
                  diffmin=min(diffmin,dmin2-dmean2)
                  diffmax=max(diffmax,dmax2-dmean2)
                  grandsum=grandsum+dmean2
                  nsum=nsum+1
                endif
              endif
            enddo
            call imclose(1)
          enddo
c           
c           for shift to mean, figure out new mean, min and max and whether
c           scaling will be needed to fit range
c           
          if(iffloat.gt.2)then
            grandmean=grandsum/nsum
            shiftmin=max(0.,grandmean+diffmin)
            shiftmean=shiftmin-diffmin
            shiftmax=grandmean+diffmax
            if(iffloat.eq.3.and.mode.ne.2.and.newmode.ne.2.and.
     &          optmax(mode+1).lt.shiftmax)then
              print *,'Densities will be compressed by',
     &            optmax(mode+1)/shiftmax,' to fit in range'
              floatxt=', mean shift&scaled'
            endif
          endif
        endif
      endif
c       
c       start looping over input images
c       
      isec=1
      isecout=1
      isecReplace = 1
      ifileout=1
      call ialprt(.true.)
      call time(tim)
      call b3ddate(dat)
      ntrunclo=0
      ntrunchi=0
      ifheaderout=0
      iftempopen=0
      do ifil=1,nfilein
        CALL IMOPEN(1,FILIN(ifil),'RO')
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMININ,DMAXIN,DMEANIN)
        call irtsiz(1,nxyz,mxyz,nxyzst)
        call irtcel(1,cell)
C         
c         get the binned size to read
c         
        call getBinnedSize(nx, iBinning, nxbin, ixOffset)
        call getBinnedSize(ny, iBinning, nybin, iyOffset)
        if (iVerbose.gt.0)
     &      print *,'Size and offsets X:',nxbin, ixOffset,', Y:',nybin, iyOffset
c         
c         get extra header information if any
c         
        call irtnbsym(1,nbsymin)
        if(nbsymin.gt.0)then
c           
c           Deallocate array if it was allocated an is not big enough
          if (maxextraIn .gt. 0 .and. nbsymin.gt.maxextraIn) then
            deallocate(extrain, stat = ierr)
            maxextraIn = 0
          endif
c           
c           Allocate array if needed
          if (maxextraIn .eq. 0) then
            maxextraIn = nbsymin + 1024
            allocate(extrain(maxextraIn), stat = ierr)
            if (ierr .ne. 0) call exitError('ALLOCATING MEMORY FOR EXTRA HEADER ARRAYS')
          endif
          call irtsym(1,nbsymin,extrain)
          call irtsymtyp(1,nbytexin,iflagxin)
c             
c             DNM 4/18/02: if these numbers do not represent bytes and
c             flags, then number of bytes is 4 times nint + nreal
c           
          if(.not.nbytes_and_flags(nbytexin,iflagxin))
     &        nbytexin=4*(nbytexin+iflagxin)
        endif
C         
c         get each section in input file
c         
        do ilis=1,nlist(ifil)
          nsecred=inlist(ilis+listind(ifil)-1)
c           
c           set output characteristics from first section, transposing size
c           for 90 degree rotation and resizing as necessary
c           
          if(isec.eq.1)then
            if (abs(abs(rotateAngle) - 90.) .lt. 0.1 .and. nxout.le.0 .and.
     &          nyout.le.0) then
              nxout=nint(nybin * expandFactor)
              nyout=nint(nxbin * expandFactor)
            endif
            if(nxout.le.0)nxout=nint(nxbin * expandFactor)
            if(nyout.le.0)nyout=nint(nybin * expandFactor)
            if(newmode.lt.0)newmode=mode
c       
c             If warping or distortions, figure out how big to allocate the arrays
            if (ifDistort + ifWarping .ne. 0) then
              allocate(nControl(numFields), stat = ierr)
              call memoryError(ierr,'ARRAY FOR NUMBER OF CONTROL POINTS')
              dx = 0.
              dy = 0.
c               
c               Expanded grid size is based on the input size for distortion and the
c               output size for warping
              if (ifDistort .ne. 0) then
                xnbig = nxmax / warpScale
                ynbig = nymax / warpScale
              else
                xnbig = iBinning * nxout / warpScale
                ynbig = iBinning * nyout / warpScale
                if (applyFirst .eq. 0) then
                  do i = 1, listot
                    dx = 1.e20
                    dy = 1.e20
                    xnbig = -dx
                    xnbig = -dx
                    xnbig = max(xnbig, (nxout + xcen(i)) * iBinning / warpScale)
                    dx = min(dx, xcen(i) * iBinning / warpScale)
                    ynbig = max(ynbig, (nyout + ycen(i)) * iBinning / warpScale)
                    dy = min(dy, ycen(i) * iBinning / warpScale)
                  enddo
                endif
              endif
              if (findMaxGridSize(dx, xnbig, dy, ynbig, nControl, maxNxg, maxNyg,
     &            listString) .ne. 0) call exitError(listString)
c              print *,maxNxg, maxNyg
              lmGrid = max(lmGrid, maxNxg, maxNyg)
            endif
            if (ifDistort + ifMagGrad + ifWarping .ne. 0) then
              allocate(fieldDx(lmgrid,lmgrid), fieldDy(lmgrid,lmgrid),
     &            tmpDx(lmgrid,lmgrid), tmpDy(lmgrid,lmgrid), stat = ierr)
              call memoryError(ierr, 'ARRAYS FOR WARPING FIELDS')
            endif
          endif
c
          if (numTaper .eq. 1) then
            numTaper = min(127, max(16, nint((nxout + nyout) / 200.)))
            write(*,'(/,a,i4,a)')'Tapering will be done over',numTaper,' pixels'
          endif
c           
c           First see if this is the first section to replace
          if (numReplace .gt. 0 .and. isecReplace .eq. 1) then
            if (nxout .ne. nxyz2(1) .or. nyout .ne. nxyz2(2)) call exitError(
     &          'EXISTING OUTPUT FILE DOES NOT HAVE RIGHT SIZE IN X OR Y')
            if (newmode .ne. modeold) call exitError(
     &          'OUTPUT MODE DOES NOT MATCH EXISTING OUTPUT FILE')
            isecout = listReplace(isecReplace) + 1
          endif
c
c           then see if need to open an output file
          if(numReplace .eq. 0 .and. isecout.eq.1)then
C             
C             Create output file, transfer header from currently open file,
c             fix it
C             
            CALL IMOPEN(2,FILOUT(ifileout),'NEW')
            call itrhdr(2,1)
            call ialmod(2,newmode)
c             
c             set new size, keep old nxyzst
c             
            NXYZ2(1)=NXOUT
            NXYZ2(2)=NYOUT
            NXYZ2(3)=NSECOUT(IFILEOUT)
            call ialsiz(2,nxyz2,nxyzst)
c             
c             if mxyz=nxyz, keep this relationship
c             
            if(mxyz(1).eq.nx.and.mxyz(2).eq.ny.and.mxyz(3).eq.nz)then
              MXYZ2(1)=NXOUT
              MXYZ2(2)=NYOUT
              MXYZ2(3)=NSECOUT(IFILEOUT)
              call ialsam(2,mxyz2)
            else
              mxyz2(1)=mxyz(1)
              mxyz2(2)=mxyz(2)
              mxyz2(3)=mxyz(3)
            endif
c             
c             keep delta the same by scaling cell size from change in mxyz
c             
            do i=1,3
              CELL2(i)=mxyz2(i)*(cell(i)/mxyz(i)) * iBinning /
     &            expandFactor
              CELL2(i+3)=90.
            enddo
            CELL2(3)=mxyz2(3)*(cell(3)/mxyz(3))
            CALL IALCEL(2,CELL2)
c             
c             shift origin by the fraction pixel offset when binning - 
c             a positive change is needed to indicate origin inside image
c             
            call irtdel(1, delt)
            call irtorg(1, xorig, yorig, zorig)
	    if (iBinning .gt. 1) then
	      ixOriginOff = -ixOffset
	      iyOriginOff = -iyOffset
	      if ((nxout .ne. nxbin .or. nyout .ne. nybin) .and. ifxform.eq.0) then
c
c		  If output size is being set from the input and binning, the
c		  origin offset is all set, but otherwise we need to adjust
c		  for the difference between unbinned pixels to left of image 
c		  in output versus pixels to left in an input image that is
c		  bigger by the binning factor.  But this only works if
c                 there is no transformation
c
		ixOriginOff = iBinning * (nxout / 2 - nxbin / 2) - ixOffset -
     &		    (nxout * iBinning - nx) / 2
		iyOriginOff = iBinning * (nyout / 2 - nybin / 2) - iyOffset -
     &		    (nyout * iBinning - ny) / 2
	      endif
c               print *,ixOffset, ixOriginOff
              xorig = xorig + delt(1) * ixOriginOff
              yorig = yorig + delt(2) * iyOriginOff
	    endif
c
c             Adjust origin if requested: it is different depending on whether
c             there are transforms and whether offset was applied before or 
c             after.  Delt can be modified, it will be reread
            if (adjustOrigin) then
              zorig = zorig - nsecred * delt(3)
              delt(1) = delt(1) * iBinning / expandFactor
              delt(2) = delt(2) * iBinning / expandFactor
              if (ifxform .eq. 0) then
                xorig = xorig - (nxbin / 2 + nint(xcen(isec)) - nxout / 2) *
     &              delt(1)
                yorig = yorig - (nybin / 2 + nint(ycen(isec)) - nyout / 2) *
     &              delt(2)
              elseif (applyFirst .ne. 0) then
                xorig = xorig - (expandFactor * (nxbin / 2. + xcen(isec))  -
     &              nxout / 2.) * delt(1)
                yorig = yorig - (expandFactor * (nybin / 2. + ycen(isec)) -
     &              nyout / 2.) * delt(2)
              else
                xorig = xorig - (expandFactor * nxbin / 2. + xcen(isec) -
     &              nxout / 2.) * delt(1)
                yorig = yorig - (expandFactor * nybin / 2. + ycen(isec) -
     &              nyout / 2.) * delt(2)
              endif
            endif
            if (adjustOrigin .or. iBinning .gt. 1)
     &          call ialorg(2, xorig, yorig, zorig)
c             
            if(trunctxt.eq.' ')then
              write(titlech,302) xftext,floatxt,dat,tim
            else
              write(titlech,301) xftext,floatxt,trunctxt
            endif
            read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
301         FORMAT('NEWSTACK: Images copied',a13,a18,a20)
302         FORMAT('NEWSTACK: Images copied',a13,a18,t57,a9,2x,a8)
            DMAX=-1.e30
            DMIN=1.e30
            DMEAN=0.
c             
c             adjust extra header information if currently open file has it
c             
            nbsymout=0
            if(nbsymin.gt.0)then
              nbytexout=nbytexin
              nbsymout=nsecout(ifileout)*nbytexout
              if (maxextraOut .gt. 0 .and. nbsymout.gt.maxextraOut) then
                deallocate(extraout, stat = ierr)
                maxextraOut = 0
              endif
c               
c               Allocate array if needed
              if (maxextraOut .eq. 0) then
                maxextraOut = nbsymout + 1024
                allocate(extraout(maxextraOut), stat = ierr)
                call memoryError(ierr, 'ARRAYS FOR EXTRA HEADER DATA')
              endif
              call ialnbsym(2,nbsymout)
              call imposn(2,0,0)
              indxout=0
            endif
          endif
C           
c           handle complex images here and skip out
c           
          if((newmode+1)/2.eq.2.or.(mode+1)/2.eq.2)then
            if((mode+1)/2.ne.2.or.(newmode+1)/2.ne.2)call exitError(
     &          'ALL INPUT FILES MUST BE COMPLEX IF ANY ARE')
            
            if (limEntered .eq. 0 .and. nx*ny*2.gt.idim) then
              idim = nx*ny*2
              lenTemp = 1
              limdim = idim + 1
              call reallocateArray()
            endif
            if(nx*ny*2.gt.idim) call exitError('INPUT IMAGE TOO LARGE FOR ARRAY.')
              
            call imposn(1,nsecred,0)
            call irdsec(1,array,*99)
            call iclcdn(array,nx,ny,1,nx,1,ny,dmin2,dmax2,dmean2)
            call imposn(2,isecout-1,0)
            call iwrsec(2,array)
            go to 80
          endif
c           
c           determine whether rescaling will be needed
c           
          optin=optmax(mode+1)
          optout=optmax(newmode+1)
c           
c           set bottom of input range to 0 unless mode 1 or 2; set bottom
c           of output range to 0 unless not changing modes
c           
          bottomin=0.
          if(dminin.lt.0..and.(mode.eq.1.or.mode.eq.2))bottomin=-optin
          bottomout=0.
          if(mode.eq.newmode)bottomout=bottomin
          rescale=.false.
c           
c           for no float: if either mode = 2, no rescale;
c           otherwise rescale from input range to output range only if
c           mode is changing
c           
          if(iffloat.eq.0.and.newmode.ne.2 .and. mode.ne.2) then
            rescale=mode.ne.newmode
          elseif(iffloat.ne.0)then
            rescale=.true.
          endif
c           
c           Handle blank images here and skip out
c           
          if (nsecred .lt. 0 .or. nsecred .ge. nz) then
            tmpmin = dmeanin
            if (ifUseFill .ne. 0) tmpmin = fillVal
            tmpmax = tmpmin
            dsumsq = 0.
            dsum = tmpmin * (float(nxout) * nyout)
            nyNeeded = 1
            call reallocateIfNeeded()
            call findScaleFactors(iffloat, ifmean, rescale, bottomin,
     &          bottomout, optin, optout, tmpmin, tmpmax, dminspec, dmaxspec,
     &          numScaleFacs, scaleFacs, scaleConsts, zmin, zmax, dsum,
     &          dsumsq, nxout, nyout, newmode, dminin, dmaxin, fraczero, shiftmin,
     &          shiftmax, shiftmean, ifil, sclfac, const)
            do i = 1, nxout
              array(i) = tmpmin * sclfac + const
            enddo
            call imposn(2, isecout - 1, 0)
            do i = 1, nyout
              call iwrlin(2, array)
            enddo
            dmin2 = array(1)
            dmax2 = dmin2
            dmean2 = dmin2
            go to 80
          endif
          if (iVerbose.gt.0) print *,'rescale', rescale
c           
c           Get the index of the transform
          if (ifxform .ne. 0) then
            lnu=lineuse(isec)+1
            call xfcopy(f(1,1,lnu), fprod)
          endif
c           
c           If doing distortions or warping, get the grid
c           
          hasWarp = .false.
          dx = 0.
          dy = 0.
          if (ifDistort .gt. 0) then
            iy = idfUse(isec) + 1
            hasWarp = .true.
            xnbig = nx / warpScale
            ynbig = ny / warpScale
          else if (ifWarping .ne. 0) then
            iy = lnu
            hasWarp = nControl(iy) .gt. 2
            xnbig = iBinning * nxout / warpScale
            ynbig = iBinning * nyout / warpScale
c             
c             For warping with center offset applied after, it will subtract the
c             offset from the grid start and add it to the grid displacements
            if (applyFirst .eq. 0) then
              dx = iBinning * xcen(isec) / warpScale
              dy = iBinning * ycen(isec) / warpScale
            endif
          endif
          if (hasWarp) then
            if (getSizeAdjustedGrid(iy, xnbig, ynbig, dx, dy, 1, warpScale, iBinning,
     &          nxGrid, nyGrid, xGridStrt, yGridStrt, xGridIntrv, yGridIntrv, fieldDx,
     &          fieldDy, lmgrid, lmgrid, listString) .ne. 0) call exitError(listString)
c
c             print *,nxGrid,nyGrid, xGridIntrv, yGridIntrv, xGridStrt, xGridStrt +(nxGrid
c             &          - 1) * xGridIntrv, yGridStrt, yGridStrt + (nyGrid - 1)*yGridIntrv
c             do ierr = 1,nyGrid
c             write(*,'(f7.2,9f8.2)')(fieldDx(i,ierr),fieldDy(i,ierr),i=1,nxGrid)
c             enddo
c
c             copy field to tmpDx,y in case there are mag grads
            tmpDx(1:nxGrid,1:nyGrid) = fieldDx(1:nxGrid,1:nyGrid)
            tmpDy(1:nxGrid,1:nyGrid) = fieldDy(1:nxGrid,1:nyGrid)
          endif
c         
c           if doing mag gradients, set up or add to distortion field
c           
          if (ifMagGrad .ne. 0) then
            magUse = min(nsecred + 1, numMagGrad)
            if (ifDistort .ne. 0) then
              call addMagGradField(tmpDx, tmpDy, fieldDx, fieldDy, lmGrid, nxbin,
     &            nybin, nxGrid, nyGrid, xGridStrt, yGridStrt, xGridIntrv, 
     &            yGridIntrv, nxbin / 2., nybin / 2., pixelMagGrad, axisRot,
     &            tiltAngles(magUse), dmagPerUm(magUse), rotPerUm(magUse))
            else
              call makeMagGradField(tmpDx, tmpDy, fieldDx, fieldDy, lmGrid,
     &            nxbin, nybin, nxGrid, nyGrid, xGridStrt, yGridStrt, xGridIntrv, 
     &            yGridIntrv, nxbin / 2., nybin / 2.,
     &            pixelMagGrad, axisRot, tiltAngles(magUse),
     &            dmagPerUm(magUse), rotPerUm(magUse))
            endif
          endif
c           
c           if transforming, and apply first is selected, get the shifts by
c           applying the offset first then multiplying that by the transform
c           Otherwise do it the old way, subtract offset from final xform, but only
c           if not warping (now that warping is known for this section)
          if (ifxform .ne. 0) then
            if (applyFirst .ne. 0) then
              call xfunit(frot, 1.)
              frot(1,3) = -xcen(isec)
              frot(2,3) = -ycen(isec)
              call xfmult(frot, f(1,1,lnu), fprod)
            elseif (.not. (ifWarping .ne. 0 .and. hasWarp)) then
              fprod(1,3) = fprod(1,3) - xcen(isec)
              fprod(2,3) = fprod(2,3) - ycen(isec)
            endif
          endif
c           
c           get maximum Y deviation with current field to adjust chunk
c           limits with
c           
          if (ifMagGrad .ne. 0 .or. hasWarp) then
            fieldMaxX = 0.
            fieldMaxY = 0.
            do iy = 1, nyGrid
              do i = 1, nxGrid
                fieldMaxX = max(fieldMaxX, abs(fieldDx(i, iy)))
                fieldMaxY = max(fieldMaxY, abs(fieldDy(i, iy)))
              enddo
            enddo
            maxFieldX = int(fieldMaxX + 1.5)
            maxFieldY = int(fieldMaxY + 1.5)
          endif
c           
c           Determine starting and ending lines needed from the input, and whether any
c           fill is needed
          call linesNeededForOutput(0, nyout - 1, needYfirst, needYlast, fillNeeded)
          nyNeeded = needYlast + 1 - needYfirst
c           
c           Now that needed input and output size is finally known, make sure memory is
c           enough
          call reallocateIfNeeded()
c           
c           figure out how the data will be loaded and saved; first see if
c           both input and output images will fit in one chunk, or if
c           entire input image will fit
c           
          nchunk = 0
          if (idim / nxbin .gt. nyNeeded) then
            linesleft = (idim - nxbin * nyNeeded) / nxout
            nchunk = (nyout + linesleft - 1) / linesleft
            if (iVerbose.gt.0) print *,'linesleft',linesleft,'  nchunk',nchunk
          endif
          if(nchunk.eq.1.or.(nchunk.gt.0.and.nchunk.le.maxchunks.and.
     &        .not.rescale))then
c             
c             Use entire input and multi-chunk output only if not rescaling
c             set up chunks for output, and set up that
c             whole image is needed for every output chunk.
c             Note that lines are numbered from 0 here
c             
            lineOutSt(1) = 0
            do ichunk = 1, nchunk
              nextline = (nyout / nchunk) * ichunk + min(ichunk, mod(nyout, nchunk))
              nLinesOut(ichunk) = nextline - lineOutSt(ichunk)
              lineOutSt(ichunk+1) = nextline
              lineInSt(ichunk) = needYfirst
              nLinesIn(ichunk) = nyNeeded
            enddo	      
            maxin=nyNeeded
            ifOutChunk=1
          else
c             
c             otherwise, break output into successively more chunks, and
c             find out how many lines are needed of input for each chunk,
c             Scan twice: first trying to fit all the output to avoid a
c             read-write to temp file; then breaking equally
c             
            ifOutChunk=-1
            iscan=1
            iytest=nyout
            do while(iscan.le.2.and.ifOutChunk.lt.0)
              nchunk=1
              do while(nchunk.le.maxchunks.and.ifOutChunk.lt.0)
                lineOutSt(1)=0
                maxin=0
                do ichunk=1,nchunk
                  nextline=(nyout/nchunk)*ichunk+min(ichunk,mod(nyout,nchunk))
                  nLinesOut(ichunk)=nextline-lineOutSt(ichunk)
                  lineOutSt(ichunk+1)=nextline
c                   
                  call linesNeededForOutput(lineOutSt(ichunk), nextline - 1, iy1, iy2,
     &                fillTmp)
                  lineInSt(ichunk)=iy1
                  nLinesIn(ichunk)=iy2+1-iy1
                  maxin=max(maxin,nLinesIn(ichunk))
                enddo
c                 
c                 Will the input and output data now fit?  Then terminate.
c                 
                if(iscan.eq.2)iytest=nLinesOut(1)
                if(idim/maxin .gt. nxbin .and. idim / iytest .gt. nxout .and.
     &              maxin*int(nxbin,kind=8)+iytest*int(nxout,kind=8).le.idim)then
                  ifOutChunk=iscan-1
                else
                  nchunk=nchunk+1
                endif
              enddo
              iscan=iscan+1
            enddo
            if(ifOutChunk.lt.0)call exitError(
     &          ' INPUT IMAGE TOO LARGE FOR ARRAY.')
          endif
          if (iVerbose.gt.0) then
	    print *,'number of chunks:',nchunk
	    do i=1,nchunk
              print *,i,lineInSt(i),nLinesIn(i),lineOutSt(i),nLinesOut(i)
	    enddo
          endif
          if (nchunk .gt. 1 .and. numTaper .gt. 0)
     &        call exitError('CANNOT TAPER OUTPUT IMAGE - IT DOES NOT '//
     &        'FIT COMPLETELY IN MEMORY')
c           
c           open temp file if one is needed
c           
          if(rescale.and.ifOutChunk.gt.0.and.nchunk.gt.1.and.
     &        iftempopen.eq.0)then
            tempext='nws      '
            tempext(4:5)=tim(1:2)
            tempext(6:7)=tim(4:5)
            tempext(8:9)=tim(7:8)
            tempname=temp_filename(FILOUT(ifileout),' ',tempext)
c             
            call imopen(3,tempname,'scratch')
            call icrhdr(3,nxyz2,nxyz2,2,title,0)
            iftempopen=1
          endif
c           
          ibbase=int(maxin,kind=8)*nxbin+1
c           
c           get the mean of section from previous scan, or a new scan
c           
          loadyst=-1
          loadynd=-1
          if (ifUseFill .ne. 0) then
            dmeansec = fillVal
          elseif(ifmean.ne.0)then
            dmeansec=secmean(ilis+listind(ifil)-1)
          elseif (.not. fillNeeded) then
            dmeansec = dmeanin
          else
            if (iVerbose .gt. 0) print *,'scanning for mean for fill',nsecred,nyNeeded,
     &          needYfirst
            wallstart = walltime()
            call scansection(array,idim,nxbin,nyNeeded, needYfirst, iBinning,ixOffset,
     &          iyOffset,nsecred,0,dmin2,
     &          dmax2,dmeansec,sdsec,loadyst,loadynd,array(idim+1), lenTemp)
            loadynd=min(loadynd,loadyst+maxin-1)
            loadtime = loadtime + walltime() - wallstart
          endif
c           
c           loop on chunks
c           
          dsum=0.
          dsumsq=0.
          tmpmin=1.e30
          tmpmax=-1.e30
          do ichunk=1,nchunk
            needyst=lineInSt(ichunk)
            needynd=needyst+nLinesIn(ichunk)-1
c             
c             first load data that is needed if not already loaded
c             
            wallstart = walltime()
            if(needyst.lt.loadyst.or.needynd.gt.loadynd)then
              if(loadyst.le.needyst.and.loadynd.ge.needyst)then
c                 
c                 move data down if it will fill a bottom region
c                 
                nmove=(loadynd+1-needyst)*int(nxbin,kind=8)
                noff=(needyst-loadyst)*int(nxbin,kind=8)
                if (iVerbose.gt.0) print *,'moving data down',nmove,noff
                do i8=1,nmove
                  array(i8)=array(i8+noff)
                enddo
                nload=needynd-loadynd
c                 call imposn(1,nsecred,loadynd+1)
c		  call irdsecl(1,array(nmove+1),nload,*99)
                call irdBinned(1, nsecred, array(nmove+1), nxbin,
     &              nload, ixOffset, iyOffset + iBinning*(loadynd + 1),
     &              iBinning, nxbin, nload, array(idim+1), lenTemp, ierr)
                if (ierr .ne. 0) go to 99
              elseif(needyst.le.loadyst.and.needynd.ge.loadyst)then
c                 
c                 move data up if it will fill top
c                 
                nmove=(needynd+1-loadyst)*int(nxbin,kind=8)
                noff=(loadyst-needyst)*int(nxbin,kind=8)
                if (iVerbose.gt.0) print *,'moving data up',nmove,noff
                do i8=nmove,1,-1
                  array(i8+noff)=array(i8)
                enddo
                nload=loadyst-needyst
c		  call imposn(1,nsecred,needyst)
c		  call irdsecl(1,array,nload,*99)
                call irdBinned(1, nsecred, array, nxbin, nload, ixOffset,
     &              iyOffset + iBinning*(needyst), iBinning, nxbin, nload,
     &              array(idim+1), lenTemp, ierr)
                if (ierr .ne. 0) go to 99

              else
c                 
c                 otherwise just get whole needed region
c                 
                nload=needynd+1-needyst
                if (iVerbose.gt.0) print *,'loading whole region', needyst, needynd,nload
c		  call imposn(1,nsecred,needyst)
c		  call irdsecl(1,array,nload,*99)
                call irdBinned(1, nsecred, array, nxbin, nload, ixOffset,
     &              iyOffset + iBinning*(needyst), iBinning, nxbin, nload,
     &              array(idim+1), lenTemp, ierr)
                if (ierr .ne. 0) go to 99
              endif
              loadyst=needyst
              loadynd=needynd
            endif
            nyload=loadynd+1-loadyst
            nych=nLinesOut(ichunk)
            npix=int(nxout, kind=8)*nych
            ibchunk=ibbase
            if(ifOutChunk.eq.0)ibchunk=ibbase+lineOutSt(ichunk)*int(nxout,kind=8)
            loadtime = loadtime + walltime() - wallstart

            if(ifxform.ne.0)then
c               
c               do transform if called for
c               
              wallstart = walltime()
              xci=nxbin/2.
              yci=nybin/2.-loadyst
              dx=fprod(1,3)
              dy=(nyout-nych)/2.+fprod(2,3) - lineOutSt(ichunk)
c		dx=f(1,3,lnu)-xcen(isec)
c		dy=(nyout-nych)/2.+f(2,3,lnu) - ycen(isec) - lineOutSt(ichunk)
              if (linesShrink .gt. 0) then
                ierr=zoomFiltInterp(array,array(ibchunk),nxbin,nyload, nxout, nych,
     &              xci ,yci, dx,dy,dmeansec) 
                if (ierr.ne. 0) then 
                  write(listString, '(a,i3)')
     &                'CALLING zoomFiltInterp FOR IMAGE REDUCTION, ERROR', ierr
                  call exitError(listString)
                endif
              elseif (.not. hasWarp .and. ifMagGrad .eq. 0) then
                call cubinterp(array,array(ibchunk),nxbin,nyload, nxout, nych,
     &              fprod,xci ,yci, dx,dy,1.,dmeansec, ifLinear)
              else
c                 
c                 If undistorting, adjust the grid start down by first loaded input line
c                 If warping, adjust it down by first output line
                ystart = yGridStrt - loadyst
                if (ifWarping .ne. 0) ystart = yGridStrt - lineOutSt(ichunk)
                call warpInterp(array,array(ibchunk),nxbin,nyload,
     &              nxout, nych, fprod,xci ,yci, dx,dy,1.,dmeansec, ifLinear, ifWarping,
     &              fieldDx, fieldDy, lmGrid, nxGrid, nyGrid, xGridStrt, ystart,
     &              xGridIntrv, yGridIntrv)
              endif
              rottime = rottime + walltime() - wallstart
            else
c               
c               otherwise repack array into output space nxout by nyout, with
c               offset as specified, using the special repack routine
c               
              IX1 = nxbin / 2 - NXOUT / 2 + nint(xcen(isec))
              IX2 = IX1 + NXOUT - 1
C               
              IYbase = NYbin / 2 - nyout / 2 + nint(YCEN(isec))
              IY1 = IYbase+lineOutSt(ichunk)-loadyst
              iy2 = iy1+nych-1
c               
              CALL IREPAK2(array(ibchunk),ARRAY,nxbin,nyload,IX1,IX2,IY1,
     &            IY2,dmeansec)
              if (iVerbose.gt.0) print *,'did repack'
            endif
            if (numTaper .gt. 0) then
              if (taperAtFill(array(ibchunk), nxout, nyout, numTaper, insideTaper) .ne. 0)
     &            call exitError('MEMORY ALLOCATION ERROR TAPERING IMAGE')
            endif
c	      
c             if no rescaling, or if mean is needed now, accumulate sums for
c             mean
c             
            if(.not.rescale.or.ifmean.ne.0)then
              if(iffloat.eq.2)then
                call iclavgsd(array(ibchunk),nxout,nych,1,nxout,1,nych,
     &              tmin2,tmax2,tsum, tsumsq, avgsec,sdsec)
		  if (iVerbose.gt.0)print *,'chunk mean&sd',ichunk,avgsec,sdsec
                dsumsq=dsumsq+tsumsq
              else
                call iclden(array(ibchunk),nxout,nych,1,nxout,1,nych,tmin2,
     &              tmax2,tmean2)
                tsum=tmean2*npix
              endif
              tmpmin=min(tmpmin,tmin2)
              tmpmax=max(tmpmax,tmax2)
              dsum=dsum+tsum
              if (iVerbose.gt.0)print *,'did iclden ',tmin2,tmax2,tmpmin,tmpmax
            else
c               
c               otherwise get new min and max quickly
c               
              do i8=1,npix
                val=array(i8+ibchunk-1)
                if(val.lt.tmpmin)tmpmin=val
                if(val.gt.tmpmax)tmpmax=val
              enddo
            endif
            if(iffloat.eq.0.and.newmode.ne.2 .and. mode.ne.2)then
c               
c               6/27/01: really want to truncate rather than rescale; so if
c               the min or max is now out of range for the input mode,
c               truncate the data and adjust the min and max
c               
              if(tmpmin.lt.bottomin.or.tmpmax.gt.optin)then
                tsum2=0.
                do i8=1,npix
                  val=max(bottomin,min(optin,array(i8+ibchunk-1)))
                  tsum2=tsum2+val
                  array(i8+ibchunk-1)=val
                enddo
                tmpmin=max(tmpmin,bottomin)
                tmpmax=min(tmpmax,optin)
                dsum=dsum+tsum2-tsum
              endif
            endif
c             
c             write all but last chunk
c             
            wallstart = walltime()
            if(.not.rescale)then
		if (iVerbose.gt.0) print *,'writing to real file',ichunk
              call imposn(2,isecout-1,lineOutSt(ichunk))
              call iwrsecl(2,array(ibchunk),nLinesOut(ichunk))
            elseif(ichunk.ne.nchunk.and.ifOutChunk.gt.0)then
		if (iVerbose.gt.0) print *,'writing to temp file',ichunk
              call imposn(3,0,lineOutSt(ichunk))
              call iwrsecl(3,array(ibbase),nLinesOut(ichunk))
            endif
            savetime = savetime + walltime() - wallstart
          enddo

          call findScaleFactors(iffloat, ifmean, rescale, bottomin, bottomout,
     &        optin, optout, tmpmin, tmpmax, dminspec, dmaxspec, numScaleFacs,
     &        scaleFacs, scaleConsts, zmin, zmax, dsum, dsumsq, nxout, nyout,
     &        newmode, dminin, dmaxin, fraczero, shiftmin, shiftmax,
     &        shiftmean, ifil, sclfac, const)

          if(rescale)then
            dmin2=1.e20
            dmax2=-1.e20
            dmean2=0.
c             set up minimum value to output based on mode
            if(newmode.eq.1)then
              denoutmin=-32767
            elseif(newmode.eq.2)then
              denoutmin=-1.e30
              optout=1.e30
            else
              denoutmin=0.
            endif
c             
c             loop backwards on chunks, reloading all but last, scaling
c             and rewriting
c             
            do ichunk=nchunk,1,-1
              ibchunk=ibbase
              if(ifOutChunk.eq.0)ibchunk=ibbase+lineOutSt(ichunk)*nxout
              if(ichunk.ne.nchunk.and.ifOutChunk.gt.0)then
		  if (iVerbose.gt.0) print *,'reading',ichunk
                call imposn(3,0,lineOutSt(ichunk))
                call irdsecl(3,array(ibbase),nLinesOut(ichunk),*99)
              endif
              do iy=1,nLinesOut(ichunk)
                istart=ibchunk+(iy-1)*int(nxout, kind=8)
                tsum=0.
                do i8=istart,istart+nxout-1
                  den=sclfac*array(i8)+const
                  if(den.lt.denoutmin)then
                    ntrunclo=ntrunclo+1
                    den=denoutmin
                  elseif(den.gt.optout)then
                    ntrunchi=ntrunchi+1
                    den=optout
                  endif
                  array(i8)=den
                  tsum=tsum+den
                  dmin2=min(dmin2,den)
                  dmax2=max(dmax2,den)
                enddo
                dmean2=dmean2+tsum
              enddo
              waLlstart = walltime()
              if (iVerbose.gt.0) print *,'writing',ichunk
              call imposn(2,isecout-1,lineOutSt(ichunk))
              call iwrsecl(2,array(ibchunk),nLinesOut(ichunk))
              savetime = savetime + walltime() - wallstart
            enddo
          else
c             
c             if not scaling
c             
            dmin2 = tmpmin
            dmax2 = tmpmax
            dmean2 = dsum
          endif
c           
          dmean2=dmean2/(float(nxout)*nyout)
          if(ifheaderout.eq.0) print *,
     &        'section   input min&max       output min&max  &  mean'
          ifheaderout=1
          write(*,'(i6,5f10.2)')isec-1,tmpmin,tmpmax,dmin2,dmax2,
     &        dmean2

C           
80        isecout=isecout+1
          DMIN = MIN(DMIN,DMIN2)
          DMAX = MAX(DMAX,DMAX2)
          if (numReplace .eq. 0) then
            DMEAN = DMEAN + DMEAN2
c           
c             transfer extra header bytes if present
c           
            if(nbsymout.ne.0.and.indxout.lt.nbsymout)then
              nbcopy=min(nbytexout,nbytexin,nbsymin)
              nbclear=nbytexout-nbcopy
              do i=1,nbcopy
                indxout=indxout+1
                extraout(indxout)=extrain(nsecred*nbytexin+i)
              enddo
              do i=1,nbclear
                indxout=indxout+1
                extraout(indxout)=0
              enddo
            endif
          else
            isecReplace = isecReplace + 1
            isecout = listReplace(isecReplace) + 1
          endif
c           
c           see if need to close stack file
c           
          if(numReplace .eq. 0 .and. isecout.gt.nsecout(ifileout))then
            if(nbsymout.gt.0)call ialsym(2,nbsymout,extraout)
            DMEAN=DMEAN/nsecout(ifileout)
            CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
            CALL IMCLOSE(2)
            isecout=1
            ifileout=ifileout+1
          endif
          isec=isec+1
        enddo
        call imclose(1)
      enddo
      if (numReplace .gt. 0) then
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        CALL IMCLOSE(2)
      endif
C       
      if(iftempopen.ne.0)call imclose(3)
      if(ntrunclo+ntrunchi.gt.0)write(*,103)ntrunclo,ntrunchi
103   format(' TRUNCATIONS OCCURRED:',i11,' at low end,',i11,
     &    ' at high end of range')
      if (iVerbose.gt.0)write(*,'(a,f8.4,a,f8.4,a,f8.4,a,f8.4)')'loadtime',
     &    loadtime,'  savetime', savetime,'  sum',loadtime+savetime,
     &    '  rottime',rottime
      call exit(0)
99    call exitError(' END OF IMAGE WHILE READING')

      CONTAINS

      subroutine reallocateIfNeeded()
      integer(kind=8) needDim
      integer*4 needTemp
      real*4 defLimit/3.75e9/
      if (limEntered .eq. 0) then
        needTemp = 1
        if (iBinning .gt. 1) needTemp = nx * iBinning
        needDim = int(nxbin, kind=8) * nyNeeded
        if (nxout .gt. 0 .and. nyout .gt. 0)
     &      needDim = needDim + int(nxout, kind=8) * nyout
        if (needDim .gt. defLimit) needDim = defLimit
        if (iVerbose.gt.0) print *,'reallocate sizes:', nxout,nyout,nxbin,nybin,needDim
        if (needDim + needTemp .gt. limdim) then
          limdim = needDim + needTemp
          call reallocateArray()
        endif
        idim = needDim
        lenTemp = needTemp
      endif
      end subroutine reallocateIfNeeded

      subroutine reallocateArray()
      if (iVerbose.gt.0) print *, 'reallocating array to',limdim/(1024*256),
     &    ' MB'
      deallocate(array)
      allocate(array(limdim), stat = ierr)
      if (ierr .ne. 0 .and. limdim .gt. limIfFail) then
        limdim = limIfFail
        idim = limdim - lenTemp
        if (iVerbose.gt.0) print *, 'failed, dropping reallocation to',
     &      limdim/(1024*256),' MB'
        allocate(array(limdim), stat = ierr)
      endif
      if (ierr .ne. 0) call exitError('REALLOCATING MEMORY FOR MAIN ARRAY')
      return
      end subroutine reallocateArray


c       Finds what lines of input are needed to produce lines from lineOutFirst through
c       lineOutLast of output (numbered from 0), and also determines if X or Y input goes
c       out of range so that fill is needed
c       
      subroutine linesNeededForOutput(lineOutFirst, lineOutLast, iyIn1, iyIn2, needFill)
      integer*4 lineOutFirst, lineOutLast, iyIn1, iyIn2, ixIn1, ixIn2, ixbase
      logical needFill
      real*4 xp1,yp1,xp2,yp2,xp3,yp3,xp4,yp4
      if(ifxform.eq.0)then
c                     
c         simple case of no transform
c         
        iybase = nybin/2 + ycen(isec) - (nyout/2)
        ixbase = nxbin/2 + xcen(isec) - (nxout/2)
        iyIn1 = max(0, iybase + lineOutFirst)
        iyIn2 = min(nybin-1, iybase + lineOutLast)
        needFill = IYbase + lineOutFirst .lt. 0 .or. iybase + lineOutLast .ge. nybin .or.
     &      ixbase .lt. 0 .or. ixbase + nxout .gt. nxbin
      else
c                     
c         transform: get input needs of 4 corners
c         pass and get back coordinates numbered from 1, subtract
c         an extra 1 to get to lines numbered from 0
c         Allow extra for distortion field Y component
c         
        xci=nxbin/2.
        yci=nybin/2.
        dx = fprod(1,3)
        dy = fprod(2,3)
c         dx=f(1,3,lnu)-xcen(isec)
c         dy=f(2,3,lnu)-ycen(isec)
        call backxform(nxout,nyout,fprod,xci ,yci,dx,dy, 1,lineOutFirst+1, xp1,yp1)
        call backxform(nxout,nyout,fprod,xci ,yci,dx,dy, nxout,lineOutFirst+1, xp2,yp2)
        call backxform(nxout,nyout,fprod,xci ,yci,dx,dy, 1,lineOutLast+1, xp3,yp3)
        call backxform(nxout,nyout,fprod,xci ,yci,dx,dy, nxout,lineOutLast+1, xp4,yp4)
        
        iyIn1=int(min(yp1,yp2,yp3,yp4))-2 - maxFieldY - linesShrink
        iyIn2=int(max(yp1,yp2,yp3,yp4))+1 + maxFieldY + linesShrink
        ixIn1=int(min(xp1,xp2,xp3,xp4))-2 - maxFieldX - linesShrink
        ixIn2=int(max(xp1,xp2,xp3,xp4))+1 + maxFieldX + linesShrink
        needFill = ixIn1.lt.0 .or. ixIn1.ge.nxbin .or. ixIn2.lt.0 .or. ixIn2.ge.nxbin .or.
     &      iyIn1.lt.0 .or. iyIn1.ge.nybin .or. iyIn2.lt.0 .or. iyIn2.ge.nybin
        iyIn1=min(nybin-1,max(0,iyIn1))
        iyIn2=min(nybin-1,max(0,iyIn2))
      endif

      end subroutine linesNeededForOutput

      END


c       Determine the scale factors SCLFAC and CONST from a host of option 
c       settings, controlling values, and values determined for the particular
c       section.  This code is really dreadful since first it calculates new
c       min and max and uses that to determine scaling.
c
      subroutine findScaleFactors(iffloat, ifmean, rescale, bottomin,
     &    bottomout, optin, optout, tmpmin, tmpmax, dminspec, dmaxspec,
     &    numScaleFacs, scaleFacs, scaleConsts, zmin, zmax, dsum, dsumsq, nxout,
     &    nyout, newmode, dminin, dmaxin, fraczero, shiftmin, shiftmax,
     &    shiftmean, ifil, sclfac, const)
      implicit none
      integer*4 iffloat, ifmean, numScaleFacs, nxout, nyout, newmode, ifil
      logical*4 rescale
      real*4 bottomin, bottomout, optin, optout, tmpmin, tmpmax, dminspec
      real*4 dmaxspec, scaleFacs(*), scaleConsts(*), zmin, zmax, sclfac, const
      real*4 dminin, dmaxin, fraczero, shiftmin, shiftmax, shiftmean
      real*8 dsum, dsumsq
c       
      real*4 avgsec, sdsec, dminnew, dmaxnew, dmin2, dmax2, zminsec, zmaxsec
      real*4 tmpmean, tmpminshf, tmpmaxshf
c       
c       calculate new min and max after rescaling under various possibilities
c       
      sclfac = 1.
      const = 0.
      dmin2=tmpmin
      dmax2=tmpmax
c       
      if(iffloat.eq.0.and.rescale)then
c         
c         no float but mode change (not to mode 2):
c         rescale from input range to output range
c         
        dmin2=(tmpmin-bottomin)*(optout-bottomout)/
     &      (optin-bottomin)+bottomout
        dmax2=(tmpmax-bottomin)*(optout-bottomout)/
     &      (optin-bottomin)+bottomout
      elseif(iffloat.lt.0 .and. numScaleFacs .eq. 0)then
c         
c         if specified global rescale, set values that dminin and dmaxin
c         map to, either the maximum range or the values specified
c         
        if(dminspec.eq.0.and.dmaxspec.eq.0)then
          dminnew=0.
          dmaxnew=optout
        else if(dminspec.eq.dmaxspec)then
          dminnew=dminin
          dmaxnew=dmaxin
        else
          dminnew=dminspec
          dmaxnew=dmaxspec
        endif
c         
c         then compute what this section's tmpmin and tmpmax map to
c         
        dmin2=(tmpmin-dminin)*(dmaxnew-dminnew)/(dmaxin-dminin) +dminnew
        dmax2=(tmpmax-dminin)*(dmaxnew-dminnew)/(dmaxin-dminin) +dminnew
      elseif(iffloat.gt.0)then
c         
c         If floating: scale to a dmin2 that will knock out fraczero of
c         the range after truncation to zero
c         
        dmin2=-optout*fraczero/(1.-fraczero)
        if(ifmean.eq.0)then
c           
c           float to range, new dmax2 is the max of the range
c           
          dmax2=optout
        elseif(iffloat.eq.2)then
c           :float to mean, it's very hairy
          call sums_to_avgsd8(dsum,dsumsq,nxout,nyout,avgsec,sdsec)
c           print *,'overall mean & sd',avgsec,sdsec
          if (tmpmin .eq. tmpmax .or. sdsec .eq. 0.) sdsec = 1.
          zminsec=(tmpmin-avgsec)/sdsec
          zmaxsec=(tmpmax-avgsec)/sdsec
          dmin2=(zminsec-zmin)*optout/(zmax-zmin)
          dmax2=(zmaxsec-zmin)*optout/(zmax-zmin)
          dmin2=max(0.,dmin2)
          dmax2=min(dmax2,optout)
        else
c           
c           shift to mean
c           
          tmpmean=dsum/(float(nxout)*nyout)
c           
c           values that min and max shift to
c           
          tmpminshf=tmpmin+shiftmean-tmpmean
          tmpmaxshf=tmpmax+shiftmean-tmpmean
c           
          if(iffloat.eq.3)then
c             
c             for no specified scaling, set new min and max to
c             shifted values
c             
            dmin2=tmpminshf
            dmax2=tmpmaxshf
            if(newmode.ne.2)then
c               
c               then, if mode is not 2, set up for scaling if range is
c               too large and/or if there is a modal shift
c               
              optin=max(optin,shiftmax)
              dmin2=tmpminshf*optout/optin
              dmax2=tmpmaxshf*optout/optin
            endif
          else
c             
c             for specified scaling of shifted means
c             
            if(dminspec.eq.dmaxspec)then
              dminnew=0.5
              dmaxnew=optout-0.5
            else
              dminnew=dminspec
              dmaxnew=dmaxspec
            endif
c             
c             for specified scaling, compute what this section's tmpmin
c             and tmpmax map to
c             
            dmin2=(tmpminshf-shiftmin)*(dmaxnew-dminnew)/
     &          (shiftmax-shiftmin)+dminnew
            dmax2=(tmpmaxshf-shiftmin)*(dmaxnew-dminnew)/
     &          (shiftmax-shiftmin)+dminnew
          endif
        endif
      endif
c       
      if(rescale)then
c         
c         if scaling, set up equation, scale and compute new mean
c         or use scaling factors directly
c         
        if (numScaleFacs .gt. 0) then
          sclfac = scaleFacs(min(numScaleFacs, ifil))
          const = scaleConsts(min(numScaleFacs, ifil))
        else
c           
c           2/9/05: keep scale factor 1 if image has no range
c           
          sclfac = 1.
          if (dmax2.ne.dmin2 .and. tmpmax.ne.tmpmin)
     &        sclfac=(dmax2-dmin2)/(tmpmax-tmpmin)
          const=dmin2-sclfac*tmpmin
        endif
      endif
      return
      end


c       getItemsToUse gets the list of transform line numbers or distortion
c       fields to apply, given the number available in nxforms, the list of
c       listot section numbers in inlist, the PIP option in option, the
c       pipinput flag if doing pip input, and a scratch string in listString.
c       error should have a base error string, ifOnePerFile > 0 to do one
c       transform per file, and nfilein should have number of files. The list
c       of items is returned in lineUse, and the number in the list in nLineUse
c
      subroutine getItemsToUse(nxforms, listot, inlist, option, listString,
     &    pipinput, error, ifOnePerFile, nfilein, lineUse, nLineUse, lmsec)
      implicit none
      integer*4 nxforms, listot, inlist(*), numXfLines, lineUse(*), nLineUse
      integer*4 ifOnePerFile, nfilein
      integer*4 lmsec, nLinetemp, ierr, i, PipGetString
      character*(*) error, option, listString
      character*80 errString
      logical*4 pipinput
c       
      write(errString, '(a,a,a)')'TOO MANY ', error, ' NUMBERS FOR ARRAYS'
      if (nxforms .eq. 1) then
c         
c         For one transform, set up single line for now
c
        nlineuse = 1
        lineuse(1) = 0
      elseif (ifOnePerFile .gt. 0) then
c         
c         For one transform per file, default is 0 to nfile - 1
c
        nLineUse = nfilein
        do i = 1, nfilein
          lineuse(i) = i - 1
        enddo
      else        
c         
c         Otherwise default comes from section list
c
        nlineuse = listot
        do i = 1, listot
          lineuse(i) = inlist(i)
        enddo
      endif
c       
      numXfLines = 0
      if (pipinput) then
        call PipNumberOfEntries(option, numXfLines)
        if (numXfLines .gt. 0) then
          nlineTemp = nlineuse
          nLineUse = 0
          do i = 1, numXfLines
            ierr = PipGetString(option, listString)
            call parselist(listString, lineuse(nLineUse + 1), nlineTemp)
            nLineUse = nLineUse + nLineTemp
            if (nLineUse .gt. lmsec) call exitError(trim(errString))
          enddo
        endif
      else
c           
        print *,'Enter list of lines to use in file, or a single ',
     &      'line number to apply that'
        print *,' transform to all sections',
     &      ' (1st line is 0; ranges OK; / for section list)'
        call rdlist(5,lineuse,nlineuse)
        if (nLineUse .gt. lmsec) call exitError(trim(errString))
      endif
        
      do i = 1, nLineUse
        if (lineUse(i) .lt. 0 .or. lineUse(i) .ge. nxforms) then
          write(errString, '(a, a,i5)')error, ' NUMBER OUT OF BOUNDS:',
     &        lineUse(i)
          call exitError(trim(errString))
        endif
      enddo
      return
      end
      

c       IREPAK2 repacks an image from a portion of a 2-d array sequentially
c       into a 1-d array (which should not be the same array).  Pixels
c       outside the range of the original array will be filled with the
c       supplied value of DMEAN.  BRRAY is the repacked array,
c       everything else follows definition of iwrpas; i.e. ARRAY is
c       dimensioned MX by MY, and the starting and ending index coordinates
c       (numbered from 0) are given by NX1, NX2, NY1, NY2
c       
      subroutine irepak2(brray,array,mx,my,nx1,nx2,ny1,ny2,dmean)
      implicit none
      integer*4 mx,my,nx1,nx2,ny1,ny2
      real*4 brray(*),array(mx,my),dmean
      integer*4 ind, iy,ix
      ind=1
      do iy=ny1+1,ny2+1
        if(iy.ge.1.and.iy.le.my)then
          do ix=nx1+1,nx2+1
            if(ix.ge.1.and.ix.le.mx)then
              brray(ind)=array(ix,iy)
            else
              brray(ind)=dmean
            endif
            ind=ind+1
          enddo
        else
          do ix=nx1+1,nx2+1
            brray(ind)=dmean
            ind=ind+1
          enddo
        endif
      enddo
      return
      end


c       SCANSECTION will determine the min DMIN2, max DMAX2, and mean DMEAN2 of section
c       NSECRED.  It will also determine the standard deviation SDSEC if IFFLOAT = 2.
c       It uses ARRAY for storage.  IDIM specifies the size of ARRAY, while NX is the
c       binned image size in X, nyNeeded is the number of lines to scan, and needYfirst
c       is the first line.  The image will be loaded in chunks if necessary.  LOADYST
c       and LOADYND are the starting and ending lines (numbered from 0) that are left
c       in ARRAY.
c       
      subroutine scansection(array,idim,nx,nyNeeded,needYfirst,nbin, ixOffset, iyOffset,
     &    nsecred,iffloat, dmin2, dmax2,dmean2,sdsec, loadyst,loadynd, temp, lenTemp)
      implicit none
      integer(kind=8) idim
      integer*4 nx,nsecred,iffloat,loadyst,loadynd, nbin, lenTemp,needYfirst, nyNeeded
      real*4 array(idim),temp(lenTemp),dmin2,dmax2,dmean2,sdsec
      integer*4 maxlines,nloads,iline,iload,nlines, ixOffset, iyOffset,ierr
      real*4 tmin2,tmax2,tmean2,avgsec
      real*8 dsum,dsumsq,tsum,tsumsq
c       
c       load in chunks if necessary, based on the maximum number
c       of lines that will fit in the array
c       
      maxlines = idim / nx
      nloads = (nyNeeded + maxlines - 1) / maxlines
      iline=needYfirst
      dmin2=1.e30
      dmax2=-dmin2
      dsum=0.
      dsumsq=0.
      do iload=1,nloads
        nlines=nyNeeded/nloads
        if(iload.le.mod(nyNeeded,nloads))nlines=nlines+1
c         call imposn(1,nsecred,iline)
        call irdBinned(1, nsecred, array, nx, nlines, ixOffset, iyOffset +
     &      nbin * iline, nbin, nx, nlines, temp, lenTemp, ierr)
        if (ierr .ne. 0) call exitError('READING FILE')
c         call irdsecl(1,array,nlines,*99)
c         
c         accumulate sums for mean and sd if float 2, otherwise
c         just the mean
c         
        if(iffloat.eq.2)then
          call iclavgsd(array,nx,nlines,1,nx,1,nlines,tmin2,tmax2,
     &        tsum,tsumsq, avgsec,sdsec)
          dsumsq=dsumsq+tsumsq
        else
          call iclden(array,nx,nlines,1,nx,1,nlines,tmin2,
     &        tmax2,tmean2)
          tsum=tmean2*nx*nlines
        endif
        dmin2=min(dmin2,tmin2)
        dmax2=max(dmax2,tmax2)
        dsum=dsum+tsum
        iline=iline+nlines
      enddo
c       
      if(iffloat.eq.2)then
        call sums_to_avgsd8(dsum,dsumsq,nx,nyNeeded,dmean2,sdsec)
      else
        dmean2=dsum/(float(nx)*nyNeeded)
      endif
      loadynd=iline-1
      loadyst=iline-nlines
      return
      end


c       BACKXFORM will determine the array coordinates XP and YP in an input
c       image that transform to the given array indexes IX, IY in an output
c       image.  Other arguments match those of CUBINTERP: the input image is
c       NXA by NYA; the output image is NXB by NYB; XC, YC is the center
c       coordinate of the input image; AMAT is the 2x2 transformation matrix
c       and XT, YT are the translations.
c       
      SUBROUTINE backxform(NXB,NYB,AMAT,XC,YC,XT,YT,ix,iy,xp,yp)
      implicit none
      integer*4 NXB,NYB,ix,iy
      real*4 AMAT(2,2),XC,YC,XT,YT,xp,yp
      real*4 xcen,ycen,xco,yco,denom,a11,a12,a21,a22,dyo,dxo
C       
C       Calc inverse transformation
C       
      XCEN = NXB/2. + XT + 0.5
      YCEN = NYB/2. + YT + 0.5
      XCO = XC + 0.5
      YCO = YC + 0.5
      DENOM = AMAT(1,1)*AMAT(2,2) - AMAT(1,2)*AMAT(2,1)
      A11 =  AMAT(2,2)/DENOM
      A12 = -AMAT(1,2)/DENOM
      A21 = -AMAT(2,1)/DENOM
      A22 =  AMAT(1,1)/DENOM
c       
c       get coordinate transforming to ix,iy
c       
      DYO = IY - YCEN
      DXO = IX - XCEN
      XP = A11*DXO + A12*DYO + XCO
      YP = A21*DXO + A22*DYO + YCO
      return
      end
