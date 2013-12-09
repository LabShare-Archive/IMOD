*       * * * * * * * * BLENDMONT.FOR * * * * * * *
c       
c       BLENDMONT will take montaged images, blend their overlapping edges
c       together, and output the blended images with essentially no overlap.
c       Transformations can be applied to align serial sections, and
c       densities can be floated over whole sections so that density
c       occupies the maximum range for each section.  The program can also
c       correct for minor or substantial displacements between pieces, using
c       cross-correlation to find substantial shifts.  Multinegative montages
c       can also be handled, with each negative transformed so as to produce
c       the best fit between the negatives.
c       
c       See the man page for further details.
c       
c       David Mastronarde, February 1989
c       12/21/98 added capability to do initial cross-correlation of overlaps
c       and correct for sloppy montages
c       12/99: revamped the treatment of overlap zones to avoid artifacts in
c       sloppy montages
c       8/22/00: made it rename existing edge function files if 'new' ones
c       are requested and they already exist
c       12/11/00: implemented byte-swapping when reading edge function files
c       built on other machines
c       6/1/01: implemented ability to write and read in edge correlation
c       displacements; added a search step to improve on cross-correlations  
c       
c       $Id$
c       
      use blendvars
      implicit none
      integer nbytes_recl_item,limneg
      include 'recl_bytes.inc'
      character*320 filnam,edgenam,plOutfile, outFile
      character*320 rootname,edgename2
      character*4 edgeext(2)/'.xef','.yef'/
      character*5 xcorrext(0:2)/'.ecd','.xecd','.yecd'/
      character*18 actionStr
      integer*4 mxyzin(3),nxyzst(3)/0,0,0/
      real*4 cell(6)/1.,1.,1.,0.,0.,0./
      real*4 delta(3),xorig, yorig, zorig
      real*4, allocatable :: binline(:)
      character*80 titlech
c       
c       structure /rotrans/
c       real*4 theta,dx,dy,xcen,ycen
c       end structure
c       
      integer*4 nskip(2)/1,2/                   !regression position skip s&l
      integer*4 nfit(2)/5,7/                    !# to include in regression s&l
      integer*4 igridstr(2),iofset(2)           !just temp usage here
c       
      parameter (limneg=30)
      integer*4 minnegx(limneg),maxnegx(limneg) !min and max coords of
      integer*4 minnegy(limneg),maxnegy(limneg) !each neg
      integer*4 jointlower(limneg,2),jointupper(limneg,2) !joints around neg
      integer*4 neglower(limneg,2),negupper(limneg,2) !negs around joint
      integer*4 negind(limneg)                  !index to arbitrary neg #'s
      integer*4 nhadjsum(limneg),njoint(2)
      integer*4 nedonjoint(15),listonjoint(15,limneg) !#, list edges on joint
      integer*4 ixpclo(15),ixpcup(15),iypclo(15),iypcup(15)
c       
      integer*4 nedgetmp(5,2)
      integer*4, allocatable :: listz(:),izwant(:), izAllWant(:)
      logical skipxforms,undistortOnly,xcWriteOut, sameEdgeShifts
      character dat*9, tim*8
      real*4 edgefrac(2),title(20)
      real*4 edgefracx,edgefracy
      equivalence (edgefrac(1),edgefracx),(edgefrac(2),edgefracy)
      integer*4, allocatable :: mapAllPc(:,:,:), nControl(:)
      integer*4, allocatable :: ixpctmp(:), iypctmp(:), izpctmp(:), negtmp(:)
      logical, allocatable :: multineg(:), multitmp(:), edgedone(:,:)
      logical active4(3,2)
      logical anypixels,inframe,dofast,anyneg,anylinesout,xinlong,testMode
      logical shifteach,docross,fromedge,exist,xcreadin,xclegacy,outputpl
      logical xcorrDebug,verySloppy,useEdges,edgesIncomplete,adjustOrigin, useFill
      logical edgesSeparated,fromCorrOnly, samePieces, noFFTsizes, yChunks, doWarp
      real*4, allocatable :: dxgridmean(:,:),dygridmean(:,:)
      real*4, allocatable :: edgedispx(:,:),edgedispy(:,:)
      real*4 aftermean(2),aftermax(2)
      character*6 edgexcorrtext(2)/'xcorr:','edges:'/
      real*4 fastf(2,3),hit(2,3),fstmp(2,3)
      real*4, allocatable :: h(:,:,:), gl(:,:,:)
c       the former rotrans structures
      real*4 hcum(6,limneg),hadj(6,limneg),r(6,limneg,2),hfram(6),rnet(6)
      integer*4 modepow(0:15)/8,15,8,0,0,0,16,0,0,9,10,11,12,13,14,15/
      integer*4  minxwant, maxxwant, modeParallel, nzAllWant, numOut, numChunks
      integer*4  minywant, maxywant, numExtra, nxybox(2), nExtra(2)
c       
      character*320 boundfile
      integer*4 inputBinning,idfNx, idfNy, idfBinning
      real*4 pixelIdf, binRatio
c       
      integer*4 modein,nlistz,minzpc,maxzpc,nsect,nxtotpix,nytotpix
      integer*4 modeout,iffloat,i,minxoverlap
      integer*4 minyoverlap,ntrial,ifsloppy,ioptabs,nxmissing,nymissing
      integer*4 nxtotwant,nytotwant,newxpieces,newypieces,ifoldedge
      integer*4 newxtotpix,newytotpix,newxframe,newyframe,newminxpiece
      integer*4 newminypiece,ifwant,nglist,ierr,nxfrmpneg,nyfrmpneg
      real*4 dmin,dmax,outmin,outmax,definmin,pixelTot, fillVal
      real*4 definmax,curinmin,curinmax,pixscale,pixadd
      real*8 tsum,cursum,grandsum,rnsum
      integer*4 ixfrm,iyfrm,ipc,nshort,nlong,ishort,ilong,indbray
      integer*4 newuse,newpcxll,newpcyll,nxfast,nyfast,iyfast,ixfast
      integer*4 indylo,indyhi,nlinesout,indxlo,indxhi,inonepiece,ioptneg
      integer*4 indx,indy,ixneg,iyneg,nxneg,iz,ineg,listfirst,ipchi
      integer*4 ix,iy,ilineout,ifill,nalong,ncross,ned,ixy,lenrec,j
      real*4 dminout,dmaxout,tmean,sdcrit,devcrit,gridScale
      integer*4 nzwant,newxoverlap,newyoverlap,izsect,iwant,ifdiddle
      integer*4 ixout,iyout,ifrevise,norder,iedgedir,iyx,imem,jedge,numneg
      integer*4 iwhich,neglo,negup,indlo,indup,joint,ied,iedge,ipclo,ipcup
      integer*4 maxswing,ishift,nshiftneg,inde,nbestedge,indbest
      real*4 erradj,errlim,dxsum,dysum,sumx,sumy,xdisp,ydisp
      real*4 beforemean,beforemax, warpScale, warpXoffset, warpYoffset
      integer*4 indgl,ilis,niter,linebase,nedgesum,indedg,indlower
      real*4 x1,y1,w1,w2,c11,c12,c21
      real*4 xgconst,ygconst,c22,denom,fb11,fb12,fb21,fb22,x1last,y1last
      integer*4 indpidef,indpl,indpu,ipiece1,ipiece2,iter,jndp1,jndp2,jndp3
      real*4 dx2,dy2,x2,y2,pixval,w3,wmax,f2b11,f2b12,f2b21,f2b22,val
      integer*4 ind12edg,ind13edg,icortyp,ind23edg,ind14edg,ind43edg
      real*4 f3b11,f3b12,f3b21,f3b22,dden,dden12,dden13,dden23,x3,y3,x3t,y3t
      real*4 dy3,dx3,bx,by,emin,w4,f4b11,f4b12,f4b21,f4b22,dden14,dden43
      integer*4 ipiece3,ipiece4,nxgr,nygr,indbray1,indbray2,jndp4
      real*4 x4,y4,dx4,dy4,xg,yg, delDmagPerUm, delRotPerUm, delIndent(2)
      real*4 dmagnew,rotnew,tiltOffset,edstart,edend,bwof, ecdBin, warpPixel,xtmp
      integer*4 iBinning, nyWrite, indentXC,numZero, mostNeeded
      integer*4 lineOffset, ixOffset, iyOffset, linesBuffered, iBufferBase
      integer*4 maxXcorrBinning, nxyXcorrTarget, numSkip, lineStart, lineEnd
      integer*4 nxyPadded, nxyBoxed, ifUseAdjusted, iyOutOffset, ifEdgeFuncOnly
      integer*4 ipfirst(4), numfirst, ixyFuncStart, ixyFuncEnd, nlinesWrite
      integer*4 iedgeDelX, iedgeDelY, ixUnaliStart, iyUnaliStart
      integer*4 iwarpNx, iwarpNy, indWarpFile
      integer*4 imodBackupFile, parWrtInitialize, getWarpGrid, readCheckWarpFile
      integer*4 getGridParameters, findMaxGridSize, getSizeAdjustedGrid,getLinearTransform
      integer*4 setCurrentWarpFile
      real*8 walltime, wallstart, fastcum, slowcum
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetBoolean, PipGetThreeFloats
      integer*4 PipGetString,PipGetFloat, PipGetIntegerArray
      integer*4 PipGetTwoIntegers, PipGetTwoFloats,PipGetLogical
      integer*4 PipGetInOutFile,PipNumberOfEntries
c       
c       fallbacks from ../../manpages/autodoc2man -2 2 blendmont
      integer numOptions
      parameter (numOptions = 71)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'imin:ImageInputFile:FN:@plin:PieceListInput:FN:@imout:ImageOutputFile:FN:@'//
     &    'plout:PieceListOutput:FN:@rootname:RootNameForEdges:CH:@'//
     &    'oldedge:OldEdgeFunctions:B:@perneg:FramesPerNegativeXandY:IP:@'//
     &    'missing:MissingFromFirstNegativeXandY:IP:@mode:ModeToOutput:I:@'//
     &    'float:FloatToRange:B:@fill:FillValue:F:@xform:TransformFile:FN:@'//
     &    'center:TransformCenterXandY:FP:@unaligned:UnalignedStartingXandY:IP:@'//
     &    'order:InterpolationOrder:I:@sections:SectionsToDo:LI:@'//
     &    'xminmax:StartingAndEndingX:IP:@yminmax:StartingAndEndingY:IP:@'//
     &    'nofft:NoResizeForFFT:B:@origin:AdjustOrigin:B:@bin:BinByFactor:I:@'//
     &    'maxsize:MaximumNewSizeXandY:IP:@minoverlap:MinimumOverlapXandY:IP:@'//
     &    'distort:DistortionField:FN:@imagebinned:ImagesAreBinned:I:@'//
     &    'gradient:GradientFile:FN:@adjusted:AdjustedFocus:B:@'//
     &    'addgrad:AddToGradient:FP:@tiltfile:TiltFile:FN:@offset:OffsetTilts:F:@'//
     &    'geometry:TiltGeometry:FT:@justUndistort:JustUndistort:B:@test:TestMode:B:@'//
     &    'sloppy:SloppyMontage:B:@very:VerySloppyMontage:B:@shift:ShiftPieces:B:@'//
     &    'edge:ShiftFromEdges:B:@xcorr:ShiftFromXcorrs:B:@readxcorr:ReadInXcorrs:B:@'//
     &    'ecdbin:BinningForEdgeShifts:F:@overlap:OverlapForEdgeShifts:IP:@'//
     &    'skip:SkipEdgeModelFile:FN:@nonzero:NonzeroSkippedEdgeUse:I:@'//
     &    'robust:RobustFitCriterion:F:@width:BlendingWidthXandY:IP:@'//
     &    'boxsize:BoxSizeShortAndLong:IP:@grid:GridSpacingShortAndLong:IP:@'//
     &    'indents:IndentShortAndLong:IP:@goodedge:GoodEdgeLowAndHighZ:IP:@'//
     &    'onegood:OneGoodEdgeLimits:IAM:@exclude:ExcludeFillFromEdges:B:@'//
     &    'unsmooth:UnsmoothedPatchFile:FN:@smooth:SmoothedPatchFile:FN:@'//
     &    'parallel:ParallelMode:IP:@subset:SubsetToDo:LI:@lines:LineSubsetToDo:IP:@'//
     &    'boundary:BoundaryInfoFile:FN:@functions:EdgeFunctionsOnly:I:@'//
     &    'aspect:AspectRatioForXcorr:F:@pad:PadFraction:F:@extra:ExtraXcorrWidth:F:@'//
     &    'numpeaks:NumberOfXcorrPeaks:I:@radius1:FilterRadius1:F:@'//
     &    'radius2:FilterRadius2:F:@sigma1:FilterSigma1:F:@sigma2:FilterSigma2:F:@'//
     &    'treat:TreatFillForXcorr:I:@xcdbg:XcorrDebug:B:@taper:TaperFraction:F:@'//
     &    'param:ParameterFile:PF:@help:usage:B:'
c       
c       initialization of many things
c       
      ixdebug = -12000
      iydebug = -12000
      inpiece(0) = 0
      iunedge(1)=7
      iunedge(2)=8
      indent(1)=3                               !minimum indent short & long
      indent(2)=3
      intgrid(1)=6                              !grid interval short & long
      intgrid(2)=6
      iboxsiz(1)=10                             !box size short & long
      iboxsiz(2)=15
      iffloat = 0
      ifsloppy = 0
      ioptabs = 0
      nxmissing = 0
      nymissing = 0
      ifoldedge = 0
      shifteach = .false.
      xclegacy = .false.
      fromedge = .false.
      xcreadin = .false.
      doMagGrad = .false.
      undistort = .false.
      focusAdjusted = .false.
      inputBinning = 1
      interpOrder = 2
      testMode = .false.
      undistortOnly = .false.
      limitData = .false.
      adjustOrigin = .false.
      noFFTsizes = .false.
      yChunks = .false.
      sameEdgeShifts = .false.
      iBinning = 1
      numAngles = 0
      numUseEdge = 0
      izUseDefLow = -1
      izUseDefHigh = -1
      modeParallel = 0
      outFile = ' '
      numXcorrPeaks = 1
      numSkip = 0
      ifUseAdjusted = 0
      iyOutOffset = 0
      ifEdgeFuncOnly = 0
      edgename2 = ' '
      boundFile = ' '
      izUnsmoothedPatch = -1
      izsmoothedPatch = -1
      robustCrit = 0.
      ecdBin = 1.
c       
c       Xcorr parameters
c       11/5/05: increased taper fraction 0.05->0.1 to protect against
c       edge effects with default filter
c       
      xcorrDebug = .false.
      ifDumpXY(1) = -1
      ifDumpXY(2) = -1
      ifillTreatment = 1
      aspectmax=2.0                             !maximum aspect ratio of block
      padFrac = 0.45
      extraWidth = 0.
      radius1 = 0.
      radius2 = 0.35
      sigma1 = 0.05
      sigma2 = 0.05
      maxXcorrBinning = 3
      nxyXcorrTarget = 1024
      verySloppy = .false.
      lmField = 1
      maxFields = 1
      useFill = .false.
      memLim = 256
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'blendmont',
     &    'ERROR: BLENDMONT - ', .true., 0, 0, 0, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0
c
      if (PipGetInOutFile('ImageInputFile', numNonOptArg + 1,
     &    'Input image file', filnam) .ne. 0) call exitError(
     &    'NO INPUT IMAGE FILE SPECIFIED')
      call imopen(1,filnam,'ro')
      call irdhdr(1,nxyzin,mxyzin,modein,dmin,dmax,dmean)
      call irtdel(1,delta)
      if (pipinput) ierr = PipGetInteger('EdgeFunctionsOnly', ifEdgeFuncOnly)
      ifEdgeFuncOnly = max(0, min(3, ifEdgeFuncOnly))
      if (ifEdgeFuncOnly .eq. 1 .or. ifEdgeFuncOnly .eq. 2) then
        ixyFuncStart = ifEdgeFuncOnly
        ixyFuncEnd = ifEdgeFuncOnly
      else
        ixyFuncStart = 1
        ixyFuncEnd = 2
      endif
c       
      if (PipGetInOutFile('ImageOutputFile', numNonOptArg + 1,
     &    'Output image file', outfile) .ne. 0 .and. ifEdgeFuncOnly .eq. 0)
     &    call exitError('NO OUTPUT IMAGE FILE SPECIFIED')
c       
      modeout=modein
      if (pipinput) then
        ierr = PipGetInteger('ModeToOutput', modeout)
      else
        write(*,'(1x,a,i2,a,$)')'Mode for output file [/ for',modeout, ']: '
        read(5,*)modeout
      endif
      if(modeout.lt.0.or.modeout.gt.15.or.
     &    (modeout.ge.3.and.modeout.le.8.and.modeout.ne.6))call exitError(
     &    'BAD MODE VALUE')
c       
c       set up output range, and default input range and minimum.  If real
c       input, use actual input range and min; otherwise use theoretical
c       range for the input mode.  This will preserve data values if no float
c       
      outmin=0.
      outmax=2**modepow(modeout)-1.
      if (modeout.eq.1) outmin = -outmax
      if(modein.eq.2)then
        definmax=dmax
        definmin=dmin
        if(modeout.eq.2)then
          outmin=dmin
          outmax=dmax
        endif
      else
        definmax=2**modepow(modein)-1.
        definmin=0.
        if (modein.eq.1) definmin = -definmax
      endif
c       
      filnam = ' '
      if (pipinput) then
        ierr = PipGetBoolean('FloatToRange', iffloat)
        ierr = PipGetString('TransformFile', filnam)
        ierr = PipGetLogical('JustUndistort', undistortOnly)
      else
        write(*,'(1x,a,$)')
     &      '1 to float each section to maximum range, 0 not to: '
        read(5,*)iffloat
c         
        write(*,'(1x,a,$)')
     &      'File of g transforms to apply (Return if none): '
        read(5,'(a)')filnam
      endif
c       
c       Preserve legacy behavior of floating to positive range for mode 1
      if (iffloat .ne. 0 .and. modein .eq. 1 .and. modeout .eq. 1) then
        outmin = 0.
        definmin = 0.
      endif

      limsect = 100000
      allocate(ixpctmp(liminit), iypctmp(liminit), izpctmp(liminit),
     &    negtmp(liminit), multitmp(limsect), stat = ierr)
      if (ierr .ne. 0) call exitError('ALLOCATING INITIAL ARRAYS')
c       
      call read_list(ixpctmp,iypctmp,izpctmp,negtmp,
     &    multitmp,npclist,minzpc,maxzpc,anyneg,pipinput)
      limnpc = npclist + 10
      allocate(ixpclist(limnpc), iypclist(limnpc), izpclist(limnpc),
     &    neglist(limnpc), limDataInd(limnpc), iedgelower(limnpc,2),
     &    iedgeupper(limnpc,2), hinv(2,3,limnpc), memIndex(limnpc),
     &    htmp(2,3,limnpc), h(2,3,limnpc), indvar(limnpc), stat = ierr)
      if (ierr .ne. 0) call exitError('ALLOCATING ARRAYS PER PIECE')
      ixpclist(1:npclist) = ixpctmp(1:npclist)
      iypclist(1:npclist) = iypctmp(1:npclist)
      izpclist(1:npclist) = izpctmp(1:npclist)
      neglist(1:npclist) = negtmp(1:npclist)
c
      nsect=maxzpc+1-minzpc
      limsect = nsect + 1
      allocate(tiltAngles(limsect), dmagPerUm(limsect), rotPerUm(limsect),
     &    listz(limsect),izwant(limsect), izAllWant(limsect), gl(2,3,limsect),
     &    multineg(limsect), izMemList(memLim), lastUsed(memLim), stat = ierr)
      if (ierr .ne. 0) call exitError('ALLOCATING ARRAYS PER PIECE')
      multineg(1:nsect) = multitmp(1:nsect)
c
      call fill_listz(izpclist,npclist,listz,nlistz)
c
      dogxforms=.false.
      doWarp = .false.
      if(filnam.ne.' ' .and. .not.undistortOnly)then
        dogxforms=.true.
c         
c         Open as warping file if possible
        indWarpFile = readCheckWarpFile(filnam, 0, 1, iwarpNx, iwarpNy, nglist, ix,
     &      warpPixel, iy, edgenam)
        if (indWarpFile .lt. -1) call exitError(edgenam)
        doWarp = indWarpFile .ge. 0
        if (doWarp) then
          if (nglist .gt. limsect)  call exitError(
     &        'TOO MANY SECTIONS IN WARPING FILE FOR TRANSFORM ARRAY')
          warpScale = warpPixel / delta(1)
          do i = 1, nglist
            if (getLinearTransform(i, gl(1,1,i)) .ne. 0)
     &          call exitError('GETTING LINEAR TRANSFORM FROM WARP FILE')
            gl(1,3,i) = gl(1,3,i) * warpScale
            gl(2,3,i) = gl(2,3,i) * warpScale
          enddo
        else
c           
c           Get regular transforms
          call dopen(3,filnam,'ro','f')
          call xfrdall2(3,gl,nglist,limsect,ierr)
          close(3)
c         
c           It is OK if ierr=-1 : more transforms than sections
          if (ierr .gt. 0) call exitError ('READING TRANSFORMS')
        endif
      endif


      if(dogxforms)then
        if (nlistz.gt.nglist) call exitError('MORE SECTIONS THAN G TRANSFORMS')

        skipxforms=.false.
        if(nlistz.lt.nsect)then
          if(nglist.eq.nlistz)then
            print *,'Looks like there are transforms only for '//
     &          'the sections that exist in file'
          elseif(nglist.eq.nsect)then
            print *,'There seem to be transforms for each Z value,'
     &          //' including ones missing from file'
            SKIPXFORMS=.TRUE.
          else
            call exitError('CANNOT TELL HOW TRANSFORMS MATCH UP TO '//
     &          'SECTIONS, BECAUSE OF MISSING SECTIONS')
          endif
        endif
      endif
c       
c       now check lists and get basic properties of overlap etc
c       
      call checklist(ixpclist,npclist,1,nxin,minxpiece,nxpieces,
     &    nxoverlap)
      call checklist(iypclist,npclist,1,nyin,minypiece,nypieces,
     &    nyoverlap)
      if(nxpieces.le.0. or. nypieces.le.0)call exitError
     &    ('CHECKLIST REPORTED A PROBLEM WITH THE PIECE LIST IN ONE DIRECTION')
c       
      nxtotpix=nxpieces*(nxin-nxoverlap)+nxoverlap
      nytotpix=nypieces*(nyin-nyoverlap)+nyoverlap
      if (pipinput) print *,'Input file:'
      write(*,115)nxtotpix,'X',nxpieces,nxin,nxoverlap
      write(*,115)nytotpix,'Y',nypieces,nyin,nyoverlap
115   format(i7,' total ',a1,' pixels in',i4,' pieces of',
     &    i6, ' pixels, with overlap of',i5)
c
      limedge = limsect * max(1, nxpieces * nypieces)
      allocate(dxgridmean(limedge,2),dygridmean(limedge,2),
     &    edgedispx(limedge,2),edgedispy(limedge,2), edgedone(limedge,2),
     &    ipiecelower(limedge,2),ipieceupper(limedge,2), ibufedge(limedge,2),
     &    ifskipEdge(limedge,2), stat = ierr)
      if (ierr .ne. 0) call exitError('ALLOCATING ARRAYS PER EDGE')
c       
c       find out if global multi-neg specifications are needed or desired
c       But first deal with correlation control parameters
c       Here are the defaults for VerySloppy
c       
      if (pipinput) then
        ierr = PipGetLogical('VerySloppyMontage', verySloppy)
        if (verySloppy) then
          ifsloppy = 1
          aspectMax = 5.
          radius1 = -0.01
          extraWidth = 0.25
          numXcorrPeaks = 16
        else
          ierr = PipGetBoolean('SloppyMontage', ifsloppy)
        endif
        ierr = PipGetFloat('AspectRatio', aspectMax)
        ierr = PipGetInteger('NumberOfXcorrPeaks', numXcorrPeaks)
        numXcorrPeaks = max(1, min(limXcorrPeaks, numXcorrPeaks))
        ierr = PipGetFloat('PadFraction', padFrac)
        ierr = PipGetFloat('ExtraXcorrWidth', extraWidth)
        ierr = PipGetFloat('FilterSigma1', sigma1)
        ierr = PipGetFloat('FilterSigma2', sigma2)
        ierr = PipGetFloat('FilterRadius1', radius1)
        ierr = PipGetFloat('FilterRadius2', radius2)
        shifteach = ifsloppy.ne.0
        if (PipGetTwoIntegers('FramesPerNegativeXandY', nxfrmpneg,
     &      nyfrmpneg) .eq. 0) ioptabs = 1
        if (.not.shifteach) ierr = PipGetLogical('ShiftPieces', shifteach)
        ierr = PipGetLogical('ShiftFromEdges', fromedge)
        ierr = PipGetLogical('ShiftFromXcorrs', xclegacy)
        ierr = PipGetLogical('ReadInXcorrs', xcreadin)
        ierr = PipGetInteger('NonzeroSkippedEdgeUse', ifUseAdjusted)
        ierr = PipGetFloat('RobustFitCriterion', robustCrit)
        ierr = PipGetLogical('TestMode', testMode)
        useFill = PipGetFloat('FillValue', fillVal) .eq. 0
        ierr = PipGetTwoIntegers('GoodEdgeLowAndHighZ', izUseDefLow,
     &      izUseDefHigh)
        ierr = PipNumberOfEntries('OneGoodEdgeLimits', numUseEdge)
        do i = 1, numUseEdge
          ierr = PipGetIntegerArray('OneGoodEdgeLimits', ixpclo, 5, 15)
          ixFrmUseEdge(i) = ixpclo(1)
          iyFrmUseEdge(i) = ixpclo(2)
          ixyUseEdge(i) = ixpclo(3)
          izLowUse(i) = ixpclo(4)
          izHighUse(i) = ixpclo(5)
        enddo
        ierr = PipGetLogical('SameEdgeShifts', sameEdgeShifts)
        if ((anyneg .or. ioptabs .ne. 0) .and. (shifteach .or. xcreadin)
     &      .and. .not.undistortOnly)
     &      call exitError('YOU CANNOT USE ShiftPieces OR '//
     &      'ReadInXcorrs WITH MULTIPLE NEGATIVES')
        if (fromedge .and. xclegacy .and. .not.undistortOnly) call exitError
     &      ('YOU CANNOT USE BOTH ShiftFromEdges AND ShiftFromXcorrs')
        if ((izUseDefLow .ge. 0 .or. numUseEdge .gt. 0) .and. shifteach) then
          if (.not. sameEdgeShifts) call
     &        exitError('YOU CANNOT USE GOOD EDGE LIMITS WHEN SHIFTING PIECES')
          if (fromedge)
     &        call exitError('YOU CANNOT USE ShiftFromEdges WITH GOOD EDGE LIMITS')
          xclegacy = .true.
        endif
      else
        if(anyneg)then
          print *,'There are multi-negative specifications in list file'
          write(*,'(1x,a,$)')'1 to do initial cross-correlations '//
     &        'in overlap zones, 0 not to: '
          read(5,*)ifsloppy
        else
          print *,'Enter the negative of one of the following '//
     &        'options to do initial','cross-correlations in overlap'//
     &        ' zones in combination with the particular option'
          write(*,'(1x,a,/,a,/,a,/,a,/,a,/,a,$)')
     &        'Enter 1 to specify division'//
     &        ' into negatives to apply to all sections,',
     &        '      2 to use edge functions to find a shift for each '
     &        //'frame to align frames',
     &        '      3 to use cross-correlation only to find a shift '//
     &        'for each frame',
     &        '      4 to use only cross-correlation displacements '//
     &        'read from a file',
     &        '      5 to use best shifts from edge functions and '//
     &        'correlations',
     &        '      6 to use best shifts from edge functions and '//
     &        'displacements read from a file: '
          read(5,*)ioptneg
          ifsloppy=0
          if(ioptneg.lt.0)ifsloppy=1
          ioptabs=abs(ioptneg)
          shifteach=ioptabs.ge.2
          fromedge=ioptabs.eq.2
          xclegacy=ioptabs.eq.3.or.ioptabs.eq.4
          xcreadin=ioptabs.eq.4.or.ioptabs.eq.6
        endif
      endif
c
      if(ioptabs.eq.1)then
        if (pipinput) then
          ierr = PipGetTwoIntegers('FramesPerNegativeXandY',
     &        nxmissing, nymissing)
        else
          write(*,'(1x,a,$)')'# of frames per negative in X;'//
     &        ' # missing from left-most negative: '
          read(5,*)nxfrmpneg,nxmissing
          write(*,'(1x,a,$)')'# of frames per negative in Y;'//
     &        ' # missing from bottom-most negative: '
          read(5,*)nyfrmpneg,nymissing
        endif
        nxneg=(nxpieces+(nxfrmpneg-1) + nxmissing)/nxfrmpneg
c         
c         derive frame number of each piece and assign negative #
c         
        do ipc=1,npclist
          ixfrm=(ixpclist(ipc)-minxpiece)/(nxin-nxoverlap)
          iyfrm=(iypclist(ipc)-minypiece)/(nyin-nyoverlap)
          ixneg=(ixfrm+nxmissing)/nxfrmpneg
          iyneg=(iyfrm+nymissing)/nyfrmpneg
          neglist(ipc)=1+ixneg+iyneg*nxneg
        enddo
c         
c         now deduce true multi-neg character of each section
c         
        do iz=minzpc,maxzpc
          ineg=iz+1-minzpc
          multineg(ineg)=.false.
          listfirst=-100000
          do ipc=1,npclist
            if(izpclist(ipc).eq.iz)then
              if(listfirst.eq.-100000)listfirst=neglist(ipc)
              multineg(ineg)=
     &            multineg(ineg).or.(neglist(ipc).ne.listfirst)
            endif
          enddo
          anyneg=anyneg.or.multineg(ineg)
        enddo
c         
      endif
c       
      plOutFile = ' '
      if (pipinput) then
        ierr = PipGetString('PieceListOutput', plOutFile)
      else
        write(*,'(1x,a,$)')
     &      'Name of new piece list file (Return for none): '
        read(5,'(a)')plOutFile
      endif
      outputpl = plOutFile .ne. ' ' .and. ifEdgeFuncOnly .eq. 0
c       
c       find out center of transforms
c       
      gxcen=minxpiece+nxtotpix/2
      gycen=minypiece+nytotpix/2
      if (pipinput) then
        ierr = PipGetTwoFloats('TransformCenterXandY', gxcen, gycen)
      else
        if(dogxforms)then
          write(*,'(1x,a,/,a,f6.1,a,f6.1,a,$)')
     &        'Enter true center coordinates of the transforms,',
     &        '    or / for the default',gxcen,',',gycen,
     &        ', the center of the image area: '
          read(5,*)gxcen,gycen
        endif
      endif
c       
c       Add 0.5 to get the center of rotation to be around the center of image
      gxcen = gxcen + 0.5
      gycen = gycen + 0.5
c       
c       get list of sections desired, set up default as all sections
c       
      do i=1,nlistz
        izpctmp(i)=listz(i)
      enddo
      nzwant=nlistz
      if (pipinput) then
        if (PipGetString('SectionsToDo', filnam) .eq. 0)
     &      call parselist(filnam, izpctmp, nzwant)
      else
        print *,'Enter list of sections to be included in output '//
     &      'file (ranges ok)','   or / to include all sections'
        call rdlist(5,izpctmp,nzwant)
      endif
c       
c       copy/pack list, eliminating non-existent sections and duplicates
      ixout = 0
      do ix = 1, nzwant
        ierr =0
        do i = 1, nlistz
          if (listz(i) .eq. izpctmp(ix)) ierr = 1
        enddo
        do i = 1, ixout
          if (izwant(i) .eq. izpctmp(ix)) ierr = 0
        enddo
        if (ierr .eq. 1) then
          ixout = ixout + 1
          izwant(ixout) = izpctmp(ix)
        endif
      enddo
      nzwant  = ixout
      if (nzwant .eq. 0) call exitError('SECTION OUTPUT LIST DOES NOT '//
     &    'INCLUDE ANY ACTUAL SECTIONS')
      nzAllWant = nzwant
c       
c       Set flag for whether to write out edge displacements
c       write edge correlations if they are not being read in and if they are
c       computed in their entirety: but this needs to be modified later
      xcWriteOut = .not.testMode.and..not.xcreadin.and.
     &    ((ifsloppy.eq.1.and.shifteach) .or.
     &    (ifsloppy.eq.0.and.shifteach.and..not.fromedge))
c       
      deallocate(ixpctmp, iypctmp, izpctmp, negtmp, multitmp, stat = ierr)
c
      if (undistortOnly) then
c         
c         If undistorting, copy sizes etc.
c         
        newxframe = nxin
        newxoverlap = nxoverlap
        newxpieces = nxpieces
        newminxpiece = minxpiece
        newyframe = nyin
        newyoverlap = nyoverlap
        newypieces = nypieces
        newminypiece = minypiece
        actionStr = 'undistorted only'
      else
c         
c         Set up output size
c         
        minxoverlap=2
        minyoverlap=2
        newxframe=100000000
        newyframe=100000000
        ntrial=0
32      minxwant=minxpiece
        minywant=minypiece
        maxxwant=minxwant+nxtotpix-1
        maxywant=minywant+nytotpix-1
        if (pipinput) then
          ierr = PipGetLogical('XcorrDebug', xcorrDebug)
          ierr = PipGetLogical('AdjustOrigin', adjustOrigin)
          ierr = PipGetLogical('ExcludeFillFromEdges', limitData)
          ierr = PipGetLogical('NoResizeForFFT', noFFTsizes)
          ierr = PipGetTwoIntegers('StartingAndEndingX', minxwant, maxxwant)
          ierr = PipGetTwoIntegers('StartingAndEndingY', minywant, maxywant)
          ierr = PipGetTwoIntegers('MaximumNewSizeXandY', newxframe,
     &        newyframe)
          ierr = PipGetTwoIntegers('MinimumOverlapXandY', minxoverlap,
     &        minyoverlap)
          ierr = PipGetInteger('BinByFactor', iBinning)
          if (iBinning .lt. 1) call exitError('BINNING MUST BE POSITIVE')
          if (iBinning .gt. maxbin) call exitError('BINNING IS TOO LARGE')
          if (PipGetString('UnsmoothedPatchFile', edgenam) .eq. 0) then
            call dopen(10, edgenam, 'new', 'f')
            izUnsmoothedPatch = 0
          endif
          if (PipGetString('SmoothedPatchFile', edgenam) .eq. 0) then
            call dopen(11, edgenam, 'new', 'f')
            izSmoothedPatch = 0
          endif
        else
          write(*,'(1x,a,/,a,4i6,a,$)')'Enter Min X, Max X, Min Y, and'//
     &        ' Max Y coordinates of desired output section,'
     &        ,'    or / for whole input section [='
     &        ,minxwant,maxxwant,minywant,maxywant,']: '
          read(5,*)minxwant,maxxwant,minywant,maxywant
          write(*,'(1x,a,$)')
     &        'Maximum new X and Y frame size, minimum overlap: '
          read(5,*)newxframe,newyframe,minxoverlap,minyoverlap
        endif
        if(ntrial.le.1)then                     !on first 2 trials, enforce min
          minxoverlap=max(2,minxoverlap)        !overlap of 2 so things look
          minyoverlap=max(2,minyoverlap)        !nice in wimp.  After that, let
        endif                                   !the user have it.
        ntrial=ntrial+1
c         
c         If no resizing desired, take exactly what is requested in one frame
        if (noFFTsizes .and. maxxwant+1-minxwant .le. newxframe .and.
     &      maxywant+1-minywant .le. newyframe) then
          nxtotwant=maxxwant+1-minxwant
          newxframe = nxtotwant
          newxtotpix = nxtotwant
          newxpieces = 1
          newxoverlap = 2
          nytotwant=maxywant+1-minywant
          newyframe = nytotwant
          newytotpix = nytotwant
          newypieces = 1
          newyoverlap = 2
        else
          nxtotwant=2*((maxxwant+2-minxwant)/2)
          nytotwant=2*((maxywant+2-minywant)/2)
          call setoverlap(nxtotwant,minxoverlap,noFFTsizes,newxframe,2,newxpieces,
     &        newxoverlap,newxtotpix)
          call setoverlap(nytotwant,minyoverlap,noFFTsizes,newyframe,2,newypieces,
     &        newyoverlap,newytotpix)
        endif
c         
        if (.not.outputpl .and. ifEdgeFuncOnly .eq. 0 .and.
     &      (newxpieces.gt.1 .or. newypieces.gt.1))
     &      call exitError('YOU MUST SPECIFY AN OUTPUT PIECE LIST FILE'
     &      //' TO HAVE MORE THAN ONE OUTPUT FRAME')
        if (iBinning .gt. 1 .and. (newxpieces.gt.1 .or. newypieces.gt.1))
     &      call exitError('WITH BINNING, OUTPUT MUST BE INTO A '//
     &      'SINGLE FRAME')
        if (pipinput) print *,'Output file:'
        write(*,115)newxtotpix,'X',newxpieces,newxframe,newxoverlap
        write(*,115)newytotpix,'Y',newypieces,newyframe,newyoverlap
c         
        if (.not.pipinput)then
          write(*,'(1x,a,$)')'1 to revise frame size/overlap: '
          read(5,*)ifrevise
          if(ifrevise.ne.0)go to 32       
        endif
c         
        newminxpiece=minxwant-(newxtotpix-nxtotwant)/2
        newminypiece=minywant-(newytotpix-nytotwant)/2
        actionStr = 'blended and recut'
      endif
      write(*,'(a,2i7)')'Starting coordinates of output in X and Y =',newminxpiece,
     &    newminypiece
c       
      nxout=newxframe
      nyout=newyframe
      dminout=1.e10
      dmaxout=-1.e10
      grandsum=0.
      call getBinnedSize(nxout, iBinning, nxbin, ixOffset)
      call getBinnedSize(nyout, iBinning, nybin, iyOffset)
      nzbin = 0
      nlinesWrite = nybin
      hxcen=nxin/2.
      hycen=nyin/2.
c       
c       get edge indexes for pieces and piece indexes for edges
c       first build a map in the array of all pieces present
c       
      allocate(mapAllPc(nxpieces, nypieces, nsect), stat = ierr)
      if (ierr .ne. 0) call exitError('ALLOCATING ARRAY TO MAP PIECES')
      do iz=1,nsect
        do iy=1,nypieces
          do ix=1,nxpieces
            mapAllPc(ix,iy,iz)=0
          enddo
        enddo
      enddo
c       
      do ipc=1,npclist
        mapAllPc(1+(ixpclist(ipc)-minxpiece)/(nxin-nxoverlap),
     &      1+(iypclist(ipc)-minypiece)/(nyin-nyoverlap),
     &      izpclist(ipc)+1-minzpc)=ipc
        do i = 1, 2
          iedgelower(ipc,i)=0
          iedgeupper(ipc,i)=0
          limDataInd(ipc) = -1
        enddo
      enddo
c       
c       look at all the edges in turn, add to list if pieces on both sides
c       
      nalong=nypieces
      ncross=nxpieces
      do ixy=1,2
        ned=0
        do iz=1,nsect
          do iy=1,nalong
            do ix=2,ncross
              if(ixy.eq.1)then
                ipclo=mapAllPc(ix-1,iy,iz)
                ipchi=mapAllPc(ix,iy,iz)
              else
                ipclo=mapAllPc(iy,ix-1,iz)
                ipchi=mapAllPc(iy,ix,iz)
              endif
              if(ipclo.ne.0 .and. ipchi.ne.0)then 
                ned=ned+1
                ipiecelower(ned,ixy)=ipclo
                ipieceupper(ned,ixy)=ipchi
                iedgelower(ipchi,ixy)=ned
                iedgeupper(ipclo,ixy)=ned
                edgedone(ned,ixy)=.false.
                ifSkipEdge(ned,ixy) = 0
                edgedispx(ned,ixy) = 0.
                edgedispy(ned,ixy) = 0.
              endif
            enddo
          enddo
        enddo
        nedge(ixy)=ned
        nalong=nxpieces
        ncross=nypieces
      enddo
c       
c       Allocate data depending on number of pieces (limvar)
      limvar = nxpieces * nypieces
      if (.not. undistortOnly) then
        allocate(bb(2, limvar), ivarpc(limvar), iallVarpc(limvar),
     &      ivarGroup(limvar), listCheck(limvar), fpsWork(20*limvar + limvar / 4 + 4),
     &      dxyvar(limvar, 2), rowTmp(limvar*2), stat = ierr)
        if (ierr .ne. 0) call exitError('ALLOCATING ARRAYS FOR FINDING SHIFTS')
c
        if (testMode) then
          i = 2 * limvar
          allocate(gradXcenLo(i),gradXcenHi(i), gradYcenLo(i),
     &        gradYcenHi(i), overXcenLo(i),overXcenHi(i),
     &        overYcenLo(i),overYcenHi(i), dxedge(limedge,2),
     &        dyedge(limedge,2),dxadj(limedge,2),dyadj(limedge,2), stat = ierr)
          if (ierr .ne. 0) call exitError('ALLOCATING ARRAYS FOR TEST MODE')
        endif
      endif
c       
c       Determine size needed for output and correlation arrays
      maxlinelength = newxframe + 32
      maxbsiz=(ifastsiz+maxbin)*maxlinelength
c       
c       Find binning up to limit that will get padded size down to target
      do nbinXcorr = 1, maxXcorrBinning
        nxyPadded = 0
        nxyBoxed = 0
        do ixy = 1, 2
          call xcorrSizes(ixy, nbinXcorr, 0, i, nxybox, nExtra, ix, iy,
     &        iz, iwant, ierr)
          nxyPadded = max(nxyPadded, (ix + 8) * (iy + 8), (iz + 8) * (iwant+8))
          nxyBoxed = max(nxyBoxed, (nxybox(1) + 4) * (nxybox(2) + 4))
        enddo
        if (nxyBoxed .lt. nxyXcorrTarget**2 .or.
     &      nbinXcorr .eq. maxXcorrBinning) exit
      enddo
      idimc = nxyPadded
      maxbsiz = max(maxbsiz, nxyBoxed * nbinXcorr**2)
c      print *,'nbinxcorr, dims',nbinXcorr,idimc,maxbsiz
      allocate(binline(maxlinelength), brray(maxbsiz), stat = ierr)
      if (ierr .ne. 0) call exitError('ALLOCATING OUTPUT LINE ARRAYS')
c       
c       get edge file name root, parameters if doing a new one
c       
      if (pipinput .and. .not.undistortOnly) then
        ierr = PipGetBoolean('OldEdgeFunctions', ifoldedge)
        if (PipGetString('RootNameForEdges', rootname) .ne. 0) call
     &      exitError('NO ROOT NAME FOR EDGE FUNCTIONS SPECIFIED')
      else if (.not.undistortOnly) then
        write(*,'(1x,a,$)')'0 for new edge files, 1 to read old ones: '
        read(5,*)ifoldedge
        write(*,'(1x,a,$)')'Root file name for edge function files: '
        read(5,'(a)')rootname
      endif
      if (ifoldEdge .ne. 0 .and. ifEdgeFuncOnly .ne. 0) call exitError('YOU '//
     &    'CANNOT USE OLD EDGE FUNCTIONS WHEN JUST COMPUTING EDGE FUNCTIONS')
c       
c       Find out about parallel mode
      if (pipinput) then
        ierr = PipGetTwoIntegers('ParallelMode', modeParallel, ix)
        if (modeParallel .ne. 0) then
          yChunks = ix .ne. 0
          if (outputpl .or. newxpieces.gt.1 .or. newypieces.gt.1)
     &        call exitError('PARALLEL MODE REQUIRES OUTPUT IN ONE PIECE '//
     &        'WITH NO PIECE LIST')
          if (undistortOnly .or. testMode .or. xcorrDebug .or.
     &        ifEdgeFuncOnly .ne. 0) call exitError('NO PARALLEL MODE ALLOWED'
     &        //'WITH UndistortOnly, EdgeFunctionsOnly, TestMode, OR '//
     &        'XcorrDebug')
          if (modeParallel .lt. 0 .and. ifoldedge .eq. 0) call exitError(
     &        'PARALLEL MODE ALLOWED ONLY WHEN USING OLD EDGE FUNCTIONS')
          if (modeParallel .lt. 0 .and. xcWriteOut) call exitError(
     &        'PARALLEL MODE NOT ALLOWED IF '//
     &        'WRITING EDGE CORRELATION DISPLACEMENTS')
           if (modeParallel .gt. 0) then
c             
c             Figure out lists to output
            if (modeParallel .le. 1) call exitError('TARGET NUMBER OF '//
     &          'CHUNKS SHOULD BE AT LEAST 2')
c             
c             Set up for breaking into Z chunks and modify for chunks in Y
            ix = nzwant
            ixout = 1
            if (yChunks) then
              ix = newytotpix / iBinning
              ixout = newminypiece 
            endif
            numChunks = min(modeParallel, ix)
            numExtra = mod(ix, numChunks)
            do i = 1, numChunks
              numOut = ix / numChunks
              if (i .le. numExtra) numOut = numOut + 1
              if (yChunks) then
                iy = ixout + iBinning * numout - 1
                if (i .eq. numChunks) iy = newminypiece + newytotpix - 1
                write(*,'(a,2i9)')'LineSubsetToDo ',ixout,iy
                indxlo = (ixout - newminypiece) / iBinning
                indxhi = (iy - newminypiece) / iBinning
                ixout = ixout + iBinning * numOut 
              else
                write(*,'(a,$)')'SubsetToDo '
                call wrlist(izwant(ixout), numOut)
                indxlo = ixout - 1
                indxhi = ixout + numOut - 2
                ixout = ixout + numOut
              endif
              if (i .eq. 1) indxlo = -1
              if (i .eq. numChunks) indxhi = -1
              write(*,'(a,2i9)')'ChunkBoundary  ', indxlo, indxhi
            enddo
c
c             Output the image size for convenient parsing
            write(*,'(a,2i9)')'Output image size:',nxbin,nybin
c             
c             Or get subset list
          elseif (modeParallel .lt. -1) then
            if (yChunks) then
              if (PipGetTwoIntegers('LineSubsetToDo', lineStart, lineEnd) .ne.
     &            0) call exitError('YOU MUST ENTER LineSubsetToDo '//
     &            'WITH THIS PARALLEL MODE')
              if (lineStart .lt. newminypiece .or. lineEnd .gt.
     &            newminypiece + newytotpix - 1) call exitError(
     &            'THE STARTING OR ENDING LINE OF THE SUBSET IS OUT OF RANGE')
              if (modeParallel .lt. -2) call exitError('ONLY DIRECT WRITING'//
     &            'TO A SINGLE FILE IS ALLOWED WITH SUBSETS OF LINES')
              if (mod(lineStart - newminypiece, iBinning) .ne. 0 .or.
     &            (mod(lineEnd + 1 - lineStart, iBinning) .ne. 0 .and.
     &            lineEnd .ne. newminypiece + newytotpix - 1)) call exitError
     &            ('STARTING LINE AND NUMBER OF LINES MUST BE A MULTIPLE '//
     &            'OF THE BINNING')
              iyOutOffset = (lineStart - newminypiece) / iBinning
              nlinesWrite = (lineEnd + 1 - lineStart) / iBinning
            else
              if (PipGetString('SubsetToDo', filnam) .ne. 0) call exitError(
     &            'YOU MUST ENTER SubsetToDo WITH THIS PARALLEL MODE')
c             
c               For direct writing, save full want list as all want list
              if (modeParallel .eq. -2) izAllWant(1:nzwant) = izwant(1:nzwant)
c               
c               In either case, replace the actual want list
              call parselist(filnam, izwant, nzwant)
            endif
            ierr = PipGetString('BoundaryInfoFile', boundfile)
          else
c             
c             Open output file: set Z size for the file (nzbin is kept as a
c             running count of sections output otherwise)
              nzbin = nzwant
          endif
        endif
      endif
c       
c       Initialize parallel writing if there is a boundary file
      ierr = parWrtInitialize(boundFile, 5, nxbin, nybin, nzAllWant)
      if (ierr. ne. 0) then
        write(*,'(/,a,i3)')'ERROR: BLENDMONT - INITIALIZING PARALLEL WRITE '//
     &      'BOUNDARY FILE, ERROR',ierr
        call exit(1)
      endif
c
      edgesIncomplete = .false.
      if(ifoldedge.ne.0 .and. .not.undistortOnly)then
c         
c         for old files, open, get edge count and # of grids in X and Y
c         
        do ixy = 1, 2
          lenrec = 24 / nbytes_recl_item
          edgenam = trim(rootname)//edgeext(ixy)
          open(iunedge(ixy),file=edgenam,status='old',
     &        form='unformatted',access='direct', recl=lenrec, err=53)
          read(iunedge(ixy),rec=1)(nedgetmp(i,ixy), i = 1, 5)
          close(iunedge(ixy))
        enddo
c         
c         make sure edge counts match and intgrid was consistent
c         
        if(nedgetmp(1,1).ne.nedge(1).or.nedgetmp(1,2).ne.nedge(2))then
          call convert_longs(nedgetmp,10)
          if(nedgetmp(1,1).ne.nedge(1).or.nedgetmp(1,2).ne.nedge(2))
     &        call exitError('WRONG # OF EDGES IN EDGE FUNCTION FILE')
          needbyteswap=1
        endif
        if (nedgetmp(4,1) .ne. nedgetmp(5,2) .or.
     &      nedgetmp(5,1) .ne. nedgetmp(4,2)) call exitError(
     &      'INCONSISTENT GRID SPACINGS BETWEEN EDGE FUNCTION FILES')
c         
c         set up record size and reopen with right record size
c         
        do ixy=1,2
          nxgrid(ixy) = nedgetmp(2,ixy)
          nygrid(ixy) = nedgetmp(3,ixy)
          lenrec=4*max(6,3*(nxgrid(ixy)*nygrid(ixy)+2))/nbytes_recl_item
          edgenam = trim(rootname)//edgeext(ixy)
          open(iunedge(ixy),file=edgenam,status='old',
     &        form='unformatted',access='direct', recl=lenrec, err=53)
        enddo
c         
c         Read edge headers to set up edgedone array
        numZero = 0
        do ixy = 1, 2
          do iedge = 1, nedge(ixy)
            read(iunedge(ixy),rec=1+iedge,err=52)ixpclo(1),ixpclo(2)
            if (needbyteswap .ne. 0) call convert_longs(ixpclo, 2)
c            print *,'edge',ixy,iedge,ixpclo(1),ixpclo(2)
            if (ixpclo(1) .ge. 0 .and. ixpclo(2) .ge. 0)
     &          edgedone(iedge,ixy) = .true.
          enddo 
          numZero = numZero + 1
52        continue
        enddo
c         
c         If either set of edge functions is incomplete, set flag; only set
c         the interval now if edges are complete
        if (numZero .lt. 2) then
          edgesIncomplete = .true.
        else
          intgrid(1) = nedgetmp(4,1)
          intgrid(2) = nedgetmp(5,1)
        endif
        go to 54
c         
c         8/8/03: if there is an error opening old files, just build new ones
c         This is to allow command files to say use old functions even if
c         a previous command file might not get run
c         
53      ifoldedge = 0
        if (ixy.eq.2) close(iunedge(1))
        write(*,'(/,a)')'WARNING: BLENDMONT - ERROR OPENING OLD EDGE '//
     &      'FUNCTION FILE; NEW EDGE FUNCTIONS WILL BE COMPUTED'
        do ixy = 1, 2
          do iedge = 1, nedge(ixy)
            edgedone(iedge,ixy) = .false.
          enddo
        enddo
      endif

54    if (modeParallel .lt. 0 .and. (ifoldedge .eq. 0 .or. edgesIncomplete))
     &    call exitError('PARALLEL MODE NOT ALLOWED IF EDGE FUNCTIONS '//
     &    'ARE INCOMPLETE OR NONEXISTENT')
      if((ifoldedge.eq.0 .or. edgesIncomplete) .and. .not.undistortOnly)then
        ifdiddle=0
c         write(*,'(1x,a,$)')
c         &           '1 to diddle with edge function parameters: '
c         read(5,*)ifdiddle
        sdcrit=2.
        devcrit=2.
        norder=2
c         
c         make smart defaults for grid parameters
c         
        gridScale = min(8., max(1., max(nxin, nyin) / 512.))
        do ixy = 1, 2
          iboxsiz(ixy) = nint(iboxsiz(ixy) * gridScale)
          indent(ixy) = nint(indent(ixy) * gridScale)
          intgrid(ixy) = nint(intgrid(ixy) * gridScale)
          lastWritten(ixy) = 0
        enddo
c         
        if (pipinput) then
          ierr = PipGetIntegerArray('BoxSizeShortAndLong', iboxsiz, 2, 2)
          ierr = PipGetIntegerArray('IndentShortAndLong', indent, 2, 2)
          ierr = PipGetIntegerArray('GridSpacingShortAndLong', intgrid, 2, 2)
        endif
c         print *,'box size', (iboxsiz(i), i=1,2),'  grid',(intgrid(i), i=1,2)

        if(ifdiddle.ne.0)then
          write(*,'(1x,a,$)')'criterion # of sds away from mean'//
     &        ' of sd and deviation: '
          read(5,*)sdcrit,devcrit
          write(*,'(1x,a,$)') '# grid positions in short and long'//
     &        ' directions to include in regression: '
          read(5,*)(nfit(i),i=1,2)
          write(*,'(1x,a,$)')'order of polynomial: '
          read(5,*)norder
          write(*,'(1x,a,$)') 'intervals at which to do regression'
     &        //' in short and long directions: '
          read(5,*)(nskip(i),i=1,2)
        endif
c         
c         get edge function characteristics for x and y edges
c         
c        print *,(iboxsiz(I),indent(i),intgrid(i),i=1,2)
        do ixy=1,2
          call setgridchars(nxyzin,noverlap,iboxsiz,indent,intgrid,
     &        ixy,0,0,0,0,nxgrid(ixy),nygrid(ixy),igridstr,iofset)
c          print *,ixy,nxgrid(ixy),nygrid(ixy),nedgetmp(2,ixy),nedgetmp(3,ixy)
          if (edgesIncomplete .and. (nxgrid(ixy) .ne. nedgetmp(2,ixy) .or.
     &        nygrid(ixy) .ne. nedgetmp(3,ixy))) call exitError('CANNOT USE'//
     &        ' INCOMPLETE OLD EDGE FUNCTION FILE WITH CURRENT PARAMETERS')
        enddo
      endif
      if(ifoldedge.eq.0  .and. .not.undistortOnly .and. modeParallel.le.0)then
        needbyteswap = 0
c           open file, write header record
c           set record length to total bytes divided by system-dependent
c           number of bytes per item
c           
        do ixy = ixyFuncStart, ixyFuncEnd
          lenrec=4*max(6,3*(nxgrid(ixy)*nygrid(ixy)+2))/nbytes_recl_item
          edgenam = trim(rootname)//edgeext(ixy)
          ierr = imodBackupFile(edgenam)
          if(ierr.ne.0)write(6,'(/,a)')' WARNING: BLENDMONT - error renaming'//
     &        ' existing edge function file'
          open(iunedge(ixy),file=edgenam,status='new'
     &        ,form='unformatted',access='direct',recl=lenrec)
c           write header record
          write(iunedge(ixy),rec=1)nedge(ixy),nxgrid(ixy),nygrid(ixy)
     &        ,intgrid(ixy),intgrid(3-ixy)
        enddo
      endif
c       
c       Do not write ecd file if not all sections are being computed
      if (ifoldedge .eq. 1 .and. nzwant .ne. nlistz) xcWriteOut = .false.
c       
c       Read old .ecd file(s)
      if(xcreadin .and. .not.undistortOnly)then
        ierr = PipGetFloat('BinningForEdgeShifts', ecdBin)
        iedgeDelX = 0
        iedgeDelY = 0
        if (PipGetTwoIntegers('OverlapForEdgeShifts', iedgeDelX, iedgeDelY)
     &      .eq. 0) then
          iedgeDelX = nxoverlap - nint(ecdBin * iedgeDelX)
          iedgeDelY = nyoverlap - nint(ecdBin * iedgeDelY)
          print *,'Adjusting by ', iedgeDelX, iedgeDelY
        endif
        edgenam = trim(rootname)//trim(xcorrext(0))
        inquire(file=edgenam,exist=exist)
        iy = 4
        if(.not.exist)then
c           
c           If the file does not exist, look for the two separate files
c           and put second name in a different variable.  Set unit number to
c           read one file then the other.
          edgenam = trim(rootname)//trim(xcorrext(1))
          inquire(file=edgenam,exist=exist)
          if (exist) then
            edgename2 = trim(rootname)//trim(xcorrext(2))
            inquire(file=edgenam,exist=exist)
          endif
          if(.not.exist)then
            edgenam = trim(rootname)//trim(xcorrext(0))
            write(*,'(/,a,a)')'ERROR: BLENDMONT - Edge correlation file does'//
     &          ' not exist: ',trim(edgenam)
            call exit(1)
          endif
          call dopen(5, edgename2, 'ro','f')
          iy = 5
        endif
        call dopen(4,edgenam,'ro','f')
        read(4,*)nedgetmp(1,1),nedgetmp(1,2)
        if(nedgetmp(1,1).ne.nedge(1).or.nedgetmp(1,2).ne.nedge(2))
     &      call exitError('WRONG # OF EDGES IN EDGE CORRELATION FILE')
        ix = 4
        
        do ixy = 1,2
          do i = 1,nedge(ixy)
            read(ix, '(a)') titlech
            call frefor(titlech, title, ixout)
            edgedispx(i,ixy) = title(1) * ecdBin
            edgedispy(i,ixy) = title(2) * ecdBin
            if (ixy .eq. 1) edgedispx(i,ixy) = edgedispx(i,ixy) + iedgeDelX
            if (ixy .eq. 2) edgedispy(i,ixy) = edgedispy(i,ixy) + iedgeDelY
c             
c             Read in skip edge flag and treat it same as when using an
c             exclusion model
            if (ixout .gt. 2) then
              if (title(3) .ne. 0) then
                ifSkipEdge(i,ixy) = 2
                if ((edgedispx(i,ixy) .ne. 0. .or.
     &              edgedispy(i,ixy).ne.0.) .and. ifUseAdjusted.gt.0) then
                  ifskipEdge(i,ixy) = 1
                  if (ifUseAdjusted .gt. 1) ifskipEdge(i,ixy) = 0
                endif
              endif
            endif
          enddo
          ix = iy
        enddo
        close(4)
        close(5)
      endif
      ixgdim = 0
      iygdim = 0
      do ixy = 1, 2
        ixgdim = max(ixgdim, nxgrid(ixy))
        iygdim = max(iygdim, nygrid(ixy))
      enddo
c       
c       make default blending width be 80% of overlap up to 50, then
c       half of overlap above 50
      iblend(1) = max(max(nxoverlap / 2, 50), min(4 * nxoverlap / 5, 50))
      iblend(2) = max(max(nyoverlap / 2, 50), min(4 * nyoverlap / 5, 50))
      if (pipinput) then
        ierr = PipGetIntegerArray('BlendingWidthXandY', iblend, 2, 2)
      else
        write(*,'(1x,a,2i5,a,$)')'Blending width in X & Y (/ for'
     &      ,iblend(1), iblend(2),'): '
        read(5,*)(iblend(i),i=1,2)
      endif
c       
c       Do other Pip-only options,
c       Read in mag gradient and distortion field files if specified
c       
      if (pipinput) then
        interpOrder = 3
        ierr = PipGetInteger('InterpolationOrder', interpOrder)
        ierr = PipGetLogical('AdjustedFocus', focusAdjusted)
        if (PipGetString('GradientFile', filnam) .eq. 0) then
          doMagGrad = .true.
          call readMagGradients(filnam, limsect, pixelMagGrad, axisRot, tiltAngles,
     &        dmagPerUm, rotPerUm, numMagGrad)
          if (numMagGrad .ne. nlistz)
     &        print *,'WARNING: BLENDMONT - # OF MAG GRADIENTS (',
     &        numMagGrad,') DOES NOT MATCH # OF SECTIONS (',nlistz,')'
          numAngles = numMagGrad
        endif
c         
c         Look for tilt angles if no mag gradients, then adjust if any
c
        if (numAngles .eq. 0 .and. PipGetString('TiltFile', filnam) .eq. 0) then
          call read_tilt_file(numAngles, 14, filnam, tiltAngles, limsect)
          if (numAngles .ne. nlistz)
     &        print *,'WARNING: BLENDMONT - # OF TILT ANGLES (',
     &        numAngles,') DOES NOT MATCH # OF SECTIONS (',nlistz,')'
        endif
        if (numAngles .gt. 0 .and. PipGetFloat('OffsetTilts', tiltOffset) .eq. 0) then
          do i = 1, numAngles
            tiltAngles(i) = tiltAngles(i) + tiltOffset
          enddo
        endif
c         
c         If doing added gradients, use a tiltgeometry entry only if no
c         gradient file
c         
        if (PipGetTwoFloats('AddToGradient', delDmagPerUm, delRotPerUm) .eq. 0) then
          if (doMagGrad) then
            do i = 1, numMagGrad
              dmagPerUm(i) = dmagPerUm(i) + delDmagPerUm
              rotPerUm(i) = rotPerUm(i) + delRotPerUm
            enddo
          else
            if (PipGetThreeFloats('TiltGeometry', pixelMagGrad, axisRot, tiltOffset)
     &          .ne. 0) call exitError('-tilt OR -gradient MUST BE ENTERED WITH -add')
            pixelMagGrad = pixelMagGrad * 10.
            numMagGrad = 1
            dmagPerUm(1) = delDmagPerUm
            rotPerUm(1) = delRotPerUm
            doMagGrad = .true.
            if (numAngles .eq. 0) then
              numAngles = 1
              tiltAngles(1) = tiltOffset
            endif
          endif
        endif
        if (doMagGrad) then
          lmField = 200
          maxFields = 16
        endif
c         
        if (PipGetString('DistortionField', filnam) .eq. 0) then
          undistort = .true.
          ierr = readCheckWarpFile(filnam, 1, 1, idfNx, idfNy, ix, idfBinning,
     &        pixelIdf, iy, edgenam)
          if (ierr .lt. 0) call exitError(edgenam)
      
          ierr = getGridParameters(1, nxField, nyField, xFieldStrt, yFieldStrt,
     &        xFieldIntrv, yFieldIntrv)
          lmField = max(lmField, nxField, nyField)

          if (PipGetInteger('ImagesAreBinned', inputBinning) .ne. 0) then
c             
c             If input binning was not specified object if it is ambiguous
c             
            if (nxin .le. idfNx * idfBinning / 2 .and. nyin .le. idfNy * idfBinning / 2)
     &          call exitError('YOU MUST SPECIFY BINNING OF IMAGES BECAUSE THEY '//
     &          'ARE NOT LARGER THAN HALF THE CAMERA SIZE')
          endif
          if (inputBinning .le. 0) call exitError
     &        ('IMAGE BINNING MUST BE A POSITIVE NUMBER')
        endif
c         
c         Allocate field arrays and load distortion field
        if (doMagGrad .or. undistort) then
          allocate(distDx(lmField,lmField),distDy(lmField,lmField),
     &        fieldDx(lmField,lmField,maxFields), fieldDy(lmField,lmField,maxFields),
     &        stat = ierr)
          call memoryError(ierr, 'ARRAYS FOR DISTORTION FIELDS')
        endif
        if (undistort) then
          if (getWarpGrid(1, nxField, nyField, xFieldStrt, yFieldStrt, xFieldIntrv,
     &        yFieldIntrv, distDx, distDy, lmField) .ne. 0) call exitError(
     &        'GETTING DISTORTION FIELD FROM WARP FILE')
c           
c           Adjust grid start and interval and field itself for the
c           overall binning
c           
          binRatio = idfBinning / float(inputBinning)
          xFieldStrt = xFieldStrt * binRatio
          yfieldStrt = yfieldStrt * binRatio
          xFieldIntrv = xFieldIntrv * binRatio
          yFieldIntrv = yFieldIntrv * binRatio
c           
c           if images are not full field, adjust grid start by half the
c           difference between field and image size
c           
          xFieldStrt = xFieldStrt - (idfNx * binRatio -  nxin) / 2.
          yfieldStrt = yfieldStrt - (idfNy * binRatio -  nyin) / 2.
c           
c           scale field
          do iy = 1, nyField
            do i = 1, nxField
              distDx(i, iy) = distDx(i, iy) * binRatio
              distDy(i, iy) = distDy(i, iy) * binRatio
            enddo
          enddo
        endif
         print *,nxField,nyField,lmField
c         print *,xFieldStrt,yfieldStrt,xFieldIntrv,yFieldIntrv
c         write(*,'(10f7.2)')(distDx(i, 5),distDy(i, 5),i=1,min(nxField,10))
c         
c         Handle debug output - open files and set flags
c         
        if (xcorrDebug) then
          if (nxpieces .gt. 1) then
            edgenam = trim(rootname)//'.xdbg'
            call imopen(3,edgenam,'new')
            ifDumpXY(1) = 0
          endif
          if (nypieces .gt. 1) then
            edgenam = trim(rootname)//'.ydbg'
            call imopen(4,edgenam,'new')
            ifDumpXY(2) = 0
          endif
        endif
c
c         Get fill treatment; if not entered and very sloppy and distortion,
c         set for taper
        if (PipGetInteger('TreatFillForXcorr', ifillTreatment) .ne. 0 .and.
     &      verySloppy .and. (undistort .or. doMagGrad)) ifillTreatment = 2
c         
c         Check for model of edges to exclude
        if (PipGetString('SkipEdgeModelFile', filnam) .eq. 0 .and.
     &      .not. undistortOnly)  call readExclusionModel(filnam,edgedispx,
     &      edgedispy,limedge, ifUseAdjusted, mapAllPc, nxpieces, nypieces,
     &      minzpc, numSkip)
      endif
c       
c       Now that skip flags have been set, write ecd file if
c       it was read in from two halves
      if (edgename2 .ne. ' ') call writeEdgeCorrelations()
c
      doFields = undistort .or. doMagGrad
      if (undistortOnly .and. .not. doFields) call exitError('YOU MUST'//
     &    'ENTER -gradient AND/OR -distort WITH -justUndistort')
      if (undistortOnly .and. testMode) call exitError(
     &    'YOU CANNOT ENTER BOTH -test AND -justUndistort')

c       
c       Set up for warping
      if (doWarp) then
        if (PipGetTwoIntegers('UnalignedStartingXandY', ixUnaliStart, iyUnaliStart)
     &      .ne. 0) then
c           
c           If there is no unaligned start entered and the output area matches the
c           warp file area, then assume the same start as the current output
          ixUnaliStart = newminxpiece
          iyUnaliStart = newminypiece
c           
c           Otherwise issue warning if sizes don't match and assume it was centered on
c           input / full output
          if (nint(warpscale * iwarpNx) .ne. newxtotpix .or.
     &        nint(warpscale * iwarpNy) .ne. newytotpix) then
            ixUnaliStart = nint(minxpiece + nxtotpix / 2. - warpScale * iwarpNx / 2.)
            iyUnaliStart = nint(minypiece + nytotpix / 2. - warpScale * iwarpNy / 2.)
            write(edgenam,'(a,a,a,2i7)')'WARNING: BLENDMONT - AREA BEING OUTPUT IS ',
     &          'DIFFERENT SIZE FROM UNALIGNED AREA; YOU MAY NEED TO ENTER ',
     &          'UnalignedStartingXandY FOR WARPING TO WORK RIGHT; ASSUMING STARTS OF',
     &          ixUnaliStart,iyUnaliStart
            write(*,'(/,a)')trim(edgenam)
          endif
        endif
c           
c         Given starting coordinate for warping coordinates, get an offset from that
c         Just divide the offset by warpScale here, it's never needed otherwise
        warpXoffset = (newminxpiece + newxtotpix / 2. -
     &      (ixUnaliStart + warpScale * iwarpNx / 2.)) / warpScale
        warpYoffset = (newminypiece + newytotpix / 2. -
     &      (iyUnaliStart + warpScale * iwarpNy / 2.)) / WarpScale
        if (setCurrentWarpFile(indWarpFile) .ne. 0) call exitError(
     &      'SETTING CURRENT WARP FILE')
        allocate(nControl(nglist), stat = ierr)
        call memoryError(ierr,'ARRAY FOR NUMBER OF CONTROL POINTS')
        if (findMaxGridSize(warpXoffset, warpXoffset + newxtotpix / warpScale,
     &      warpYoffset, warpYoffset + newytotpix / warpScale, nControl, lmWarpX, lmWarpY,
     &      edgenam) .ne. 0) call exitError(edgenam)
        allocate(warpDx(lmWarpX, lmWarpY), warpDy(lmWarpX, lmWarpY), stat = ierr)
        call memoryError(ierr,'ARRAYS FOR WARPING GRIDS')
      endif

      call PipDone()
c       
c       Allocate more arrays now
      deallocate(mapAllPc)
      allocate(xcray(idimc/2), xdray(idimc/2), mappiece(nxpieces, nypieces),
     &    mapDisjoint(nxpieces, nypieces), anyDisjoint(nxpieces, nypieces),
     &    limDataLo(limvar,2,2), limDataHi(limvar,2,2),
     &    dxgrbf(ixgdim,iygdim,limedgbf), dygrbf(ixgdim,iygdim,limedgbf),
     &    ddengrbf(ixgdim,iygdim,limedgbf), dxgrid(ixgdim,iygdim),
     &    dygrid(ixgdim,iygdim), ddengrid(ixgdim,iygdim), 
     &    sdgrid(ixgdim,iygdim), stat = ierr)
      call memoryError(ierr, 'CORRELATION OR EDGE BUFFER ARRAYS')
      if (numXcorrPeaks .gt. 1 .and. .not. xclegacy) then
        allocate(xeray(idimc/2), stat = ierr)
        call memoryError(ierr, 'CORRELATION ARRAY')
      endif
c       
c       Set maximum load; allow one extra slot if doing fields
c       Limit by the maximum number of pieces that could be needed, which is
c       the full array, or just one if undistorting
      npixin = nxin * int(nyin, kind = 8)
      mostNeeded = max(2, nxpieces * nypieces)
      if (undistortOnly) mostNeeded = 1
c       
c       First compute a limit based on the minimum memory we would like to use
      if (doFields) then
        maxload = min(memMinimum/npixin - 1, memlim, maxFields, mostNeeded)
        ix = 1
      else
        maxload = min(memMinimum/npixin, memlim, mostNeeded)
        ix = 0
      endif
c       
c       Increase that to 4 or whatever is needed if it is below that
c       but if this doesn't fit into the preferred memory limit, then
c       just go for 2 pieces and make sure that fits too!
      maxload = max(maxload, min(4, mostNeeded))
      if ((maxload + ix) * npixin .gt. memPreferred) maxload =min(2,mostNeeded)
      if ((maxload + ix) * npixin .gt. memMaximum) call exitError(
     &    'IMAGES TOO LARGE FOR 32-BIT ARRAY INDEXES')
      maxsiz = (maxload + ix) * npixin
      allocate(array(maxsiz), stat = ierr)
      if (ierr .ne. 0 .and. maxload .gt. min(2,mostNeeded)) then
        maxload =min(2,mostNeeded)
        maxsiz = (maxload + ix) * npixin
        allocate(array(maxsiz), stat = ierr)
      endif
      if (ierr .ne. 0) call exitError('ALLOCATING MAIN IMAGE ARRAY')
      write(*,'(a,i5,a)')'Allocated', maxsiz / (1024*256),
     &    ' MB of memory for main image array'
c       
c       initialize memory allocator and dx,dy lists for looking for near piece
c       
      call clearShuffle()
      call initNearList()
c
      intgrcopy(1)=intgrid(1)
      intgrcopy(2)=intgrid(2)
c       
c       All errors checked, exit if setting up parallel
      if (modeParallel .gt. 0) call exit(0)
c       
c       Now open output files after errors have been checked, unless direct 
c       writing in parallel mode
      if (modeParallel .ne. -2 .and. ifEdgeFuncOnly .eq. 0) then
c         
c         Do as much of header as possible, shift origin same as in newstack
        call imopen(2,outfile,'new')
        call itrhdr(2,1)
        call ialnbsym(2,0)
        call ialsymtyp(2,0,0)
        call ialmod(2,modeout)
        call ialsiz(2,nxyzbin,nxyzst)
        call irtorg(1, xorig, yorig, zorig)
        xorig = xorig - delta(1) * ixOffset
        yorig = yorig - delta(2) * iyOffset
        if (adjustOrigin) then
          xorig = xorig - delta(1) * (newminxpiece - minxpiece)
          yorig = yorig - delta(2) * (newminypiece - minypiece)
          zorig = zorig - delta(3) * (izwant(1) - listz(1))
        endif
        call ialorg(2, xorig, yorig, zorig)
        cell(1)=nxbin*delta(1)*iBinning
        cell(2)=nybin*delta(2)*iBinning
c         
c         Finish it and exit if setting up for direct writing
        if (modeParallel .eq. -1) then
          call ialsam(2,nxyzbin)
          cell(3)=nzbin*delta(3)
          call ialcel(2,cell)
        endif
c       
        call b3ddate(dat)
        call time(tim)
        write(titlech,90) actionStr,dat,tim
90      format( 'BLENDMONT: Montage pieces ',a, t57, a9, 2x, a8 )
        call iwrhdrc(2,titlech,1,dmin,dmax,dmean)
        if (modeParallel .eq. -1) call exit(0)
      else if (ifEdgeFuncOnly .eq. 0) then
        call imopen(2,outfile,'old')
        call irdhdr(2,ixpclo,ixpcup,ixout,wll,wlr,wul)
        if (ixpclo(1) .ne. nxbin .or. ixpclo(2) .ne. nybin .or.
     &      ixout .ne. modeout) call exitError(
     &      'EXISTING OUTPUT FILE DOES NOT HAVE RIGHT SIZE OR MODE')
      endif
c
      if (outputpl) call dopen(3,ploutfile,'new','f')
      fastcum = 0.
      slowcum = 0.
c       
c       loop on z: do everything within each section for maximum efficiency
c       
      do ilistz=1,nlistz
        doingEdgeFunc = .true.
        izsect=listz(ilistz)
c         
c         test if this section is wanted in output: if not, skip in test mode
c         
        ifwant=0
        do iwant=1,nzwant
          if(izwant(iwant).eq.izsect)ifwant=1
        enddo
        if (ifwant .eq. 0 .and. (testMode .or. undistortOnly)) cycle
c         
c         Look up actual section to write if doing direct parallel writes
        if (ifwant .ne. 0 .and. modeParallel .eq. -2 .and. .not. yChunks) then
          do i = 1, nzAllWant
            if (izAllWant(i) .eq. izsect) numOut = i - 1
          enddo
        endif
c
        write(*,'(a,i4)')' working on section #',izsect
        call flush(6)
        multng=multineg(izsect+1-minzpc)
        xinlong=nxpieces .gt. nypieces
c         
c         make a map of pieces in this section and set up index to data limits
c         
        do iyfrm=1,nypieces
          do ixfrm=1,nxpieces
            mappiece(ixfrm,iyfrm)=0
          enddo
        enddo
        ipclo = 0
        do ipc=1,npclist
          if(izpclist(ipc).eq.izsect)then
            ixfrm=1+(ixpclist(ipc)-minxpiece)/(nxin-nxoverlap)
            iyfrm=1+(iypclist(ipc)-minypiece)/(nyin-nyoverlap)
            mappiece(ixfrm,iyfrm)=ipc
            ipclo = ipclo + 1
            limDataInd(ipc) = ipclo
            do i = 1, 2
              limDataLo(ipclo,i,1) = -1
              limDataLo(ipclo,i,2) = -1
              limDataHi(ipclo,i,1) = -1
              limDataHi(ipclo,i,2) = -1
            enddo
          endif
        enddo
C
        if(.not. undistortOnly)then 
c           
c           First get edge functions for the section if they haven't been done
          call findSectionEdgeFunctions()
        endif
c         
c         now if doing multinegatives, need to solve for h transforms
c         
        if(multng .and. .not. undistortOnly)then
          call findMultinegTransforms()
        endif                                   !end of multineg stuff

        if (.not. undistortOnly) then
c           
c           initialize edge buffer allocation
c           
          jusedgct=0
          do ixy=1,2
            do i=1,nedge(ixy)
              ibufedge(i,ixy)=0
            enddo
          enddo
          do i=1,limedgbf
            iedgbflist(i)=-1
            lasedguse(i)=0
          enddo 
c           
c           if this section is not wanted in output, skip out
c           
          if(ifwant.eq.0) cycle
c           
c           scan through all edges in this section to determine limits for
c           when a point is near an edge
c           
          do ixy=ixyFuncStart,ixyFuncEnd
            edgelonear(ixy)=0.
            edgehinear(ixy)=nxyzin(ixy)-1.
            do iedge=1,nedge(ixy)
              if(izpclist(ipiecelower(iedge,ixy)).eq.izsect)then
                jedge = iedge
                if (useEdges) call findEdgeToUse(iedge, ixy, jedge)
c                print *,'reading header of edge',ixy,jedge,' for edge',ixy,iedge
                if (needbyteswap.eq.0)then
                  read(iunedge(ixy),rec=1+jedge)nxgr,nygr,(igridstr(i),
     &                iofset(i),i=1,2)
                else
                  read(iunedge(ixy),rec=1+jedge)nxgr,nygr,
     &                (igridstr(i), iofset(i),i=1,2)
                  call convert_longs(nxgr,1)
                  call convert_longs(nygr,1)
                  call convert_longs(igridstr,2)
                  call convert_longs(iofset,2)
                endif
                edgehinear(ixy)=min(edgehinear(ixy),
     &              float(igridstr(ixy)))
                edgelonear(ixy)=max(edgelonear(ixy),float
     &              ((min(nxgr,nygr)-1)*intgrid(1)+iofset(ixy)+10))
c                print *,nxgr,nygr, (igridstr(i), iofset(i),i=1,2)
              endif
            enddo
c            print *,ixy,edgelonear(ixy),edgehinear(ixy)
          enddo
          edgesSeparated = edgehinear(1) - edgelonear(1) .gt. 0.05 * nxin .and.
     &        edgehinear(2) - edgelonear(2) .gt. 0.05 * nyin
        endif
c         
c         Now analyze for h transforms if need to shift each piece - this sets
c         multng because it is a flag that hinv exists and needs to be used
c         
        mapDisjoint = 0
        anyDisjoint = .false.
        if(shifteach .and. .not. undistortOnly .and. ifEdgeFuncOnly .ne. 1
     &      .and. ifEdgeFuncOnly .ne. 2)then
          call getBestPieceShifts()
c           
c           Analyze for disjoint edges on this section
c           Look for non-overlap between cross-corner pieces and code by type
          do iyfrm = 1, nypieces - 1
            do ixfrm = 1, nxpieces - 1
              if (mappiece(ixfrm, iyfrm) .gt. 0 .and. mappiece(ixfrm+1, iyfrm)
     &            .gt. 0 .and. mappiece(ixfrm, iyfrm+1) .gt. 0 .and.
     &            mappiece(ixfrm+1, iyfrm+1) .gt. 0) then
                ix = mappiece(ixfrm, iyfrm)
                iy = mappiece(ixfrm+1, iyfrm+1)
                if (ixpclist(ix) + h(1,3,ix) + nxin .le.
     &              ixpclist(iy) + h(1,3,iy)) mapDisjoint(ixfrm, iyfrm) = 1
                if (iypclist(ix) + h(2,3,ix) + nyin .le.
     &              iypclist(iy) + h(2,3,iy)) mapDisjoint(ixfrm, iyfrm) = 3
                ix = mappiece(ixfrm, iyfrm+1)
                iy = mappiece(ixfrm+1, iyfrm)
                if (ixpclist(ix) + h(1,3,ix) + nxin .le.
     &              ixpclist(iy) + h(1,3,iy)) mapDisjoint(ixfrm, iyfrm) = 2
                if (iypclist(iy) + h(2,3,iy) + nyin .le.
     &              iypclist(ix) + h(2,3,ix)) mapDisjoint(ixfrm, iyfrm) = 4
                if (mapDisjoint(ixfrm, iyfrm) .ne. 0) 
     &              anyDisjoint(ixfrm:ixfrm+1, iyfrm:iyfrm+1) = .true.
c                if (mapDisjoint(ixfrm, iyfrm) .ne. 0) print *,'disjoint',ixfrm,iyfrm
              endif
            enddo
          enddo
        endif
c         
c         if doing g transforms, get inverse and recenter it from center
c         of output image to corner of image
c         
        if(dogxforms)then
          if(skipxforms)then
            indgl=izsect+1-minzpc
          else
            do ilis=1,nlistz
              if(listz(ilis).eq.izsect)indgl=ilis
            enddo
          endif
          call xfcopy(gl(1,1,indgl),hit)
          hit(1,3)=hit(1,3)+(1.-hit(1,1))*gxcen-hit(1,2)*gycen
          hit(2,3)=hit(2,3)+(1.-hit(2,2))*gycen-hit(2,1)*gxcen
          call xfinvert(hit,ginv)
        endif
c         
c         Adjust flag for shuffler to know whether to undistort, and clear
c         out the undistorted pieces in memory
c         
        doingEdgeFunc = .false.
        if (doFields) call clearShuffle()
        if (testMode .or. ifEdgeFuncOnly .ne. 0) cycle ! To end of section loop
c         
c         If warping, find out if there is warping on this section and get grid
        secHasWarp = .false.
        if (doWarp) then
          secHasWarp = nControl(indgl) .gt. 2
        endif
        if (secHasWarp) then
c           
c           Get the grid without adjustment of coordinates for any offset, then adjust
c           the grid start for the old starting X coordinate
          if (getSizeAdjustedGrid(indgl, newxtotpix / warpScale, newytotpix / warpScale,
     &        warpXoffset, warpYoffset, 0, warpScale, 1, nxWarp, nyWarp, xWarpStrt,
     &        yWarpStrt, xWarpIntrv, yWarpIntrv, warpDx, warpDy, lmWarpX, lmWarpY,
     &        edgenam) .ne. 0) call exitError(edgenam)
          xWarpStrt = xWarpStrt + ixUnaliStart
          yWarpStrt = yWarpStrt + iyUnaliStart
c          print *,nxWarp, nyWarp, xWarpStrt, yWarpStrt, xWarpIntrv, yWarpIntrv
c          write(*,'(10f7.2)')((warpDx(ix,iy), warpDy(ix,iy), ix=1,10),iy=3,4)
        endif
c         
c         if floating, need to get current input min and max
c         To pad edges properly, need current mean: put it into dmean
c         
        call  crossvalue(xinlong,nxpieces,nypieces,nshort,nlong)
        curinmax=-1.e10
        curinmin=1.e10
        cursum=0.
        rnsum=0.
        do ilong=nlong,1,-1
          do ishort=nshort,1,-1
            call crossvalue(xinlong,ishort,ilong,ixfrm,iyfrm)
            if(mappiece(ixfrm,iyfrm).gt.0)then
              call shuffler(mappiece(ixfrm,iyfrm),indbray)
              do iy=1,nyin
                tsum=0.
                do i=indbray+(iy-1)*nxin,indbray+iy*nxin-1
                  curinmin=min(curinmin,array(i))
                  curinmax=max(curinmax,array(i))
                  tsum=tsum+array(i)
                enddo
                cursum=cursum+tsum
                rnsum=rnsum+nxin
              enddo
            endif
          enddo
        enddo
        dmean=cursum/rnsum
        dfill = dmean
        if (useFill) dfill = fillVal
        if(iffloat.eq.0)then
          curinmin=definmin
          curinmax=definmax
        endif
c         
c         now get output scaling factor and additive factor
c         
        pixscale=(outmax-outmin)/max(1.,curinmax-curinmin)
        pixadd=outmin-pixscale*curinmin
c         
c         look through memory list and renumber them with priorities
c         backwards from the first needed piece
c         
        newuse=jusecount-1
        do ilong=1,nlong
          do ishort=1,nshort
            call crossvalue(xinlong,ishort,ilong,ixfrm,iyfrm)
            if(mappiece(ixfrm,iyfrm).gt.0)then
              do i=1,maxload
                if(izmemlist(i).eq.mappiece(ixfrm,iyfrm))then
                  lastused(i)=newuse
                  newuse=newuse-1
                endif
              enddo
            endif
          enddo
        enddo
c         
c         UNDISTORTING ONLY: loop on all frames in section, in order as
c         they were in input file
c         
        if (undistortOnly) then
          call clearShuffle()
          doingEdgeFunc = .true.
          do ipc = 1, npclist
            if (izpclist(ipc) .eq. izsect) then
              call shuffler(ipc, indbray)
              tsum=0.
              do i = 1, npixin
                val=pixscale*array(i + indbray - 1)+pixadd
                array(i + indbray - 1)=val
                dminout=min(dminout,val)
                dmaxout=max(dmaxout,val)
                tsum=tsum+val
              enddo
              grandsum=grandsum+tsum
              nzbin=nzbin+1
              call iwrsec(2, array(indbray))
c               
              if (outputpl) write(3,'(2i6,i4)')newpcxll,newpcyll,izsect
            endif
          enddo
          cycle                                 ! To end of section loop
        endif
c         
c         GET THE PIXEL OUT
c         -  loop on output frames; within each frame loop on little boxes
c         
        call  crossvalue(xinlong,newxpieces,newypieces,nshort,nlong)
c         
        do ilong=1,nlong
          do ishort=1,nshort
            call crossvalue(xinlong,ishort,ilong,ixout,iyout)
c             write(*,'(a,2i4)')' composing frame at',ixout,iyout
            newpcyll=newminypiece+(iyout-1)*(newyframe-newyoverlap)
            newpcxll=newminxpiece+(ixout-1)*(newxframe-newxoverlap)
            anypixels = iBinning .gt. 1 .or. newxpieces * newypieces .eq. 1
            anylinesout=.false.
            tsum=0.
            lineOffset = iyOffset
            linesBuffered = 0
            iBufferBase = 0
c             
c             do fast little boxes
c             
            nxfast=(newxframe+(ifastsiz-1))/ifastsiz
            nyfast=(newyframe+(ifastsiz-1))/ifastsiz
            if (yChunks) nyfast = (lineEnd - lineStart + ifastsiz) / ifastsiz
c             
c             loop on boxes, get lower & upper limits in each box
c             
            do iyfast=1,nyfast
              indylo=newpcyll+(iyfast-1)*ifastsiz
              indyhi=min(indylo+ifastsiz,newpcyll+newyframe)-1
              if (yChunks) then
                indylo = lineStart + (iyfast-1)*ifastsiz
                indyhi = min(indylo + ifastsiz - 1, lineEnd)
              endif
              nlinesout=indyhi+1-indylo
c               
c               fill array with dfill
c               
              do i=1,nxout*nlinesout
                brray(i + iBufferBase)=dfill
              enddo
c               
              do ixfast=1,nxfast
                indxlo=newpcxll+(ixfast-1)*ifastsiz
                indxhi=min(indxlo+ifastsiz,newpcxll+newxframe)-1
c                 
c                 check # of edges, and prime piece number, for each corner
c                 
                dofast=.true.
                inframe=.false.
                inonepiece=0
                samePieces = .true.
                debug = (ixdebug .ge. indxlo .and. ixdebug .le. indxhi) .or.
     &                (iydebug .ge. indylo .and. iydebug .le. indyhi)
                do indy=indylo,indyhi,indyhi-indylo
                  do indx=indxlo,indxhi,indxhi-indxlo
                    call countedges(indx,indy,xg,yg, useEdges)
                    if(numpieces.gt.0)then
                      if(inonepiece.eq.0)then
                        inonepiece=inpiece(1)
                        numfirst = numpieces
                        ipfirst(1:4) = inpiece(1:4)
                      endif
                      dofast=dofast.and. numpieces.eq.1 .and.
     &                    inpiece(1).eq.inonepiece
                      samePieces = samePieces .and. numpieces .eq. numfirst
                      do i=1,numpieces
                        inframe=inframe .or. (xinpiece(i).ge.0. .and.
     &                      xinpiece(i).le.nxin-1. .and. yinpiece(i)
     &                      .ge.0. .and. yinpiece(i).le.nyin-1.)
                        samePieces = samePieces .and. ipfirst(i).eq.inpiece(i)
                      enddo
                    else
                      samePieces = .false.
                    endif
                  enddo
                enddo
                if (debug) print *,indylo,indyhi,numpieces,xinpiece(1),yinpiece(1)
c                 
c                 ALL ON ONE PIECE: do a fast transform of whole box
c                 
                wallstart = walltime()
                if(dofast.and.inframe)then
                  if (debug) print *,  'fast box',indxlo,indxhi,indylo,indyhi
                  call xfunit(fastf,1.)         !start with unit xform
c                   
c                   if doing g xforms, put operations into xform that will
c                   perform inverse of xform
c                   
                  if(dogxforms)then
                    call xfcopy(ginv,fastf)
                  endif
c                   
c                   shift coordinates down to be within piece
c                   
                  fastf(1,3)=fastf(1,3)-ixpclist(inonepiece)
                  fastf(2,3)=fastf(2,3)-iypclist(inonepiece)
c                   
c                   if doing h's, implement the h inverse
c                   
                  if(multng)then
                    call xfmult(fastf,hinv(1,1,inonepiece),fstmp)
                    call xfcopy(fstmp,fastf)
                  endif
c                   
c                   now add 1 to get array index
c                   
                  fastf(1,3)=fastf(1,3)+1.
                  fastf(2,3)=fastf(2,3)+1.
c                   
                  call shuffler(inonepiece,indbray)
                  call fastinterp(brray(iBufferBase + 1),nxout,nlinesout,
     &                array(indbray),nxin,nyin,indxlo,indxhi,indylo,
     &                indyhi,newpcxll,fastf,fastf(1,3),
     &                fastf(2,3),inonepiece)
                  anypixels=.true.
                  fastcum = fastcum + walltime() - wallstart
c                   
                elseif(inframe)then
c                   
c                   in or near an edge: loop on each pixel
c                   
                  niter=10
                  linebase=iBufferBase + 1 - newpcxll
                  do indy=indylo,indyhi
                    do indx=indxlo,indxhi
c                       Set debug .or. or .and. here
                      debug = indx .eq. ixdebug .or. indy .eq. iydebug
                      if (samePieces) then
c                         
c                         If it is all the same pieces in this box, then update
c                         the positions in the pieces
                        xg = indx
                        yg = indy
                        if (secHasWarp) then
                          call interpolateGrid(xg - 0.5, yg - 0.5, warpDx, warpDy,
     &                        lmWarpX, nxWarp, nyWarp, xWarpStrt, yWarpStrt, xWarpIntrv,
     &                        yWarpIntrv, xg, yg)
                          xg = xg + indx
                          yg = yg + indy
                        endif
                        if (dogxforms) then
                          xtmp = ginv(1,1)*xg + ginv(1,2)*yg + ginv(1,3)
                          yg = ginv(2,1)*xg + ginv(2,2)*yg + ginv(2,3)
                          xg = xtmp
                        endif
                        do i = 1, numPieces
                          call positionInPiece(xg, yg, inpiece(i), xinpiece(i),
     &                        yinpiece(i))
                        enddo
                      else
                        call countedges(indx,indy,xg,yg, useEdges)
                      endif
                      if (debug) write(*,'(2i6,i7,i2,a,i3,a,5i5)')indx,indy,
     &                    numedges(1), numedges(2),' edges',numpieces,
     &                    ' pieces', (inpiece(i),i=1,numpieces)
c                       
c                       load the edges and compute edge fractions
c                       
                      call computeEdgeFractions()
c                       
c                       get indices of pieces and edges and the weighting
c                       of each piece: for now, numbers
                      call getPieceIndicesAndWeighting(useEdges)
c                       
c                       NOW SORT OUT THE CASES OF 1, 2, 3 or 4 PIECES

                      call getPixelFromPieces()
c                       
c                       stick limited pixval into array and mark as output
c                       
                      brray(linebase+indx)=
     &                    max(curinmin,min(curinmax,pixval))
                      anypixels=.true.
                    enddo
                    linebase=linebase+nxout
                  enddo
                  slowcum = slowcum + walltime() - wallstart
                endif
              enddo
c               
c               if any pixels have been present in this frame, write line out
c               
              if (modeParallel .ne. -2 .or. yChunks) numOut = nzbin
              if(anypixels)then
                do i=1,nxout*nlinesout
                  brray(i+iBufferBase)=pixscale*brray(i+iBufferBase)+pixadd
                enddo
c                 
c                 Set line position based on binned pixels output
c                 Set lines to write based on what is in buffer already
c                 Get new number of lines left in buffer; if at top of
c                 frame, increment lines to write if there are any lines left
c                 
                ilineout=((iyfast-1)*ifastsiz - iyOffset) / iBinning
                call parWrtPosn(2,numOut,ilineout + iyOutOffset)
                ilineout = linesBuffered + nLinesOut
                nyWrite = (ilineOut - lineOffset) / iBinning
                linesBuffered = mod(ilineout - lineOffset, iBinning)
                if (indyhi .eq. newpcyll + newyframe - 1 .and.
     &              linesBuffered .gt. 0) nyWrite = nyWrite + 1
c                 
c                 write data
c                 
                call iwrBinned(2, brray, binline, nxout, nxbin, ixOffset,
     &              ilineout, nyWrite, lineOffset, iBinning, dminout,
     &              dmaxout,tsum)
c                 
c                 Set Y offset to zero after first time, set base and
c                 move remaining lines to bottom
c                 
                lineOffset = 0
                ifill = (ilineout - linesBuffered) * nxout
                iBufferBase = nxout * linesBuffered
                do i = 1, iBufferBase
                  brray(i) = brray(i + ifill)
                enddo
c                 
c                 if this is the first time anything is written, and it
c                 wasn't the first set of lines, then need to go back and
c                 fill the lower part of frame with mean values
c                 
                if(.not.anylinesout.and.iyfast.gt.1)then
                  val=dfill*pixscale+pixadd
                  brray(1:nxout)=val
                  call parWrtPosn(2,numOut,iyOutOffset)
                  do ifill=1,(iyfast-1) * ifastsiz
                    call parWrtLin(2,brray)
                  enddo
                  tsum=tsum+val*nxout*(iyfast-1)*ifastsiz
                endif
                anylinesout=.true.
              endif
            enddo
c             
c             if any pixels present, write piece coordinates
c             
            if(anypixels)then
              grandsum=grandsum+tsum
c               write(*,'(a,i5)')' wrote new frame #',nzbin
              nzbin=nzbin+1
c               
              if (outputpl) write(3,'(2i6,i4)')newpcxll,newpcyll,izsect
            endif
c             
          enddo                                 ! Loop on frames - short dim
        enddo                                   ! Loop on frames - long dim
      enddo                                     ! Loop on sections
c       
      close(3)
c      write(*,'(a,2f12.6)')'fast box and single pixel times:',fastcum,slowcum
      if (ifEdgeFuncOnly .eq. 0 .and. .not. testMode) then
c       
c         If direct parallel, output stats
        pixelTot = (float(nxbin) * nlinesWrite) * nzbin
        tmean = grandsum / pixelTot
        if (modeParallel .eq. -2) then
          write(*,'(a,3g15.7,f15.0)')'Min, max, mean, # pixels=',dminout,
     &        dmaxout, tmean, pixelTot
        else 
c           
c           otherwise finalize the header
c           
          call ialsiz(2,nxyzbin,nxyzst)
          call ialsam(2,nxyzbin)
          cell(3)=nzbin*delta(3)
          call ialcel(2,cell)
          call iwrhdr(2,title,-1,dminout,dmaxout, tmean)
        endif
        call imclose(2)
      endif
      if (undistortOnly) call exit(0)
c       
c       write edge correlations 
c       
      if(xcWriteOut) call writeEdgeCorrelations()
c       
c       rewrite header for new edge functions so that they have later date
c       than the edge correlations; close files
c       
      do ixy = ixyFuncStart, ixyFuncEnd
        if (ifoldedge .eq. 0) write(iunedge(ixy),rec=1)nedge(ixy),
     &      nxgrid(ixy),nygrid(ixy) ,intgrid(ixy),intgrid(3-ixy)
        close(iunedge(ixy))
      enddo
      do ixy = 1, 2
        if (ifDumpXY(ixy) .gt. 0) then
          call iwrhdr(2+ixy,title,-1,0.,255.,128.)
          call imclose(2+ixy)
        endif
      enddo
      if (izUnsmoothedPatch .ge. 0) close(10)
      if (izSmoothedPatch .ge. 0) close(11)
c       
      call exit(0)

      CONTAINS

c       Write the full edge correlation file, or the X or Y component only
c
      subroutine writeEdgeCorrelations()
      edgenam = trim(rootname)//trim(xcorrext(mod(ifEdgeFuncOnly,3)))
      call dopen(4,edgenam,'new','f')
      if (ixyFuncStart .eq. 1) write(4,'(2i7)')nedge(1),nedge(2)
      do ixy = ixyFuncStart, ixyFuncEnd
        if (numSkip .eq. 0 .and. nedge(ixy) .gt. 0) then
          write(4,'(f9.3,f10.3)')(edgedispx(i,ixy),edgedispy(i,ixy),
     &        i=1,nedge(ixy))
        elseif (nedge(ixy) .gt. 0) then
          write(4,'(f9.3,f10.3,i4)')(edgedispx(i,ixy),edgedispy(i,ixy),
     &        ifskipEdge(i, ixy), i=1,nedge(ixy))
        endif
      enddo
      close(4)
      return 
      end subroutine writeEdgeCorrelations


c       GETS EDGE FUNCTIONS FOR A SECTION IF THEY HAVEN'T BEEN DONE YET
c
      subroutine findSectionEdgeFunctions()
c       
c       Check if any edges are to be substituted on this section
c       Do so if any use values are different from original and none are 0
      useEdges = .false.
      if (numUseEdge .gt. 0 .or. izUseDefLow .ge. 0) then
        numZero = 0
        do ixy = 1,2
          do iedge=1,nedge(ixy)
            if(izsect.eq. izpclist(ipiecelower(iedge,ixy))) then
              call findEdgeToUse(iedge, ixy, jedge)
              if (jedge .eq. 0) numZero = numZero + 1
              if (jedge .ne. iedge) useEdges = .true.
            endif
          enddo
        enddo
        if (numZero .gt. 0) useEdges = .false.
c         print *,'numzero, useedges',numZero,useEdges
      endif
c       
c       loop on short then long direction
c       
      do iedgedir=1,2
        call  crossvalue(xinlong,iedgedir,3-iedgedir,ixy,iyx)
        if (iyx .eq. ifEdgeFuncOnly) cycle
c         
c         loop on all edges of that type with pieces in section that are
c         not done yet
c         
        do iedge=1,nedge(ixy)
          if(izsect.eq. izpclist(ipiecelower(iedge,ixy))) then
            jedge = iedge
            if (useEdges) call findEdgeToUse(iedge, ixy, jedge)
c             print *,'checking edge',ixy, jedge,' for edge',ixy, iedge
            if (.not.edgedone(jedge,ixy)) then
c               
c               do cross-correlation if the sloppy flag is set and either
c               we are shifting each piece or the pieces are't on same neg
c               
c               print *,'Doing edge ', ixy, jedge
              docross=ifsloppy.ne.0.and.(shifteach.or.(
     &            anyneg.and.neglist(ipiecelower(jedge,ixy))
     &            .ne.neglist(ipieceupper(jedge,ixy))))
              call doedge(jedge,ixy,edgedone,sdcrit,devcrit,
     &            nfit, norder, nskip, docross, xcreadin, xclegacy,
     &            edgedispx, edgedispy, limedge)
c               
c               after each one, check memory list to see if there's any
c               pieces with undone lower edge in orthogonal direction
c               
              if (.not.useEdges .and. ixy .ne. ifEdgeFuncOnly) then
                do imem=1,maxload
                  ipc=izmemlist(imem)
                  if(ipc.gt.0)then
                    jedge=iedgelower(ipc,iyx)
                    if(jedge.gt.0)then
                      if(.not.edgedone(jedge,iyx))then
c                         print *,'Doing edge ', iyx, jedge
                        docross=ifsloppy.ne.0.and.(shifteach.or.(
     &                      anyneg.and.neglist(ipiecelower(jedge,iyx))
     &                      .ne.neglist(ipieceupper(jedge,iyx))))
                        call doedge(jedge,iyx,edgedone,sdcrit, devcrit,
     &                      nfit, norder, nskip, docross, xcreadin,
     &                      xclegacy, edgedispx, edgedispy, limedge)
                      endif
                    endif
                  endif
                enddo
              endif
            endif
          endif
        enddo
      enddo
      return
      end subroutine findSectionEdgeFunctions


c       FINDS H TRANSFORMS IF MULTIPLE NEGATIVES (UNTESTED!)
c
      subroutine findMultinegTransforms()
      real*4 sind,cosd
c       
c       first make index of negatives and find coordinates of each
c       
      numneg=0
      do i=1,limneg
        maxnegx(i)=-1000000
        minnegx(i)=1000000
        maxnegy(i)=-1000000
        minnegy(i)=1000000
      enddo
      do ipc=1,npclist
        if(izpclist(ipc).eq.izsect)then
          iwhich=0
          do i=1,numneg
            if(neglist(ipc).eq.negind(i))iwhich=i
          enddo
          if(iwhich.eq.0)then
            numneg=numneg+1
            negind(numneg)=neglist(ipc)
            iwhich=numneg
          endif
c           point max coordinate 1 past end: it'll be easier
          minnegx(iwhich)=min(minnegx(iwhich),ixpclist(ipc))
          maxnegx(iwhich)=max(maxnegx(iwhich),ixpclist(ipc)+nxin)
          minnegy(iwhich)=min(minnegy(iwhich),iypclist(ipc))
          maxnegy(iwhich)=max(maxnegy(iwhich),iypclist(ipc)+nyin)
        endif
      enddo
c       
c       look at all edges, make tables of joints between negs
c       
      do ixy=1,2
        njoint(ixy)=0
        do i=1,numneg
          jointupper(i,ixy)=0
          jointlower(i,ixy)=0
        enddo
c         
        do iedge=1,nedge(ixy)
          neglo=neglist(ipiecelower(iedge,ixy))
          negup=neglist(ipieceupper(iedge,ixy))
          if(izpclist(ipiecelower(iedge,ixy)).eq.izsect.and.
     &        neglo.ne.negup)then
c             convert neg #'s to neg indexes
            do i=1,numneg
              if(negind(i).eq.neglo)indlo=i
              if(negind(i).eq.negup)indup=i
            enddo
c             see if joint already on list
            joint=0
            do j=1,njoint(ixy)
              if(neglower(j,ixy).eq.indlo.and.
     &            negupper(j,ixy).eq.indup) joint=j
            enddo
c             if not, add to list
            if(joint.eq.0)then
              njoint(ixy)=njoint(ixy)+1
              joint=njoint(ixy)
              neglower(joint,ixy)=indlo
              negupper(joint,ixy)=indup
c               point the negatives to the joint
              jointlower(indlo,ixy)=joint
              jointupper(indup,ixy)=joint
              nedonjoint(joint)=0
            endif
c             add edge to list of ones on that joint
            nedonjoint(joint)=nedonjoint(joint)+1
            listonjoint(nedonjoint(joint),joint)=iedge
          endif
        enddo
c         
c         now process all edges on each joint to get a rotrans
c         
        do joint=1,njoint(ixy)
          do ied=1,nedonjoint(joint)
            iedge=listonjoint(ied,joint)
            call readEdgeFunc(iedge, ixy, ied)

            ipclo=ipiecelower(iedge,ixy)
            ixpclo(ied)=ixgrdstbf(ied) +ixpclist(ipclo)
            iypclo(ied)=iygrdstbf(ied) +iypclist(ipclo)
            ipcup=ipieceupper(iedge,ixy)
            ixpcup(ied)=ixofsbf(ied) +ixpclist(ipcup)
            iypcup(ied)=iyofsbf(ied) +iypclist(ipcup)
          enddo
          call joint_to_rotrans(dxgrbf,dygrbf,ixgdim,iygdim,
     &        nxgrbf,nygrbf,intgrid(ixy),intgrid(3-ixy)
     &        ,ixpclo,iypclo,ixpcup,iypcup,nedonjoint(joint)
     &        ,r(1,joint,ixy))
          write(*,'(1x,a,2i4,a,f6.2,a,2f6.1)')
     &        char(ixy+ichar('W'))//' joint, negatives'
     &        ,neglist(ipclo),neglist(ipcup),'   theta ='
     &        ,r(1,joint,ixy),'   dx, dy ='
     &        ,r(2,joint,ixy),r(3,joint,ixy)
        enddo
      enddo
c       
c       Resolve the edges into rotrans centered on each negative
c       Initially set the rotrans for each negative to null
c       
      maxswing=0
      do i=1,numneg
        hcum(1,i)=0.
        hcum(2,i)=0.
        hcum(3,i)=0.
        hcum(4,i)=0.5*(maxnegx(i)+minnegx(i))
        hcum(5,i)=0.5*(maxnegy(i)+minnegy(i))
        maxswing=max(maxswing,(maxnegx(i)-minnegx(i))/2,
     &      (maxnegy(i)-minnegy(i))/2)
      enddo
c       
c       start loop - who knows how long this will take
c       
      ishift=1
      erradj=1.e10
      nshiftneg=100
      errlim=0.1*numneg
      do while(ishift.le.nshiftneg.and.erradj.gt.errlim)
c         
c         set sum of adjustment h's to null
c         
        erradj=0.
        do i=1,numneg
          call lincom_rotrans(hcum(1,i),0.,hcum(1,i),0.,hadj(1,i))
          nhadjsum(i)=0
        enddo
c         
c         loop on joints, adding up net adjustments still needed
c         
        do ixy=1,2
          do joint=1,njoint(ixy)
c             
c             get net rotrans still needed at this joint: subtract upper
c             h and add lower h to joint rotrans
c             
            negup=negupper(joint,ixy)
            neglo=neglower(joint,ixy)
            call lincom_rotrans(hcum(1,negup),-1.,r(1,joint,ixy),-1.,
     &          rnet)
            call lincom_rotrans(hcum(1,neglo),1.,rnet,1.,rnet)
c             
c             now add half of the net needed to the upper adjustment
c             subtract half from the lower adjustment
c             
            call lincom_rotrans(rnet,0.5,hadj(1,negup),1.
     &          ,hadj(1,negup))
            nhadjsum(negup)=nhadjsum(negup)+1
            call lincom_rotrans(rnet,-0.5,hadj(1,neglo),1.
     &          ,hadj(1,neglo))
            nhadjsum(neglo)=nhadjsum(neglo)+1
          enddo
        enddo
c         
c         get the average adjustment, adjust hcum with it
c         
        dxsum=0.
        dysum=0.
        do i=1,numneg
          erradj=erradj+(abs(hadj(2,i))+abs(hadj(3,i))+
     &        abs(hadj(1,i))*maxswing)/nhadjsum(i)
          call lincom_rotrans(hadj(1,i),1./nhadjsum(i),hcum(1,i),1.
     &        ,hcum(1,i))
          dxsum=dxsum+hcum(2,i)
          dysum=dysum+hcum(3,i)
        enddo
c         
      enddo                                     !end of cycle
c       
c       shift all dx and dy to have a mean of zero
c       
      do i=1,numneg
        hcum(1,i)=hcum(1,i)-dxsum/numneg
        hcum(2,i)=hcum(2,i)-dysum/numneg
      enddo
c       
c       compute the h function (and hinv) centered on corner of frame
c       
      do ipc=1,npclist
        if(izpclist(ipc).eq.izsect)then
          do i=1,numneg
            if(negind(i).eq. neglist(ipc))then
              call recen_rotrans(hcum(1,i),float(ixpclist(ipc))
     &            ,float(iypclist(ipc)),hfram)
              h(1,1,ipc)=cosd(hfram(1))
              h(2,1,ipc)=sind(hfram(1))
              h(1,2,ipc)=-h(2,1,ipc)
              h(2,2,ipc)=h(1,1,ipc)
              h(1,3,ipc)=hfram(2)
              h(2,3,ipc)=hfram(3)
              call xfinvert(h(1,1,ipc),hinv(1,1,ipc))
            endif
          enddo
        endif
      enddo
      return
      end subroutine findMultinegTransforms


c       FIND THE SHIFTS OF EACH PIECE THAT BEST ALIGN THE PIECES
c
      subroutine getBestPieceShifts()
      real*4 edispMean
c       
c       Set the multineg flag to indicate that there are h transforms
      multng=.true.
      do ixy=1,2
        edispMean = 0.
        do iedge=1,nedge(ixy)
          if(izpclist(ipiecelower(iedge,ixy)).eq.izsect)then
c             
c             need displacements implied by edges, unless this is to be
c             done by old cross-correlation only
c             
            if(.not.xclegacy)then
              call edgeswap(iedge,ixy,inde)
c               
c               compute the mean current displacement of upper relative
c               to lower implied by the d[xy]mean
c               
              sumx=0.
              sumy=0.
              do ix=1,nxgrbf(inde)
                do iy=1,nygrbf(inde)
                  sumx=sumx+dxgrbf(ix,iy,inde)
                  sumy=sumy+dygrbf(ix,iy,inde)
                enddo
              enddo  
              dxgridmean(iedge,ixy)=sumx / (nxgrbf(inde)*nygrbf(inde))
              dygridmean(iedge,ixy)=sumy / (nxgrbf(inde)*nygrbf(inde))
c               
c               adjust these by the current displacements implied by
c               starting and offset coordinates of the edge areas
c               
              if(ixy.eq.1)then
                dxgridmean(iedge,ixy)=dxgridmean(iedge,ixy)+
     &              ixofsbf(inde)-ixgrdstbf(inde)+nxin-nxoverlap
                dygridmean(iedge,ixy)=dygridmean(iedge,ixy)+
     &              iyofsbf(inde)-iygrdstbf(inde)
              else
                dxgridmean(iedge,ixy)=dxgridmean(iedge,ixy)+
     &              ixofsbf(inde)-ixgrdstbf(inde)
                dygridmean(iedge,ixy)=dygridmean(iedge,ixy)+
     &              iyofsbf(inde)-iygrdstbf(inde)+nyin-nyoverlap
              endif
c               
c               But if this edge is skipped for edge functions and included
c               for finding shifts, substitute the (read-in) correlation shift
              if (ifskipEdge(iedge,ixy) .eq. 1) then
                dxgridmean(iedge, ixy) = -edgedispx(iedge, ixy)
                dygridmean(iedge, ixy) = -edgedispy(iedge, ixy)
              endif
            endif
c             
            if(.not.fromedge.and..not.xcreadin.and.
     &          .not.(ifsloppy.eq.1.and.ifoldedge.eq.0))then
c               
c               If the edges of the image are lousy, it's better to use
c               correlation, so here is this option.  Compute the
c               correlations unless doing this by edges only,
c               if they aren't already available
c               
              if (ifskipEdge(iedge,ixy) .gt. 0) then
                xdisp = 0.
                ydisp = 0.
              else
                call shuffler(ipiecelower(iedge,ixy),indlo)
                call shuffler(ipieceupper(iedge,ixy),indup)
                
                call getExtraIndents(ipiecelower(iedge,ixy), 
     &              ipieceupper(iedge,ixy), ixy, delIndent)
                indentXC = 0
                if (delIndent(ixy) .gt. 0. .and. ifillTreatment .eq. 1)
     &              indentXC = int(delIndent(ixy)) + 1
                call xcorredge(array(indlo),array(indup),
     &              ixy,xdisp,ydisp,xclegacy,indentXC)
              endif
              edgedispx(iedge,ixy)=xdisp
              edgedispy(iedge,ixy)=ydisp
            endif
c             write(*,'(1x,a,2i4,a,2f8.2,a,2f8.2)')
c             &               char(ixy+ichar('W'))//' edge, pieces'
c             &               ,ipiecelower(iedge,ixy),ipieceupper(iedge,ixy),
c             &               '  dxygridmean:',dxgridmean(iedge,ixy),
c             &               dygridmean(iedge,ixy),'  xcorr:',-xdisp,-ydisp
c             dxgridmean(iedge,ixy)=-xdisp
c             dygridmean(iedge,ixy)=-ydisp
c             endif
          endif
          if (ixy .eq. 1)
     &        edispMean = edispMean + edgedispx(iedge,ixy) / nedge(ixy)
          if (ixy .eq. 2) 
     &        edispMean = edispMean + edgedispy(iedge,ixy) / nedge(ixy)
        enddo
        if ((ixy .eq. 1 .and. abs(edispMean * nxpieces) .gt. nxin * 3) .or.
     &      (ixy .eq. 2 .and. abs(edispMean * nypieces) .gt. nyin * 3))
     &      write(*,'(/,a,f8.0,a)')'WARNING: mean edge shift of', edispMean,
     &      ' in '// char(ixy+ichar('W'))//' may give artifacts; consider'//
     &      ' adjusting overlaps with edpiecepoint and -overlap option'
      enddo
c       
c       If there is only one piece in one direction, then do it from
c       correlation only unless directed to use the edge, because the error
c       is zero in either case and it is impossible to tell which is better
      fromCorrOnly = xclegacy .or.
     &    (.not.fromedge .and. (nxpieces .eq. 1 .or. nypieces .eq. 1))
c       
      if (.not. fromedge) then
        call find_best_shifts(edgedispx, edgedispy,limedge,-1, izsect,h,
     &      nbestedge,beforemean,beforemax, aftermean(1), aftermax(1))
        indbest=1
      endif
      if (.not. fromCorrOnly) then
        call find_best_shifts(dxgridmean, dygridmean,limedge,1, izsect,h,
     &      nbestedge,beforemean,beforemax, aftermean(2), aftermax(2))
        indbest=2
      endif
c       
c       if first one was better based upon mean, redo it and reset the
c       index to 1
c       
      if(.not.(fromCorrOnly.or.fromedge) .and.
     &    (aftermean(1).lt.aftermean(2))) then
        call find_best_shifts(edgedispx, edgedispy,limedge,-1, izsect,h,
     &      nbestedge,beforemean,beforemax, aftermean(1), aftermax(1))
        indbest=1
      endif
      write(*,'(i5,a,2f7.1,a,a,2f7.2)')nbestedge,
     &    ' edges, mean&max error before:', beforemean,beforemax,
     &    ', after by ', edgexcorrtext(indbest),
     &    aftermean(indbest),aftermax(indbest)
      if (testMode)then
        write(*,'(a,i4,a,2f8.3,a,2f9.4)')' section:',
     &      izsect,'  gradient:',dmagPerUm(min(ilistz, numMagGrad)),
     &      rotPerUm(min(ilistz, numMagGrad)),
     &      '  mean, max error:', aftermean(indbest),aftermax(indbest)
        if (indbest .eq. 1) then
          call findBestGradient(edgedispx,edgedispy,limedge,-1,izsect,
     &        dmagnew,rotnew)
        else
          call findBestGradient(dxgridmean,dygridmean,limedge,1,izsect,
     &        dmagnew,rotnew)
        endif
        write(*,'(a,2f9.4)')' Total gradient implied by displacements:',
     &      dmagPerUm(min(ilistz, numMagGrad))+dmagnew,
     &      rotPerUm(min(ilistz, numMagGrad))+rotnew
      endif
      return
      end subroutine getBestPieceShifts


c       LOADS THE EDGES AND COMPUTES EDGE FRACTIONS FOR A PIXEL
c
      subroutine computeEdgeFractions()
      nedgesum=0
      do ixy=1,2
        do ied=1,numedges(ixy)
          iedge=inedge(ied,ixy)
          call edgeswap(iedge,ixy,indedg)
          indlower=inedlower(ied,ixy)
          indedge4(ied,ixy)=indedg
          if (edgesSeparated) then
            if(ixy.eq.1) then
              edgefrac4(ied,ixy)=0.5+(xinpiece(indlower)-
     &            (ixgrdstbf(indedg)+ (nxgrbf(indedg)-1) *intgrid(1)/2.)) /
     &            min(nxgrbf(indedg)*intgrid(1),iblend(1))
            else
              edgefrac4(ied,ixy)=0.5+(yinpiece(indlower)
     &            -(iygrdstbf(indedg)+(nygrbf(indedg)-1) *intgrid(1)/2.))/
     &            min(nygrbf(indedg)*intgrid(1),iblend(2))
            endif
          else
            if(ixy.eq.1) then
              bwof=max(0,nxgrbf(indedg)*intgrid(1)-iblend(1))/2
              edstart = max(0.55 * nxin, ixgrdstbf(indedg) -intgrid(1)/2.+bwof)
              edend = min(0.45 * nxin, ixofsbf(indedg)+ (nxgrbf(indedg)-0.5)*
     &            intgrid(1)-bwof) + ixgrdstbf(indedg) - ixofsbf(indedg)
              bwof=(xinpiece(indlower)-edstart)/(edend-edstart)
            else
              bwof=max(0,nygrbf(indedg)*intgrid(1)-iblend(2))/2
              edstart = max(0.55 * nyin, iygrdstbf(indedg) -intgrid(1)/2.+bwof)
              edend = min(0.45 * nyin, iyofsbf(indedg)+ (nygrbf(indedg)-0.5)*
     &            intgrid(1)-bwof) + iygrdstbf(indedg) - iyofsbf(indedg)
              bwof=(yinpiece(indlower)-edstart)/(edend-edstart)
            endif
            if (debug)print *,ied,ixy,edgefrac4(ied,ixy),bwof
            edgefrac4(ied,ixy)=bwof
          endif
          active4(ied,ixy)=edgefrac4(ied,ixy).lt..999
     &        .and.edgefrac4(ied,ixy).gt..001
          if(active4(ied,ixy)) nedgesum=nedgesum+1
          if(edgefrac4(ied,ixy).lt.0.)edgefrac4(ied,ixy)=0.
          if(edgefrac4(ied,ixy).gt.1.)edgefrac4(ied,ixy)=1.
          if (debug) print *,ied,ixy,edgefrac4(ied,ixy),active4(ied,ixy)
        enddo
      enddo
      return
      end subroutine computeEdgeFractions



c       SORT OUT THE CASES OF 1, 2, 3 or 4 PIECES AND COMPUTE PIXEL
c
      subroutine getPixelFromPieces()
      real*4 oneintrp
c       
      if(nactivep.le.1)then                     !ONE PIECE
        if(wll.gt.0.)then
          indpidef=indp1
        elseif(wlr.gt.0.)then
          indpidef=indp2
        elseif(wul.gt.0.)then
          indpidef=indp3
        else
          indpidef=indp4
        endif
        if(nactivep.eq.0)indpidef=1
c         
        call shuffler(inpiece(indpidef),indbray)
        pixval=oneintrp(array(indbray),nxin,nyin, xinpiece(indpidef),
     &      yinpiece(indpidef), inpiece(indpidef))
c         
c         ONE EDGE, TWO PIECES
c         
      elseif(nactivep.eq.2)then
c         
c         find the pieces around the edge, set edge index
c         
        if(wll.gt.0..and.wlr.gt.0.)then
          indpl=indp1
          indpu=indp2
          w1=wll
          indedg=indedge4(inde12,1)
        elseif(wul.gt.0..and.wur.gt.0.)then
          indpl=indp3
          indpu=indp4
          w1=wul
          indedg=indedge4(inde34,1)
        elseif(wll.gt.0..and.wul.gt.0.)then
          indpl=indp1
          indpu=indp3
          w1=wll
          indedg=indedge4(inde13,2)
        else
          indpl=indp2
          indpu=indp4
          w1=wlr
          indedg=indedge4(inde24,2)
        endif
c         
c         set up pieces #'s, and starting coords
c         
        ipiece1=inpiece(indpl)
        ipiece2=inpiece(indpu)
        x1=xinpiece(indpl)
        y1=yinpiece(indpl)
        w2=1.-w1
c         
c         set up to solve equation for (x1,y1) and (x2,y2)
c         given their difference and the desired xg,yg
c         
        xgconst=xg-w1*ixpclist(ipiece1) - w2*ixpclist(ipiece2)
        ygconst=yg-w1*iypclist(ipiece1) - w2*iypclist(ipiece2)
        if(multng)then
          xgconst=xgconst-w1*h(1,3,ipiece1) - w2*h(1,3,ipiece2)
          ygconst=ygconst-w1*h(2,3,ipiece1) - w2*h(2,3,ipiece2)
          c11=w1*h(1,1,ipiece1) + w2*h(1,1,ipiece2)
          c12=w1*h(1,2,ipiece1) + w2*h(1,2,ipiece2)
          c21=w1*h(2,1,ipiece1) + w2*h(2,1,ipiece2)
          c22=w1*h(2,2,ipiece1) + w2*h(2,2,ipiece2)
          denom=c11*c22-c12*c21
          fb11=w2*h(1,1,ipiece2)
          fb12=w2*h(1,2,ipiece2)
          fb21=w2*h(2,1,ipiece2)
          fb22=w2*h(2,2,ipiece2)
        endif
c         
c         in this loop, use the difference between (x1,y1)
c         and (x2,y2) at the one place to solve for values of
c         those coords; iterate until stabilize
c         
        x1last=-100.
        y1last=-100.
        iter=1
        do while (iter.le.niter .and. (abs(x1-x1last) .gt. 0.01
     &      .or. abs(y1-y1last) .gt. 0.01))
          call dxydgrinterp(x1,y1,indedg,x2,y2,dden)
          x1last=x1
          y1last=y1
          dx2=x2-x1
          dy2=y2-y1
          if(multng)then
            bx=xgconst-fb11*dx2-fb12*dy2
            by=ygconst-fb21*dx2-fb22*dy2
            x1=(bx*c22-by*c12)/denom
            y1=(by*c11-bx*c21)/denom
          else
            x1=xgconst-w2*dx2
            y1=ygconst-w2*dy2
          endif
          x2=x1+dx2
          y2=y1+dy2
          iter=iter+1
        enddo
c         
c         get the pixel from the piece with the bigger weight
c         and adjust density by the difference across edge
c         Except average them within 4 pixels of center!
c         
        if(abs(w1-w2)*max(iblend(1),iblend(2)).lt.2.5)then
          call shuffler(ipiece1,indbray1)
          call shuffler(ipiece2,indbray2)
          pixval=w1*oneintrp(array(indbray1),nxin, nyin, x1,y1,ipiece1) +
     &        w2*oneintrp(array(indbray2),nxin, nyin, x2,y2,ipiece2)
        elseif(w1.gt.w2)then
          call shuffler(ipiece1,indbray)
          pixval=oneintrp(array(indbray),nxin,nyin, x1,y1,ipiece1) + w2*dden
        else
          call shuffler(ipiece2,indbray)
          pixval=oneintrp(array(indbray),nxin,nyin, x2,y2,ipiece2) - w1*dden
        endif
c         
c         THREE PIECES AND TWO EDGES
c         
      elseif(nactivep.eq.3)then
c         
c         now decide between the three cases of divergent, serial, or
c         convergent functions, and assign pieces and weights accordingly
c         
c         DIVERGENT: if lower piece is same for x and y edge
c         
        if(wur.le.0.)then
          w1=wll
          w2=wlr
          w3=wul
          jndp1=indp1
          jndp2=indp2
          jndp3=indp3
          ind12edg=indedge4(inde12,1)
          ind13edg=indedge4(inde13,2)
          icortyp=1
c           
c           CONVERGENT: if upper piece same for x and y edge
c           
        elseif(wll.le.0.)then
          w3=wur
          w1=wul
          w2=wlr
          jndp1=indp3
          jndp2=indp2
          jndp3=indp4
          ind13edg=indedge4(inde34,1)
          ind23edg=indedge4(inde24,2)
          icortyp=2
c           
c           SERIAL: lower of one is the upper of the other
c           
        else
          w1=wll
          w3=wur
          jndp1=indp1
          jndp3=indp4
          if(wlr.le.0.)then
            w2=wul
            jndp2=indp3
            ind12edg=indedge4(inde13,2)
            ind23edg=indedge4(inde34,1)
          else
            w2=wlr
            jndp2=indp2
            ind12edg=indedge4(inde12,1)
            ind23edg=indedge4(inde24,2)
          endif
          icortyp=3
        endif
        
        ipiece1=inpiece(jndp1)
        ipiece2=inpiece(jndp2)
        ipiece3=inpiece(jndp3)
        x1=xinpiece(jndp1)
        y1=yinpiece(jndp1)
        wmax=max(w1,w2,w3)
c         
c         set up to solve equations for new (x1,y1), (x2,y2)
c         and (x3,y3) given the differences between them and
c         the desired weighted coordinate (xg,yg)
c         
        xgconst=xg-w1*ixpclist(ipiece1)-
     &      w2*ixpclist(ipiece2)-w3*ixpclist(ipiece3)
        ygconst=yg-w1*iypclist(ipiece1)-
     &      w2*iypclist(ipiece2)-w3*iypclist(ipiece3)
        if(multng)then
          xgconst=xgconst-w1*h(1,3,ipiece1)-w2*h(1,3,ipiece2)-w3*h(1,3,ipiece3)
          ygconst=ygconst-w1*h(2,3,ipiece1)-w2*h(2,3,ipiece2)-w3*h(2,3,ipiece3)
          c11=w1*h(1,1,ipiece1)+w2* h(1,1,ipiece2)+w3*h(1,1,ipiece3)
          c12=w1*h(1,2,ipiece1)+w2* h(1,2,ipiece2)+w3*h(1,2,ipiece3)
          c21=w1*h(2,1,ipiece1)+w2* h(2,1,ipiece2)+w3*h(2,1,ipiece3)
          c22=w1*h(2,2,ipiece1)+w2* h(2,2,ipiece2)+w3*h(2,2,ipiece3)
          denom=c11*c22-c12*c21
          f2b11=w2*h(1,1,ipiece2)
          f2b12=w2*h(1,2,ipiece2)
          f2b21=w2*h(2,1,ipiece2)
          f2b22=w2*h(2,2,ipiece2)
          f3b11=w3*h(1,1,ipiece3)
          f3b12=w3*h(1,2,ipiece3)
          f3b21=w3*h(2,1,ipiece3)
          f3b22=w3*h(2,2,ipiece3)
        endif
c         
c         do iteration, starting with coordinates and solving
c         for new coordinates until convergence
c         
        x1last=-100.
        y1last=-100.
        iter=1
        do while(iter.le.niter.and.(abs(x1-x1last) .gt.0.01
     &      .or.abs(y1-y1last).gt.0.01))
          if(icortyp.eq.1)then
c             
c             divergent case
c             
            call dxydgrinterp(x1,y1,ind12edg,x2,y2,dden12)
            call dxydgrinterp(x1,y1,ind13edg,x3,y3,dden13)
          elseif(icortyp.eq.2)then
c             
c             convergent case
c             
            call dxydgrinterp(x1,y1,ind13edg,x3,y3,dden13)
c             
            if(iter.eq.1)then
              x2=x3+ixgrdstbf(ind23edg) -ixofsbf(ind23edg)
              y2=y3+iygrdstbf(ind23edg) -iyofsbf(ind23edg)
            endif
            call dxydgrinterp(x2,y2,ind23edg,x3t,y3t,dden23)
            x2=x2+x3-x3t
            y2=y2+y3-y3t
          else
c             
c             serial case
c             
            call dxydgrinterp(x1,y1,ind12edg,x2,y2 ,dden12)
            call dxydgrinterp(x2,y2,ind23edg,x3,y3 ,dden23)
          endif
c           
c           solve equations for new coordinates
c           
          x1last=x1
          y1last=y1
          dx2=x2-x1
          dy2=y2-y1
          dx3=x3-x1
          dy3=y3-y1
          if(multng)then
            bx=xgconst-f2b11*dx2-f2b12*dy2 -f3b11*dx3-f3b12*dy3
            by=ygconst-f2b21*dx2-f2b22*dy2 -f3b21*dx3-f3b22*dy3
            x1=(bx*c22-by*c12)/denom
            y1=(by*c11-bx*c21)/denom
          else
            x1=xgconst-w2*dx2-w3*dx3
            y1=ygconst-w2*dy2-w3*dy3
          endif
          x2=x1+dx2
          y2=y1+dy2
          x3=x1+dx3
          y3=y1+dy3
          iter=iter+1
        enddo
c         
c         take pixel from the piece with the highest weight
c         
        if(w1.eq.wmax)then
          call shuffler(ipiece1,indbray)
          pixval=oneintrp(array(indbray),nxin,nyin, x1,y1,ipiece1)
        elseif(w2.eq.wmax)then
          call shuffler(ipiece2,indbray)
          pixval=oneintrp(array(indbray),nxin,nyin, x2,y2,ipiece2)
        else
          call shuffler(ipiece3,indbray)
          pixval=oneintrp(array(indbray),nxin,nyin, x3,y3,ipiece3)
        endif
c         
c         Adjust for differences in mean density: divergent
c         
        if(icortyp.eq.1)then
          if(w1.eq.wmax)then
            pixval=pixval + w2*dden12 + w3*dden13
          elseif(w2.eq.wmax)then
            pixval=pixval+(w2-1.)*dden12+w3*dden13
          else
            pixval=pixval+w2*dden12+(w3-1.)*dden13
          endif
c           
c           convergent
c           
        elseif(icortyp.eq.2)then
          if(w1.eq.wmax)then
            pixval=pixval-(w1-1.)*dden13-w2*dden23
          elseif(w2.eq.wmax)then
            pixval=pixval-w1*dden13-(w2-1.)*dden23
          else
            pixval=pixval-w1*dden13 -w2*dden23
          endif
c           
c           serial
c           
        else
c           
          if(w1.eq.wmax)then
            pixval=pixval-(w1-1.)*dden12+w3*dden23
          elseif(w2.eq.wmax)then
            pixval=pixval-w1*dden12+w3*dden23
          else
            pixval=pixval-w1*dden12+(w3-1.)*dden23
          endif
        endif
      else
c         
c         FOUR PIECES, THREE EDGES USED FOR SOLUTION
c         
c         First, need to have only 3 active edges, so if
c         there are four, knock one out based on ex and ey
c         
        if(nedgesum.eq.4)then
          emin=min(ex,1.-ex,ey,1.-ey)
          if(ex.eq.emin)then
            active4(inde34,1)=.false.
          elseif(1.-ex.eq.emin)then
            active4(inde12,1)=.false.
          elseif(ey.eq.emin)then
            active4(inde24,2)=.false.
          else
            active4(inde13,2)=.false.
          endif
        endif
c         
c         here there is always a serial chain from ll to ur, through either ul
c         or lr, and in each case the fourth piece is either divergent from
c         the first or converging on the fourth
c         
        w1=wll
        w3=wur
        if(.not.active4(inde12,1).or.
     &      .not.active4(inde24,2))then
          w2=wul
          w4=wlr
          jndp2=indp3
          jndp4=indp2
          ind12edg=indedge4(inde13,2)
          ind23edg=indedge4(inde34,1)
          if(.not.active4(inde24,2))then
            ind14edg=indedge4(inde12,1)
            icortyp=1                           !divergent
          else
            ind43edg=indedge4(inde24,2)
            icortyp=2                           !convergent
          endif
        else
          w2=wlr
          w4=wul
          jndp2=indp2
          jndp4=indp3
          ind12edg=indedge4(inde12,1)
          ind23edg=indedge4(inde24,2)
          if(.not.active4(inde34,1))then
            ind14edg=indedge4(inde13,2)
            icortyp=1
          else
            ind43edg=indedge4(inde34,1)
            icortyp=2
          endif
        endif
c         
        ipiece1=inpiece(indp1)
        ipiece2=inpiece(jndp2)
        ipiece3=inpiece(indp4)
        ipiece4=inpiece(jndp4)
        x1=xinpiece(indp1)
        y1=yinpiece(indp1)
        wmax=max(w1,w2,w3,w4)
c         
c         set up to solve equations for new (x1,y1), (x2,y2)
c         (x3,y3), and (x4,y4) given the differences between
c         them and the desired weighted coordinate (xg,yg)
c         
        xgconst=xg-w1*ixpclist(ipiece1)-w2*ixpclist(ipiece2)
     &      -w3*ixpclist(ipiece3)-w4*ixpclist(ipiece4)
        ygconst=yg-w1*iypclist(ipiece1)-w2*iypclist(ipiece2)
     &      -w3*iypclist(ipiece3)-w4*iypclist(ipiece4)
        if(multng)then
          xgconst=xgconst-w1*h(1,3,ipiece1)-w2*h(1,3,ipiece2)
     &        -w3*h(1,3,ipiece3)-w4*h(1,3,ipiece4)
          ygconst=ygconst-w1*h(2,3,ipiece1)-w2*h(2,3,ipiece2)
     &        -w3*h(2,3,ipiece3)-w4*h(2,3,ipiece4)
          c11=w1*h(1,1,ipiece1)+w2*h(1,1,ipiece2)
     &        +w3*h(1,1,ipiece3)+w4*h(1,1,ipiece4)
          c12=w1*h(1,2,ipiece1)+w2*h(1,2,ipiece2)
     &        +w3*h(1,2,ipiece3)+w4*h(1,2,ipiece4)
          c21=w1*h(2,1,ipiece1)+w2*h(2,1,ipiece2)
     &        +w3*h(2,1,ipiece3)+w4*h(2,1,ipiece4)
          c22=w1*h(2,2,ipiece1)+w2*h(2,2,ipiece2)
     &        +w3*h(2,2,ipiece3)+w4*h(2,2,ipiece4)
          denom=c11*c22-c12*c21
          f2b11=w2*h(1,1,ipiece2)
          f2b12=w2*h(1,2,ipiece2)
          f2b21=w2*h(2,1,ipiece2)
          f2b22=w2*h(2,2,ipiece2)
          f3b11=w3*h(1,1,ipiece3)
          f3b12=w3*h(1,2,ipiece3)
          f3b21=w3*h(2,1,ipiece3)
          f3b22=w3*h(2,2,ipiece3)
          f4b11=w4*h(1,1,ipiece4)
          f4b12=w4*h(1,2,ipiece4)
          f4b21=w4*h(2,1,ipiece4)
          f4b22=w4*h(2,2,ipiece4)
        endif
c         
c         do iteration, starting with coordinates and solving
c         for new coordinates until convergence
c         
        x1last=-100.
        y1last=-100.
        iter=1
        do while(iter.le.niter.and.(abs(x1-x1last) .gt.0.01
     &      .or.abs(y1-y1last).gt.0.01))
          call dxydgrinterp(x1,y1,ind12edg,x2,y2 ,dden12)
          call dxydgrinterp(x2,y2,ind23edg,x3,y3 ,dden23)
          if(icortyp.eq.1)then
c             
c             divergent case
c             
            call dxydgrinterp(x1,y1,ind14edg,x4,y4 ,dden14)
          else
c             
c             convergent case
c             
            if(iter.eq.1)then
              x4=x3+ixgrdstbf(ind43edg) -ixofsbf(ind43edg)
              y4=y3+iygrdstbf(ind43edg) -iyofsbf(ind43edg)
            endif
            call dxydgrinterp(x4,y4,ind43edg,x3t,y3t ,dden43)
            x4=x4+x3-x3t
            y4=y4+y3-y3t
          endif
c           
c           solve equations for new coordinates
c           
          x1last=x1
          y1last=y1
          dx2=x2-x1
          dy2=y2-y1
          dx3=x3-x1
          dy3=y3-y1
          dx4=x4-x1
          dy4=y4-y1
          if(multng)then
            bx=xgconst-f2b11*dx2-f2b12*dy2
     &          -f3b11*dx3-f3b12*dy3-f4b11*dx4-f4b12*dy4
            by=ygconst-f2b21*dx2-f2b22*dy2
     &          -f3b21*dx3-f3b22*dy3-f4b21*dx4-f4b22*dy4
            x1=(bx*c22-by*c12)/denom
            y1=(by*c11-bx*c21)/denom
          else
            x1=xgconst-w2*dx2-w3*dx3-w4*dx4
            y1=ygconst-w2*dy2-w3*dy3-w4*dy4
          endif
          x2=x1+dx2
          y2=y1+dy2
          x3=x1+dx3
          y3=y1+dy3
          x4=x1+dx4
          y4=y1+dy4
          iter=iter+1
        enddo
c         
c         take pixel from the piece with the highest weight
c         and adjust density appropriately for case
c         
        if(w1.eq.wmax)then
          call shuffler(ipiece1,indbray)
          pixval=oneintrp(array(indbray),nxin,nyin, x1,y1,ipiece1)
          if(icortyp.eq.1)then
            pixval=pixval+(w2+w3)*dden12+w3*dden23+w4*dden14
          else
            pixval=pixval+(1.-w1)*dden12+(w3+w4)*dden23 -w4*dden43
          endif
        elseif(w2.eq.wmax)then
          call shuffler(ipiece2,indbray)
          pixval=oneintrp(array(indbray),nxin,nyin, x2,y2,ipiece2)
          if(icortyp.eq.1)then
            pixval=pixval+(w2+w3-1.)*dden12+w3*dden23+ w4*dden14
          else
            pixval=pixval-w1*dden12+(w3+w4)*dden23-w4*dden43
          endif
        elseif(w3.eq.wmax)then
          call shuffler(ipiece3,indbray)
          pixval=oneintrp(array(indbray),nxin,nyin, x3,y3,ipiece3)
          if(icortyp.eq.1)then
            pixval=pixval+(w2+w3-1.)*dden12+(w3-1.)*dden23+ w4*dden14
          else
            pixval=pixval-w1*dden12+(w3+w4-1.)*dden23 -w4*dden43
          endif
        else
          call shuffler(ipiece4,indbray)
          pixval=oneintrp(array(indbray),nxin,nyin, x4,y4,ipiece4)
          if(icortyp.eq.1)then
            pixval=pixval+(w2+w3)*dden12+w3*dden23 +(w4-1.)*dden14
          else
            pixval=pixval-w1*dden12+(w3+w4-1.)*dden23 -(w4-1.)*dden43
          endif
        endif
      endif
      end subroutine getPixelFromPieces

      end

c       GET INDICES OF PIECES AND EDGES AND THE WEIGHTING OF EACH PIECE
c
      subroutine getPieceIndicesAndWeighting(useEdges)
      use blendvars
      implicit none
      logical*4 useEdges
      real*4 er3,eb3,el4,eb4,fx,fy,dr1,dt1,dl2,dt2,dr3,db3,dl4,db4,dla,dra
      real*4 dba,dta,ax,ay,f12,f13,f34,f24,er1,et1,el2,et2
      integer*4 ipfrom, ipxto, ipyto, ixyDisjoint, ied, ipc,i, indedg, ixy, jedge
      real*4 bwof, edstart, edend, wsum

c       for now, numbers 1 to 4 represent lower left, lower right, upper left,
c       and upper right 
      if(numpieces.gt.1)then
        indp1=0                                 !index of piece 1, 2, 3, or 4
        indp2=0                                 !if the point is in them
        indp3=0
        indp4=0
        inde12=0                                !index of edges between 1 and 2
        inde13=0                                !1 and 3, etc
        inde34=0
        inde24=0
        f12=0.                                  !edge fractions between 1 and 2
        f13=0.                                  !1 and 3, etc
        f34=0.
        f24=0.
        er1=0.                                  !end fractions to right and top
        et1=0.                                  !of piece 1, left and bottom of
        el2=0.                                  !piece 3, etc
        et2=0.
        er3=0.
        eb3=0.
        el4=0.
        eb4=0.
        fx=0.                                   !the composite edge fractions
        fy=0.                                   !in x and y directions
        if(numpieces.gt.2)then
          do ipc=1,numpieces
            i = 2 * (inpyframe(ipc) - minyframe) + 1 +
     &          inpxframe(ipc) - minxframe
            indp1234(i) = ipc
          enddo
          do ied=1,numedges(1)
            if(inedlower(ied,1).eq.indp1)inde12=ied
            if(inedlower(ied,1).eq.indp3)inde34=ied
          enddo
          do ied=1,numedges(2)
            if(inedlower(ied,2).eq.indp1)inde13=ied
            if(inedlower(ied,2).eq.indp2)inde24=ied
          enddo
          if (debug) print *,inde12,inde34,inde13,inde24
          if(inde12.ne.0)f12=edgefrac4(inde12,1)
          if(inde34.ne.0)f34=edgefrac4(inde34,1)
          if(inde13.ne.0)f13=edgefrac4(inde13,2)
          if(inde24.ne.0)f24=edgefrac4(inde24,2)
          if (debug) write(*,'(a,4i6,a,4f8.4)')'piece 1234',
     &        inpiece(indp1),inpiece(indp2),inpiece(indp3),
     &        inpiece(indp4),'  edge fractions',f12,f34,f13,f24
        else
c           
c           two piece case - identify as upper or lower to
c           simplify computation of end fractions
c           
          if(numedges(1).gt.0)then
            fx=edgefrac4(1,1)
            if(0.5*(yinpiece(1)+yinpiece(2)).gt.nyin/2)then
              inde12=1
              if(xinpiece(1).gt.xinpiece(2))then
                indp1=1
                indp2=2
              else
                indp1=2
                indp2=1
              endif
            else
              inde34=1
              fy=1.
              if(xinpiece(1).gt.xinpiece(2))then
                indp3=1
                indp4=2
              else
                indp3=2
                indp4=1
              endif
            endif
          else
            fy=edgefrac4(1,2)
            if(0.5*(xinpiece(1)+xinpiece(2)).gt.nxin/2)then
              inde13=1
              if(yinpiece(1).gt.yinpiece(2))then
                indp1=1
                indp3=2
              else
                indp1=2
                indp3=1
              endif
            else
              inde24=1
              fx=1.
              if(yinpiece(1).gt.yinpiece(2))then
                indp2=1
                indp4=2
              else
                indp2=2
                indp4=1
              endif
            endif
          endif
        endif
c         
c         get distance to top, right, bottom, or left edges
c         as needed for each piece, and compute end fractions
c         
        if(indp1.gt.0)then
          dr1=nxin-1.-xinpiece(indp1)
          dt1=nyin-1.-yinpiece(indp1)
          er1=(dr1+1.)/iblend(1)
          et1=dt1/iblend(2)
        endif                   
        if(indp2.gt.0)then
          dl2=xinpiece(indp2)
          dt2=nyin-1.-yinpiece(indp2)
          el2=(dl2+1.)/iblend(1)
          et2=(dt2+1.)/iblend(2)
        endif                   
        if(indp3.gt.0)then
          dr3=nxin-1.-xinpiece(indp3)
          db3=yinpiece(indp3)
          er3=(dr3+1.)/iblend(1)
          eb3=(db3+1.)/iblend(2)
        endif                   
        if(indp4.gt.0)then
          dl4=xinpiece(indp4)
          db4=yinpiece(indp4)
          el4=(dl4+1.)/iblend(1)
          eb4=(db4+1.)/iblend(2)
        endif
c         
c         If 3 pieces, check for a disjoint edge: use previous state if pieces
c         match the last time
        if (numPieces .eq. 3 .and. anyDisjoint(minxframe, minyframe)) then
          if (inpiece(indp1) .eq. lastp1 .and. inpiece(indp2) .eq.lastp2 .and.
     &        inpiece(indp3) .eq. lastp3 .and. inpiece(indp4) .eq. lastp4) then
            ixyDisjoint = lastxyDisjoint
          else
c             
c             if pieces have changed, analyze edge starts and ends.
c             First find the missing edges and set some indexes
            ixyDisjoint = 0
            if (indp1 .eq. 0) then
              inedge(2,1) = iedgelower(inpiece(indp2), 1)
              inedge(2,2) = iedgelower(inpiece(indp3), 2)
              ipfrom = indp4
              ipxto = indp2
              ipyto = indp3
            else if (indp2 .eq. 0) then
              inedge(2,1) = iedgeupper(inpiece(indp1), 1)
              inedge(2,2) = iedgelower(inpiece(indp4), 2)
              ipfrom = indp3
              ipxto = indp1
              ipyto = indp4
            else if (indp3 .eq. 0) then
              inedge(2,1) = iedgelower(inpiece(indp4), 1)
              inedge(2,2) = iedgeupper(inpiece(indp1), 2)
              ipfrom = indp2
              ipxto = indp4
              ipyto = indp1
            else if (indp4 .eq. 0) then
              inedge(2,1) = iedgeupper(inpiece(indp3), 1)
              inedge(2,2) = iedgeupper(inpiece(indp2), 2)
              ipfrom = indp1
              ipxto = indp3
              ipyto = indp2
            endif
c             
c             Replace with used edge numbers
            if (useEdges) then
              do ixy = 1, 2
                call findEdgeToUse(inedge(2, ixy), ixy, jedge)
                if (jedge .ne. 0) inedge(2, ixy) = jedge
              enddo
            endif
c             
c             Analyze X edge first then Y edge - don't worry if both are bad
            if (inedge(2,1) .gt. 0 .and. inedge(2,2) .gt. 0) then
c               
c               Get start of already included X edge and translate it into
c               piece on missing X edge, and get end of the missing X edge
c               in that piece.  If end is before start, it is disjoint
              if (indp1 .eq. 0 .or. indp3 .eq. 0) then
                indedg = indedge4(1,1)
                bwof=max(0,nxgrbf(indedg)*intgrid(1)-iblend(1))/2
                edstart = ixofsbf(indedg) - intgrid(1)/2. + bwof +
     &              xinpiece(ipxto) - xinpiece(ipfrom)
                call edgeswap(inedge(2,1),1,indedg)
                bwof=max(0,nxgrbf(indedg)*intgrid(1)-iblend(1))/2
                edend = ixofsbf(indedg) + (nxgrbf(indedg)-0.5)*intgrid(1)-bwof
                if (edend .le. edstart) ixyDisjoint = 1
              else
c                 
c                 Other two cases, compare end of existing to start of missing
                indedg = indedge4(1,1)
                bwof=max(0,nxgrbf(indedg)*intgrid(1)-iblend(1))/2
                edend = ixgrdstbf(indedg) +(nxgrbf(indedg)-0.5)*intgrid(1) -
     &              bwof +  xinpiece(ipxto) - xinpiece(ipfrom)
                call edgeswap(inedge(2,1),1,indedg)
                bwof=max(0,nxgrbf(indedg)*intgrid(1)-iblend(1))/2
                edstart = ixgrdstbf(indedg) - intgrid(1)/2. + bwof
                if (edend .le. edstart) ixyDisjoint = 1
              endif
c               
c               Similarly two cases for Y
              if (indp1 .eq. 0 .or. indp2 .eq. 0) then
                indedg = indedge4(1,2)
                bwof=max(0,nygrbf(indedg)*intgrid(1)-iblend(2))/2
                edstart = iyofsbf(indedg) - intgrid(1)/2. + bwof +
     &              yinpiece(ipyto) - yinpiece(ipfrom)
                call edgeswap(inedge(2,2),2,indedg)
                bwof=max(0,nygrbf(indedg)*intgrid(1)-iblend(2))/2
                edend = iyofsbf(indedg) + (nygrbf(indedg)-0.5)*intgrid(1)-bwof
                if (edend .le. edstart) ixyDisjoint = 2
              else
                indedg = indedge4(1,2)
                bwof=max(0,nygrbf(indedg)*intgrid(1)-iblend(2))/2
                edend = iygrdstbf(indedg) +(nygrbf(indedg)-0.5)*intgrid(1) -
     &              bwof +  yinpiece(ipyto) - yinpiece(ipfrom)
                call edgeswap(inedge(2,2),2,indedg)
                bwof=max(0,nygrbf(indedg)*intgrid(1)-iblend(2))/2
                edstart = iygrdstbf(indedg) - intgrid(1)/2. + bwof
                if (edend .le. edstart) ixyDisjoint = 2
              endif
c               
c               If one was found, compute the start and end for the half of
c               the edge to be used on the other axis
              if (ixyDisjoint .eq. 1) then
                indedg = indedge4(1,2)
                bwof=max(0,nygrbf(indedg)*intgrid(1)-iblend(2))/2
                if (indp1 .eq. 0 .or. indp2 .eq. 0) then
                  startSkew = iyofsbf(indedg) +(nygrbf(indedg)-1)*intgrid(1)/2.
                  endSkew = iyofsbf(indedg) + (nygrbf(indedg)-0.5)*intgrid(1)-
     &                bwof
                else
                  startSkew = iygrdstbf(indedg) - intgrid(1)/2. + bwof
                  endSkew = iygrdstbf(indedg) +(nygrbf(indedg)-1)*intgrid(1)/2.
                endif
              else if (ixyDisjoint .eq. 2) then
                indedg = indedge4(1,1)
                bwof=max(0,nxgrbf(indedg)*intgrid(1)-iblend(1))/2
                if (indp1 .eq. 0 .or. indp3 .eq. 0) then
                  startSkew = ixofsbf(indedg) +(nxgrbf(indedg)-1)*intgrid(1)/2.
                  endSkew = ixofsbf(indedg) + (nxgrbf(indedg)-0.5)*intgrid(1)
     &                -bwof
                else
                  startSkew = ixgrdstbf(indedg) - intgrid(1)/2. + bwof
                  endSkew = ixgrdstbf(indedg) +(nxgrbf(indedg)-1)*intgrid(1)/2.
                endif
              endif
            endif
c             
c             Save for future use
            lastp1 = inpiece(indp1)
            lastp2 = inpiece(indp2)
            lastp3 = inpiece(indp3)
            lastp4 = inpiece(indp4)
            lastxyDisjoint = ixyDisjoint
          endif
c           
c           Now if there is disjoint edge, modify edge and end fractions to
c           start at the midpoint of the piece
          if (ixyDisjoint .eq. 1) then
            if (indp1 .eq. 0) then
              edgefrac4(1,2) = max(0., min(1., (yinpiece(indp4)-
     &            startSkew) / (endSkew-startSkew)))
              db4 = max(0., yinpiece(indp4) - startSkew)
              eb4 = (db4 + 1.) / iblend(2)
              if (debug) write(*,'(a,3f8.1,a,f8.4,f8.1,f8.4)')'s&e skew, y',
     &            startSkew,endSkew,yinpiece(indp4),
     &            ' mod ef, db4, eb4',edgefrac4(1,2),db4,eb4
            else if (indp2 .eq. 0) then
              edgefrac4(1,2) = max(0., min(1., (yinpiece(indp3)-
     &            startSkew) / (endSkew-startSkew)))
              db3 = max(0., yinpiece(indp3) - startSkew)
              eb3 = (db3 + 1.) / iblend(2)
            else if (indp3 .eq. 0) then
              edgefrac4(1,2) = max(0., min(1., (yinpiece(indp2)-
     &            startSkew) / (endSkew-startSkew)))
              dt2 = max(0., endSkew - yinpiece(indp2))
              et2 = (dt2 + 1.) / iblend(2)
            else
              edgefrac4(1,2) = max(0., min(1., (yinpiece(indp1)-
     &            startSkew) / (endSkew-startSkew)))
              dt1 = max(0., endSkew - yinpiece(indp1))
              et1 = (dt1 + 1.) / iblend(2)
            endif
          else if (ixyDisjoint .eq. 2) then
            if (indp1 .eq. 0) then
              edgefrac4(1,1) = max(0., min(1., (xinpiece(indp4)-
     &            startSkew) / (endSkew-startSkew)))
              dl4 = max(0., xinpiece(indp4) - startSkew)
              el4 = (dl4 + 1.) / iblend(1)
            else if (indp2 .eq. 0) then
              edgefrac4(1,1) = max(0., min(1., (xinpiece(indp3)-
     &            startSkew) / (endSkew-startSkew)))
              dr3 = max(0., endSkew - xinpiece(indp3))
              er3 = (dr3 + 1.) / iblend(1)
            else if (indp3 .eq. 0) then
              edgefrac4(1,1) = max(0., min(1., (xinpiece(indp2)-
     &            startSkew) / (endSkew-startSkew)))
              dl2 = max(0., xinpiece(indp2) - startSkew)
              el2 = (dl2 + 1.) / iblend(1)
            else
              edgefrac4(1,1) = max(0., min(1., (xinpiece(indp1)-
     &            startSkew) / (endSkew-startSkew)))
              dr1 = max(0., endSkew - xinpiece(indp1))
              er1 = (dr3 + 1.) / iblend(1)
            endif
          endif
        endif
c         
c         If there are 4 pieces, fx and fy are weighted sum of the f's in the
c         two overlapping edges.  The weights ex and ey are modified
c         fractional distances across the overlap zone.  First get the
c         distances to the borders of the overlap zone (dla, etc) and
c         use them to get absolute fractional distances across zone, ax in the
c         y direction and ay in the x direction.  They are used to make ex
c         slide from being a distance across the lower overlap to being
c         a distance across the upper overlap.  This gives continuity with the
c         edge in 2 or 3-piece cases
c         
        if(numpieces.eq.4) then
          dla=min(dl2,dl4)
          dra=min(dr1,dr3)
          dba=min(db3,db4)
          dta=min(dt1,dt2)
          ax=dba/(dta+dba)
          ay=dla/(dra+dla)
          ex=((1-ay)*db3+ay*db4)/ ((1-ay)*(dt1+db3)+ay*(dt2+db4))
          ey=((1-ax)*dl2+ax*dl4)/ ((1-ax)*(dr1+dl2)+ax*(dr3+dl4))
          fx=(1-ex)*f12+ex*f34
          fy=(1-ey)*f13+ey*f24
        elseif(numpieces.eq.3)then
c           
c           Three-piece case is simple, only two edges
c           
          fx=edgefrac4(1,1)
          fy=edgefrac4(1,2)
        endif
c         
c         weighting factors are a product of the two f's,
c         attenuated if necessary by fractional distance to
c         end of piece, then normalized to sum to 1.
c         
        wll=min(1-fx,et1)*min(1-fy,er1)
        wlr=min(fx,et2)*min(1-fy,el2)
        wul=min(1-fx,eb3)*min(fy,er3)
        wur=min(fx,eb4)*min(fy,el4)
        wsum=wll+wlr+wul+wur
        if(wsum.gt.0.)then
          wll=wll/wsum
          wlr=wlr/wsum
          wul=wul/wsum
          wur=wur/wsum
        endif
c         
c         count up active pieces implied by the w's
c         
        nactivep=0
        if(wll.gt.0.)nactivep=nactivep+1
        if(wlr.gt.0.)nactivep=nactivep+1
        if(wul.gt.0.)nactivep=nactivep+1
        if(wur.gt.0.)nactivep=nactivep+1
c         
c         filter out cross-corner 2-piece cases, pick the
c         piece where the point is most interior
c         
        if(nactivep.eq.2.and.wll*wur.gt.0.)then
          if(min(dt1,dr1).lt.min(db4,dl4))then
            wll=0.
          else
            wur=0.
          endif
          nactivep=1
        elseif(nactivep.eq.2.and.wul*wlr.gt.0.)then
          if(min(dt2,dl2).lt.min(db3,dr3))then
            wlr=0.
          else
            wul=0.
          endif
          nactivep=1
        endif                       
      else
c         
c         the one-piece case, avoid all that computation
c         
        nactivep=1
        indp1=1
        wll=1.
      endif
      if (debug)write(*,'(a,i3,a,4f8.4)')'Active',nactivep,'  weights',wll,wlr,wul,wur
      return
      end subroutine getPieceIndicesAndWeighting
