*****   XFMODEL.FOR*********************************************
*       Will take a model from wimp or imod, and either
*       a) use corresponding points in two sections to obtain a transformation
*       between the sections, or
*       b) transform the points in the model to match a new alignment of images
c       
c       For more details, see man page
*       
c       $Id$
c       
      implicit none
      include 'model.inc'
      integer nflimit,limpcl,idim,msizexr
      parameter (nflimit=100000,limpcl=100000)
      real*4 f(2,3,nflimit),g(2,3,nflimit),gtmp(2,3)
      integer*4 nxyz(3),mxyz(3),nx,ny,nz,mode
      equivalence (nxyz(1),nx),(nxyz(2),ny),(nxyz(3),nz)
      real*4 delt(3)/0.,0.,0./
      integer*4 nsec(nflimit),listz(nflimit),indfl(nflimit)
      integer*4 numInChunks(nflimit), numChunks
      integer*4 ixpclist(limpcl),iypclist(limpcl),izpclist(limpcl)
      parameter (msizexr=19)
      real*4, allocatable :: xr(:,:)
      character*320 modelfile,newmodel,oldxfgfile,oldxffile,newxffile,idfFile
      character*320 magGradFile
      logical gotthis,gotlast,exist,readw_or_imod
      integer*4 getimodmaxes,getimodscales
      integer*4 limpnts/4/                      !min # of points for regression
c       
      integer*4 i,nlistz,nfout,npclist,izrange,iffillgap,indf,indval,ifShiftScale
      integer*4 minxpiece,nxpieces,nxoverlap,minypiece,nypieces,nyoverlap
      real*4 xhaf,yhaf,xcen,ycen,critmean,critmax,dmin,dmax,dmean
      integer*4 ifxfmod,iftrans,ifrotrans,ifprealign,ntofind, ifmagrot
      integer*4 ifsingle,izsingle,iffullrpt,ierr,ierr2,ifflip,indg
      real*4 zz,zdex,zthis,zlast, shiftScale
      integer*4 nundefine,iobj,ipt,iz,nfgin,indind,izmin,izmax,ibase
      integer*4 lastsec,npnts,iobject,ninobj,ipnt,isol,ipntmax,j
      real*4 const,rsq,fra,theta,sinth,costh,gmag,devmax
      real*4 xdev,ydev,devpnt,devavg,devsd,xx,yy,xlast,ylast,xnew,ynew
      integer*4 loop,noldg,izsec,il,indobj,maxx,maxy,maxz, ifBack, iter
      integer*4 lineUse, lineToUse, ifMagGrad, indControl
      logical done
      real*4 atan2d
c       
      integer*4 ifDistort, idfBinning, iBinning, idfNx, idfNy, indIdf,indPreWarp
      integer*4 nxGrid, nyGrid, lmGrid, ipreWarpNx, ipreWarpNy, iwarpNx, iwarpNy
      real*4 xGridStrt, yGridStrt, xGridIntrv, yGridIntrv, pixelIdf, binRatio
      real*4 pixelModel, pixelWarp, pixelPrewarp, prewarpScale, warpScale
      real*4, allocatable :: fieldDx(:,:), fieldDy(:,:), warpDx(:,:,:), warpDy(:,:,:)
      real*4, allocatable :: xWarpStrt(:), yWarpStrt(:), xWarpIntrv(:), yWarpIntrv(:)
      integer*4, allocatable :: nControl(:,:), nxWarp(:), nyWarp(:)
      character*10240 stringList
      real*4 pixelMagGrad, axisRot, xmodMin,xmodMax,ymodMin,ymodMax,gridExtendFrac
      real*4 tiltAngles(nflimit), dmagPerUm(nflimit), rotPerUm(nflimit), dx, dy, dx1, dy1
      integer*4 numMagGrad,indWarpFile,lmWarpX, lmWarpY,lmWarpZ
      integer*4 getSizeAdjustedGrid,readCheckWarpFile,getGridParameters,clearWarpFile
      integer*4 setCurrentWarpFile, findMaxGridSize, getLinearTransform
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetBoolean, PipNumberOfEntries
      integer*4 PipGetString,PipGetTwoIntegers, PipGetFloatArray, PipGetFloat
      integer*4 PipGetIntegerArray, PipGetNonOptionArg, PipGetTwoFloats
      integer*4 PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  xfmodel
c       
      integer numOptions
      parameter (numOptions = 24)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@output:OutputFile:FN:@image:ImageFile:FN:@'//
     &    'piece:PieceListFile:FN:@allz:AllZhaveTransforms:B:@'//
     &    'center:CenterInXandY:FP:@transonly:TranslationOnly:B:@'//
     &    'rottrans:RotationTranslation:B:@magrot:MagRotTrans:B:@'//
     &    'sections:SectionsToAnalyze:LI:@single:SingleSection:I:@'//
     &    'full:FullReportMeanAndMax:FP:@prealign:PrealignTransforms:FN:@'//
     &    'edit:EditTransforms:FN:@xforms:XformsToApply:FN:@'//
     &    'useline:UseTransformLine:I:@chunks:ChunkSizes:LI:@'//
     &    'back:BackTransform:B:@scale:ScaleShifts:F:@'//
     &    'distort:DistortionField:FN:@binning:BinningOfImages:I:@'//
     &    'gradient:GradientFile:FN:@param:ParameterFile:PF:@help:usage:B:'
c       
c       set defaults
c       
      modelfile = ' '
      xcen = 0.
      ycen = 0.
      ifBack = 0
      iBinning = 1
      ifDistort = 0
      ifMagGrad = 0
      numChunks = 0
      shiftScale = 1.
      gridExtendFrac = 0.1
      idim=100000
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'xfmodel',
     &    'ERROR: XFMODEL - ', .true., 2, 1, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0

c       
c       get parameters
c       
      if (pipinput) then
        ierr = PipGetString('ImageFile', modelfile)
      else
        write(*,'(1x,a,$)')
     &      'Image file (or Return to enter Xcen, Ycen directly): '
        read(*,'(a)')modelfile
      endif
c       
c       if no image file, get crucial info directly
c       DNM 8/28/02: just get xcen, ycen since origin and delta aren't needed
c       
      if(modelfile.eq.' ')then
        if (pipinput) then
          ierr = PipGetTwoFloats('CenterInXandY', xcen, ycen)
        else
          print *,char(7),
     &        'Be SURE to enter CENTER coordinates, NOT NX and NY'
          write(*,'(1x,a,$)')'Xcen, Ycen: '
          read(*,*)xcen,ycen
        endif
        do i=1,nflimit
          listz(i)=i-1
        enddo
        nlistz=nflimit
        nfout=0
      else
c         
c         otherwise get header info from image file
        call imopen(1,modelfile,'ro')
c         
c         get header info for proper coordinate usage
        call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
        call irtdel(1,delt)
c         call irtorg(1,xorig,yorig,zorig)
c         write(*,'(/,a,a,/)')' This header info MUST be the'
c         &           ,' same as when model was built'
c         
        modelfile = ' '
        if (pipinput) then
          ierr = PipGetString('PieceListFile', modelfile)
        else
          write(*,'(1x,a,$)') 'Piece list file if image is a'//
     &        ' montage, otherwise Return: '
          read(*,'(a)')modelfile
        endif
        call read_piece_list(modelfile,ixpclist,iypclist,izpclist,
     &      npclist)
        if (npclist.gt.limpcl)call exitError(
     &      'too many piece coordinates for arrays')
c         
c         if no pieces, set up mocklist
        if(npclist.eq.0)then
          do i=1,nz
            ixpclist(i)=0
            iypclist(i)=0
            izpclist(i)=i-1
          enddo
          npclist=nz
        endif
c         get ordered list of z values 
        call fill_listz(izpclist,npclist,listz,nlistz)
        if (nlistz.gt.nflimit)call exitError(
     &      'too many Z values for arrays')
        
        call checklist(ixpclist,npclist,1,nx,minxpiece
     &      ,nxpieces,nxoverlap)
        call checklist(iypclist,npclist,1,ny,minypiece
     &      ,nypieces,nyoverlap)
        xhaf=(nx+(nxpieces-1)*(nx-nxoverlap))/2.
        yhaf=(ny+(nypieces-1)*(ny-nyoverlap))/2.

c         DNM 8/28/02: don't scale any more
c         but still add minxpiece - the index coordinates are montage
c         coordinates starting at minxpiece.
c         
c         [xy]cen is the amount to SUBTRACT from real model coordinates to
c         get coordinates centered on the center of the image.  Adding [xy]
c         orig shifts coordinates to the lower left corner of image.
c         Subtracting 0.5*nx*delt(1) (half of the image width in real
c         coordinates) then shifts coords to center of image.  Hence this:
c         
c         xcen=(minxpiece+xhaf)*delt(1)-xorig
c         ycen=(minypiece+yhaf)*delt(2)-yorig
        xcen=(minxpiece+xhaf)
        ycen=(minypiece+yhaf)
        nfout=nlistz

      endif
c       
c       find out if gaps in z and ask how to store f's if there are gaps
c       
      izrange=listz(nlistz)+1-listz(1)
      iffillgap=0
      if(izrange.gt.nlistz)then
        write(*,'(1x,a,i4,a)')'There are',izrange-nlistz,
     &      ' gaps in section Z values'
        if (pipinput) then
          ierr = PipGetBoolean('AllZhaveTransforms', iffillgap)
          if (iffillgap .eq. 0) then
            print *,'Xform lists will be assumed to have xforms ',
     &          'only for existing sections'
          else
            print *,'Xform lists will be assumed to have xforms ',
     &          'for all sections'
          endif
        else
          write(*,'(1x,a,/,a$)')'Enter 0 for xform lists'//
     &        ' that have xforms only for existing sections,',
     &        '    or 1 for xform lists that have xforms for'//
     &        ' all Z values in range: '
          read(*,*)iffillgap
        endif
      endif
c       
c       make index from z values to transform list: index=0 for non-existent
c       
      if (pipinput .and. PipGetString('ChunkSizes', stringList) .eq. 0)
     &    call parselist(stringList, numInChunks, numChunks)
      do i=1,nflimit
        indfl(i)=0
      enddo
      if (numChunks .eq. 0) then
        do i=1,nlistz
          indf=listz(i)+1-listz(1)
          indval=i
          if(iffillgap.ne.0)indval=indf
          indfl(indf)=indval
        enddo
        if(nfout.gt.0)nfout=indfl(listz(nlistz)+1-listz(1))
      else
c         
c         or fill index list 
c         
c        print *,numChunks,(numInChunks(j),j=1,numChunks)
        indf = 1
        do j = 1, numChunks
          do i = 1, numInChunks(j)
            indfl(indf) = j
            indf = indf + 1
            if (indf .ge. nflimit) call exitError(
     &          'TOO MANY SECTIONS IN CHUNKS FOR ARRAYS')
          enddo
        enddo
      endif
c       print *,(indfl(i), i=1,nlistz)
c       
      ifxfmod = 0
      ifprealign = 0
      if (PipGetInOutFile('InputFile', 1, 'Input model file', modelfile)
     &    .ne. 0) call exitError('NO INPUT FILE SPECIFIED')
      oldxfgfile = ' '
      oldxffile = ' '
      idfFile = ' '
      magGradFile = ' '
      iftrans=0
      ifrotrans=0
      ifmagrot = 0
      lineUse = -1
      if (pipinput) then
        ierr = PipGetBoolean('TranslationOnly', iftrans)
        ierr = PipGetBoolean('RotationTranslation', ifrotrans)
        ierr = PipGetBoolean('MagRotTrans', ifmagrot)
        ierr = PipGetInteger('UseTransformLine', lineUse)
        ifShiftScale = 1 - PipGetFloat('ScaleShifts', shiftScale)

        if (PipGetString('XformsToApply', oldxffile) .eq. 0) ifxfmod = 1
        if (PipGetString('DistortionField', idfFile) .eq. 0) ifxfmod = 1
        if (PipGetString('GradientFile', magGradFile) .eq. 0) ifxfmod = 1
        if (PipGetString('PrealignTransforms', oldxfgfile) .eq. 0)
     &      ifprealign = 1
c         
c         if back-transform, first check for legality
c         
        if (PipGetBoolean('BackTransform', ifBack) .eq. 0) then
          if (oldxffile .ne. ' ' .and. oldxfgfile .ne. ' ') call exitError(
     &        'You cannot enter both -xform and -prealign with -back')
          if (ifxfmod + ifprealign .eq. 0)
     &        call exitError('You must enter -xform, -prealign,'//
     &        ' -distort or -gradient with -back')
c           
c           in any case, set filename for use in back-transform, clear out
c           transform filename, set flag for prealign back-transform if any
c           Set xfmodel -1 if xform is givem, or if preali with no
c           distortions; i.e. preali with distortion will retransform forward
c           to original (distorted) aligned stack
c           
          if (oldxffile .ne. ' ' .or. (idfFile .eq. ' ' .and.
     &        magGradFile .eq. ' ')) ifxfmod = -1
          if (oldxffile .ne. ' ') oldxfgfile = oldxffile
          oldxffile = ' '
          if (oldxfgfile .ne. ' ') ifprealign = 1
        endif
c         
        if (iftrans + ifrotrans + ifmagrot .gt. 1) call exitError
     &      ('Only one of -trans, -rottrans, -magrot may be entered')
        if (iftrans + ifrotrans + ifmagrot .gt. 0 .and. ifxfmod .ne. 0)
     &      call exitError('You cannot both find transforms '//
     &      'and transform a model')
        if (ifxfmod .eq. 0 .and. ifShiftScale .ne. 0) call exitError
     &      ('You cannot enter -scale unless you are transforming a model')
        if (iftrans .ne. 0) ifxfmod = 2
        if (ifrotrans .ne. 0) ifxfmod = 3
        if (ifmagrot .ne. 0) ifxfmod = 4

        if (.not.(ifxfmod .eq. -1 .or. (ifxfmod .eq. 1 .and. 
     &      (oldxffile .ne. ' ' .or. oldxfgfile .ne. ' '))))  then
          if (lineUse .ge. 0) call exitError('You cannot enter -useline'//
     &        ' unless you are transforming a model')
          if (numChunks .gt. 0) call exitError('You cannot enter -chunks'//
     &        ' unless you are transforming a model')
        endif
        if (lineuse .ge. 0 .and. numChunks .gt. 0) call exitError('It is '//
     &      'meaningless to enter both -useline and -chunks')
      else
c         
        write(*,'(1x,a,/,a,/,a/,a,$)') 'Enter 0 to find '//
     &      'transformations, 2 to find X/Y translations only,',
     &      '    3 to find translations and rotations only,',
     &      '    4 to find translation, rotation and mag change only,',
     &      '    1 to transform model, or -1 to back-transform model: '
        read(*,*)ifxfmod
      endif
c       
c       set variables for restricted fits
c       
      if(ifxfmod.eq.2)then
        ifxfmod=0
        iftrans=1
        limpnts=1
      elseif(ifxfmod.eq.3)then
        ifrotrans=1
        ifxfmod=0
        limpnts=2
      elseif(ifxfmod.eq.4)then
        ifrotrans=2
        ifxfmod=0
        limpnts=3
      endif
c       
c       read in the model now since its range is needed
      exist=readw_or_imod(modelfile)
      if(.not.exist)go to 91
c       
c       shift the data to image coordinates before using
      call scale_model(0)
c       
c       Get model range
      xmodMin = 1.e38
      xmodMax = -1.e38
      ymodMin = 1.e38
      ymodMax = -1.e38
      do i = 1, n_point
        xmodMin = min(xmodMin, p_coord(1, i))
        xmodMax = max(xmodMax, p_coord(1, i))
        ymodMin = min(ymodMin, p_coord(2, i))
        ymodMax = max(ymodMax, p_coord(2, i))
      enddo
c       
c       Extend the range so that properly extrapolated grids can be used to find inverse
c       point for forward transforms
      xlast = xmodMax + 1 - xmodMin
      xmodMin = xmodMin - gridExtendFrac * xlast
      xmodMax = xmodMax + gridExtendFrac + xlast
      ylast = ymodMax + 1 - ymodMin
      ymodMin = ymodMin - gridExtendFrac * ylast
      ymodMax = ymodMax + gridExtendFrac + ylast
c       
      ifsingle=0
      iffullrpt = 0
      ntofind=0
      if (pipinput) then
        if (PipGetInOutFile('OutputFile', 2, ' ', newxffile) .ne. 0)
     &      call exitError('NO OUTPUT FILE SPECIFIED')
        if(ifxfmod.eq.0)then
          oldxffile = ' '
          ierr = PipGetString('EditTransforms', oldxffile)

          if (PipGetTwoFloats('FullReportMeanAndMax', critmean, critmax)
     &        .eq. 0) iffullrpt = 1
          if (PipGetInteger('SingleSection', izsingle) .eq. 0) ifsingle = 1
          if (PipGetString('SectionsToAnalyze', stringList) .eq. 0)
     &        call parselist(stringList, nsec, ntofind)
        else
          newmodel = newxffile
          if (idfFile .ne. ' ') then
            ifDistort = 1
            indIdf = readCheckWarpFile(idfFile, 1, 1, idfNx, idfNy, i, idfBinning,
     &          pixelIdf, iz, stringList)
            if (indIdf .lt. 0) call exitError(stringList)
            if (getGridParameters(1, nxGrid, nyGrid, xGridStrt, yGridStrt, xGridIntrv,
     &          yGridIntrv) .ne. 0) call exitError('GETTING DISTORTION FIELD PARAMETERS')
            lmGrid = max(nxGrid, ceiling((xmodMax - xModMin) / xGridIntrv),
     &          nyGrid, ceiling((ymodMax - yModMin) / yGridIntrv)) + 2
            allocate(fieldDx(lmGrid,lmGrid), fieldDy(lmGrid,lmGrid), stat=ierr)
            call memoryError(ierr, 'ARRAYS FOR DISTORTION FIELD')
c             
c             if the center is not yet defined, need to get it now
c             
            if (xcen .eq. 0. .and. ycen .eq. 0.) then
              ierr = getimodmaxes(maxx, maxy, maxz)
              xcen = maxx / 2.
              ycen = maxy / 2.
              write(*,'(a,2f8.1)')'Using model header to determine '//
     &            'center coordinates:', xcen, ycen
            endif
c             
c             insist on binning unless situation is unambiguous, and convert
c             the distortion field by the difference in binning
c             
            if (PipGetInteger('BinningOfImages', iBinning) .ne. 0) then
              
              if (2. * xcen .le. idfNx * idfBinning / 2 .and.
     &            2. * ycen .le. idfNy * idfBinning / 2) call exitError
     &            ('YOU MUST SPECIFY BINNING OF IMAGES BECAUSE THEY '//
     &            'ARE NOT LARGER THAN HALF THE CAMERA SIZE')
            endif
            if (iBinning .le. 0) call exitError
     &          ('IMAGE BINNING MUST BE A POSITIVE NUMBER')
            binRatio = 1.
            if (iBinning .ne. idfBinning) binRatio = idfBinning / float(iBinning)
            if (getSizeAdjustedGrid(1, (xmodMax - xmodMin) / binRatio,
     &          (ymodMax - ymodMin) / binRatio,
     &          ((xmodMax + xmodMin) / binRatio - idfNx) / 2.,
     &          ((ymodMax + ymodMin) / binRatio - idfNy) / 2., 0, binRatio, 1, 
     &          nxGrid, nyGrid, xGridStrt, yGridStrt, xGridIntrv,
     &          yGridIntrv, fieldDx, fieldDy, lmGrid, lmGrid, stringList) .ne. 0)
     &          call exitError(stringList)
            ierr = clearWarpFile(indIdf)
c             
c             Need to shift field by difference between image and camera
c             centers
c             
            xGridStrt = xGridStrt - (idfNx * binRatio / 2. - xcen)
            yGridStrt = yGridStrt - (idfNy * binRatio / 2. - ycen)
c             print *,ixGridStrt,ixGridStrt,xcen,ycen,idfnx,idfny,binratio
          endif
        endif
c         
c         mag gradients now
c         
        if (magGradFile .ne. ' ') then
          ifMagGrad = 1
          call readMagGradients(magGradFile, nflimit, pixelMagGrad, axisRot,
     &        tiltAngles, dmagPerUm, rotPerUm, numMagGrad)
        endif
        
      else
c         
c         old input: get prealignment or back transforms
c         
        if(ifxfmod.lt.0)then
          ifprealign=1
        else
          write(*,'(1x,a,$)')
     &        'Model built on raw sections (0) or prealigned ones(1): '
          read(*,*)ifprealign
        endif
c         
        if(ifprealign.ne.0)then
          write(*,'(1x,a,$)')
     &        'File of old g transforms used in prealignment: '
          read(*,'(a)')oldxfgfile
        endif
c         
        if(ifxfmod.eq.0)then
          write(*,'(1x,a,$)') 'Name of old file of f transforms'//
     &        ' to edit (Return if none): '
          read(*,'(a)')oldxffile
c           
          write(*,'(1x,a,$)')'Name of new file of f transforms: '
          read(*,'(a)')newxffile
c           
          write(*,118)
118       format(' Enter / to find transforms for all section pairs,',/,
     &        '      or -999 to find transforms relative to a single',
     &        ' section,',/,
     &        '      or a list of the section numbers to find',
     &        ' transforms for',/,'         (enter number of second',
     &        ' section of each pair, ranges are ok)')
          call rdlist(5,nsec,ntofind)
          if(ntofind.eq.1.and.nsec(1).eq.-999)then
            write(*,'(1x,a,$)')'Number of single section to find'//
     &          ' transforms relative to: '
            read(*,*)izsingle
            ifsingle=1
            print *,'Now enter list of sections to find transforms',
     &          ' for (/ for all)'
            ntofind=0
            call rdlist(5,nsec,ntofind)
          endif
c           
          write(*,'(1x,a,$)')'1 for full reports of deviations for'//
     &        ' sections with bad fits, 0 for none: '
          read(*,*)iffullrpt
c           
          if(iffullrpt.ne.0)then
            write(*,'(1x,a,$)')'Enter criterion mean deviation and'//
     &          ' max deviation; full reports will be given',
     &          '     for sections with mean OR max greater than'//
     &          ' these criteria: '
            read(*,*)critmean,critmax
          endif
c           
        else
          write(*,'(1x,a,$)')'New model file name: '
          read(*,'(a)')newmodel
c           
          if(ifxfmod.gt.0)then
            write(*,'(1x,a,$)') 'File for list of g transforms to apply: '
            read(*,'(a)')oldxffile
          endif
c           
        endif
      endif
      call PipDone()       
c       
c       if the center is not yet defined, use the model header sizes
c       
      if (xcen .eq. 0. .and. ycen .eq. 0.) then
        ierr = getimodmaxes(maxx, maxy, maxz)
        xcen = maxx / 2.
        ycen = maxy / 2.
        write(*,'(a,2f8.1)')'Using model header to determine '//
     &      'center coordinates:', xcen, ycen
      endif
c       
c       first fill array with unit transforms in case things get weird
c       
      do i=1,nflimit
        call xfunit(f(1,1,i),1.)
      enddo
c       
c       Assess whether either file is a warping file
      indPreWarp = -1
      indWarpFile = -1
      lmWarpX = 0
      lmWarpY = 0
      lmWarpZ = 0
      if (ifprealign.ne.0) then
        indPreWarp = readCheckWarpFile(oldxfgfile, 0, 1, ipreWarpNx, iPreWarpNy, noldg, i,
     &      pixelPreWarp, iz, stringList)
        if (indPreWarp .lt. -1) call exitError(stringList)
        if (indPreWarp .ge. 0) lmWarpZ = noldg
      endif
      if (oldxffile.ne.' ') then
        indWarpFile = readCheckWarpFile(oldxffile, 0, 1, iwarpNx, iwarpNy, nfgin, i,
     &      pixelWarp, iz, stringList)
        if (indWarpFile .lt. -1) call exitError(stringList)
        if (indWarpFile .ge. 0) lmWarpZ = max(lmWarpZ, nfgin)
      endif
c       
c       One or other warping exists
      if (lmWarpZ .gt. 0) then
        allocate(nControl(lmWarpZ,2), stat = ierr)
        call memoryError(ierr, 'ARRAY FOR NUMBER OF CONTROL POINTS')
c         
c         Need to know the pixel size of the model.  Take image pixel first.
        pixelModel = delt(1)
        if (delt(1) .le. 0) ierr = getimodscales(pixelModel, xlast, ylast)
c         
c         Find out how big grids need to be
        if (indPreWarp .ge. 0) then
          write(*,'(a,a)')'Warping file opened: ',trim(oldxfgfile)
          prewarpScale = pixelPrewarp / pixelModel
          ierr = setCurrentWarpFile(indPreWarp)
          if (findMaxGridSize(xmodMin / prewarpScale, xModMax / prewarpScale,
     &        yModMin / prewarpScale, yModMax / prewarpScale, nControl(1,1), lmWarpX,
     &        lmWarpY, stringList) .ne. 0) call exitError(stringList)
        endif
        if (indWarpFile .ge. 0) then
          write(*,'(a,a)')'Warping file opened: ',trim(oldxffile)
          warpScale = pixelWarp / pixelModel
          ierr = setCurrentWarpFile(indWarpFile)
          if (findMaxGridSize(xmodMin / warpScale, xModMax / warpScale,
     &        yModMin / warpScale, yModMax / warpScale, nControl(1,2), i, iz,
     &        stringList) .ne. 0) call exitError(stringList)
          lmWarpX = max(lmWarpX, i)
          lmWarpY = max(lmWarpY, iz)
        endif
c         
c         Allocate arrays for grid and parameters for each section
        allocate(warpDx(lmWarpX,lmWarpY,lmWarpZ), warpDy(lmWarpX,lmWarpY,lmWarpZ),
     &      nxWarp(lmWarpZ), xWarpStrt(lmWarpZ), xWarpIntrv(lmWarpZ), nyWarp(lmWarpZ),
     &      yWarpStrt(lmWarpZ), yWarpIntrv(lmWarpZ), stat = ierr)
        call memoryError(ierr, 'ARRAYS FOR WARPING GRIDS')
      endif
c       
c       back-transform if necessary
      if(ifprealign.ne.0)then
        if (indPreWarp .ge. 0) then
          ierr = setCurrentWarpFile(indPreWarp)
          if (noldg .gt. nflimit) call exitError(
     &        'too many sections in warp file for transform array')
          if (ifShiftScale .eq. 0) shiftScale = prewarpScale
          indControl = 1
          do i = 1, noldg
            if (getLinearTransform(i, g(1,1,i)) .ne. 0)
     &          call exitError('GETTING LINEAR TRANSFORM FROM WARP FILE')
            if (nControl(i,1) .gt. 2) then
              if (getSizeAdjustedGrid(i, (xmodMax - xmodMin) / prewarpScale,
     &            (ymodMax - ymodMin) / prewarpScale,
     &            ((xmodMax + xmodMin) / prewarpScale - ipreWarpNx) / 2.,
     &            ((ymodMax + ymodMin) / prewarpScale - ipreWarpNy) / 2., 0, prewarpScale,
     &            1,  nxWarp(i), nyWarp(i), xWarpStrt(i), yWarpStrt(i), xWarpIntrv(i),
     &            yWarpIntrv(i), warpDx(1,1,i), warpDy(1,1,i), lmWarpX, lmWarpY,
     &            stringList) .ne. 0) call exitError(stringList)
            endif
          enddo
        else
          call dopen(3,oldxfgfile,'ro','f')
c         
c           get g transforms into g list
          call xfrdall2(3,g,noldg,nflimit,ierr)
          if (ierr .ne. 0) call exitError('too many transforms for arrays')
          close(3)
        endif
c         
c         invert the g's into the g list
        do indg=1,noldg
          g(1,3,indg) = g(1,3,indg) * shiftScale
          g(2,3,indg) = g(2,3,indg) * shiftScale
          call xfinvert(g(1,1,indg),gtmp)
          call xfcopy(gtmp,g(1,1,indg))
        enddo
c         
c         apply inverse g's to all points in model
c         Set up to use a single section if back transforming and user 
c         specified it or there is only one transform
c         
        lineToUse = -1
        if (ifxfmod .lt. 0) then
          lineToUse = lineUse
          if (noldg .eq. 1 .and. numChunks .eq. 0) then
            lineToUse = 0
            print *,'There is only one transform and it is being',
     &          ' applied at all Z values'
          endif
        endif
        write(*,'(a,a)')'Back-transforming model with inverse of transforms from ',
     &      trim(oldxfgfile)
        if (indPreWarp .ge. 0) call warpModel(noldg, 1)
        call transformModel(g, noldg, nflimit, xcen, ycen, indfl, listz, lineToUse,
     &      nundefine)
        
        if(ifxfmod.lt.0 .and. ifDistort + ifMagGrad .eq. 0)then
c           
c           write out back-transformed model
          call rescaleWriteModel(newmodel, nundefine)
          call exit(0)
        endif
      endif
c       
c       read in the old f's or g's for whatever purpose 
c       
      if (indWarpFile .lt. 0) nfgin=0
      lineToUse = -1
      if(oldxffile.ne.' ')then
        if (indWarpFile .ge. 0) then
          ierr = setCurrentWarpFile(indWarpFile)
          if (nfgin .gt. nflimit) call exitError(
     &        'too many sections in warp file for transform array')
          if (ifShiftScale .eq. 0) shiftScale = warpScale
          indControl = 2
          do i = 1, nfgin
            if (getLinearTransform(i, f(1,1,i)) .ne. 0)
     &          call exitError('GETTING LINEAR TRANSFORM FROM WARP FILE')
            if (nControl(i,2) .gt. 2) then
              if (getSizeAdjustedGrid(i, (xmodMax - xmodMin) / warpScale,
     &            (ymodMax - ymodMin) / warpScale,
     &            ((xmodMax + xmodMin) / warpScale - iWarpNx) / 2.,
     &            ((ymodMax + ymodMin) / warpScale - iWarpNy) / 2., 0, warpScale,
     &            1,  nxWarp(i), nyWarp(i), xWarpStrt(i), yWarpStrt(i), xWarpIntrv(i),
     &            yWarpIntrv(i), warpDx(1,1,i), warpDy(1,1,i), lmWarpX, lmWarpY,
     &            stringList) .ne. 0) call exitError(stringList)
            endif
          enddo
        else
          call dopen(1,oldxffile,'ro','f')
          call xfrdall2(1,f,nfgin,nflimit,ierr)
          if (ierr .ne. 0) call exitError('too many transforms for arrays')
          close(1)
        endif
        do indg=1,nfgin
          f(1,3,indg) = f(1,3,indg) * shiftScale
          f(2,3,indg) = f(2,3,indg) * shiftScale
        enddo
        lineToUse = lineUse
        if (nfgin .eq. 1 .and. numChunks .eq. 0) then
          lineToUse = 0
          print *,'There is only one transform and it is being',
     &        ' applied at all Z values'
        endif
      endif
c       
c       TRANSFORMING/UNDISTORTING MODEL 
c       
      if(ifxfmod.ne.0)then
        if (ifDistort + ifMagGrad .ne. 0) then
          if (ifBack .ne. 0) then
            print *,'Redistorting model'
          else
            print *,'Undistorting model'
          endif

          do i=1,n_point
            if (ifMagGrad .ne. 0) 
     &          iz = max(1, min(nint(p_coord(3, i) + 1.), numMagGrad))

            if (ifBack .eq. 0) then
c               
c               undistort the model - find point that distorts to the 
c               given model point
c               
              iter = 1
              xlast = p_coord(1,i)
              ylast = p_coord(2,i)
              done = .false.
              do while (iter .lt. 10 .and. .not.done)
                dx1 = 0.
                dy1 = 0.
                dx = 0.
                dy = 0.
                if (IfMagGrad .ne. 0)
     &              call magGradientShift(xlast, ylast, nint(2. * xcen), nint(2. * ycen),
     &              xcen, ycen, pixelMagGrad, axisRot, tiltAngles(iz), dmagPerUm(iz),
     &              rotPerUm(iz), dx1, dy1)

                if (ifDistort .ne. 0)
     &              call interpolateGrid(xlast + dx1, ylast + dy1, fieldDx, fieldDy,
     &              lmGrid, nxGrid, nyGrid, xGridstrt, yGridStrt, xGridIntrv,
     &              yGridIntrv, dx, dy)
                xnew = p_coord(1,i) - (dx + dx1)
                ynew = p_coord(2,i) - (dy + dy1)
                done = abs(xnew - xlast) .lt. 0.01 .and.
     &              abs(ynew - ylast) .lt. 0.01
                xlast = xnew
                ylast = ynew
                iter = iter + 1
              enddo     
              p_coord(1,i) = xnew
              p_coord(2,i) = ynew
            else
c               
c               or redistort the model
c               
              dx1 = 0.
              dy1 = 0.
              dx = 0.
              dy = 0.
              if (IfMagGrad .ne. 0)
     &            call magGradientShift(p_coord(1,i), p_coord(2,i),
     &            nint(2. * xcen), nint(2. * ycen),
     &            xcen, ycen, pixelMagGrad, axisRot, tiltAngles(iz),
     &            dmagPerUm(iz), rotPerUm(iz), dx1, dy1)

              if (ifDistort .ne. 0)
     &            call interpolateGrid(p_coord(1,i) + dx1, p_coord(2,i) + dy1,
     &            fieldDx, fieldDy, lmGrid, nxGrid, nyGrid, xGridstrt,
     &            yGridStrt,  xGridIntrv, yGridIntrv, dx, dy)
              p_coord(1,i) = p_coord(1,i) + dx1 + dx
              p_coord(2,i) = p_coord(2,i) + dy1 + dy
            endif
          enddo
c           
c           if there was prealignment and no new transforms, get the 
c           prealignment transforms back by inversion and set up to use them
c           
          if (nfgin .eq. 0 .and. ifprealign .ne. 0 .and.
     &          ifxfmod .gt. 0) then
            write(*,'(a,a)')'Transforming model with reinverted transforms from ',
     &          trim(oldxfgfile)
            do indg=1,noldg
              call xfinvert(g(1,1,indg), f(1,1,indg))
            enddo
            nfgin = noldg
            indWarpFile = indPreWarp
          endif
        endif
c         
        nundefine = 0
c         
c         transform the model
c         
        if (oldxffile .ne. ' ') write(*,'(a,a)')
     &      'Transforming model with transforms from ', trim(oldxffile)
        if (nfgin .ne. 0) call transformModel(f, nfgin, nflimit,
     &      xcen, ycen, indfl, listz, lineToUse, nundefine)
        if (indWarpFile .ge. 0) call warpModel(nfgin, 0)
        
        call rescaleWriteModel(newmodel, nundefine)
      else
c         
c         SEARCH FOR POINTS TO DERIVE XFORMS FROM
c         
c         allocate xr, limiting it to much more than could be needed for smaller model
        idim = min(idim, n_point)
        allocate(xr(msizexr,idim), stat=ierr)
        call memoryError(ierr, 'ARRAY FOR DATA MATRIX')

c         first find min and max z in model
        izmin=100000
        izmax=-izmin
        do iobj=1,max_mod_obj
          ibase=ibase_obj(iobj)
          do ipt=1,npt_in_obj(iobj)
            i=abs(object(ipt+ibase))
            zz=p_coord(3,i)
c             zdex=(zz+zorig)/delt(3)
            zdex=zz
            iz=int(zdex-nint(zdex)+0.5) + nint(zdex)
c             if(iz.lt.listz(1).or.iz.gt.listz(nlistz))go to 93
            izmin=min0(izmin,iz)
            izmax=max0(izmax,iz)
          enddo           
        enddo
c         
c         if didn't specify list of section #'s, make such a list from range
c         
        if(ntofind.le.0)then
          ntofind=ifsingle+izmax-izmin
          do i=1,ntofind
            nsec(i)=izmin+i-ifsingle
          enddo
        endif
        nfout=max(nfout,nfgin)
        write(*,122)
122     format(40x,'Deviations between transformed points on',/,
     &      41x,'section and points on previous section',/,
     &      32x,'Mean     Max  @object & point #   X-Y Position')
        do loop=1,ntofind
          izsec=nsec(loop)
c           
c           make sure this is a section in list and find previous z
c           
          lastsec=-100000
          do il=2,nlistz
            if(izsec.eq.listz(il))lastsec=listz(il-1)
          enddo
c           
c           but if doing to single section, allow section to be first in list
c           as well and set lastsec to z of single section
c           
          if(ifsingle.ne.0 .and. (lastsec.ne.-100000 .or.
     &        izsec.eq.listz(1)))lastsec=izsingle
          if(lastsec.ne.-100000)then
c             
c             get points out of objects with points in both this and previous
c             section
c             
            npnts=0
            do iobject=1,max_mod_obj
              gotlast=.false.
              gotthis=.false.
              ninobj=npt_in_obj(iobject)
              do indobj=1,ninobj
                ipnt=object(indobj+ibase_obj(iobject))
                if(ipnt.gt.0.and.ipnt.le.n_point)then
c                   zdex=(p_coord(3,ipnt)+zorig)/delt(3)
                  zdex=p_coord(3,ipnt)
                  iz=int(zdex-nint(zdex)+0.5) + nint(zdex)
                  if((iz.eq.izsec).and.(.not.gotthis.or.
     &                (p_coord(3,ipnt).lt.zthis)))then
c                     
c                     if in second section, and either haven't gotten a point
c                     there before, or this point has a lower z than the
c                     previous point, put x and y into 1st and 2nd
c                     column: independent vars
c                     
                    xr(1,npnts+1)=p_coord(1,ipnt)-xcen
                    xr(2,npnts+1)=p_coord(2,ipnt)-ycen
                    gotthis=.true.
                    zthis=p_coord(3,ipnt)
                    xr(6,npnts+1)=iobject
                    xr(7,npnts+1)=indobj
                  elseif((iz.eq.lastsec).and.(.not.gotlast.or.
     &                  (p_coord(3,ipnt).gt.zlast)))then
c                     
c                     if in first section, and either haven't gotten a point
c                     there before, or this point is higher in z than the
c                     previous point, put x and y into 4th and 5th
c                     column: dependent vars
c                     
                    xr(4,npnts+1)=p_coord(1,ipnt)-xcen
                    xr(5,npnts+1)=p_coord(2,ipnt)-ycen
                    gotlast=.true.
                  endif
                endif
              enddo
              if(gotthis.and.gotlast)npnts=npnts+1
              if (npnts.ge.idim)then
                print *
                print *,'ERROR: XFMODEL - ',
     &              'too many points for arrays on section', iz
                call exit(1)
              endif
            enddo                               !done with looking at objects
            if(npnts.ge.limpnts)then
c               
c               now if there are at least limpnts points, do regressions:
c               first last section x then l.s. y as function of this section
c               x and y
c               save results in xform for izsec
c               
              indf=indfl(izsec+1-listz(1))
              if(indf.le.0)then
                print *
                print *,'ERROR: XFMODEL - z value out of range for ',
     &              'transforms: ',zz
                call exit(1)
              endif
c               
              call findxf(xr,msizexr,4,npnts,xcen,ycen,iftrans,ifrotrans,2,f(1,1,indf),
     &            devavg, devsd,devmax,ipntmax)
C               
c               keep track of the highest & lowest transforms obtained
c               
              nfout=max0(nfout,indf)
              write(*,121)npnts,izsec,devavg,devmax,
     &            nint(xr(6,ipntmax)),nint(xr(7,ipntmax)),
     &            xr(8,ipntmax),xr(9,ipntmax)
121           format(i4,
     &            ' points, section #',i4,2x,2f9.2,2i6,4x,2f9.2)
              if(iffullrpt.ne.0.and.
     &            (devmax.ge.critmax.or.devavg.gt.critmean))
     &            write(*,124)((xr(i,j),i=6,13),j=1,npnts)
124           format('    Object  Point       position        ',
     &            'deviation vector   angle   magnitude',/,
     &            (f10.0,f6.0,4f10.2,f9.0,f10.2))
            else
              print *,'less than',limpnts,' points for section # ',
     &            izsec
            endif
          endif
        enddo                                   !end of loop the loop
c         
c         write out enough for whole file, and at least as many as were in
c         an input file if any
c         
        call dopen(2,newxffile,'new','f')
        do i=1,nfout
          call xfwrite(2,f(1,1,i),*94)
        enddo
        close(2)
      endif
      call exit(0)
91    call exitError('reading model file')
92    call exitError('reading old f/g file')
94    call exitError('writing out f file')


      CONTAINS

      subroutine warpModel(nsecin, ifback)
      integer*4 nsecin
      integer*4 iobj, ipt, i, iz, indind, indg, ifback
      real*4 zz,dx,dy
c       
      nundefine=0
      do iobj=1,max_mod_obj
        do ipt=1,npt_in_obj(iobj)
          i=abs(object(ibase_obj(iobj)+ipt))
          zz=p_coord(3,i)
          iz=int(zz-nint(zz)+0.5) + nint(zz)
          indind=iz+1 -listz(1)
          if (.not. ((indind.lt.1.or.indind.gt.nflimit) .and. lineToUse .lt. 0)) then
            if (lineToUse .lt. 0) then
              indg=indfl(indind)
            else
              indg = lineToUse + 1
            endif
            if (indg.ge.1 .and. indg.le.nsecin) then
              if (nControl(indg, indControl) .gt. 2) then
                if (ifback .ne. 0) then
                  call interpolateGrid(p_coord(1,i),  p_coord(2,i), warpDx(1,1,indg),
     &                warpDy(1,1,indg), lmWarpX, nxWarp(indg), nyWarp(indg),
     &                xWarpStrt(indg), yWarpStrt(indg), xWarpIntrv(indg),
     &                yWarpIntrv(indg), dx, dy)
                  p_coord(1,i) = p_coord(1,i) + dx
                  p_coord(2,i) = p_coord(2,i) + dy
                else
                  call findInversePoint( p_coord(1,i),  p_coord(2,i), warpDx(1,1,indg),
     &                warpDy(1,1,indg), lmWarpX, nxWarp(indg), nyWarp(indg),
     &                xWarpStrt(indg), yWarpStrt(indg), xWarpIntrv(indg),
     &                yWarpIntrv(indg), p_coord(1,i),  p_coord(2,i), dx, dy)
                endif
              endif
            endif
          endif
        enddo
      enddo
      return
      end subroutine warpModel

      end


      subroutine transformModel(f, nfgin, nflimit, xcen, ycen,
     &    indfl, listz, lineToUse, nundefine)
      implicit none
      include 'model.inc'
      real*4 f(2,3,*), xcen, ycen
      integer*4 nfgin, nflimit, nundefine, indfl(*), listz(*), lineToUse
      integer*4 iobj, ipt, i, iz, indind, indg
      real*4 zz
c       
      nundefine=0
      do iobj=1,max_mod_obj
        do ipt=1,npt_in_obj(iobj)
          i=abs(object(ibase_obj(iobj)+ipt))
          zz=p_coord(3,i)
          iz=int(zz-nint(zz)+0.5) + nint(zz)
          indind=iz+1 -listz(1)
          if ((indind.lt.1.or.indind.gt.nflimit) .and. lineToUse .lt. 0)then
            nundefine=nundefine+1
          else
            if (lineToUse .lt. 0) then
              indg=indfl(indind)
            else
              indg = lineToUse + 1
            endif
            if(indg.lt.1.or.indg.gt.nfgin)then
              nundefine=nundefine+1
            else
              call xfapply(f(1,1,indg),xcen,ycen,p_coord(1,i),
     &            p_coord(2,i),p_coord(1,i),p_coord(2,i))
            endif
          endif
        enddo
      enddo
      return
      end


      subroutine rescaleWriteModel(newmodel, nundefine)
      implicit none
      include 'model.inc'
      integer*4 nundefine
      character*(*) newmodel
      
c       
      if(nundefine.gt.0)print *,nundefine,
     &    ' points with Z values out of range of transforms'
c       
c       write model out
c       shift the data back for saving
c       
      call scale_model(1)
      call write_wmod(newmodel)
      return
      end
