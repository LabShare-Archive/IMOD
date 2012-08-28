! * * * * BOXSTARTEND * * * * *
!
! BOXSTARTEND will clip out volumes of image centered on the starting
! or ending points of model contours, or on all points in a model
! contour.  It can place each volume into a separate file, or stack all
! the extracted volumes into a single output file.  In the latter
! case, the program can generate two lists of piece coordinates to
! allow the volumes to be examined in two different ways.  One list
! will assemble all of the volumes into a single square array, with
! the extracted starting or ending points all appearing on the same
! section.  The other list will display each piece in its true
! position in the original volume of images.
!
! See man page for details
!
! $Id$
!
! David Mastronarde 4/23/90
!
program boxstartend
  implicit none
  include 'smallmodel.inc90'
  integer IXDIM, IDIM, LIMPCL, LIMOBJ, LIMSEC
  parameter (IXDIM = 500, IDIM = IXDIM * IXDIM * IXDIM, LIMPCL = 50000, LIMOBJ = 10000)
  parameter (LIMSEC = 10000)
  real*4 array(IDIM), brray(IDIM), agvArray(IDIM)
  equivalence (array, agvArray), (brray, agvArray)
  integer*4 nxyz(3), mxyz(3), nxyz2(3), nxyzst(3), ind(3), nx, ny, nz
  equivalence (nx, nxyz(1)), (ny, nxyz(2)), (nz, nxyz(3))
  logical exist
  character*320 modelFile
  real*4 delta(3), origin(3), cell(6), title(20), offset(3)
  integer*4 ixPcList(LIMPCL), iyPcList(LIMPCL), izPcList(LIMPCL)
  integer*4 listz(LIMPCL)
  integer*4 iobjClip(LIMOBJ), iobjFlags(LIMOBJ), loadObjs(LIMOBJ)
  character*320 inFile, pieceFile, pieceFile2, convNum, rootName
  character*320 concat
  character*10240 listString
  data nxyzst/0, 0, 0/
  character*9 dateStrn
  character*8 timeStrn
  common / bigarr / agvArray
  !
  character*80 titlech
  character*6 textStrtEnd
  real*4 g(2,3,LIMSEC), gtmp(2,3)
  !
  real*4 dmin, dmax, dmean, xCen, yCen, dsum, tmin, tmax, xNew, yNew, dmin2, dmax2
  integer*4 mode, ierr, izRange, numPcList, numListz, minXpiece, numXpieces, imobj
  integer*4 nxOverlap, minYpiece, numYpieces, nyOverlap, maxXpiece, maxYpiece
  integer*4 numObjTot, ifXform, nxg, i, ix, iy, indXmin, indXmax, indYmin
  integer*4 indYmax, indZmin, indZmax, numObjClip, numLoadObj, imodObj, ifUse
  integer*4 ifStartEnd, numPixBox, numPixSq, npixLeft, npixRight, ibBase, ipnt, ilist
  integer*4 iaBase, nzBefore, nzAfter, nzClip, ifSeries, ifAverage, numFile
  integer*4 numPredict, iobj, numGutter, nzOut, numClip, loopStart, loopEnd, loop
  integer*4 indLeft, indRight, indBot, indTop, indZlo, indZhi, indZ, indg
  integer*4 indar, ipc, ixpc, iypc, ipcXstart, ipcYstart, ipcXend, ipcYend, kti, jnd
  integer*4 newYpiece, newXpiece, indX, indY, iz, nyPixBox, nzPixBox, npixBot, npixTop
  real*4 tmean, dmean2, xyScale, zScale, xofs, yofs, zofs
  logical*4 backXform
  integer*4 getImodObjSize, getImodFlags, getImodHead
  logical*4 readSmallMod, getModelObjectList
  !
  logical pipInput
  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetString, PipGetInteger, PipGetLogical
  integer*4 PipGetInOutFile, PipGetTwoIntegers, PipGetThreeIntegers
  !
  ! fallbacks from ../../manpages/autodoc2man -2 2  boxstartend
  !
  integer numOptions
  parameter (numOptions = 19)
  character*(40 * numOptions) options(1)
  options(1) = &
      'image:InputImageFile:FN:@model:ModelFile:FN:@'// &
      'output:OutputFile:FN:@series:SeriesRootName:CH:@'// &
      'piece:PieceListFile:FN:@array:ArrayPieceList:FN:@'// &
      'true:TruePieceList:FN:@objects:ObjectsToUse:LI:@box:BoxSizeXY:I:@'// &
      'slices:SlicesBelowAndAbove:IP:@which:WhichPointsToExtract:I:@'// &
      'xminmax:XMinAndMax:IP:@yminmax:YMinAndMax:IP:@'// &
      'zminmax:ZMinAndMax:IP:@xforms:XformsToApply:FN:@'// &
      'back:BackTransform:B:@blank:BlankBetweenImages:I:@'// &
      'param:ParameterFile:PF:@help:usage:B:'
  !
  ! defaults
  ifStartEnd = -1
  numGutter = 5
  backXform = .false.
  ifXform = 0
  !
  ! Pip startup: set error, parse options, check help, set flag if used
  call PipReadOrParseOptions(options, numOptions, 'boxstartend', &
      'ERROR: BOXSTARTEND - ', .true., 4, 2, 1, numOptArg, &
      numNonOptArg)
  pipInput = numOptArg + numNonOptArg > 0

  if (PipGetInOutFile('InputImageFile', 1, 'Enter input image file name', &
      inFile) .ne. 0) call exitError('NO INPUT IMAGE FILE SPECIFIED')

  call imopen(1, inFile, 'RO')
  call irdhdr(1, nxyz, mxyz, mode, dmin, dmax, dmean)
  call irtdel(1, delta)
  call irtorg(1, origin(1), origin(2), origin(3))
  !
  ! Open other files
  !
  pieceFile = ' '
  if (pipInput) then
    ierr = PipGetString('PieceListFile', pieceFile)
  else
    write(*,'(1x,a,$)') &
        'Piece list file if image is a montage, otherwise Return: '
    read(*,50) pieceFile
  endif
  call read_piece_list(pieceFile, ixPcList, iyPcList, izPcList, numPcList)
  !
  ! if no pieces, set up mocklist
  if (numPcList == 0) then
    do i = 1, nz
      ixPcList(i) = 0
      iyPcList(i) = 0
      izPcList(i) = i - 1
    enddo
    numPcList = nz
  endif
  ! make ordered list of z values
  call fill_listz(izPcList, numPcList, listz, numListz)

  izRange = listz(numListz) + 1 - listz(1)
  call checkList(ixPcList, numPcList, 1, nx, minXpiece, numXpieces, nxOverlap)
  call checkList(iyPcList, numPcList, 1, ny, minYpiece, numYpieces, nyOverlap)
  xCen = minXpiece + (nx + (numXpieces - 1) * (nx - nxOverlap)) / 2.
  yCen = minYpiece + (ny + (numYpieces - 1) * (ny - nyOverlap)) / 2.
  maxXpiece = minXpiece + (nx + (numXpieces - 1) * (nx - nxOverlap)) - 1
  maxYpiece = minYpiece + (ny + (numYpieces - 1) * (ny - nyOverlap)) - 1
  !
  call imodPartialMode(1)

  if (PipGetInOutFile('ModelFile', 2, 'Name of input model file', &
      modelFile) .ne. 0) call exitError('NO INPUT MODEL FILE SPECIFIED')
  exist = readSmallMod(modelFile)
  if (.not.exist) call exitError('OPENING MODEL FILE')
  
  numObjTot = getImodObjSize()
  if (numObjTot > LIMOBJ) call exitError('TOO MANY MODEL OBJECTS FOR ARRAYS')
  !
  if (pipInput) then
    ifXform = 1 - PipGetString('XformsToApply', pieceFile)
    ierr = PipGetLogical('BackTransform', backXform)
    if (ifXform .ne. 0 .and. backXform) ifXform = -1
  else
    write(*,'(1x,a,$)') '0 to take coordinates as they are, 1 to'// &
        ' transform, -1 to back-transform: '
    read(5,*) ifXform
    if (ifXform .ne. 0) then
      write(*,'(1x,a,$)') 'Name of file with transforms: '
      read(5, 50) pieceFile
    endif
  endif

  if (ifXform .ne. 0) then
    call dopen(4, pieceFile, 'old', 'f')
    call xfrdall(4, g, nxg,*99)
    if (ifXform < 0) then
      do i = 1, nxg
        call xfinvert(g(1, 1, i), gtmp)
        call xfcopy(gtmp, g(1, 1, i))
      enddo
    endif
  endif
  !
  indXmin = minXpiece
  indXmax = maxXpiece
  indYmin = minYpiece
  indYmax = maxYpiece
  indZmin = -99999
  indZmax = -99999
  numObjClip = 0
  if (pipInput) then
    ierr = PipGetTwoIntegers('XMinAndMax', indXmin, indXmax)
    ierr = PipGetTwoIntegers('YMinAndMax', indYmin, indYmax)
    ierr = PipGetTwoIntegers('ZMinAndMax', indZmin, indZmax)
    if (PipGetString('ObjectsToUse', listString) == 0) &
        call parseList(listString, iobjClip, numObjClip)
  else
    write(*,'(1x,a,/,a,$)') 'Enter minimum and maximum X and Y'// &
        ' index coordinates within which', &
        '    ends should be contained, or / for no limits: '
    read(*,*) indXmin, indXmax, indYmin, indYmax
    write(*,'(1x,a,/,a,$)') 'Enter minimum and maximum'// &
        ' section numbers within which', &
        '    boxes should be contained, or / for no limits: '
    read(*,*) indZmin, indZmax
    print *,'Enter list of numbers of objects whose ends should '// &
        'be clipped (Return for all)'
    call rdlist(5, iobjClip, numObjClip)
  endif
  !
  indXmin = max(indXmin, minXpiece)
  indYmin = max(indYmin, minYpiece)
  indXmax = min(indXmax, maxXpiece)
  indYmax = min(indYmax, maxYpiece)
  !
  ! indZmin=max(indZmin, listz(1))
  ! indZmax=min(indZmax, listz(numListz))
  !
  !
  numLoadObj = 0
  ierr = getImodFlags(iobjFlags, LIMOBJ)

  do imodObj = 1, numObjTot
    ifUse = 0
    if (numObjClip > 0) then
      do imobj = 1, numObjClip
        if (iobjClip(imobj) == imodObj) ifUse = 1
      enddo
    else
      ifUse = mod(iobjFlags(imodObj), 4)
    endif
    if (ifUse .ne. 0) then
      numLoadObj = numLoadObj + 1
      loadObjs(numLoadObj) = imodObj
    endif
  enddo
  if (.not.getModelObjectList(loadObjs, numLoadObj)) &
      call exitError('LOADING MODEL DATA')
  !
  ! convert to index coords in the current volume
  !
  call scaleModelToImage(1, 0)
  !
  if (pipInput) then
    ierr = PipGetInteger('WhichPointsToExtract', ifStartEnd)
    if (PipGetInteger('BoxSizeXY', numPixBox) .ne. 0) then
      if (PipGetThreeIntegers('VolumeSizeXYZ', numPixBox, nyPixBox, nzPixBox) .ne. 0) &
          call exitError('BOX SIZE MUST BE ENTERED WITH -box OR -volume')
    else
      nyPixBox = numPixBox
      nzPixBox = numPixBox
    endif
  else
    write(*,'(1x,a,$)') 'Clip out starts (0) or ends (1) or all points (-1): '
    read(*,*) ifStartEnd
    !
    write(*,'(1x,a,$)') 'Box size in pixels: '
    read(*,*) numPixBox
    nyPixBox = numPixBox
    nzPixBox = numPixBox
  endif
  numPixSq = numPixBox * nyPixBox
  npixLeft = (numPixBox - 1) / 2
  npixRight = numPixBox - npixLeft - 1
  npixBot = (nyPixBox - 1) / 2
  npixTop = nyPixBox - npixBot - 1
  offset(1) = -0.5 * (1 + npixRight - npixLeft)
  offset(2) = -0.5 * (1 + npixTop - npixBot)
  ibBase = IDIM - numPixSq - 1
  iaBase = ibBase - numPixSq
  if (iaBase < 0) call exitError('BOX SIZE TOO LARGE FOR ARRAYS')
  !
  nzBefore = nzPixBox / 2
  nzAfter = nzPixBox - 1 - nzBefore
  if (pipInput) then
    ierr = PipGetTwoIntegers('SlicesBelowAndAbove', nzBefore, nzAfter)
  else
    write(*,'(1x,a,$)') '# of sections before and # of sections'// &
        ' after endpoint to clip out: '
    read(*,*) nzBefore, nzAfter
  endif
  ! print *,nzBefore, nzAfter
  nzClip = nzBefore + nzAfter + 1
  if (nzClip < 1 .or. numPixBox < 1 .or. nyPixBox < 1) &
      call exitError('BOX SIZE AND NUMBER OF SLICES MUST BE POSITIVE')
  offset(3) = 0.
  if (mod(nzBefore, 2) == mod(nzAfter, 2)) offset(3) = -0.495
  !
  ! Manage the Z limits if none entered
  if (indZmin == -99999) indZmin = listz(1) - nzBefore
  if (indZmax == -99999) indZmax = listz(numListz) + nzAfter
  !
  if (pipInput) then
    ifSeries = 1 - PipGetString('SeriesRootName', rootName)
  else
    write(*,'(1x,a,$)') '1 to output numbered series of files,'// &
        ' 0 for single output file: '
    read(5,*) ifSeries
    if (ifSeries .ne. 0) then
      write(*,'(1x,a,$)') 'Root name for output files: '
      read(5, 50) rootName
    endif
  endif

  if (ifSeries .ne. 0) then
    ifAverage = 0
    pieceFile = ' '
    pieceFile2 = ' '
    !
    ! precount the points
    !
    numPredict = 0
    do iobj = 1, max_mod_obj
      if (npt_in_obj(iobj) > 0) then
        if (ifStartEnd < 0) then
          numPredict = numPredict + npt_in_obj(iobj)
        else
          numPredict = numPredict + 1
        endif
      endif
    enddo
  else
    ifAverage = 1
    if (iaBase < numPixSq * nzClip) then
      ifAverage = 0
      print *,'WARNING: BOXSTARTEND - VOLUMES TOO LARGE TO COMPUTE AVERAGE'
    else
      do ix = 1, numPixSq * nzClip
        agvArray(ix) = 0.
      enddo
    endif
    if (PipGetInOutFile('OutputFile', 3, 'Output image file name', inFile) .ne. 0) &
        call exitError('NO OUTPUT IMAGE FILE SPECIFIED')
    !
    call imopen(2, inFile, 'new')
    nxyz2(1) = numPixBox
    nxyz2(2) = nyPixBox
    nxyz2(3) = 1
    call itrhdr(2, 1)
    call ialsiz(2, nxyz2, nxyzst)
    !
    pieceFile = ' '
    pieceFile2 = ' '
    if (pipInput) then
      ierr = PipGetString('ArrayPieceList', pieceFile)
      ierr = PipGetString('TruePieceList', pieceFile2)
      ierr = PipGetInteger('BlankBetweenImages', numGutter)
    else
      write(*,'(1x,a,$)') 'Output file for 2D array piece list, or Return for none: '
      read(5, 50) pieceFile
50    format(A)
      !
      if (pieceFile .ne. ' ') then
        write(*,'(1x,a,$)') 'Number of empty pixels between clips: '
        read(*,*) numGutter
      endif
      print *, 'Output file for real coordinate piece list, or Return for none: '
      read(5, 50) pieceFile2
    endif
    !
    if (pieceFile .ne. ' ') call dopen(3, pieceFile, 'new', 'f')
    if (pieceFile2 .ne. ' ') call dopen(4, pieceFile2, 'new', 'f')
  endif
  !
  ! set up for loop on model objects
  !
  nzOut = 0
  numClip = 0
  numFile = 0
  dsum = 0.
  dmin2 = 1.e30
  dmax2 = -1.e30
  do iobj = 1, max_mod_obj
    if (npt_in_obj(iobj) > 0) then
      loopStart = 1
      loopEnd = npt_in_obj(iobj)
      if (ifStartEnd == 0) then
        loopEnd = 1
      elseif (ifStartEnd > 0) then
        loopStart = loopEnd
      endif

      do loop = loopStart, loopEnd
        ipnt = abs(object(loop + ibase_obj(iobj)))
        ind(1:3) = nint(p_coord(1:3, ipnt) + offset(1:3))
        write(*,'(3f12.5,3i7)') (p_coord(i, ipnt), i=1, 3), (ind(i), i=1, 3)
        !
        ! is it inside limits?
        !
        indLeft = ind(1) - npixLeft
        indRight = ind(1) + npixRight
        indBot = ind(2) - npixBot
        indTop = ind(2) + npixTop
        indZlo = ind(3) - nzBefore
        indZhi = ind(3) + nzAfter
        ! print *,ind(3), indZlo, indZhi
        if (ind(1) >= indXmin .and. ind(1) <= indXmax .and. &
            ind(2) >= indYmin .and. ind(2) <= indYmax .and. &
            indZlo >= indZmin .and. indZhi <= indZmax) then
          !
          ! set up the file if doing series, use this counter regardless
          !
          numFile = numFile + 1
          if (ifSeries .ne. 0) then
            if (numPredict < 100) then
              write(convNum, '(i2.2)') numFile
            elseif (numPredict < 1000) then
              write(convNum, '(i3.3)') numFile
            elseif (numPredict < 10000) then
              write(convNum, '(i4.4)') numFile
            else
              write(convNum, '(i5.5)') numFile
            endif
            inFile = concat(concat(rootName, '.'), convNum)
            call imopen(2, inFile, 'new')
            nxyz2(1) = numPixBox
            nxyz2(2) = nyPixBox
            nxyz2(3) = nzClip
            call itrhdr(2, 1)
            call ialsiz(2, nxyz2, nxyzst)
            dsum = 0.
            dmin2 = 1.e30
            dmax2 = -1.e30
          endif
          !
          ! loop on sections
          !
          do indZ = indZlo, indZhi
            if (ifXform .ne. 0) then
              indg = 0
              do ilist = 1, numListz
                if (listz(ilist) == indZ) indg = ilist
              enddo
              if (indg .ne. 0) then
                call xfapply(g(1, 1, indg), xCen, yCen, p_coord(1, ipnt), &
                    p_coord(2, ipnt), xNew, yNew)
                ind(1) = nint(xNew + offset(1))
                ind(2) = nint(yNew + offset(2))
                indLeft = ind(1) - npixLeft
                indRight = ind(1) + npixRight
                indBot = ind(2) - npixBot
                indTop = ind(2) + npixTop
              endif
            endif
            indar = indZ + 1 - indZlo
            !
            ! zero out the box
            !
            do iy = 1, numPixSq
              brray(iy + ibBase) = dmean
            enddo
            !
            ! loop on pieces, find intersection with each piece
            !
            do ipc = 1, numPcList
              if (izPcList(ipc) == indZ) then
                ixpc = ixPcList(ipc)
                iypc = iyPcList(ipc)
                ipcXstart = max(indLeft, ixpc)
                ipcYstart = max(indBot, iypc)
                ipcXend = min(indRight, ixpc + nx - 1)
                ipcYend = min(indTop, iypc + ny - 1)
                if (ipcXstart <= ipcXend .and. ipcYstart <= ipcYend) then
                  !
                  ! if it intersects, read in intersecting part, move it
                  ! into appropriate part of array
                  !
                  call imposn(1, ipc - 1, 0)
                  call irdpas(1, array(iaBase + 1), numPixBox, nyPixBox,  &
                      ipcXstart - ixpc, ipcXend - ixpc, ipcYstart - iypc,  &
                      ipcYend - iypc,*99)
                  do iy = 1, ipcYend + 1 - ipcYstart
                    do ix = 1, ipcXend + 1 - ipcXstart
                      brray(ibBase + ix + ipcXstart - indLeft +  &
                          (iy + ipcYstart - indBot - 1) * numPixBox) =  &
                          array(iaBase + ix + (iy - 1) * numPixBox)
                    enddo
                  enddo
                endif
              endif
            enddo
            !
            ! write piece list and slice
            !
            if (pieceFile2 .ne. ' ') write(4, '(3i6)') indLeft, indBot, indZ
            call iclden(brray(ibBase + 1), numPixBox, nyPixBox, 1, numPixBox, 1, &
                nyPixBox, tmin, tmax, tmean)
            dmin2 = min(dmin2, tmin)
            dmax2 = max(dmax2, tmax)
            dsum = dsum + tmean
            call iwrsec(2, brray(ibBase + 1))
            nzOut = nzOut + 1
            !
            ! Add to average only if flag set
            if (ifAverage .ne. 0) then
              do ix = 1, numPixSq
                jnd = ix + (indar - 1) * numPixSq
                agvArray(jnd) = agvArray(jnd) + brray(ibBase + ix)
              enddo
            endif
          enddo
          !
          ! Done with Z loop, close up particle file
          if (ifSeries .ne. 0) then
            dmean2 = dsum / nzClip
            do i = 1, 3
              cell(i) = nxyz2(i) * delta(i)
              cell(i + 3) = 90.
            enddo
            call ialsiz(2, nxyz2, nxyzst)
            call ialsam(2, nxyz2)
            call ialcel(2, cell)
            call ialorg(2, origin(1) - delta(1) * indLeft, &
                origin(2) - delta(2) * indBot, origin(3) - delta(3) * indZlo)
            call b3ddate(dateStrn)
            call time(timeStrn)
            write(titlech, 111) dateStrn, timeStrn
            read(titlech, '(20a4)') (title(kti), kti = 1, 20)
111         format('BOXSTARTEND: Individual box clipped out' ,t57,a9,2x,a8)
            call iwrhdr(2, title, 1, dmin2, dmax2, dmean2)
            call imclose(2)
          endif
          numClip = numClip + 1
        endif
      enddo
    endif
  enddo
  !
  ! take care of averages and finish stack file
  !
  if (ifSeries == 0) then
    if (ifAverage .ne. 0) then
      do indar = 1, nzClip
        do ix = 1, numPixSq
          jnd = ix + (indar - 1) * numPixSq
          agvArray(jnd) = agvArray(jnd) / numClip
        enddo
        jnd = 1 + (indar - 1) * numPixSq
        call iclden(agvArray(jnd), numPixBox, nyPixBox, 1, numPixBox, 1, nyPixBox, tmin, &
            tmax, tmean)
        dmin2 = min(dmin2, tmin)
        dmax2 = max(dmax2, tmax)
        dsum = dsum + tmean
        call iwrsec(2, agvArray(jnd))
        nzOut = nzOut + 1
      enddo
    endif
    dmean2 = dsum / nzOut
    nxyz2(3) = nzOut
    do i = 1, 3
      cell(i) = nxyz2(i) * delta(i)
      cell(i + 3) = 90.
    enddo
    call ialsiz(2, nxyz2, nxyzst)
    call ialsam(2, nxyz2)
    call ialcel(2, cell)

    call b3ddate(dateStrn)
    call time(timeStrn)
    textStrtEnd = 'starts'
    if (ifStartEnd > 0) textStrtEnd = 'ends  '
    if (ifStartEnd < 0) textStrtEnd = 'points'
    write(titlech, 101) numClip, textStrtEnd, dateStrn, timeStrn
    read(titlech, '(20a4)') (title(kti), kti = 1, 20)
101 format('BOXSTARTEND: ',i4,1x,a6,' clipped out and averaged' ,t57,a9,2x,a8)
    call iwrhdr(2, title, 1, dmin2, dmax2, dmean2)
    call imclose(2)
    numClip = numClip + 1
  endif
  !
  ! now put out piece list if desired
  !
  if (pieceFile .ne. ' ') then
    newYpiece = nint(sqrt(float(numClip)))
    newXpiece = (numClip + newYpiece-1) / newYpiece
    indX = 0
    indY = 0
    do i = 1, numClip
      ixpc = indX * (numPixBox + numGutter)
      iypc = indY * (nyPixBox + numGutter)
      do iz = 0, nzClip - 1
        write(3, '(3i6)') ixpc, iypc, iz
      enddo
      indX = indX + 1
      if (indX >= newXpiece) then
        indX = 0
        indY = indY + 1
      endif
    enddo
    close(3)
  endif
  print *,numFile, ' points boxed out'
  call exit(0)
99 call exitError('READING FILE')
end program boxstartend
