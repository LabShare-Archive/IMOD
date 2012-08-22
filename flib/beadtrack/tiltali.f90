! $Id$
!
! Routine to run the tiltalign operation
!
subroutine tiltali(ifDidAlign, ifAlignDone, resMean, iview)
  use tltcntrl
  implicit none
  include 'smallmodel.inc90'
  integer maxMetroTrials
  parameter (maxMetroTrials = 5)

  integer*4 ifDidAlign, iview, ifAlignDone
  real*4 resMean(*)
  !
  real*4 var(5*size(tilt) + 3*maxReal), grad(5*size(tilt) + 3*maxReal)
  real*4 varSave(5*size(tilt) + 3*maxReal)
  external funct
  double precision error
  real*4 DTOR
  data DTOR/0.0174532/
  integer*4 iminTiltSolv, i, nprojpt, iv, maxVar
  real*4 tiltSolveMin, tiltSolveMax, angle, tiltRange, fInit, rmsScale
  integer*4 nvarSearch, ifMapTilt, jpt, kpt, nvarGeometric, ier, kount, nsum
  real*4 f, fFinal, rsum
  integer*4 ior, ipt, ivOrig, j, metroLoop
  real*4 trialScale(maxMetroTrials) /1.0, 0.9, 1.1, 0.75, 0.5/
  real*4 resTmp(25000)
  logical itemOnList

  xyzFixed = .false.
  robustWeights = .false.
  ifAnyAlf = 0
  call proc_model(xcen, ycen, xdelt, ydelt, xorig, yorig, scaleXY, &
      nviewAll, minInView, iview, nviewLocal, iobjSeq, numObjDo, mapFileToView, &
      mapViewToFile, xx, yy, isecView, maxProjPt, maxReal, irealStr, &
      iobjAli, nview, nprojpt, nrealPt, izExclude, numExclude)
  ifDidAlign = 0
  if (nview >= minViewsTiltAli) then
    !
    ! if enough views, set up for solving tilt axis and tilt
    ! angles depending on range of tilt angles
    ! reload tilt from nominal angles to get the increments right
    ! Find minimum tilt for this angle range
    !
    tiltSolveMin = 1.e10
    tiltSolveMax = -1.e10
    rsum = 1.e10
    do iv = 1, nview
      angle = tiltAll(mapViewToFile(iv))
      tiltSolveMin = min(tiltSolveMin, angle)
      tiltSolveMax = max(tiltSolveMax, angle)
      tilt(iv) = DTOR * angle
      if (abs(angle) < rsum) then
        rsum = abs(angle)
        iminTiltSolv = iv
      endif
    enddo
    tiltRange = tiltSolveMax - tiltSolveMin
    ifRotFix = 0
    if (tiltRange < rangeDoAxis) ifRotFix = iminTiltSolv
    ifMapTilt = 1
    if (tiltRange < rangeDoTilt) ifMapTilt = 0
    ! print *,'ifrotfix', ifRotFix, '  ifmaptilt', ifMapTilt, &
    ! '  imintilt&solv', minTiltInd, iminTiltSolv
    call proc_vars(ifMapTilt, iminTiltSolv, var, nvarSearch)
    !
    ! find out how many points have not been done yet and decide
    ! whether to reinitialize dxy and xyz
    !
    nsum = 0
    do jpt = 1, nrealPt
      do i = 1, 3
        if (xyzSave(i, iobjAli(jpt)) == 0.) nsum = nsum + 1
      enddo
    enddo
    if (nsum > 0.2 * nrealPt) initXyzDone = 0

    !
    ! check h allocation; if it is not enough, try  to make it enough for the
    ! full set of views
    maxVar = nvarSearch + 3 * nrealPt
    iv = max((maxVar + 3) * maxVar, (3 * min(nrealPt, maxRealForDirectInit))**2)
    if (iv > maxH) then
      maxVar = (nviewAll + nview - 1) * nvarSearch / nview
      iv = max((maxVar + 3) * maxVar, iv)
      if (maxH > 0) deallocate(h)
      maxH = iv
      ! print *,'Allocated h to', maxH, ' based on', maxVar, maxVar + 3
      ! Here H is a double so it can be passed to solveXyzd as sprod but is allocated to 
      ! give maxH real elements because metro uses reals
      allocate(h(maxH / 2), stat = iv)
      call memoryError(iv, 'ARRAY FOR H MATRIX')
    endif
    !
    if (initXyzDone == 0) then
      !
      ! first time, initialize xyz and dxy
      !
      call remap_params(var)
      !
      call solveXyzd(xx, yy, isecView, irealStr, nview, nrealPt, tilt, rot, gmag, comp, &
          xyz, dxy, 0., h, error, ier)
      if (ier .ne. 0) write(*,'(/,a,/)') 'WARNING: failed to initialize X/Y/Z'// &
          ' coordinates for tiltalign solution'
      initXyzDone = 1
    else
      do jpt = 1, nrealPt
        kpt = iobjAli(jpt)
        xyz(1:3, jpt) = xyzSave(1:3, kpt) / scaleXY
      enddo
      do iv = 1, nview
        dxy(1:2, iv) = dxySave(1:2, mapViewToFile(iv)) / scaleXY
      enddo
    endif
    !
    ! pack the xyz into the var list
    !
    nvarGeometric = nvarSearch
    do jpt = 1, nrealPt - 1
      do i = 1, 3
        nvarSearch = nvarSearch + 1
        var(nvarSearch) = xyz(i, jpt)
      enddo
    enddo
    !
    ! save the variable list for multiple trials and
    ! ability to restart on errors.
    ! 1 / 25 / 06: changed to restart on all errros, including too many cycles,
    ! since varying metro factor  works for this too
    !
    varSave(1:nvarSearch) = var(1:nvarSearch)
    !
    metroLoop = 1
    ier = 1
    rmsScale = scaleXY**2 / nprojpt
    do while (metroLoop <= maxMetroTrials .and. ier .ne. 0)
      firstFunct = .true.
      call funct(nvarSearch, var, fInit, grad)
      ! WRITE(6, 70) fInit
      ! 70      FORMAT(/' Variable Metric minimization',T50, &
      ! 'Initial F:',T67,E14.7)
      !
      CALL metro(nvarSearch, var, funct, F, Grad, facMetro * trialScale(metroLoop), &
          eps, -nCycle, IER, H, KOUNT, rmsScale)
      metroLoop = metroLoop + 1
      if (ier .ne. 0) then
        if (metroLoop <= maxMetroTrials) then
          print *,'Metro error #', ier, ', Restarting with step ', &
              'factor of ', facMetro * trialScale(metroLoop)
          var(1:nvarSearch) = varSave(1:nvarSearch)
        endif
      endif
    enddo
    !
    ! Final call to FUNCT
    CALL FUNCT(nvarSearch, var, fFinal, Grad)
    !
    ! unscale all the points, dx, dy, and restore angles to
    ! degrees
    !
    tiltRange = 0
    ifMapTilt = 0
    do i = 1, nrealPt
      ior = iobjAli(i)
      xyz(1:3, i) = xyz(1:3, i) * scaleXY
      xyzSave(1:3, ior) = xyz(1:3, i)
      rsum = 0.
      do ipt = irealStr(i), irealStr(i + 1) - 1
        rsum = rsum + sqrt(xresid(ipt)**2 + yresid(ipt)**2)
        resTmp(ipt + 1 - irealStr(i)) = scaleXY* &
            sqrt(xresid(ipt)**2 + yresid(ipt)**2)
      enddo
      resMean(ior) = rsum * scaleXY / (irealStr(i + 1) - irealStr(i))
      ! write(*,'(i4,(10f7.3))') ior, resMean(ior), (resTmp(ipt), &
      ! ipt = 1, min(9, irealStr(i + 1) - irealStr(i)))
      tiltRange = tiltRange + rsum * scaleXY
      ifMapTilt = ifMapTilt + irealStr(i + 1) - irealStr(i)
    enddo
    !
    WRITE(6, 98) nview, KOUNT, sqrt(fFinal * rmsScale), tiltRange / ifMapTilt
98  format(i4,' views,',i5,' cycles, F =',f13.6,', mean residual =', f8.2)
    !
    ! Error reports:
    if (IER .NE. 0) then
      print *,'tiltalign error going to view', iview, &
          ' even after varying step factor'
    END IF
    !
    do iv = 1, nview
      ivOrig = mapViewToFile(iv)
      dxySave(1, ivOrig) = dxy(1, iv) * scaleXY
      dxySave(2, ivOrig) = dxy(2, iv) * scaleXY
      rotOrig(ivOrig) = rot(iv) / DTOR
      tiltOrig(ivOrig) = tilt(iv) / DTOR
      gmagOrig(ivOrig) = gmag(iv)

!!$        ifMapTilt = 0
!!$        rsum = 0
!!$        do i = 1, nprojpt
!!$        if (iv == isecView(i)) then
!!$        rsum = rsum + scaleXY * sqrt(xresid(i)**2 + yresid(i)**2)
!!$        ifMapTilt = ifMapTilt + 1
!!$        endif
!!$        enddo
!!$        write(*,'(i4,3f8.2,f8.4,2f10.2,f8.2)') ivOrig, rotOrig(ivOrig), &
!!$                    tiltOrig(ivOrig), tiltOrig(ivOrig) - tiltAll(ivOrig), gmag(iv), &
!!$                    dxySave(1, ivOrig), dxySave(2, ivOrig), rsum / ifMapTilt
    enddo
!!$      do ior = 1, max_mod_obj
!!$      if (itemOnList(ior, iobjAli, nrealPt)) &
!!$                    write(*,'(i4,3f10.2)') ior, (xyzSave(j, ior), j = 1, 3)
!!$      enddo

    ifDidAlign = 1
    ifAlignDone = 1
  endif
  return
end subroutine tiltali
