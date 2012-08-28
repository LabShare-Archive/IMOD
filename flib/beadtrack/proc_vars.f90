! INPUT_VARS gets the specifications for the geometric variables
! rotation, tilt, mag, etc.  It fills the VAR array and
! the different variable and mapping arrays.
!
! $Id$
!
subroutine proc_vars(ifMapTilt, irefTilt, var, nvarSearch)
  !
  use tltcntrl
  use mapSep
  implicit none

  real*4 var(*)
  integer*4 ifMapTilt
  integer*4 mapList(size(tilt)), irefTilt, nvarSearch
  real*4 rotStart, defRot, power, fixDum, grpSize(size(tilt))
  integer*4 i, iv, nvarAngles, iflin, iref1
  real*4 dtor/0.0174532/
  character*8 varName(4*size(tilt))
  !
  ! Remap the separate group information for the current views
  !
  do iv = 1, ngsep
    nsepInGrp(iv) = nsepInGrpIn(iv)
    do i = 1, nsepInGrp(iv)
      ivsep(i, iv) = ivsepIn(i, iv)
    enddo
    call mapSeparateGroup(ivsep(1, iv), nsepInGrp(iv), mapFileToView, &
        nfileViews)
  enddo
  !
  ! Use the grouping to get the mapping
  !
  iflin = 1
  call setGrpSize(tilt, nview, 0., grpSize)
  call makeMapList(nview, mapList, grpSize, mapFileToView, nfileViews, nmapRot, &
      ivSpecStrRot, ivSpecEndRot, nmapSpecRot, nRanSpecRot, mapList, 0)
  !
  ! Set up default rotation value so fixed rotation will be correct
  !
  nvarSearch = 0
  iref1 = ifRotFix
  rotStart = 0.
  if (iref1 > 0) rotStart = rotOrig(mapViewToFile(iref1))
  defRot = rotStart * dtor
  !
  ! get the variables allocated
  !
  call analyze_maps(rot, mapRot, linRot, frcRot, fixedRot, fixDum, &
      iflin, mapList, nview, iref1, 0, defRot, 'rot ', var, varName, &
      nvarSearch, mapViewToFile)
  !
  ! reload the old rotation values by looking at all mappings that are
  ! not linear combos
  !
  do i = 1, nview
    rot(i) = rotOrig(mapViewToFile(i)) * dtor
    if (mapRot(i) > 0 .and. linRot(i) == 0) &
        var(mapRot(i)) = rot(i)
  enddo
  !
  ! set up for fixed tilt angles
  !
  do i = 1, nview
    mapList(i) = 0
  enddo
  iflin = 0
  power = 0.
  if (ifMapTilt .ne. 0) then
    if (nmapTilt > 1) then
      iflin = 1
      power = 1.
    endif
    call setGrpSize(tilt, nview, power, grpSize)
    call makeMapList(nview, mapList, grpSize, mapFileToView, nfileViews, &
        nmapTilt, ivSpecStrTilt, ivSpecEndTilt, nmapSpecTilt, &
        nRanSpecTilt, mapList, 0)
  endif
  !
  call analyze_maps(tilt, mapTilt, linTilt, frcTilt, fixedTilt, &
      fixedTilt2, iflin, mapList, nview, irefTilt, 0, -999., &
      'tilt', var, varName, nvarSearch, mapViewToFile)
  !
  ! set tiltInc so it will give back the right tilt for whatever
  ! mapping or interpolation scheme is used
  !
  do iv = 1, nview
    tiltInc(iv) = 0.
    if (mapTilt(iv) .ne. 0) then
      if (linTilt(iv) > 0) then
        tiltInc(iv) = tilt(iv) - (frcTilt(iv) * var(mapTilt(iv)) + &
            (1. - frcTilt(iv)) * var(linTilt(iv)))
      elseif (linTilt(iv) == - 1) then
        tiltInc(iv) = tilt(iv) - (frcTilt(iv) * var(mapTilt(iv)) + &
            (1. - frcTilt(iv)) * fixedTilt)
      elseif (linTilt(iv) == - 2) then
        tiltInc(iv) = tilt(iv) - (frcTilt(iv) * var(mapTilt(iv)) + &
            (1. - frcTilt(iv)) * fixedTilt2)
      else
        tiltInc(iv) = tilt(iv) - var(mapTilt(iv))
      endif
    endif
  enddo
  !
  ! Now reload the previous values and put them into vars
  !
  do i = 1, nview
    if (mapTilt(i) > 0) tilt(i) = dtor * tiltOrig(mapViewToFile(i))
    if (mapTilt(i) > 0 .and. linTilt(i) == 0 .and. &
        abs(tiltInc(i)) < 1.e-6) var(mapTilt(i)) = tilt(i)
  enddo
  nvarAngles = nvarSearch
  !
  ! Set up the mag mapList
  !
  iflin = 1
  call setGrpSize(tilt, nview, 0., grpSize)
  call makeMapList(nview, mapList, grpSize, mapFileToView, nfileViews, nmapMag, &
      ivSpecStrMag, ivSpecEndMag, nmapSpecMag, nRanSpecMag, mapList, 0)
  !
  ! Set up mag variables
  !
  call analyze_maps(gmag, mapGmag, linGmag, frcGmag, fixedGmag, fixDum, &
      iflin, mapList, nview, irefTilt, 0, 1., 'mag ', var, varName, &
      nvarSearch, mapViewToFile)
  !
  ! Reload values and stuff them into vars by looking at all mappings
  ! that are not linear combos
  !
  do i = 1, nview
    gmag(i) = gmagOrig(mapViewToFile(i))
    if (mapGmag(i) > 0 .and. linGmag(i) == 0) &
        var(mapGmag(i)) = gmag(i)
  enddo
  !
  ! This should be done once before calling
  !
  mapProjStretch = 0
  projStrRot = defRot
  projSkew = 0.
  mapBeamTilt = 0
  beamTilt = 0.
  return
end subroutine proc_vars
