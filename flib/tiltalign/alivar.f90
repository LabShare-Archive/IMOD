! Modules for alignment variables shared by tiltalign and beadtrack
!
! $Id$
!
! Array size limits: in one place!
module arraymaxes
  implicit none
  integer MAXGRP, maxView, maxWgtRings, maxRealForDirectInit
  parameter (MAXGRP = 20, maxWgtRings = 10, maxRealForDirectInit = 200)
  integer maxProjPt, maxReal
end module arraymaxes

! Main alignment variables
module alivar
  use arraymaxes
  implicit none
  !
  real*4, allocatable :: xx(:), yy(:), xyz(:,:), dxy(:,:)
  real*4, allocatable :: xresid(:), yresid(:), weight(:), viewMedianRes(:), trackResid(:)
  integer*4, allocatable :: isecView(:), irealStr(:)
  !
  integer*4, allocatable :: mapTilt(:), mapGmag(:), mapComp(:)
  integer*4, allocatable :: mapDmag(:), mapSkew(:), mapRot(:)
  integer*4, allocatable :: linTilt(:), linGmag(:), linComp(:)
  integer*4, allocatable :: linDmag(:), linSkew(:), linRot(:)
  integer*4, allocatable :: mapAlf(:), linAlf(:)
  integer*4 nrealPt, mapDmagStart, mapDumDmag, ifAnyAlf, nview, ifRotFix
  real*4 dumDmagFac
  real*4, allocatable :: frcTilt(:), frcGmag(:), frcComp(:)
  real*4, allocatable :: frcDmag(:), frcSkew(:), frcRot(:)
  real*4, allocatable :: rot(:), tilt(:), gmag(:), comp(:)
  real*4, allocatable :: tiltInc(:), dmag(:), skew(:)
  real*4, allocatable :: frcAlf(:), alf(:)
  real*4 fixedTilt, fixedGmag, fixedComp, fixedDmag, fixedSkew
  real*4 fixedTilt2, fixedRot, fixedAlf, projStrRot, projSkew, beamTilt, kfacRobust
  real*4 smallWgtMaxFrac, smallWgtThreshold
  integer*4, allocatable :: mapViewToFile(:), mapFileToView(:)
  integer*4, allocatable :: mapRealToTrack(:), mapTrackToReal(:), indFullTrack(:)
  integer*4 nfileViews, mapProjStretch, mapBeamTilt
  !
  real*4, allocatable :: glbRot(:), glbTilt(:), glbAlf(:)
  real*4, allocatable :: glbGmag(:), glbDmag(:), glbSkew(:)
  integer*4 incrGmag, incrDmag, incrSkew, incrTilt, incrAlf, incrRot
  logical firstFunct, xyzFixed, robustWeights/.false./, patchTrackModel/.false./
  logical*4 robustByTrack/.false./
  integer*4 numWgtGroups, numFullPatchTracks/0/, numFullTracksUsed, numTrackGroups
  integer*4, allocatable :: indProjWgtList(:), ivStartWgtGroup(:), ipStartWgtView(:)
  integer*4, allocatable :: itrackGroup(:)

CONTAINS

  subroutine allocateAlivar(numProjPt, numView, numReal, ierr)
    implicit none
    integer*4 numProjPt, numView, numReal, ierr
    maxProjPt = numprojPt + 10
    maxView = numView + 4
    maxReal = numReal + 4
    allocate(xx(maxProjPt), yy(maxProjPt), xyz(3, maxReal), dxy(2, maxView), &
        xresid(maxProjPt), yresid(maxProjPt), weight(maxProjPt), viewMedianRes(maxView), &
        isecView(maxProjPt), irealStr(maxReal), indProjWgtList(maxProjPt), &
        ivStartWgtGroup(maxView * maxWgtRings + 1), &
        ipStartWgtView(maxView * maxWgtRings + 1), &
        mapTilt(maxView), mapGmag(maxView), mapComp(maxView), &
        mapDmag(maxView), mapSkew(maxView), mapRot(maxView), &
        linTilt(maxView), linGmag(maxView), linComp(maxView), &
        linDmag(maxView), linSkew(maxView), linRot(maxView), &
        mapAlf(maxView), linAlf(maxView), &
        frcTilt(maxView), frcGmag(maxView), frcComp(maxView), &
        frcDmag(maxView), frcSkew(maxView), frcRot(maxView), &
        rot(maxView), tilt(maxView), gmag(maxView), comp(maxView), &
        tiltInc(maxView), dmag(maxView), skew(maxView), frcAlf(maxView), alf(maxView), &
        mapViewToFile(maxView), mapFileToView(nfileViews), &
        glbRot(maxView), glbTilt(maxView), glbAlf(maxView), &
        glbGmag(maxView), glbDmag(maxView), glbSkew(maxView), stat = ierr)
    return
  end subroutine allocateAlivar
end module alivar

! Mapping variables
module mapsep
  use arraymaxes
  implicit none
  integer*4 nsepingrp(MAXGRP), ngsep
  integer*4, allocatable :: ivsep(:,:)

CONTAINS

  subroutine allocateMapsep(ierr)
    integer*4 ierr
    allocate(ivsep(maxView, MAXGRP), stat = ierr)
    return
  end subroutine allocateMapsep
end module mapsep
