! $Id$
!
! Module for variables created when new mapping into local subsets added(?)
module tltcntrl
  !
  use alivar
  ! allocated to limview or maxInArea
  real*4, allocatable :: tiltOrig(:), gmagOrig(:), rotOrig(:), dxySave(:,:), tiltAll(:)
  integer*4, allocatable :: iobjAli(:), ivsepIn(:,:), izExclude(:)
  integer*4 nmapSpecMag(MAXGRP), ivSpecStrMag(MAXGRP), ivSpecEndMag(MAXGRP)
  integer*4 nmapSpecRot(MAXGRP), ivSpecStrRot(MAXGRP), ivSpecEndRot(MAXGRP)
  integer*4 nmapSpecTilt(MAXGRP), ivSpecStrTilt(MAXGRP)
  integer*4 ivSpecEndTilt(MAXGRP)
  integer*4 nsepInGrpIn(MAXGRP)
  integer*4 nviewAll, minTiltInd, minInView, minViewsTiltAli, initXyzDone
  real*4 rangeDoAxis, rangeDoTilt, scaleXY, xcen, ycen, xorig, yorig
  real*4 xdelt, ydelt, facMetro, eps
  integer*4 nCycle, nviewLocal, numObjDo, maxH, numExclude
  integer*4 nmapMag, nmapRot, nmapTilt, nRanSpecMag, nRanSpecRot, nRanSpecTilt
  ! Allocated to maxAllReal, and (3, maxAllReal)
  integer*4, allocatable :: iobjSeq(:)
  real*4, allocatable :: xyzSave(:,:)
  real*8, allocatable :: h(:)
end module tltcntrl
