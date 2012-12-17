! A package that used to provide an interface to NCAR graphics and that
! has calls similar to DNM's old graphics package calls.  It provides
! for drawing with multiple line thicknesses, now using line widths
! in Postscript
!
! $Id$
!

module psparams
  integer*4 nThick/1/, ifPSopen/0/
  real*4 width/7.5/, unitsPerInch/300./, safe/0.00022/, xcur/0./, ycur/0./
  real*4 unitLen, extraLen, halfThick, symScale/0.15/
end module psparams

subroutine psSetup(ithickSet, widthSet, upiSet, safeSet, ifset)
  use psparams
  character*80 filename
  common /gmetaname/filename
  data filename /'gmeta.ps'/

  if (ifPSopen == 0) then
    ierr = imodBackupFile(filename)
    if (ierr .ne. 0) write(6,*) &
        ' Error attempting to rename existing file ', filename
    call psOpen(filename, 0.5, 1.75, unitsPerInch)
    ifPSopen = 1
  endif
  nThick = ithickSet
  fThick = nThick - 1.
  if (nThick == 2) fThick = 1.5
  if (nThick < 2) fThick = 1.
  call psLineWidth(fThick)
  if (ifset .ne. 0) then
    if (widthSet > 0) width = widthSet
    if (upiSet > 0.) unitsPerInch = upiSet
    if (safeSet >= 0.) safe = safeSet
  else
    widthSet = width
    upiSet = unitsPerInch
    safeSet = safe
  endif
  xcur = 0.
  ycur = 0.
  unitLen = 1. / unitsPerInch
  extraLen = max(0., unitLen * (nThick - 3))
  halfThick = 0.5 * unitLen * (nThick - 1)
  ! write(*,*) 'udlen,exlen,hafthk', unitLen, extraLen, halfThick
  return
end subroutine psSetup

subroutine psPointAbs(x, y)
  use psparams
  xcur = x
  ycur = y
  call psPoint(trnc(x), trnc(y))
  return
end subroutine psPointAbs

subroutine psMoveAbs(xin, yin)
  use psparams
  xcur = xin
  ycur = yin
  if (nThick == 1) call psFirstPoint(trnc(xcur), trnc(ycur))
  return
end subroutine psMoveAbs

subroutine psMoveInc(dx, dy)
  use psparams
  call imma(xcur + dx, xcur + dy)
  return
end subroutine psMoveInc


subroutine psVectInc(dx, dy)
  use psparams
  call imva(xcur + dx, ycur + dy)
  return
end subroutine psVectInc

subroutine psVectAbs(x, y)
  use psparams
  if (nThick <= 1) then
    call psVector(trnc(x), trnc(y))
  else
    ddx = x - xcur
    ddy = y - ycur
    ! write(*,*) 'ddx,ddy', ddx, ddy
    if (ddx .ne. 0. .or. ddy .ne. 0.) then
      ddlen = sqrt(ddx**2 + ddy**2)
      xinc = 0.5 * ddx * extraLen / ddlen
      yinc = 0.5 * ddy * extraLen / ddlen
      xs = trnc(xcur - xinc)
      xe = trnc(x + xinc)
      ys = trnc(ycur - yinc)
      ye = trnc(y + yinc)
      call psFirstPoint(xs, ys)
      call psVector(xe, ye)
    endif
  endif
  xcur = x
  ycur = y
  return
end subroutine psVectAbs

subroutine psExit()
  call psPakOff()
  call exit(0)
  return
end subroutine psExit

subroutine psPakOff()
  use psparams
  if (ifPSopen == 0) return
  call psFrame()
  call psClose()
  ifPSopen = 0
end subroutine psPakOff

!
! Plot out the data to screen or printer view script
!
subroutine pltout(metaScreen)
  character*120 comstr
  character*80 filename, imodPath
  character*20 imodshell, outcom
  common /gmetaname/filename

  call psPakOff()
  !
  ! 10/28/03: switch to calling imodpsview for printing too and run
  ! tcsh explicitly
  !
  if (metaScreen == 0) then
    outcom = 'imodpsview -p'
  else
    outcom = 'imodpsview'
  endif
  if (imodGetenv('IMOD_DIR', imodPath) .ne. 0) then
    print *, 'impak failed to get IMOD_DIR environment '// &
        'variable for running imodpsview'
    return
  endif
  if (imodGetenv('IMOD_CSHELL', imodshell) .ne. 0) &
      imodshell = 'tcsh'
  write(comstr, '(a,1x,a,1x,a,a,a,1x,a,1x,a)') &
      trim(imodshell), '-f', trim(imodPath), '/bin/', trim(outcom), trim(filename)
  call system(comstr)
  if (metaScreen .ne. 0) write(*,101) trim(filename)
101 format(/,' WARNING: If you start making more plots, a new plot' &
      ,' file will be started,',/, '          the ', &
      'current file will become a backup (',a,'~),' &
      ,/,'          and a previous backup will be deleted.')
  return
end subroutine pltout

! Wrapper functions for old names in use here and symbol routine
subroutine imset(ithickSet, widthSet, upiSet, safeSet, ifset)
  call psSetup(ithickSet, widthSet, upiSet, safeSet, ifset)
end subroutine imset

subroutine imma(xin, yin)
  call psMoveAbs(xin, yin)
  return
end subroutine imma

subroutine imva(x, y)
  call psVectAbs(x, y)
  return
end subroutine imva

subroutine imexit()
  call psExit()
  return
end subroutine imexit

subroutine imsymb(x, y, itype)
  call psSymbol(x, y, itype)
  return
end subroutine imsymb

subroutine symsiz(size)
  call psSymSize(size)
  return
end subroutine symsiz

subroutine imgrid(xStart, yStart, xRange, yRange, nTicks, tickSize)
  call psGridLine(xStart, yStart, xRange, yRange, nTicks, tickSize)
  return
end subroutine imgrid

subroutine imlgrd(xStart, yStart, xRange, yRange, tickVals, nTicks, tickSize)
  call psLogGrid(xStart, yStart, xRange, yRange, tickVals, nTicks, tickSize)
  return
end subroutine imlgrd

! Wrappers to old names in psf.c
subroutine point(x, y)
  call psPoint(x, y)
  return
end subroutine point

subroutine frstpt(x, y)
  call psFirstPoint(x, y)
  return
end subroutine frstpt

subroutine vector(x, y)
  call psVector(x, y)
  return
end subroutine vector

subroutine frame()
  call psFrame()
  return
end subroutine frame

subroutine wtstr(x, y, text, jsize, jor, jctr)
  character*(*) text
  call psWriteText(x, y, text, jsize, jor, jctr)
  return
end subroutine wtstr
