! irdhdr.f90  - Header "reading" routine
!
! $Id$
!
! Returns basic header information about a file opened with iiuOpen/imopen and prints 
! a standard header output
!
! [nxyz] - size of file in X, Y, and Z  ^
! [mxyz] - Number of intervals (sample size) in X, Y, Z
! [imode] - Data storage mode (0 for bytes, 1 or 6 for signed or unsigned 2-byte 
! integers, 2 for floats, 3 and 4 for short integer or floating point complex, 16 for
! RGB)  ^
! [dmin], [dmax], [dmean] - Min, Max, & Mean densities
!
subroutine irdhdr(iunit, nxyz, mxyz, imode, dmin, dmax, dmean)
  implicit none
  integer*4 nxyz(3), mxyz(3), nxyzst(3), mapcrs(3), labels(20, 10)
  real*4 delta(3), cell(6), tiltOrig(3), tilt(3)
  character*1 lxyz(3) /'X', 'Y', 'Z'/
  !
  character*27 modeNames(8), modeLabel, minLabel, maxLabel, meanLabel
  data modeNames/'(byte)', '(16-bit integer)', '(32-bit real)', &
      '(complex integer)', '(complex)', '(unknown)', &
      '(unsigned 16-bit integer)', 'RGB color'/
  integer*4 nameMap(0:16) /1, 2, 3, 4, 5, 6, 7, 6, 6, 0, 0, 0, 0, 0, 0, 0, 8/
  !
  integer*4 i, k, ispg, iunit, imode, idtype, numExtra, numLabels
  integer*4 lensnum, nd1, nd2, iflags, imodFlags, ifBrief
  real*4 dmin, dmax, dmean, vd1, vd2, xorig, yorig, zorig
  logical*4 doExtra, doPrint
  integer*4 iiuRetBrief, iiuRetPrint
  ifBrief = iiuRetBrief()
  doPrint = iiuRetPrint() > 0
  doExtra = ifBrief == 0 .and. doPrint

  call iiuRetBasicHead(iunit, nxyz, mxyz, imode, dmin, dmax, dmean)
  call iiuRetImodFlags(iunit, imodFlags, k)
  call iiuFileInfo(iunit, i, k, iflags)
  call iiuRetSize(iunit, nxyz, mxyz, nxyzst)

  if (btest(iflags, 2) .and. doExtra) write(6, 1005)
1005 format(/,20x,'This file has an old-style MRC header.')

  if (btest(iflags, 4) .and. doExtra) &
    write(6, '(/,9x,a)') 'This file has invalid axis indices, adjusting them to 1,2,3'
  !
  !
  if (btest(iflags, 3) .and. doExtra) write(6, 1007)
1007 format(/,'  Assuming that the header entry for 128 ', &
      'integers per section is an error')
  !
  ! Get and Write out header information
  !
  call iiuRetDataType(iunit, idtype, lensNum, nd1, nd2, vd1, vd2)
  call iiuRetDelta(iunit, delta)
  call iiuRetCell(iunit, cell)
  call iiuRetAxisMap(iunit, mapcrs)
  call iiuRetOrigin(iunit, xorig, yorig, zorig)
  call iiuRetLabels(iunit, labels, numLabels)
  call iiuRetTilt(iunit, tilt)
  call iiuRetTiltOrig(iunit, tiltOrig)
  call iiuRetSpaceGroup(iunit, ispg)
  call iiuRetNumExtended(iunit, numExtra)
  minLabel = ' '
  maxLabel = ' '
  meanLabel = ' '
  if (imode < 0 .or. imode > 16) then
    modeLabel = '(unknown)'
  else if (imode == 0 .and. (btest(iflags, 1) .or. btest(imodFlags, 0))) then
    !
    ! For signed bytes, the file value was already adjusted on read-in, subtract it back
    modeLabel = '(bytes - signed in file)'
    write(minLabel, 1010) dmin - 128.
    write(maxLabel, 1010) dmax - 128.
    write(meanLabel, 1010) dmean -128.
1010 format(' (',g13.5,' in file)')
  else
    modeLabel = modeNames(nameMap(imode))
  endif
  if (doExtra) write(6, 1000) nxyz, imode, modeLabel, &
      nxyzst, mxyz, delta, (cell(k), k = 4, 6), (lxyz(mapcrs(k)), k = 1, 3), &
      xorig, yorig, zorig, dmin, minLabel, dmax, maxLabel, dmean, &
      meanLabel, tiltOrig, tilt, ispg, numExtra, idtype, lensNum, &
      numLabels, ((labels(i, k), i = 1, 20), k = 1, numLabels)
  !
  ! for unix output without carriagecontrol:
  ! DNM changed leading 2X to 1X on each line, changed tilt angle output
  ! from 6f6.1 to f5.1, 5f6.1, eliminated 1X before titles, eliminated 80th
  ! char by changing 20A4 to 19A4, A3
  !
1000 format(/, &
      1x,'Number of columns, rows, sections .....',3I8,/, &
      1x,'Map mode ..............................',i5,3x,a,/, &
      1x,'Start cols, rows, sects, grid x,y,z ...',i5,2i6,1x,3i7,/, &
      1x,'Pixel spacing (Angstroms)..............',1x,3g11.4,/, &
      1x,'Cell angles ...........................',3F9.3,/, &
      1x,'Fast, medium, slow axes ...............',3(4x,a1),/, &
      1x,'Origin on x,y,z .......................',1x,3g12.4,/, &
      1x,'Minimum density .......................',g13.5,a,/, &
      1x,'Maximum density .......................',g13.5,a,/, &
      1x,'Mean density ..........................',g13.5,a,/, &
      1x,'tilt angles (original,current) ........',6f6.1,/, &
      1x,'Space group,# extra bytes,idtype,lens .',4I9,//, &
      1x,i5,' Titles :' / 10(19a4,a3/))
  !
  if (doPrint .and. ifBrief > 0) then
    write(6, 1008) nxyz, delta, imode, dmin, dmax, dmean, (labels(i, 1), i = 1, 20)
    if (numLabels > 1) write(6, 1009) (labels(i, numLabels), i = 1, 20)
    write(6,*)
  endif
1008 format(' Dimensions:',3i7,'   Pixel size:', 3g11.4,/,' Mode:', i3, &
      15x,'Min, max, mean:', 3g13.5, /, 19a4,a3)
1009 format(19a4,a3)
  !
  ! DNM changed definitions and output:
  ! itype = 0       normal mono data
  ! itype = 1       tilt set    N1 = axis, v1=delta angle, v2=start angle
  ! itype = 2       serial stereo pairs n1=axis  v1, v2= Left, right angles
  ! itype = 3       avg mono n1=#secs wide, n2=#secs skip
  ! itype = 4       avg stereo n1=#secs wide, n2=#secs skip v1, v2=L, R angle
  if (doPrint .and. idtype > 0 .and. ifBrief <= 0) then
    if (idtype == 1) then
      write(6, 1001) lxyz(nd1), vd1, vd2
    else if (idtype == 2) then
      write(6, 1002) lxyz(nd1), vd1, vd2
    else if (idtype == 3) then
      write(6, 1003) nd1, nd2
    else if (idtype == 4) then
      write(6, 1004) nd1, nd2, vd1, vd2
    endif
1001 format(5x,' TILT data set, axis= ',a1, &
        ' delta,start angle= ',2f8.2,/)
1002 format(' SERIAL STEREO data set, axis= ',a1,' left angle= ', &
        f8.2,' right angle= ',f8.2,/)
1003 format(5x,' AVERAGED data set, Navg,Noffset   =  ',2i6,/)
1004 format(5x,' AVG STEREO data set, Navg,Noffset= ', &
        2i3,' L,R angles= ',2f8.2,/)
  endif
  !
  return
end subroutine irdhdr
