! XF2ROTMAGSTR
! A simple program to convert transforms to rotation-mag-stretch
!
program xf2rotmagstr
  implicit none
  integer maxSect
  character*320 strFile, errString
  parameter (maxSect = 10000)
  real*4 xform(2,3,maxSect), pixelSize
  integer*4 nxfRead, i, ierr, nx, ny, ibin, iflags, indWarpFile, numControl
  real*4 theta, smag, str, phi, smagMean
  integer*4 separateLinearTransform, readCheckWarpFile, getLinearTransform
  integer*4 getNumWarpPoints, getWarpGridSize
  !
  call setExitPrefix('ERROR: XF2ROTMAGSTR -')
  call getinout(1, strFile, strFile)
  indWarpFile = readCheckWarpFile(strFile, 0, 1, nx, ny, nxfRead, ibin, pixelSize, &
      iflags, errString)
  if (indWarpFile < -1) call exitError(errString)
  if (indWarpFile >= 0) then
    print *,'Reading warping file and extracting linear transformations'
    ierr = 1
    if (nxfRead <= maxSect) then
      do i = 1, nxfRead
        numControl = 0
        if (mod(iflags / 2, 2) > 0) then
          ierr = getNumWarpPoints(i, numControl)
        else 
          ierr = getWarpGridSize(i, nx, ny, numControl)
        endif
        if (numControl > 2) then
          if (separateLinearTransform(i) .ne. 0) call exitError( &
              'TRYING TO EXTRACT LINEAR TRANSFORM FROM WARPING TRANSFORM')
        endif
        ierr = getLinearTransform(i, xform(1, 1, i))
      enddo
      ierr = 0
    endif
  else

    call dopen(1, strFile, 'ro', 'f')
    call xfrdall2(1, xform, nxfRead, maxSect, ierr)
    if (ierr .eq. 2) call exitError('READING TRANSFORM FILE')
  endif
  if (ierr .eq. 1) call exitError('TOO MANY TRANSFORMS IN FILE FOR ARRAY')

  do i = 1, nxfRead
    call amat_to_rotmagstr(xform(1, 1, i), theta, smag, str, phi)
    smagMean = smag * sqrt(abs(str))
    write(*,110) i, theta, smag, str, phi, smagMean
110 format(i5, ': rot=',f8.2,', mag=',f7.4, &
        ', str=',f7.4,' on',f7.1,' axis', &
        ', Mean mag=',f7.4)
  enddo
  call exit(0)
end program xf2rotmagstr
