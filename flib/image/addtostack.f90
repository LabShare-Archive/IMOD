!*************ADDTOSTACK**********************************************
!
! ADDTOSTACK will append sections from one or more files to an
! existing image file.  The X and Y dimensions of all files must match.
! All sections will be appended from the files that are being added.
! No image transformation or scaling is performed.  The header of the
! resulting image file will be set to give "pixel spacing" of 1 (grid
! and cell sizes equal to image size) .
! See man page for details.
!
! David Mastronarde  5/28/90
!
program addtostack
  !
  implicit none
  integer*4 nxyz(3), mxyz(3), nxyzst(3), nxyz2(3), mxyz2(3), nx, ny, nz
  equivalence (nx, nxyz(1)), (ny, nxyz(2)), (nz, nxyz(3))
  real*4 title(20), cell(6)
  real*4, allocatable :: array(:)
  integer*4 ifcopy, numInFiles, i, mode, nzorig, ifil, iz, ierr, mode2, kti
  real*4 dmin, dmax, dmean, tmin, tmax, tmean, dsum, dmin2, dmax2, dmean2
  !
  character*320, allocatable :: inFile(:)
  character*320 comline, outFile
  character dat*9, tim*8
  character*80 titlech
  !
  write(*,'(1x,a,$)') 'Name of file to add sections to: '
  read(*,101) outFile
101 format(a)
  write(*,102)
102 format(' Enter 1 to make copy of file and append to copy',/ &
      ,9x,'(which needs more free disk space and requires you', &
      ' to purge old file),',/,'    or 0 to append to existing', &
      ' file',/,9x,'(which is quicker but could make the file' &
      ,' unusable in case of error)')
  read(*,*) ifcopy
  !
  write(*,'(1x,a,$)') &
      'Number of files to add onto end of this file: '
  read(*,*) numInFiles

  allocate(inFile(numInFiles + 10), stat=ierr)
  call memoryError(ierr, 'ARRAY FOR FILENAMES')

  print *,'Enter file names, one per line'
  do i = 1, numInFiles
    read(*,101) inFile(i)
  enddo
  !
  if (ifcopy .ne. 0) call copy_to_backup(outFile)
  !
  call imopen(2, outFile, 'old')
  call irdhdr(2, nxyz, mxyz, mode, dmin, dmax, dmean)
  call time(tim)
  call b3ddate(dat)
  call irtsiz(2, nxyz, mxyz, nxyzst)
  call irtcel(2, cell)
  !
  ! Just in case the I/O routines can handle > 2 Gpix, make the allocation work
  allocate(array(int(nx, kind=8) * ny + 10), stat=ierr)
  call memoryError(ierr, 'ARRAY FOR IMAGES')

  dsum = nz * dmean
  call imposn(2, nz, 0)
  nzorig = nz
  !
  do ifil = 1, numInFiles
    call imopen(1, inFile(ifil), 'ro')
    call irdhdr(1, nxyz2, mxyz2, mode2, dmin2, dmax2, dmean2)
    if (nxyz2(1) .ne. nx .or. nxyz2(2) .ne. ny) then
      print *,'FILE SIZE MISMATCH: APPEND OPERATION TRUNCATED'
      go to 90
    endif
    do iz = 1, nxyz2(3)
      call irdsec(1, array,*99)
      call iclden(array, nx, ny, 1, nx, 1, ny, tmin, tmax, tmean)
      dmin = min(dmin, tmin)
      dmax = max(dmax, tmax)
      dsum = dsum + tmean
      nz = nz + 1
      call iwrsec(2, array)
    enddo
    call imclose(1)
  enddo
90 dmean = dsum / nz
  !
  ! 12/21/00 DNM: switch to unconditionally setting the header to the
  ! values for proper scaling
  !
  call ialsiz_sam_cel(2, nx, ny, nz)
  !
  ! call ialsiz(2, nxyz, nxyzst)
  ! if mxyz=nxyz, keep this relationship
  ! if (mxyz(1) ==nx.and.mxyz(2) ==ny.and.mxyz(3) ==nzorig) then
  ! call ialsam(2, nxyz)
  ! cell(3) =(cell(3)*nz) /nzorig
  ! CALL IALCEL(2, CELL)
  ! endif
  !
  write(titlech, 302) nz - nzorig, numInFiles, dat, tim
302 format('ADDTOSTACK:',i4,' sections from',i3,' files appended' ,t57,a9,2x,a8)
  read(titlech, '(20a4)') (title(kti), kti = 1, 20)
  !
  call iwrhdr(2, title, 1, dmin, dmax, dmean)
  call imclose(2)
  !
  call exit(0)
99 write(6, 450)
450 format(' END OF IMAGE WHILE READING')
  go to 90
end program
