! REPACKSEED is a companion program for the script Transferfid.
! It is used to give the user a list of how points correspond between
! the original fiducial model and the new seed model, and to repack
! the seed model to eliminate empty contours.  See man page for details.
!
! $Id$
!
program repackseed
  implicit none
  integer idim
  include 'smallmodel.inc90'
  parameter (idim = max_obj_num)
  character*320 fidName, seedName, outFile, xyzName, matchName
  integer*4 ifidInA(idim), mapList(idim), iobjToDelete(idim)
  integer*4 ixyzCont(idim), ixyzPoint(idim), ixyzObj(idim), imodObj(idim), imodCont(idim)
  integer*4 iobj, nmap, numOriginal, i, nxyz, nmapLen, ierr, numDelete
  integer*4 izOrig, izTrans, ifBtoA, indab, max_obj_orig, ibase, ipt, ip
  real*4 dum, xOrig(idim), yOrig(idim), xImScale, yImScale, zImScale
  character*1 abtext(2) /'A', 'B'/
  integer*4 getImodScales, deleteImodCont
  logical*4 readSmallMod
  !
  call setExitPrefix('ERROR: REPACKSEED - ')
  write(*,'(1x,a,$)') 'Name of fiducial file from first axis: '
  read(5, 101) fidName
101 format(a)
  write(*,'(1x,a)') 'Enter name of file with X/Y/Z coordinates,' &
      //' or Return if none available'
  read(5, 101) xyzName
  write(*,'(1x,a,$)') 'Name of input file with new seed model: '
  read(5, 101) seedName
  write(*,'(1x,a,$)') 'Name of output file for packed model: '
  read(5, 101) outFile
  write(*,'(1x,a,$)') 'Name of output file for matching coordinates,'// &
      ' or Return for none: '
  read(5, 101) matchName
  write(*,'(1x,a,$)') 'Section numbers in original and second series,'// &
      ' 1 for B to A: '
  read(5,*) izOrig, izTrans, ifBtoA
  indab = 1
  if (ifBtoA .ne. 0) indab = 2
  !
  ! read original file
  !
  if (.not.readSmallMod(fidName)) call exitError('Reading original fiducial file')
  call scale_model(0)
  !
  ! read xyz file if any
  !
  nxyz = 0
  if (xyzName .ne. ' ') then
    call dopen(1, xyzName, 'ro', 'f')
10  read(1,*,end = 20) ixyzPoint(nxyz + 1), dum, dum, dum, &
        ixyzObj(nxyz + 1), ixyzCont(nxyz + 1)
    nxyz = nxyz + 1
    go to 10
20  continue
  endif
  close(1)
  !
  ! Keep track of original object numbers
  !
  numOriginal = 0
  max_obj_orig = max_mod_obj
  do iobj = 1, max_mod_obj
    call objToCont(iobj, obj_color, imodObj(iobj), imodCont(iobj))
    xOrig(iobj) = 0.
    yOrig(iobj) = 0.
    ibase = ibase_obj(iobj)
    do ipt = 1, npt_in_obj(iobj)
      ip = object(ibase + ipt)
      if (nint(p_coord(3, ip) - izOrig) == 0) then
        xOrig(iobj) = p_coord(1, ip)
        yOrig(iobj) = p_coord(2, ip)
      endif
    enddo

    if (nxyz == 0) then
      !
      ! if no xyz file available, have to assume that every contour with
      ! at least one point will be a fiducial
      !
      if (npt_in_obj(iobj) > 0) then
        numOriginal = numOriginal + 1
        ifidInA(iobj) = numOriginal
      else
        ifidInA(iobj) = 0
      endif
    else
      !
      ! look for object in xyz list, take number if found
      !
      ifidInA(iobj) = 0
      do i = 1, nxyz
        if (ixyzObj(i) == imodObj(iobj) .and. ixyzCont(i) == imodCont(iobj)) &
            ifidInA(iobj) = ixyzPoint(i)
      enddo
    endif
  enddo
  !
  ! read new seed model
  !
  if (.not.readSmallMod(seedName)) call exitError('Reading new seed file')
  call scale_model(0)
  if (matchName .ne. ' ') then
    call dopen(1, matchName, 'new', 'f')
    ierr = getImodScales(xImScale, yImScale, zImScale)
    if (ierr == 0) then
      write(1, '(3i6,f12.3)') izOrig, izTrans, ifBtoA, xImScale
    else
      write(1, '(3i6)') izOrig, izTrans, ifBtoA
    endif
  endif
  !
  ! pack objects down and accumulate map list
  !
  nmap = 0
  numDelete = 0
  do iobj = 1, max_mod_obj
    if (npt_in_obj(iobj) > 0) then
      if (matchName .ne. ' ' .and. xOrig(iobj) .ne. 0. .and. yOrig(iobj) &
          .ne. 0 .and. iobj <= max_obj_orig) then
        ip = object(ibase_obj(iobj) + 1)
        write(1, 107) xOrig(iobj), yOrig(iobj), p_coord(1, ip), p_coord(2, ip)
107     format(4f12.3)
      endif
      nmap = nmap + 1
      mapList(nmap) = ifidInA(iobj)
      npt_in_obj(nmap) = npt_in_obj(iobj)
      ibase_obj(nmap) = ibase_obj(iobj)
      obj_color(1, nmap) = obj_color(1, iobj)
      obj_color(2, nmap) = obj_color(2, iobj)
    else
      numDelete = numDelete + 1
      iobjToDelete(numDelete) = iobj
    endif
  enddo

  do iobj = nmap + 1, max_mod_obj
    npt_in_obj(iobj) = 0
  enddo
  max_mod_obj = nmap

  ! Delete contours in inverse order
  do ip = numDelete, 1, -1
    iobj = iobjToDelete(ip)
    if (deleteImodCont(imodObj(iobj), imodCont(iobj)) .ne. 0)  &
        call exitError('Deleting contour from IMOD model')
  enddo

  !
  ! output the model and the map list
  !
  call scale_model(1)
  call write_wmod(outFile)
  if (matchName .ne. ' ') then
    write(*,108)
108 format(//,' The correspondence between points is as follows',/)
    close(1)
  else
    write(*,102)
102 format(//,' Make the following entries to Setupcombine or ', &
        'Solvematch to describe how ',/, &
        ' fiducials correspond between the first and second axes',/)
  endif
  call int_iwrite(outFile, nmap, nmapLen)

  if (ifBtoA == 0) then
    write(*,109) abtext(indab)
109 format(' Points in ',a,': ',$)
    call wrlist(mapList, nmap)
    write(*,110) abtext(3 - indab), outFile(1:nmapLen)
110 format(' Points in ',a,': 1-',a)
  else
    write(*,110) abtext(3 - indab), outFile(1:nmapLen)
    write(*,109) abtext(indab)
    call wrlist(mapList, nmap)
  endif

  if (matchName .ne. ' ') then
    write(*,111) trim(matchName)
111 format(/,' Solvematch will be able to match up points using ', &
        'data in ',a,/' regardless of how well points track or ', &
        'whether you add or delete points')
  else
    if (nxyz == 0) then
      write(*,103) abtext(indab)
103   format(/,' These lists may be wrong if they include a contour', &
          ' in ',a,' that will not',/,' correspond to a fiducial ', &
          'point (e.g., if it has only one point)')
    endif
    write(*,105)
105 format(/,' These lists may be thrown off if you delete a contour', &
        ' in either set',/,' and will be thrown off if one of the', &
        ' seed points fails to track')
  endif
  call exit(0)
end program repackseed
