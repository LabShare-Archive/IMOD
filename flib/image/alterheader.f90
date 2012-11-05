! PROGRAM TO ALTER HEADER INFORMATION WITHOUT REWRITING FILE
! For each header entry selected by the user, the program prints the
! current values and asks for new values, checking for legality in some
! cases.  See man page for details.
!
! David Mastronarde 10/24/88
!
! $Id$
  !
program alterheader
  implicit none
  integer nfunc, idim, nx, ny, nz, maxextra
  parameter (nfunc = 22)
  parameter (idim = 2100, maxextra = idim * idim * 4)
  integer*4 NXYZ(3), MXYZ(3), NXYZST(3), mcrs(3), listdel(1000), iBinning(3)
  real*4 delt(3), tilt(3), TITLE(20,10), cell(6), array(idim*idim)
  equivalence (nxyz(1), nx), (nxyz(2), ny), (nxyz(3), nz)
  common /bigimg/ array
  !
  CHARACTER*320 FILIN
  character*20 funcin, funcup, lastFunc
  character*9 param(nfunc)
  data param/'ORG', 'CEL', 'DAT', 'DEL', 'MAP', 'SAM', 'TLT' , 'TLT_ORIG', 'TLT_ROT', &
      'LAB', 'MMM', 'RMS', 'FIXPIXEL', 'FIXEXTRA', 'FIXMODE', 'SETMMM', 'FEIPIXEL', &
      'INVERTORG', 'REAL', 'FFT', 'HELP', 'DONE'/
  !
  !
  DATA NXYZST/0, 0, 0/
  integer*4 mode, iwhich, i, itype, lens, n1, n2, n3, ntitle, ndel
  integer*4 newtitle, iold, ifdel, id, j, nbytex, iflag, ifok, iz, nbsym, numInt, numReal
  real*4 dmin, dmax, dmean, origx, origy, origz, v1, v2
  real*4 dmins, dmaxs, sd, rms, pixel
  real*8 dmeans, sums, sumsqs, totn, tsum, sumsq
  integer*4 maxLines, numChunks, iChunk, numLines, iflags, ifImod
  logical*4 nbytes_and_flags, noBinning
  !
  ! Read in input file
  !
  call getinout(1, filin, filin)
  !
  call ialbrief(0)
  CALL IMOPEN(2, FILIN, 'OLD')
  call irdhdr(2, nxyz, mxyz, mode, dmin, dmax, dmean)
  !
  print *,'If you make a mistake, interrupt with Ctrl-C instead of exiting with DONE'
30 write(*,102)
102 format(1x,'Options: org, cel, dat, del, map, sam, tlt, ', &
      'tlt_orig, tlt_rot, lab, mmm,',/, ' rms, fixpixel, feipixel, ', &
      'fixextra, fixmode, invertorg, setmmm, real, fft,',/,' help, OR done')
  write(*,'(1x,a,$)') 'Enter option: '
  read(5, 101) funcin
101 FORMAT(A)
  !
  ! Exit if a -1 is received after feipixel, it means there was no pixel available
  if (funcin == '-1' .and. lastFunc == 'FEIPIXEL') call exit(0)
  call strupcase(funcup, funcin)
  iwhich = 0
  do i = 1, nfunc
    if (funcup == param(i)) iwhich = i
  enddo
  lastFunc = funcup
  go to(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 16, 12, 13, 17, 18, 19, 20, 21, 22, 14, 15), &
      iwhich
  print *,'Not a legal entry, try again'
  go to 30
  !
  ! ORIGIN
  !
1 call irtorg(2, origx, origy, origz)
  write(*,111) origx, origy, origz
111 format(' Alter origin.  The origin is the offset FROM the', &
      ' first point in the image',/,' file TO the center of', &
      ' the coordinate system, expressed in true coordinates.', &
      /,' Current x, y, z:',3g11.4,/,'New x, y, z: ',$)
  read(5,*) origx, origy, origz
  call ialorg(2, origx, origy, origz)
  go to 30
  !
  ! CELL
  !
2 call irtcel(2, cell)
  write(*,112) (cell(i), i = 1, 6)
112 format(' Alter cell.  Current size and angles:',/,3g11.4,3f9.3, &
      /,'New size and angles: ',$)
  read(5,*) (cell(i), i = 1, 6)
  if (cell(1) > 0 .and. cell(2) > 0 .and. cell(3) > 0) then
    call ialcel(2, cell)
  else
    print *,'No good, cell(1-3) must be positive'
  endif
  go to 30
  !
  ! data type etc
  !
3 call irtdat(2, itype, lens, n1, n2, v1, v2)
  write(*,113) itype, lens, n1, n2, v1, v2
113 format(' Alter data type.  Current type, lens, n1, n2, v1, v2:' &
      ,/,4i5,2f10.3,/,' Enter new type (0 regular serial sections' &
      ', 1 tilt series, 2 serial stereo',/,' pairs, 3 averaged', &
      ' serial sections, 4 averaged serial stereo pairs): ',$)
  read(5,*) itype
  if (itype < 0 .or. itype > 4) then
    print *,'No good, type must be 0-4'
    go to 30
  endif
  if (itype == 0) then
    print *,'You do not need to change any of the other parameters'
  elseif (itype == 1) then
    n2 = 0
    write(*,'(1x,a,$)') &
        '1, 2, or 3 if the tilt is around the X, Y or Z axis: '
    read(5,*) n1
    if (n1 <= 0 .or. n1 > 3) then
      print *,'Value no good'
      go to 30
    endif
    write(*,'(1x,a,$)') 'Increment in tilt angle between views,' &
        //' tilt angle of first view: '
    read(5,*) v1, v2
  endif
  if (itype >= 3) then
    write(*,'(1x,a,$)') &
        'Number of original sections averaged into one section: '
    read(5,*) n1
    write(*,'(1x,a,a,/,a,$)') 'Enter the spacing between the', &
        ' original section numbers contributing', &
        '    to successive sections in this file: '
    read(5,*) n2
  endif
  if (itype == 2 .or. itype == 4) then
    write(*,'(1x,a,$)') 'Tilt angles of left and right eye views: '
    read(5,*) v1, v2
  endif
  write(*,213) itype, lens, n1, n2, v1, v2
213 format(' Proposed new type, lens, n1, n2, v1, v2:',/,4i5,2f10.3, &
      /,' Enter / to accept, or a new type,', &
      ' lens, n1, n2, v1, v2: ',$)
  read(5,*) itype, lens, n1, n2, v1, v2
  if (itype >= 0 .and. itype <= 4) then
    call ialdat(2, itype, lens, n1, n2, v1, v2)
  else
    print *,'No good, type must be 0-4'
  endif
  go to 30
  !
  ! DELTA
  !
4 call irtdel(2, delt)
  call irtsam(2, mxyz)
  call irtcel(2, cell)
  !
  ! DNM 12/25/00: this used to call ialdel, which set mxyz to 1, 1, 1
  ! and cell size to the desired deltas; decided to change it to
  ! preserve mxyz instead
  !
  write(*,114) (delt(i), i = 1, 3)
114 format(' Alter delta - changes cell sizes to achieve desired', &
      ' pixel spacing',/,' Current delta x, y, z:',3G11.4, &
      /,'New delta x, y, z: ',$)
  read(5,*) (delt(i), i = 1, 3)
  if (delt(1) > 0 .and. delt(2) > 0 .and. delt(3) > 0) then
    ! call ialdel(2, delt)
    do i = 1, 3
      cell(i) = mxyz(i) * delt(i)
    enddo
    call ialcel(2, cell)
  else
    print *,'No good, must be positive'
  endif
  go to 30
  !
  ! MAPPING
  !
5 call irtmap(2, mcrs)
  write(*,115) (mcrs(i), i = 1, 3)
115 format(' Alter mapping.  Current mapping constants:',3i3,/ &
      ,'New constants: ',$)
  read(5,*) (mcrs(i), i = 1, 3)
  n1 = 0
  n2 = 0
  n3 = 0
  do i = 1, 3
    if (mcrs(i) == 1) n1 = n1 + 1
    if (mcrs(i) == 2) n2 = n2 + 1
    if (mcrs(i) == 3) n3 = n3 + 1
  enddo
  if (n1 == 1 .and. n2 == 1 .and. n3 == 1) then
    call ialmap(2, mcrs)
  else
    print *,'No good, must be permutation of 1, 2, 3'
  endif
  go to 30
  !
  ! SAMPLING
  !
6 call irtsam(2, mxyz)
  write(*,116) (mxyz(i), i = 1, 3)
116 format(' Alter sampling (mxyz).  Current x, y, z:',3i5,/ &
      ,'New x, y, z: ',$)
  read(5,*) (mxyz(i), i = 1, 3)
  if (mxyz(1) > 0 .and. mxyz(2) > 0 .and. mxyz(3) > 0) then
    call ialsam(2, mxyz)
  else
    print *,'No good, must be positive'
  endif
  go to 30
  !
  ! TILT - current angles
  !
7 call irttlt(2, tilt)
  write(*,117) (tilt(i), i = 1, 3)
117 format(' Alter current tilt angles.  Current angles:',6f6.1, &
      /,'New current angles: ',$)
  read(5,*) (tilt(i), i = 1, 3)
  call ialtlt(2, tilt)
  go to 30
  !
  ! TILT_ORIG - original angles
  !
8 call irttlt_orig(2, tilt)
  write(*,118) (tilt(i), i = 1, 3)
118 format(' Alter original tilt angles.  Current angles:',6f6.1, &
      /,'New original angles: ',$)
  read(5,*) (tilt(i), i = 1, 3)
  call ialtlt_orig(2, tilt)
  go to 30
  !
  ! TILT_ROT - rotate current angles
  !
9 write(*,119)
119 format(' Rotate current tilt angles.', /,'Angles to rotate by: ',$)
  read(5,*) (tilt(i), i = 1, 3)
  call ialtlt_rot(2, tilt)
  go to 30
  !
  ! LAB - delete selected labels or add one
  !
10 call irtlab(2, title, ntitle)
  write(*,'(a,/)') ' Delete labels or add one label.  Current labels are:'
  write(*,'(i3,1x,19a4)') (i, (title(j, i), j = 1, 19), i = 1, ntitle)
  write(*,'(/,a,a,/,a,a)') 'To delete labels, enter numbers of labels to delete ', &
      '(ranges ok)', 'To add a label, enter 0 or the NEGATIVE of the label ', &
      'number to add it after'
  call rdlist(5, listdel, ndel)
  if (ndel == 1 .and. listdel(1) <= 0) then
    if (ntitle >= 10) then
      print *,'You need to delete some labels before adding any'
      go to 30
    endif
    ifdel = min(-listdel(1), ntitle)
    do iold = ntitle, ifdel + 1, -1
      do j = 1, 20
        title(j, iold + 1) = title(j, iold)
      enddo
    enddo
    print *,'Enter new label'
    read(6, 101) filin
    read(filin, '(20a4)') (title(j, iold + 1), j = 1, 20)
    newtitle = ntitle + 1
  else
    newtitle = 0
    do iold = 1, ntitle
      ifdel = 0
      do id = 1, ndel
        if (iold == listdel(id)) ifdel = 1
      enddo
      if (ifdel == 0) then
        newtitle = newtitle + 1
        do j = 1, 20
          title(j, newtitle) = title(j, iold)
        enddo
      endif
    enddo
  endif
  if (newtitle > 0) then
    write(*,'(a,/)') ' New label list would be:'
    write(*,'(i3,1x,19a4)') (i, (title(j, i), j = 1, 19), i = 1, newtitle)
  else
    write(*,101) ' New label list would be empty'
  endif
  write(*,'(/,1x,a,$)') '1 to confirm changing to this label list, 0 not to: '
  read(5,*) ifok
  if (ifok .ne. 0) call iallab(2, title, newtitle)
  go to 30
  !
  ! MMM - recompute min/max/mean
  !
11 maxLines = idim**2 / nx
  numChunks = (ny + maxLines - 1) / maxLines
  write(*,121)
121 format(' Recomputing min/max/mean of images - takes a while...')
  call imposn(2, 0, 0)
  dmin = 1.e10
  dmax = -1.e10
  tsum = 0.
  sumsq = 0.
  totn = 0.
  do iz = 1, nz
    do iChunk = 1, numChunks
      numLines = min(maxLines, ny - (iChunk - 1) * maxLines)
      call irdsecl(2, array, numLines, *99)
      call iclavgsd(array, nx, numLines, 1, nx, 1, numLines, dmins, dmaxs, sums, &
          sumsqs, dmean, sd)
      dmin = min(dmin, dmins)
      dmax = max(dmax, dmaxs)
      tsum = tsum + sums
      sumsq = sumsq + sumsqs
      totn = totn + nx * numLines
    enddo
  enddo
  dmeans = tsum / totn
  rms = sqrt((sumsq - totn * dmeans**2) / totn)
  dmean = dmeans
  call ialrms(2, rms)
  if (iwhich == 12) write(*,162) rms
162 format(' New RMS value = ', g13.5)
  go to 30
  !
  ! RMS - first inform of current RMS value
  !
16 call irtrms(2, rms)
  write(*,161) rms
161 format(' Current RMS value = ', g13.5)
  go to 11
  !
12 write(*,122)
122 format(' Changing sample and cell sizes to match image size, ' &
      ,/,' which will make pixel spacing be 1.0 1.0 1.0.')
  call irtcel(2, cell)
  cell(1) = nx
  cell(2) = ny
  cell(3) = nz
  call ialcel(2, cell)
  call ialsam(2, nxyz)
  go to 30
  !
  ! FIXPIECES - Remove flag for piece coordinates from header
  !
13 write(*,123)
123 format(' Marking header as not containing any ', &
      'piece coordinates.')
  call irtsymtyp(2, nbytex, iflag)
  if (mod(iflag / 2, 2) > 0) iflag = iflag - 2
  call ialsymtyp(2, nbytex, iflag)
  go to 30
  !
  ! FIXMODE - change between 1 and 6
  !
17 if (mode .ne. 6 .and. mode .ne. 1) then
    print *,'Only mode 6 can be changed to mode 1, or 1 to 6'
    go to 30
  endif
  !
  mode = 7 - mode
  write(*,1124) mode
1124 format(/,'Changing mode to',i2)
  call ialmod(2, mode)
  if (dmax > 32767 .and. mode == 1) write(*,124) dmax
124 format(/,'The file maximum is', f12.1, ' and numbers bigger than', &
      ' 32767 will not be',/, &
      ' represented correctly in this mode.')
  if (dmin < 0 .and. mode == 6) write(*,2124) dmin
2124 format(/,'The file minimum is', f12.1, ' and negative numbers', &
      ' will not be',/, ' represented correctly in this mode.')
  go to 30
  !
  ! SETMMM - set the min, max, mean
18 write(*,218) dmin, dmax, dmean
218 format(' Alter min/max/mean.  Current values:',6g15.5, &
      /,'New min, max, mean: ',$)
  read(5,*) dmin, dmax, dmean
  go to 30
  !
  ! FEIPIXEL - use the pixel size in extra header to set pixel spacing
19 call irtnbsym(2, nbsym)
  if (nbsym <= 0) then
    print *,'No extended header information in this file'
    go to 30
  endif
  if (nbsym > maxextra) then
    print *,'Extended header data too large for array'
    go to 30
  endif
  call irtsym(2, nbsym, array)
  call irtsymtyp(2, numInt, numReal)
  if (nbytes_and_flags(numInt, numReal)) then
    print *,'The extended header is not in Agard/FEI format'
    go to 30
  endif
  if (numReal < 12) then
    print *,'There is no pixel size in this extended header (too few values per section)'
    go to 30
  endif
  pixel = array(numInt + 12) * 1.e10
  if (pixel <= 0) then
    print *,'Pixel size in extended header is not a usable value:', pixel
    go to 30
  endif
  call irtdel(2, delt)
  call irtImodFlags(2, iflags, ifImod)
  noBinning = iand(iflags, 2) .ne. 0
  do i = 1, 3
    iBinning(i) = nint(delt(i))
    if (abs(delt(i) - iBinning(i)) > 1.e-6 .or. iBinning(i) <= 0 .or. iBinning(i) > 4) &
        noBinning = .true.
  enddo
  if (noBinning) then
    iBinning(1:3) = 1
    if (iand(iflags, 2) .ne. 0) write(*,2194)
2194 format(/,'The pixel size has already been transferred to the standard pixel spacing')
    write(*,219) pixel
219 format('Pixel size in extended header is',g11.4,' Angstroms',/, 'Enter', &
        ' 1 to set the pixel spacing to this value, 0 not to, -1 to abort: ',$)
    read(5,*) ifok
    if (ifok < 0) then
      if (iand(iflags, 2) .ne. 0) then
        write(*,2194)
      else
        write(*, '(/,a)')'The existing regular pixel spacing did not correspond to '// &
            'a binning'
      endif
      write(*,101)'The pixel size in the extended header is not being used'
      call exit(0)
    endif
    iBinning(1:3) = 1
  else
    write(*,2191) pixel
    if (iBinning(1) > 1) write(*,2192) iBinning(1)
    write(*,2193) pixel * iBinning(1)
2191 format('Pixel size in extended header is',g11.4,' Angstroms')
2192 format('  but the data ', 'seem to have been binned by',i2)
2193 format('Enter 1 to set the pixel spacing to', g11.4,', 0 not to: ',$)
    read(5,*) ifok
  endif
  if (ifok == 0) go to 30

  iflags = ior(iflags, 2)
  call ialImodFlags(2, iflags)
  call irtsam(2, mxyz)
  call irtcel(2, cell)
  cell(1:3) = mxyz(1:3) * pixel * iBinning(1:3)
  call ialcel(2, cell)
  go to 30
  !
  ! INVERTORG  - invert the sign of the origin
20 call ialorg(2, origx, origy, origz)
  origx = -origx
  origy = -origy
  origz = -origz
  write(*,120) origx, origy, origz
120 format('Inverting sign of origin: new origin = ',3g13.6)
  call ialorg(2, origx, origy, origz)
  go to 30
  !
  ! REAL: make an FFT real
21 if (mode .ne. 4) then
    print *,'Must be mode 4 to change file to real'
    go to 30
  endif
  call ialmod(2, 2)
  mode = 2
  nxyz(1) = nxyz(1) * 2
  call ialsiz(2, nxyz, nxyzst)
  write(*,221) mode, nxyz(1)
221 format('Changing mode to ',i1,' and X size to ',i6)
  go to 30
  !
  ! FFT: restore a real file
22 if (mode .ne. 2 .or. mod(nx, 2) .ne. 0 .or. mod(nx / 2, 2) .ne. 1) then
    print *,'Must be mode 2 and NX must (odd #) * 2 to change file to fft'
    go to 30
  endif
  call ialmod(2, 4)
  mode = 4
  nxyz(1) = nxyz(1) / 2
  call ialsiz(2, nxyz, nxyzst)
  write(*,221) mode, nxyz(1)
  go to 30
  !
14 write(*,201)
201 format(/,' org = change x,y,z origin', &
      /,' cel = change cell size', &
      /,' dat = change data type (for tilt series, stereo', &
      ' pairs, or averaged sections)', &
      /,' del = change delta (pixel size) directly', &
      /,' map = change x,y,z mapping to rows, columns, sections', &
      /,' sam = change mxyz sampling', &
      /,' tlt = change alpha, beta, gamma current tilt angles', &
      /,' tlt_orig = change original tilt angles', &
      /,' tlt_rot = rotate current tilt angles', &
      /,' lab = delete selected labels', &
      /,' mmm = fix min/max/mean and set RMS value by ', &
      'reading all images' &
      /,' rms = set RMS value, does same actions as mmm', &
      /,' fixpixel = fix pixel spacing by setting cell and', &
      ' sample sizes to image size', &
      /,' feipixel = set pixel spacing from pixel size in ', &
      'Agard/FEI extended header', &
      /,'            sample sizes to image size', &
      /,' fixextra = fix extra header so that file', &
      ' does not look like a montage' &
      /,' fixmode = change mode from 6 to 1 (unsigned to signed ', &
      'integer) or 1 to 6', &
      /,' setmmm = set min, max, mean to entered values', &
      /,' real = change mode 4 file to mode 2 and change X size', &
      /,' fft = change compatible-sized mode 2 file back to mode 4', &
      /,' help = type this again', &
      /,' done = exit',/)
  go to 30
  !
15 CALL IWRHDR(2, TITLE, -1, dmin, dmax, dmean)
  call irdhdr(2, nxyz, mxyz, mode, dmin, dmax, dmean)
  CALL IMCLOSE(2)
  call exit(0)
99 print *
  print *,'ERROR: ALTERHEADER - reading file'
  call exit(1)
END program
