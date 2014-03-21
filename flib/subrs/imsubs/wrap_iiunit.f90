! wrap_iiunit.f90: relatively simple wrappers to some iiunit functions
!
! $Id$       

! WRAPPERS FOR THE iiuOpen ROUTINE WITH INFORMATION PRINTED

! Standard imopen subroutine call:
! Opens a file on unit [iunit] with the filename [name] and with [attribute] one of
! 'NEW', 'OLD', 'RO' (for read-only), and 'SCRATCH'
!
subroutine imopen(iunit, name, attribute)
  implicit none 
  integer*4 iunit, ierr, iiuOpenPrint
  character*(*) name, attribute
  ierr = iiuOpenPrint(iunit, name, attribute)
  return
end subroutine imopen


! A function call with non-zero return on error, if exitOnError is turned off
!
integer*4 function iiuOpenPrint(iunit, name, attribute)
  implicit none 
  integer*4 iunit, ierr
  character*(*) name, attribute
  character*8 attrib
  integer*4 iflags, itype, numKBytes
  logical*4 doExtra, doPrint
  integer*4 iiuRetBrief, iiuRetPrint, iiuOpen
  doPrint = iiuRetPrint() > 0
  doExtra = iiuRetBrief() == 0 .and. doPrint
  
  call strupcase(attrib, attribute)
  ierr = iiuOpen(iunit, name, attribute)
  iiuOpenPrint = ierr
  if (ierr .ne. 0) return
  call iiuFileInfo(iunit, numKBytes, itype, iflags)
  
  if (attrib(1:1) == 'N' .or. attrib(1:1) == 'S' .or. numKBytes < 0) then
    if (doPrint) write(6,2000) trim(attrib), iunit, trim(name)
  else
    if (doPrint)write(6,2100) trim(attrib), iunit, trim(name), numKBytes
  endif
2000 format(/,1x,a,' image file on unit',i4,' : ',a)
2100 format(/,1x,a,' image file on unit',i4,' : ',a, '     Size= ',i10,' K')
  if (doPrint .and. iiuRetBrief() == 0) then
    if (itype == 1) then
      write(6,2200) 'This is a TIFF file.'
    else if (itype == 5) then
      write(6,2200) 'This is an HDF file.'
    elseif (itype .ne. 2) then
      write(6,2200) 'This is a non-MRC file.'
    endif
    if (btest(iflags, 0)) write(6,2200) 'This is a byte-swapped file.'
  endif
2200  format(/,20x,a)
  call flush(6)
  return
end function iiuOpenPrint


! WRAPPERS TO THE READ FUNCTIONS WITH ERROR RETURNS

! Reads a single line from the current position
! Replace with:  ierr = iiuReadLines(iunit, array, 1)
!
subroutine irdlin(iunit, array, *)
  implicit none
  integer*4 iunit, iiuReadLines
  real*4 array(*)
  if (iiuReadLines(iunit, array, 1) == 0) return
  return 1
end subroutine irdlin


! Reads multiple lines from the current position
! Replace with:  ierr = iiuReadLines(iunit, array, numLines)
!
subroutine irdsecl(iunit, array, numLines, *)
  implicit none
  integer*4 iunit, numLines, iiuReadLines
  real*4 array(*)
  if (iiuReadLines(iunit, array, numLines) == 0) return
  return 1
end subroutine irdsecl


! Reads the current section
! Replace with:  ierr = iiuReadSection(iunit, array)
!
subroutine irdsec(iunit, array, *)
  implicit none
  integer*4 iunit, iiuReadSection
  real*4 array(*)
  if (iiuReadSection(iunit, array) == 0) return
  return 1
end subroutine irdsec


! Reads a subarea of the current section and places in the lower-left of an array
! Replace with:  ierr = iiuReadSecPart(iunit, array, mx, indx1, indx2, indy1, indy2)
! BE CAREFUL TO DROP THE Y DIMENSION FROM THE CALL
!
subroutine irdpas(iunit, array, mx, my, indx1, indx2, indy1, indy2, *)
  implicit none
  integer*4 mx, my, indx1, indx2, indy1, indy2, iunit, iiuReadSecPart
  real*4 array(mx, my)
  if (iiuReadSecPart(iunit, array, mx, indx1, indx2, indy1, indy2) == 0) return
  return 1
end subroutine irdpas


! WRAPPERS for functions that involve logicals: data conversion, printing, extra header
! test
!
subroutine ialcon(iunit, convert)
  implicit none
  logical convert
  integer*4 iunit, ifConvert
  ifConvert = 0
  if (convert) ifConvert = 1
  call iiuAltConvert(iunit, ifConvert)
  return
end subroutine ialcon

subroutine ialprt(doPrint)
  implicit none
  logical doPrint
  integer*4 ifPrint
  ifPrint = 0
  if (doPrint) ifPrint = 1
  call iiuAltPrint(ifPrint)
  return
end subroutine ialprt

! Wrapper for extraIsNbytesAndFlags
!
logical function nbytes_and_flags(nint, nreal)
  implicit none
  integer*4 nint, nreal, extraIsNbytesAndFlags
  nbytes_and_flags = extraIsNbytesAndFlags(nint, nreal) > 0
  return
end function nbytes_and_flags
