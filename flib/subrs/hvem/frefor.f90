! FREFOR, FREFOR2, FREFOR3 for reading variable numbers of elements on a line
!
! $Id$

! !
! Reads numeric values from a character string in [card], returns the
! values in [xnum] and the number of values in [numFields].  Values may be
! separated by any number of spaces, commas, or tabs.  Prints a warning
! message if there is non-numeric data in a field and stops reading numbers.
! If the first character in [card] is /, it returns with [xnum] and [numFields]
! unmodified.
! !
subroutine frefor(card, xnum, numFields)
  implicit none
  real*4 xnum(*)
  integer*4 numeric, numFields
  character*(*) card
  !
  call frefor3(card, xnum, numeric, 0, numFields, 0)
  return
end subroutine frefor

! !
! Reads numeric values from a character string in [card], returns the values in [xnum]
! and the number of values in [numFields].  Returns 1 in the array [numeric] if there is
! a numeric value in the respective field, or 0 if not. Reads up to [limNum] values.  If
! the first character in [card] is /, it returns with [xnum] and [numFields] unmodified.
! !
subroutine frefor2(card, xnum, numeric, numFields, limNum)
  implicit none
  real*4 xnum(*)
  integer*4 numeric(*), limNum, numFields
  character*(*) card
  !
  call frefor3(card, xnum, numeric, 1, numFields, limNum)
  return
end subroutine frefor2


! !
! Reads numeric values from a character string in [card], returns the values in [xnum]
! and the number of values in [numFields].  If [ifCheckNumeric] is 0, [numeric[ is not
! accessed and it prints a warning and stops reading values if a non-numeric value is
! found in a field.  If [ifCheckNumeric] is 1, it returns 1 in the array [numeric] if
! there is a numeric value in the respective field, or 0 if not.  Reads up to [limNum]
! values, or reads an unlimited number if limNum is 0.  If the first character in [card]
! is /, it returns with [xnum] and [numFields] unmodified.
! !
subroutine frefor3(card, xnum, numeric, ifCheckNumeric, numFields, limNum)
  implicit none
  real*4 xnum(*)
  integer*4 numFields, istart, numToSep, numToBlank, numToComma, iend, lenCard, numToTab
  integer*4 numeric(*), ifCheckNumeric, limNum
  character*(*) card
  character*1 blank /' '/, tab
  !
  if (card(1:1) == '/') return
  tab = achar(9)
  lenCard = len_trim(card)
  numFields = 0
  istart = 1
  !
  ! Search for starting point - skip separators
  do while (card(istart:istart) == blank .or. card(istart:istart) == ',' .or.  &
      card(istart:istart) == tab)
    istart = istart + 1
    if (istart > lenCard) return
  enddo
  !
  ! Decode up to limNum fiields
  do while (istart <= lenCard .and. (numFields < limNum .or. limNum <= 0))
    numFields = numFields + 1
    !
    ! Find end of field
    numToBlank = index(card(istart:), blank) - 1
    numToComma = index(card(istart:), ',') - 1
    numToTab = index(card(istart:), tab) - 1
    if (numToBlank <= 0 .and. numToComma <= 0 .and. numToTab <= 0) then
      numToSep = lenCard + 1 - istart
    else
      if (numToBlank <= 0) numToBlank = lenCard + 1
      if (numToComma <= 0) numToComma = lenCard + 1
      if (numToTab <= 0) numToTab = lenCard + 1
      numToSep = min(numToBlank, numToComma, numToTab)
    endif
    iend = istart + numToSep - 1
    !
    ! Get the number
    if (ifCheckNumeric > 0) then
      numeric(numFields) = 0
      xnum(numFields) = 0.
      read(card(istart:iend),*,err = 98) xnum(numFields)
      numeric(numFields) = 1
98    istart = iend + 2
    else
      read(card(istart:iend),*,err = 99) xnum(numFields)
      istart = iend + 2
    endif
    !
    ! Skip over recurring separators
    do while (card(istart:istart) == blank .or. card(istart:istart) == ',' .or. &
        card(istart:istart) == tab)
      istart = istart + 1
      if (istart > lenCard) return
    enddo
  enddo
  return
99 write(*,31) numFields
31 format('WARNING: Invalid input in field',i4, '; input ignored from there on')
  numFields = numFields - 1
  return
end subroutine

