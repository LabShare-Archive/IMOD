! $Id$
!
! !
! Concatenates the string [str2] to the end of string [str1],
! eliminating leading and trailing blanks.  It can handle either
! argument being blank; if both are blank, it will return ' '.  Embedded
! blanks are retained.
! !
character*320 function concat(str1,str2)
  character*(*) str1, str2
  concat = trim(adjustl(str1))//trim(adjustl(str2))
  return
end function concat
