! * * * * * MODEL2POINT * * * * *
!
! MODEL2POINT will convert an IMOD or WIMP model file to a list of
! points with integer or floating point coordinates.  Each point will be
! written on a separate line, and the coordinates may be written either
! with no information about which object or contour the point came from,
! or with contour numbers, or with object numbers.
!
! See man page for details
!
! $Id$
!
program model2point
  implicit none
  character*320 modelfile, pointfile
  logical*4 printObj, printCont, floating, scaled
  integer*4 ierr, iobject, ninobj, ipt, ipnt, modObj, modCont, npnts, numOffset, ifValues
  logical exist, readw_or_imod, getModelObjectRange
  real*4 genVal, fillVal
  integer*4 nobjTot, imodObj, getImodObjSize, getContValue, getPointValue
  !
  include 'model.inc90'

  integer*4 numOptArg, numNonOptArg
  integer*4 PipGetLogical, PipGetBoolean, PipGetInteger, PipGetFloat
  integer*4 PipGetInOutFile
  !
  ! fallbacks from ../../manpages/autodoc2man -3 2  model2point
  !
  integer numOptions
  parameter (numOptions = 10)
  character*(40 * numOptions) options(1)
  options(1) = &
      'input:InputFile:FN:@output:OutputFile:FN:@float:FloatingPoint:B:@'// &
      'scaled:ScaledCoordinates:B:@object:ObjectAndContour:B:@contour:Contour:B:@'// &
      'zero:NumberedFromZero:B:@values:ValuesInLastColumn:I:@fill:FillValue:F:@'// &
      'help:usage:B:'
  !
  printObj = .false.
  printCont = .false.
  floating = .false.
  scaled = .false.
  numOffset = 0
  fillVal = 0.
  ifValues = 0
  !
  ! Pip startup: set error, parse options, check help, set flag if used
  !
  call PipReadOrParseOptions(options, numOptions, 'model2point', &
      'ERROR: MODEL2POINT - ', .false., 2, 1, 1, numOptArg, numNonOptArg)
  if (PipGetInOutFile('InputFile', 1, ' ', modelfile) &
      .ne. 0) call exitError('NO INPUT FILE SPECIFIED')
  if (PipGetInOutFile('OutputFile', 2, ' ', pointfile) &
      .ne. 0) call exitError('NO OUTPUT FILE SPECIFIED')
  !
  ! read model in
  !
  call imodPartialMode(1)
  exist = readw_or_imod(modelfile)
  if (.not.exist) call exitError('READING MODEL FILE')
  nobjTot = getImodObjSize()
  !
  call dopen(1, pointfile, 'new', 'f')
  ierr = PipGetLogical('ObjectAndContour', printObj)
  ierr = PipGetLogical('Contour', printCont)
  ierr = PipGetLogical('FloatingPoint', floating)
  ierr = PipGetLogical('ScaledCoordinates', scaled)
  ierr = PipGetBoolean('NumberedFromZero', numOffset)
  ierr = PipGetInteger('ValuesInLastColumn', ifValues)
  ierr = PipGetFloat('FillValue', fillVal)
  !
  ! scan through all objects to get points
  !
  npnts = 0
  do imodObj = 1, nobjTot
    if (.not.getModelObjectRange(imodObj, imodObj)) then
      write(*,'(/,a,i6)') 'ERROR: MODEL2POINT - LOADING DATA FOR OBJECT #', &
          imodobj
      call exit(1)
    endif

    if (.not.scaled) call scale_model(0)
    do iobject = 1, max_mod_obj
      ninobj = npt_in_obj(iobject)
      if (ninobj > 0) then
        call objtocont(iobject, obj_color, modObj, modCont)
        do ipt = 1, ninobj
          ipnt = object(ipt + ibase_obj(iobject))
          if (ipnt > 0) then
            if (printObj) write(1, 103) modObj - numOffset
            if (printCont .or. printObj) write(1, 103) modCont - numOffset
            if (floating .or. ifValues .ne. 0) then
              if ((ifValues < 0 .and. (ipt == 1 .or. .not.printCont))  &
                  .or. ifValues > 0) then
                if (ifValues < 0) then
                  genVal = fillVal
                  if (ipt == 1) then
                    if (getContValue(modObj, modCont, genVal) .ne. 0) genVal = fillVal
                  endif
                else
                  if (getPointValue(modObj, modCont, ipt, genVal) .ne. 0) genVal = fillVal
                endif
                write(1, 105) p_coord(1, ipnt), p_coord(2, ipnt), p_coord(3, ipnt), genVal
              else
                write(1, 104) p_coord(1, ipnt), p_coord(2, ipnt), p_coord(3, ipnt)
              endif
            else
              write(1, 102) nint(p_coord(1, ipnt)), nint(p_coord(2, ipnt)), &
                  nint(p_coord(3, ipnt))
            endif
            npnts = npnts + 1
          endif
        enddo
      endif
    enddo
  enddo
102 format(3i7)
103 format(i6,$)
104 format(3f12.2)
105 format(3f12.2,g16.6)
  print *,npnts, ' points output to file'
  call exit(0)
end program
