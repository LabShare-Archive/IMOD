*       * * * * * MODEL2POINT * * * * *
c       
c       MODEL2POINT will convert an IMOD or WIMP model file to a list of
c       points with integer or floating point coordinates.  Each point will be
c       written on a separate line, and the coordinates may be written either
c       with no information about which object or contour the point came from,
c       or with contour numbers, or with object numbers.
c       
c       See man page for details
c
c       $Id$
c       $Log$
c       Revision 3.3  2007/10/18 22:21:41  mast
c       Added option to number from zero
c
c       Revision 3.2  2007/10/14 18:03:42  mast
c       Made it output index coordinates unless option given
c
c       Revision 3.1  2007/09/12 16:29:25  mast
c       PIP conversion and output options
c
c       
      implicit none
      character*320 modelfile,pointfile
      logical*4 printObj, printCont, floating, scaled
      integer*4 ierr,iobject,ninobj,ipt,ipnt,modObj, modCont, npnts, numOffset
      logical exist,readw_or_imod,getModelObjectRange
      integer*4 nobjTot, imodObj,getImodObjSize
c       
      include 'model.inc'

      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetLogical,PipGetBoolean
      integer*4 PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  model2point
c       
      integer numOptions
      parameter (numOptions = 7)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@output:OutputFile:FN:@float:FloatingPoint:B:@'//
     &    'object:ObjectAndContour:B:@contour:Contour:B:@'//
     &    'zero:NumberedFromZero:B:@help:usage:B:'
c
      printObj = .false.
      printCont = .false.
      floating = .false.
      scaled = .false.
      numOffset = 0
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'model2point',
     &    'ERROR: MODEL2POINT - ', .false., 2, 1, 1, numOptArg,
     &    numNonOptArg)
      if (PipGetInOutFile('InputFile', 1, ' ', modelfile)
     &    .ne. 0) call exitError('NO INPUT FILE SPECIFIED')
      if (PipGetInOutFile('OutputFile', 2, ' ', pointfile)
     &    .ne. 0) call exitError('NO OUTPUT FILE SPECIFIED')
c       
c       read model in
c       
      call imodPartialMode(1)
      exist=readw_or_imod(modelfile)
      if(.not.exist)call exitError('READING MODEL FILE')
      nobjTot = getImodObjSize()
c       
      call dopen(1,pointfile,'new','f')
      ierr = PipGetLogical('ObjectAndContour', printObj)
      ierr = PipGetLogical('Contour', printCont)
      ierr = PipGetLogical('FloatingPoint', floating)
      ierr = PipGetBoolean('NumberedFromZero', numOffset)
c       
c       scan through all objects to get points
c       
      npnts=0
      do imodObj = 1, nobjTot
        if (.not.getModelObjectRange(imodObj, imodObj)) then
          write(*,'(/,a,i6)') 'ERROR: MODEL2POINT - LOADING DATA FOR OBJECT #',
     &        imodobj
          call exit(1)
        endif
      
        if (.not.scaled) call scale_model(0)
        do iobject=1,max_mod_obj
          ninobj=npt_in_obj(iobject)
          if(ninobj.gt.0)then
            call objtocont(iobject, obj_color, modObj, modCont)
            do ipt=1,ninobj
              ipnt=object(ipt+ibase_obj(iobject))
              if(ipnt.gt.0)then
                if (printObj) write(1,103)modObj - numOffset
                if (printCont .or. printObj) write(1,103)modCont - numOffset
                if (floating) then
                  write(1,104) p_coord(1,ipnt), p_coord(2,ipnt),p_coord(3,ipnt)
                else
                  write(1,102) nint(p_coord(1,ipnt)),
     &                nint(p_coord(2,ipnt)), nint(p_coord(3,ipnt))
                endif
                npnts=npnts+1
              endif
            enddo
          endif
        enddo
      enddo
102   format(3i7)
103   format(i6,$)
104   format(3f10.2)
      print *,npnts,' points output to file'
      call exit(0)
      end
