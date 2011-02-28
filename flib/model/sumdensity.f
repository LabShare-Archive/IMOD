****    SUMDENSITY
c       
c       SUMDENSITY will sum the density within contours of selected IMOD 
c       model objects.  Pixels within a specified distance of the boundary
c       contour can be excluded.  A threshold can be set so that only pixels
c       above threshold are summed, and the threshold value subtracted from
c       the pixel values in the sum.  Data are reported by surface if there
c       are surfaces defined in the model contours.
c       
c       See man page for more details
c       
c       David Mastronarde 11/10/98
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 3.9  2005/12/30 20:23:59  mast
c       Needed to open file readonly
c       
c       Revision 3.8  2005/05/27 04:55:11  mast
c       Added report of pixel count output to model and used partial mode
c       
c       Revision 3.7  2005/05/13 16:05:07  mast
c       Added ability to trim output model down by requiring adjacent points
c       
c       Revision 3.6  2005/01/29 20:25:54  mast
c       Added ability to handle nested contours
c       
c       Revision 3.5  2005/01/17 06:38:52  mast
c       Added checks for array limits
c       
c       Revision 3.4  2005/01/11 19:23:58  mast
c       Added pixel model output, fixed bug in computing sum and average
c       above threshold
c       
c       Revision 3.3  2005/01/10 06:35:44  mast
c       Fixed fallback option
c       
c       Revision 3.2  2004/09/21 22:21:13  mast
c       Removed a ; for SGI
c       
c       Revision 3.1  2004/09/10 05:56:49  mast
c       Initial addition to package
c       
c       
      implicit none
      integer imsiz,limflag,limsurf,limobj
      parameter (imsiz=4100, limflag = 9999, limobj = 9999, limsurf = 10000)
      real*4 array(imsiz*imsiz)
      integer*4 nxyz(3),mxyz(3),nx,ny,nz
      equivalence (nx,nxyz(1)),(ny,nxyz(2)),(nz,nxyz(3))
      character*120 infile,outfile,modelfile,pixelModel
      include 'model.inc'
      common /bigcom/array
      logical readw_or_imod
      integer*4 iSurface(max_obj_num),iobjflag(limflag),iobjdo(limobj)
      integer*4 listsurf(limsurf),numInBorder(limsurf), numFiltered(limsurf)
      integer*4 numAboveThresh(limsurf),isurf, levels(max_obj_num)
      integer*4 insideIndex(max_obj_num), insideCont(max_obj_num)
      real*8 sumsAbove(limsurf), sumTemp, sumsqsAbove(limsurf), sumsqTemp
      character*120 line
c       
      integer*4 mode,i,j,nobjdo,iunitOut,numTot,numInTemp,numAboveTemp
      integer*4 iobj,imodobj,imodobj2,imodcont,ifdo,iconThresh,numSurf
      real*4 dmin,dmax,dmean,sumTot,avgAbove,absSum,absThresh
      integer*4 ierr,ierr2,irefObject,ifReverse,ifVerbose,nThreshIn,numInside
      integer*4 getimodhead,getimodflags,getimodsurfaces, ifDiagonal
      real*4 xyscal,zscale,xofs,yofs,zofs
      integer*4 newObj, newImodObj, numNewCont, numObjOrig, nestErr
      integer*4 ifflip,getimodobjsize, getImodNesting, numAdjacentNeeded
      integer*4 numFiltTemp
      logical*4 firstNewCont
      real*4 border, polarity, bufferInside
      real*8 sumAbove, sumsqAbove
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetBoolean
      integer*4 PipGetString,PipGetFloat
      integer*4 PipGetNonOptionArg, PipGetInOutFile, PipPrintHelp
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  sumdensity
c       
      integer numOptions
      parameter (numOptions = 16)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'image:ImageFile:FN:@model:ModelFile:FN:@'//
     &    'output:OutputFile:FN:@pixel:PixelModel:FN:@'//
     &    'adjacent:AdjacentRequired:I:@'//
     &    'diagonals:DiagonalsCount:B:@objects:ObjectsToDo:LI:@'//
     &    'border:BorderSize:F:@inside:InsideBorder:F:@'//
     &    'absolute:AbsoluteThreshold:F:@'//
     &    'contrast:ContrastThreshold:I:@'//
     &    'reference:ReferenceObject:I:@reverse:ReversePolarity:B:@'//
     &    'verbose:VerboseOutput:B:@param:ParameterFile:PF:@'//
     &    'help:usage:B:'
c       
c       Set all defaults here
c       
      infile = ' '
      outfile = ' '
      modelfile = ' '
      pixelModel = ' '

      nobjdo=0
      border=0.
      bufferInside = 0.
      irefObject = -1
      polarity = 1.
      ifReverse = 0
      ifVerbose = 0
      numAdjacentNeeded = 0
      ifDiagonal = 0
c       
c       Pip startup: set error, parse options, do help output
c       
      call PipReadOrParseOptions(options, numOptions, 'sumdensity',
     &    'ERROR: SUMDENSITY - ', .false., 2, 2, 1, numOptArg,
     &    numNonOptArg)
c       
c       Get the files; output file is optional
c       
      if (PipGetInOutFile('ImageFile', 1, ' ', infile)
     &    .ne. 0) call exitError('NO INPUT FILE SPECIFIED')
      if (PipGetInOutFile('ModelFile', 2, ' ', modelfile)
     &    .ne. 0) call exitError('NO MODEL FILE SPECIFIED')
      ierr = PipGetInOutFile('OutputFile', 3, ' ', outfile)
c       
c       open image file
c       
      call imopen(1,infile,'ro')
      call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
c       
c       open model file and get header info
c       
      call imodPartialMode(1)
      if(.not.readw_or_imod(modelfile))
     &    call exitError ('READING MODEL FILE')

      ierr=getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip)
      if (ierr .ne. 0)
     &    call exitError('getting model header')
      if (getimodflags(iobjflag,limflag) .ne. 0)
     &    call exitError('getting object types')
      if (getimodsurfaces(iSurface) .ne. 0)
     &    call exitError('getting surface numbers')
c       
      nThreshIn = 0
      if (PipGetFloat('AbsoluteThreshold', absThresh) .eq. 0)
     &    nThreshIn  = nThreshIn + 1
      if (PipGetInteger('ContrastThreshold', iconThresh) .eq. 0) then
        nThreshIn  = nThreshIn + 1
        absThresh = (iconThresh + 0.5) * (dmax - dmin) / 255. + dmin
      endif
      if (PipGetInteger('ReferenceObject', irefObject) .eq. 0) then
          nThreshIn  = nThreshIn + 1
        if (irefObject .le. 0 .or. irefObject .gt. getimodobjsize())
     &      call exitError(
     &      'ILLEGAL OBJECT NUMBER FOR REFERENCE OBJECT')
        if (mod(iobjflag(irefObject), 4) .ne. 0) call exitError(
     &      'REFERENCE OBJECT IS NOT A CLOSED CONTOUR OBJECT')
      endif
      if (nThreshIn .eq. 0) call exitError(
     &    'A THRESHOLD HAS NOT BEEN SPECIFIED')
      if (nThreshIn .gt. 1) call exitError(
     &    'A THRESHOLD WAS SPECIFIED IN MORE THAN ONE WAY')
      
      ierr = PipGetFloat('BorderSize', border)
      ierr = PipGetBoolean('ReversePolarity', ifReverse)
      if (ifReverse .ne. 0) polarity = -1.
      if (PipGetString('ObjectsToDo', line) .eq. 0)
     &    call parselist(line, iobjdo,nobjdo)
      ierr = PipGetBoolean('VerboseOutput', ifVerbose)
      ierr = PipGetString('PixelModel', pixelModel)
      bufferInside = border
      ierr = PipGetFloat('InsideBorder', bufferInside)
      ierr = PipGetInteger('AdjacentRequired', numAdjacentNeeded)
      ierr = PipGetBoolean('DiagonalsCount', ifDiagonal)
      call PipDone()
c       
      iunitOut = 6
      if (outfile .ne. ' ') then
        iunitOut = 7
        call dopen(7, outfile, 'new', 'f')
      endif
c       
c       compute the reference areas if object defined
c       
      if (irefObject .gt. 0) then
        call getOneObject(irefObject)
c         
        numTot = 0
        sumTot = 0.
        do iobj = 1,max_mod_obj
          if (npt_in_obj(iobj).gt.2) then
            call sum_inside_contour(array, imsiz, nx, ny, nz, iobj,
     &          irefObject, border, dmin - dmax, 1., 0, insideCont,
     &          bufferInside, numInTemp, numAboveTemp, absSum,
     &          sumAbove, sumsqAbove, 0, 0, 0)
            numTot = numTot + numInTemp
            sumTot = sumTot + absSum
          endif
        enddo
        if (numTot .eq. 0) call exitError('REFERENCE OBJECT DID'//
     &      ' NOT HAVE USEFUL CONTOURS FOR GETTING THRESHOLD')
        absThresh = sumTot / numTot

        write(*,'(a,i7,a,g15.6)')' Threshold computed from',numTot,
     &      ' pixels is',absThresh
      endif
c       
c       Setup for pixel output
c       
      newImodObj = 0
      numNewCont = 0
      numObjOrig = getimodobjsize()
      if (pixelModel .ne. ' ') then
        newImodObj = numObjOrig + 1
        firstNewCont = .true.
      endif
c       
c       loop on objects
c       
      do imodobj = 1, numObjOrig
c         
c         do object if no list, or on list, and if closed contour
c         exclude reference object
c         
        ifdo = 1
        if (nobjdo .gt. 0) then
          ifdo = 0
          do i = 1, nobjdo
            if (iobjdo(i) .eq. imodobj) ifdo = 1
          enddo
        endif
        if (mod(iobjflag(imodobj), 4) .ne. 0 .or. imodobj .eq. irefObject)
     &      ifdo = 0
c         
        if (ifdo .ne. 0) then
c           
c           If doing pixel output, go to new object if last one had output
          if (numNewCont .gt. 0) then
            newImodObj = newImodObj + 1
            numNewCont = 0
          endif
          call getOneObject(imodobj)
c           
          nestErr = getImodNesting(imodobj, 1, levels, insideIndex,
     &        insideCont, iobj, isurf, max_obj_num)
          if (nestErr .ne. 0)print *,'ERROR ',nesterr,
     &        ' GETTING INSIDE CONTOUR INFORMATION FOR OBJECT',imodobj
c           
c           look for valid contours and make a list of surface numbers
c           
          numSurf = 0
          do iobj = 1, max_mod_obj
            if (npt_in_obj(iobj) .gt. 2 .and.
     &          256 - obj_color(2,iobj) .eq. imodobj) then
              isurf = 0
              do i = 1, numSurf
                if (iSurface(iobj) .eq. listsurf(i)) isurf = i
              enddo
              if (isurf .eq. 0) then
                numSurf = numSurf + 1
                if (numSurf .gt. limsurf) call exitError
     &              ('TOO MANY SURFACES FOR ARRAYS')
                listsurf(numSurf) = iSurface(iobj)
                numInBorder(numSurf) = 0
                numAboveThresh(numSurf) = 0
                sumsAbove(numSurf) = 0
                sumsqsAbove(numSurf) = 0
                numFiltered(numSurf) = 0
              endif
            endif
          enddo
c           
c           order surface list
c           
          do i = 1, numSurf - 1
            do j = i + 1, numSurf
              if (listsurf(i) . gt. listsurf(j)) then
                isurf = listsurf(i)
                listsurf(i) = listsurf(j)
                listsurf(j) = isurf
              endif
            enddo
          enddo    
c           
c           Do contours and add results for surfaces
c           
          do iobj = 1, max_mod_obj
            call objtocont(iobj,obj_color,imodobj2,imodcont)
            if (npt_in_obj(iobj) .gt. 2 .and. imodobj2 .eq. imodobj .and.
     &          (nestErr .ne. 0 .or. levels(imodcont) .eq. 0 .or.
     &          mod(levels(imodcont), 2) .ne. 0)) then
              isurf = 0
              do i = 1, numSurf
                if (iSurface(iobj) .eq. listsurf(i)) isurf = i
              enddo
c               
c               If doing pixel output, allocate new contour first time or if
c               last one non-empty
c               
              newObj = 0
              if (newImodObj .gt. 0 .and. max_mod_obj .lt. max_obj_num .and.
     &            n_point .lt. max_pt - 10) then
                if (firstNewCont .or. npt_in_obj(max_mod_obj) .gt. 0)
     &              max_mod_obj = max_mod_obj + 1
                if (max_mod_obj .eq. max_obj_num) print *,
     &              'TOO MANY CONTOURS TO CONTINUE ADDING TO PIXEL MODEL'
                newObj = max_mod_obj
                npt_in_obj(max_mod_obj) = 0
                obj_color(2, newObj) = 256 - newImodObj
                ibase_obj(newObj) = n_point
                obj_color(1, newObj) = 1
                firstNewCont = .false.
              endif
c               
              numInside = 0
              numFiltTemp = -1
              if (nestErr .eq. 0) numInside =
     &            insideIndex(imodcont + 1) - insideIndex(imodcont)
c               
              call sum_inside_contour(array, imsiz, nx, ny, nz, iobj,
     &            imodobj,border, absThresh, polarity, numInside,
     &            insideCont(insideIndex(imodcont)),bufferInside, numInTemp,
     &            numAboveTemp, absSum, sumAbove, sumsqAbove, newObj, numAdjacentNeeded,
     &            ifDiagonal)
              if (newObj .gt. 0 .and. npt_in_obj(newObj) .gt. 0)
     &            numNewCont = numNewCont + 1
              if (newObj .gt. 0) numFiltTemp = npt_in_obj(newObj)

              numInBorder(isurf) = numInBorder(isurf) +numInTemp
              numAboveThresh(isurf) = numAboveThresh(isurf) + numAboveTemp
              sumsAbove(isurf) = sumsAbove(isurf) + sumAbove
              sumsqsAbove(isurf) = sumsqsAbove(isurf) + sumsqAbove
              numFiltered(isurf) = numFiltered(isurf) + numFiltTemp
              if (ifVerbose .ne. 0) call print_result(iunitOut, mode,
     &            imodobj, 'cont', imodcont, numInTemp,numAboveTemp,
     &            sumAbove, sumsqAbove, numFiltTemp)
            endif
          enddo     
          numInTemp = 0
          numAboveTemp = 0
          sumTemp = 0.
          sumsqTemp = 0.
          numFiltTemp = 0
          do isurf = 1, numSurf
            numInTemp = numInTemp + numInBorder(isurf)
            numAboveTemp = numAboveTemp + numAboveThresh(isurf)
            sumTemp = sumTemp + sumsAbove(isurf)
            sumsqTemp = sumsqTemp + sumsqsAbove(isurf)
            numFiltTemp = numFiltTemp + numFiltered(isurf)
            call print_result(iunitOut,mode, imodobj,'surf',listsurf(isurf),
     &          numInBorder(isurf), numAboveThresh(isurf), sumsAbove(isurf),
     &          sumsqsAbove(isurf), numFiltered(isurf))
          enddo
          call print_result(iunitOut, mode, imodobj,'ALL ',numSurf,
     &        numInTemp, numAboveTemp, sumTemp, sumsqTemp, numFiltTemp)
c           
c           Put object back to model
c           
          if (newImodObj .gt. 0) then
            call scale_model(1)
            call putModelObjects()
          endif
        endif
      enddo

      if (newImodObj .gt. 0) then
        if (numNewCont .eq. 0) newImodObj = newImodObj - 1
        do i = numObjOrig + 1, newImodObj
          call putimodflag(i, 2)
          call putsymtype(i, 0)
          call putsymsize(i, 1)
        enddo
        n_point = -1
        call write_wmod(pixelModel)
      endif

      call exit(0)
99    call exitError('READING FILE')
      end


      subroutine print_result(iunitOut, mode, imodobj, typeText, icont,
     &    numIn, numAbove, sumAbove, sumsqAbove, numFilt)
      implicit none
      integer*4 iunitOut, mode, imodobj, icont, numIn, numAbove, numFilt
      real*8 sumAbove, sumsqAbove
      real*4 avgAbove, sdAbove
      character*(*) typeText
      if (numIn .eq. 0) return
      avgAbove = 0.
      call sums_to_avgsd8(sumAbove, sumsqAbove, numabove, 1, avgAbove, sdAbove)
      if (numFilt .lt. 0) then
        if (mode .eq. 2) then
          write(iunitOut, 102)imodobj,typeText, icont, numIn,
     &        numAbove, (100. * numAbove) / numIn, sumAbove, avgAbove, sdAbove
102       format(' obj',i5,2x,a,i6,2i10,f7.2,3e15.6)
        else
          write(iunitOut, 103)imodobj,typeText, icont, numIn,
     &        numAbove, (100. * numAbove) / numIn, sumAbove, avgAbove, sdAbove
103       format(' obj',i5,2x,a,i6,2i10,f7.2,f13.0,f11.3, f11.3)
        endif
      else
        if (mode .eq. 2) then
          write(iunitOut, 104)imodobj,typeText, icont, numIn, numAbove,
     &        (100. * numAbove) / numIn, sumAbove, avgAbove, sdAbove, numFilt
104       format(i5,1x,a,i6,2i9,f7.2,3e15.6,i9)
        else
          write(iunitOut, 105)imodobj,typeText, icont, numIn, numAbove,
     &        (100. * numAbove) / numIn, sumAbove, avgAbove, sdAbove, numFilt
105       format(i5,1x,a,i6,2i10,f7.2,f13.0,f11.3,f11.3,i10)
        endif
      endif
      return
      end


c       SUM_INSIDE_CONTOUR computes the sum of pixels inside a contour,
c       excluding a border region
c       Input parameters:
c       ARRAY is the array for reading image, dimensioned to IMSIZ**2
c       NX, NY, Nz are dimensionf of the file
c       IOBJ is the "object" number
c       imodObj is IMOD object number
c       BORDER is the size of the exclusion zone
c       ABSTHRESH is the threshold for counting pixels
c       POLARITY is 1 to count bright pixels, or -1 for dark pixels
c       numInside is the number of inside contours
c       insideCont is an array with their contour numbers
c       bufferInside is the border size outside inner contours
c       Returned parameters:
c       numInBorder  is the number of pixels inside the borders
c       numAboveThresh is the number of pixels above threshold
c       absSum is the absolute sum of the pixels above threshold
c       avgAbove is the average of the those pixels after subtracting
c       the threshold
c       newObj is an "object" number in which to place points above threshold
c       numAdjacentNeeded is a minimum number of adjacent points required
c       to place a point in the output model
c       ifDiagonal is 1 if corner (diagonal) points count as adjacent
c       
      subroutine sum_inside_contour(array, imsiz, nx, ny, nz, iobj, imodObj,
     &    border, absThresh, polarity, numInside, insideCont, bufferInside,
     &    numInBorder, numAboveThresh, absSum, sumAbove, sumsqAbove,
     &    newObj, numAdjacentNeeded, ifDiagonal)
      implicit none
      integer limpts
      parameter (limpts=10000)
      integer*4 imsiz, iobj, numInBorder, numAboveThresh, nx, ny, nz, newObj
      integer*4 numAdjacentNeeded, ifDiagonal
      real*4 array(*), absThresh, absSum, avgAbove, border,polarity
      real*4 bufferInside
      real*4 cx(limpts),cy(limpts),ex(limpts),ey(limpts)
      integer*4 imodObj, numInside, insideCont(*)
      include 'model.inc'
      integer*4 ibase, ix, iy, iz, ipt, i, ixst, ixnd, iyst, iynd, nxload
      integer*4 nyload, ninobj, ilas, jobj, jobjLast, ninjobj, incont
      integer*4 iobjfromcont
      logical inside, interior, outsideBorder, trace
      real*4 xmin, xmax, ymin, ymax, xpix, ypix, val, radsq
      real*4 xtrace,ytrace
      real*8 sumAbove, sumsqAbove
c       
c       Set these to imod pixel values - .5 to get trace
c       
      xtrace = -100.5
      ytrace = -100.5
c       
      ninobj = npt_in_obj(iobj)
      if (ninobj .gt. limpts) call exitError('TOO MANY POINTS IN CONTOURS FOR ARRAYS')
      jobjLast = -1
      numInBorder = 0
      numAboveThresh = 0
      absSum = 0.
      sumAbove = 0.
      sumsqAbove = 0.
c       
c       copy contour into arrays and get min/max
c       
      xmin = 10000000.
      xmax = -xmin
      ymin = xmin
      ymax = xmax
      ibase = ibase_obj(iobj)
      iz = nint(p_coord(3, object(ibase + 1)))
      if (iz .lt. 0 .or. iz .ge. nz) call exitError
     &    ('CONTOUR Z VALUE OUT OF RANGE OF IMAGE FILE')
      do i = 1, ninobj
        ipt = object(ibase + i)
        cx(i) = p_coord(1, ipt)
        cy(i) = p_coord(2, ipt)
        xmin = min(xmin, cx(i))
        xmax = max(xmax, cx(i))
        ymin = min(ymin, cy(i))
        ymax = max(ymax, cy(i))
      enddo
c       
c       adjust min/max to be inside borders of image enough for expansion
c       
      xmin = max(xmin, 1.)
      xmax = min(xmax, nx - 3.)
      ymin = max(ymin, 1.)
      ymax = min(ymax, ny - 3.)
      if (xmin.ge.xmax .or. ymin.ge.ymax) return
c       
c       get boundaries of load, a bit bigger
c       
      ixst = nint(xmin - 1.)
      ixnd = nint(xmax + 2.)
      iyst = nint(ymin - 1.)
      iynd = nint(ymax + 2.)
      nxload = ixnd + 1 - ixst
      nyload = iynd + 1 - iyst
      if (nxload * nyload .gt. imsiz**2) call exitError('CONTOUR TOO BIG FOR IMAGE ARRAY')
c       
c       load image
c       
      call imposn(1, iz, 0)
      call irdpas(1, array, nxload, nyload, ixst, ixnd, iyst, iynd, *99)
c       
c       loop on pixels.  Use coordinates of pixel centers
c       
      do iy = 0, nyload - 1
        do ix = 0, nxload - 1
          xpix = ixst + 0.5 + ix
          ypix = iyst + 0.5 + iy
          trace = xtrace.eq.xpix .and. ytrace.eq.ypix
c           
c           test if inside the contour and if outside border region
c           
          if (inside(cx, cy, ninobj, xpix, ypix)) then
            if (trace) print *,'inside',iobj
            interior = outsideBorder(cx, cy, ninobj, border, xpix, ypix)
            if (trace .and. interior) print *,'passes border test'
c             
c             Test for inside or within border regions of inside contours
c             
            incont = 1
            do while (incont .le. numInside .and. interior)
              jobj = iobjfromcont(imodobj, insideCont(incont))
c               
c               Load contour into arrays unless it is the last loaded
c               
              if (jobj .ne. jobjLast) then
                ibase = ibase_obj(jobj)
                ninjobj = min(npt_in_obj(jobj), limpts)
                do i = 1, ninjobj
                  ipt = object(ibase + i)
                  ex(i) = p_coord(1, ipt)
                  ey(i) = p_coord(2, ipt)
                enddo
                jobjLast = jobj
              endif
              if (ninjobj .gt. 2) then
                interior = .not. inside(ex, ey, ninjobj, xpix, ypix)
                if (trace)  print *,'outside inner cont?',
     &              insideCont(incont),jobj, interior
                if (interior) then
                  interior = outsideBorder(ex, ey, ninjobj, bufferInside,
     &                xpix, ypix)
                  if (trace) print *,'outside border of cont?',
     &                insideCont(incont),jobj, interior
                endif
              endif
              incont = incont + 1
            enddo
c             
c             If pixel is in interior, count it, and see if it is above the
c             threshold, if so add to sums
c             
            if (interior) then
              numInBorder = numInBorder + 1
              val = array(1 + ix + iy * nxload)
              if (polarity * (val - absThresh) .gt. 0) then
                numAboveThresh = numAboveThresh +1
                absSum = absSum + val
                sumAbove = sumAbove + polarity * (val - absThresh)
                sumsqAbove = sumsqAbove + (polarity * (val - absThresh))**2

                if (newObj .ne. 0 .and. n_point .lt. max_pt - 10) then
                  n_point = n_point + 1
                  if (n_point .eq. max_pt - 10) print *,
     &                'TOO MANY POINTS TO CONTINUE ADDING TO PIXEL MODEL'
                  p_coord(1, n_point) = xpix
                  p_coord(2, n_point) = ypix
                  p_coord(3, n_point) = iz
                  npt_in_obj(newObj) = npt_in_obj(newObj) + 1
                  object(n_point) = n_point
                  pt_label(n_point) = 0
                endif
              endif
            endif
          endif
        enddo
      enddo
c       
c       Trim model points down if adjacent points required
c       
      if (newObj .gt. 0 .and. numAdjacentNeeded .gt. 0) then
        radsq = 1.2
        if (ifDiagonal .gt. 0) radsq = 2.2
c         
c         count adjacent points and mark if it meets required number
c         
        do ix = n_point + 1 - npt_in_obj(newObj), n_point
          incont = 0
          do iy = n_point + 1 - npt_in_obj(newObj), n_point
            if (ix .ne. iy .and. (p_coord(1, ix) - p_coord(1, iy))**2 +
     &          (p_coord(2, ix) - p_coord(2, iy))**2 .le. radsq)
     &          incont = incont + 1
          enddo
          if (incont .ge. numAdjacentNeeded) pt_label(ix) = 1
        enddo
c         
c         pack the points down and adjust the count
c         
        iy = n_point - npt_in_obj(newObj)
        do ix = n_point + 1 - npt_in_obj(newObj), n_point
          if (pt_label(ix) .gt. 0) then
            iy = iy + 1
            p_coord(1, iy) = p_coord(1,ix)
            p_coord(2, iy) = p_coord(2,ix)
            pt_label(iy) = 0
          endif
        enddo    
        npt_in_obj(newObj) = npt_in_obj(newObj) - (n_point - iy)
        n_point = iy
      endif

      return
99    call exitError('READING IMAGE FILE')
      end




      logical function typeonlist(itype,ityplist,ntyplist)
      implicit none
      integer*4 ityplist(*),itype,ntyplist,i
      typeonlist=.true.
      if(ntyplist.eq.1.and.ityplist(1).eq.-999)return
      do i=1,ntyplist
        if(itype.eq.ityplist(i))return
      enddo
      typeonlist=.false.
      return
      end
      
      subroutine  getOneObject(iobj)
      implicit none
      include 'model.inc'
      integer*4 iobj
      logical getModelObjectRange
      if (.not.getModelObjectRange(iobj, iobj)) then
        print *
        print *, 'ERROR: SUMDENSITY - LOADING DATA FOR OBJECT #', iobj
        call exit(1)
      endif
      call scale_model(0)
      return
      end


      logical function outsideBorder(cx, cy, ninobj, border, xpix, ypix)
      implicit none
      real*4 cx(*), cy(*), xpix, ypix, border
      integer*4 ninobj, ilas, i
      real*4 borderSq, dx, dy, t
c       
      borderSq = border**2
      ilas = ninobj
      i = 1
      outsideBorder = .true.
c       
c       compute distance from each line segment of contour and make
c       sure pixel is not in the border region
c       
      do while (i .le. ninobj .and. outsideBorder)
        dx = cx(i) - cx(ilas)
        dy = cy(i) - cy(ilas)
        t = 0.
        if (dx .ne. 0. .or. dy .ne. 0.) t = max(0., min(1.,
     &      ((xpix - cx(ilas)) * dx + (ypix - cy(ilas)) * dy) /
     &      (dx**2 + dy**2)))
        dx = xpix - (cx(ilas) + t * dx)
        dy = ypix - (cy(ilas) + t * dy)
        outsideBorder = dx**2 + dy**2 .gt. borderSq
        ilas = i
        i = i + 1
      enddo
      return
      end
