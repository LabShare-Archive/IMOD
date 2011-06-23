c
c       IDFPROCESS will do several simple operations on image distortion field
c       files: it can compute the inverse of the fields in one file, or the 
c       product or difference of fields in two files, and it can report 
c       statistics on the vectors of each distortion field.
c       
c       See man page for more details.
c       
c       David Mastronarde 1/23/06
c
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c
c       $Log$
c       Revision 1.2  2006/09/28 21:24:57  mast
c       changed call to avgsd8
c
c       Revision 1.1  2006/01/24 06:46:35  mast
c       Added to package
c
c
c
      implicit none
      integer maxGrid, lmGrid, lmsec, lmall
      parameter (maxGrid=4, lmGrid=256, lmsec=1000, lmall=2000000)

      real*4 allDx1(lmall), allDy1(lmall), allDx2(lmall), allDy2(lmall)
      real*4 fieldDx(lmGrid, lmGrid, maxGrid), fieldDy(lmGrid, lmGrid, maxGrid)
      real*4 pixel(2), xIntrv(2), yIntrv(2)
      real*4 xIntrv1(lmsec), yIntrv1(lmsec), xIntrv2(lmsec), yIntrv2(lmsec)
      integer*4 iBinning(2), idfNx(2), idfNy(2), numFields(2)
      integer*4 nxGrid(2), nyGrid(2), ixStrt(2), iyStrt(2)
      integer*4 nxGrid1(lmsec), nyGrid1(lmsec), ixStrt1(lmsec), iyStrt1(lmsec)
      integer*4 nxGrid2(lmsec), nyGrid2(lmsec), ixStrt2(lmsec), iyStrt2(lmsec)
      integer*4 ifProd, ifStat, ifInv, ifDiff, iref, indOut, ind1, ind2, ichg
      character*120 inFile1, inFile2, outFile
      integer*4 ierr, numToDo, izdo, iz, ix, iy, istat
      real*4 avg,sd,vmax
c
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetBoolean
      integer*4 PipGetInOutFile
c	  
c       fallbacks from ../../manpages/autodoc2man -2 2  idfprocess
c       
      integer numOptions
      parameter (numOptions = 9)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'in1:InputFile1:FN:@in2:InputFile2:FN:@'//
     &    'output:OutputFile:FN:@product:Product:B:@'//
     &    'difference:Difference:B:@inverse:Inverse:B:@'//
     &    'stats:Statistics:B:@second:SecondGrid:B:@help:usage:B:'
c
      inFile2 = ' '
      outFile = ' '
      ifProd = 0
      ifDiff = 0
      ifInv = 0
      ifStat = 0
      iref = 1
      indOut = 2
c
      call PipReadOrParseOptions(options, numOptions, 'idfprocess',
     &    'ERROR: IDFPROCESS - ', .false., 1, 2, 1, numOptArg, numNonOptArg)
c
      ierr = PipGetBoolean('Product', ifProd)
      ierr = PipGetBoolean('Difference', ifDiff)
      ierr = PipGetBoolean('Inverse', ifInv)
      ierr = PipGetBoolean('Statistics', ifStat)

      if (ifProd + ifDiff + ifInv .gt. 1) call errorexit(
     &    'ONLY ONE OF -product, -diff, AND -inverse CAN BE ENTERED')

      if (PipGetInOutFile('InputFile1', 1, ' ', inFile1) .ne. 0)
     &    call errorexit('NO INPUT FILE SPECIFIED')
c       
c       Get second input file if needed and set index for output file
c
      if (ifProd + ifDiff .gt. 0) then
        if (PipGetInOutFile('InputFile2', 2, ' ', inFile2) .ne. 0)
     &      call errorexit('NO SECOND INPUT FILE SPECIFIED')
        indOut = 3
        iref = 0
        ierr = PipGetBoolean('SecondGrid', iref)
        iref = iref + 1
        if (iref .eq. 1) then
          ind1 = 1
          ind2 = 3
          ichg = 2
        else
          ind1 = 3
          ind2 = 2
          ichg = 1
        endif
      endif
c       
c       Only check for output file if doing an operation.
c       Set up for stats if no output file
c
      if (ifProd + ifDiff + ifInv .gt. 0)
     &    ierr = PipGetInOutFile( 'OutputFile', indOut, ' ', outFile)
      if (outFile .eq. ' ') ifStat = 1
      if (ifProd + ifDiff + ifInv + ifStat .eq. 0) call errorexit(
     &    'NO OPERATION SPECIFIED ON INPUT')
c       
c       Load the input file(s)
c
      call loadDistortions(inFile1, allDx1, allDy1, lmall, idfNx(1), idfNy(1),
     &    numFields(1), iBinning(1), pixel(1), ixStrt1, xIntrv1, nxGrid1,
     &    iyStrt1, yIntrv1, nyGrid1, lmsec)
      numToDo = numFields(1)
      if (inFile2 .ne. ' ') then
        call loadDistortions(inFile2, allDx2, allDy2, lmall, idfNx(2),
     &      idfNy(2), numFields(2), iBinning(2), pixel(2), ixStrt2,
     &      xIntrv2, nxGrid2, iyStrt2, yIntrv2, nyGrid2, lmsec)
        if (numFields(1) .ne. numFields(2) .and. numFields(1) .gt. 1 .and.
     &      numFields(2) .gt. 1) call errorexit(
     &      'NUMBER OF FIELDS DOES NOT MATCH BETWEEN THE TWO FILES')
        numToDo = max(numFields(1), numFields(2))
      endif
c       
c       Start output file
c       
      if (outFile .ne. ' ') call idfStartFile(outFile, 14, idfNx(iref),
     &    idfNy(iref), numToDo, iBinning(iref), pixel(iref))
c       
c       Loop on fields
c
      do izdo = 1, numToDo
c         
c         get field from first file
c
        iz = min(izdo, numFields(1))
        call getSecDistortions(iz, allDx1, allDy1, nxGrid1, nyGrid1,
     &      fieldDx(1,1,1), fieldDy(1,1,1), lmGrid)
        nxGrid(1) = nxGrid1(iz)
        xIntrv(1) = xIntrv1(iz)
        ixStrt(1) = ixStrt1(iz)
        nyGrid(1) = nyGrid1(iz)
        yIntrv(1) = yIntrv1(iz)
        iyStrt(1) = iyStrt1(iz)
c         
c         get field from second file and translate one field to reference
c
        if (inFile2 .ne. ' ') then
          iz = min(izdo, numFields(2))
          call getSecDistortions(iz, allDx2, allDy2, nxGrid2, nyGrid2,
     &        fieldDx(1,1,2), fieldDy(1,1,2), lmGrid)
          nxGrid(2) = nxGrid2(iz)
          xIntrv(2) = xIntrv2(iz)
          ixStrt(2) = ixStrt2(iz)
          nyGrid(2) = nyGrid2(iz)
          yIntrv(2) = yIntrv2(iz)
          iyStrt(2) = iyStrt2(iz)

          call idfnewGrid(fieldDx(1,1,ichg), fieldDy(1,1,ichg), lmGrid,
     &        ixStrt(ichg), xIntrv(ichg), nxGrid(ichg),
     &        iyStrt(ichg), yIntrv(ichg), nyGrid(ichg),
     &        ixStrt(iref), xIntrv(iref), nxGrid(iref),
     &        iyStrt(iref), yIntrv(iref), nyGrid(iref),
     &        fieldDx(1,1,3), fieldDy(1,1,3))
        endif

        istat = 4
        if (ifProd .ne. 0) then
          call idfProduct(fieldDx(1,1,ind1), fieldDy(1,1,ind1),
     &        fieldDx(1,1,ind2), fieldDy(1,1,ind2), lmGrid, ixStrt(iref),
     &        xIntrv(iref), nxGrid(iref), iyStrt(iref), yIntrv(iref),
     &        nyGrid(iref), fieldDx(1,1,4), fieldDy(1,1,4))
        elseif (ifInv .ne. 0) then
          call idfInverse(fieldDx(1,1,1), fieldDy(1,1,1), lmGrid, ixStrt(1),
     &        xIntrv(1), nxGrid(1), iyStrt(1), yIntrv(1), nyGrid(1),
     &        fieldDx(1,1,4), fieldDy(1,1,4))
        elseif (ifDiff .ne. 0) then
          do iy = 1, nyGrid(iref)
            do ix = 1, nxGrid(iref)
              fieldDx(ix, iy, 4) = fieldDx(ix, iy, ind1) - fieldDx(ix,iy, ind2)
              fieldDy(ix, iy, 4) = fieldDy(ix, iy, ind1) - fieldDy(ix,iy, ind2)
            enddo
          enddo
        else
          istat = 1
        endif

        if (ifStat .ne. 0) then
          call idfStats(fieldDx(1,1,istat), fieldDy(1,1,istat), lmGrid,
     &        nxGrid(iref), nyGrid(iref), avg, sd, vmax, ix, iy)
          write(6, 101) izdo - 1, avg, sd, vmax, fieldDx(ix,iy,istat),
     &        fieldDy(ix,iy,istat),ixStrt(iref) + (ix - 1) * xIntrv(iref),
     &        iyStrt(iref) + (iy - 1) * yIntrv(iref)
101       format('Field',i4,': mean',f7.2,', sd',f7.2,', max',f6.1,' (',
     &        f7.1,',',f7.1,') at',2f6.0)
        endif

        if (outFile .ne. ' ') call idfWriteField(14, ixStrt(iref),
     &      xIntrv(iref), nxGrid(iref), iyStrt(iref), yIntrv(iref),
     &      nyGrid(iref), fieldDx(1,1,4), fieldDy(1,1,4), lmGrid)
      enddo
      if (outFile .ne. ' ') close(14)
      call exit(0)
      end


      subroutine idfInverse(fieldDx, fieldDy, lmGrid, ixGridStrt, xGridIntrv,
     &    nxGrid, iyGridStrt, yGridIntrv, nyGrid, dxInv, dyInv)
      implicit none
      integer*4 lmGrid, ixGridStrt, nxGrid, iyGridStrt, nyGrid
      real*4 yGridIntrv, xGridIntrv
      real*4 fieldDx(lmGrid, lmGrid), fieldDy(lmGrid, lmGrid)
      real*4 dxInv(lmGrid, lmGrid), dyInv(lmGrid, lmGrid)
      integer*4 ix, iy, iter
      real*4 xlast, ylast, xnew, ynew, dx, dy, xgrid, ygrid
      logical*4 done
c       
      do iy = 1, nyGrid
        ygrid = iyGridStrt + (iy - 1) * yGridIntrv
        do ix = 1, nxGrid
          xgrid = ixGridStrt + (ix - 1) * xGridIntrv
          done = .false.
          iter = 1
          xlast = xgrid
          ylast = ygrid
          do while (iter .lt. 10 .and. .not.done)
            call interpolateGrid(xlast, ylast, fieldDx, fieldDy, lmGrid, nxGrid,
     &          nyGrid, float(ixGridstrt), float(iyGridStrt), xGridIntrv, yGridIntrv,
     &          dx, dy)
c            print *, ix, iy, xlast, ylast, -dx, -dy
            xnew = xgrid - dx
            ynew = ygrid - dy
            done = abs(xnew - xlast) .lt. 0.01 .and.
     &          abs(ynew - ylast) .lt. 0.01
            xlast = xnew
            ylast = ynew
            iter = iter + 1
          enddo     
          dxInv(ix, iy) = -dx
          dyInv(ix, iy) = -dy
        enddo
      enddo
      return
      end


      subroutine idfProduct(dx1, dy1, dx2, dy2, lmGrid, ixGridStrt, xGridIntrv,
     &    nxGrid, iyGridStrt, yGridIntrv, nyGrid, dxProd, dyProd)
      implicit none
      integer*4 lmGrid, ixGridStrt, nxGrid, iyGridStrt, nyGrid
      real*4 yGridIntrv, xGridIntrv
      real*4 dx1(lmGrid, lmGrid), dy1(lmGrid, lmGrid), dx2(lmGrid), dy2(lmGrid)
      real*4 dxProd(lmGrid, lmGrid), dyProd(lmGrid, lmGrid)
      integer*4 ix, iy
      real*4 dx, dy, xgrid, ygrid
c       
      do iy = 1, nyGrid
        ygrid = iyGridStrt + (iy - 1) * yGridIntrv
        do ix = 1, nxGrid
          xgrid = ixGridStrt + (ix - 1) * xGridIntrv
          call interpolateGrid(xgrid + dx1(ix, iy), ygrid + dy1(ix, iy), dx2, dy2,
     &        lmGrid, nxGrid, nyGrid, float(ixGridstrt), float(iyGridStrt), xGridIntrv,
     &        yGridIntrv, dx, dy)
          dxProd(ix, iy) = dx1(ix, iy) + dx
          dyProd(ix, iy) = dy1(ix, iy) + dy
        enddo
      enddo
      return
      end

          
      subroutine idfNewGrid(dx1, dy1, lmGrid, ixStrt1, xIntrv1, nx1, iyStrt1,
     &    yIntrv1, ny1, ixStrt2, xIntrv2, nx2, iyStrt2, yIntrv2, ny2, dx2, dy2)
      implicit none
      integer*4 lmGrid, ixStrt1, nx1, iyStrt1, ny1
      integer*4 ixStrt2,  nx2, iyStrt2, ny2, ix, iy
      real*4 xIntrv1, yIntrv1, xIntrv2, yIntrv2
      real*4 dx1(lmGrid, lmGrid), dy1(lmGrid, lmGrid)
      real*4 dx2(lmGrid, lmGrid), dy2(lmGrid, lmGrid)
      real*4 xgrid, ygrid
      do iy = 1, ny2
        ygrid = iyStrt2 + (iy - 1) * yIntrv2
        do ix = 1, nx2
          xgrid = ixStrt2 + (ix - 1) * xIntrv2
          call interpolateGrid(xgrid, ygrid, dx1, dy1, lmGrid, nx1, ny1, float(ixStrt1),
     &        float(iyStrt1), xIntrv1, yIntrv1, dx2(ix, iy), dy2(ix, iy))
        enddo
      enddo
      return
      end


      subroutine idfStats(dxGrid, dyGrid, lmGrid, nxgrid, nyGrid, avg, sd,
     &    vmax, ixmax, iymax)
      implicit none
      integer*4 lmGrid, nxgrid, nyGrid, ixmax, iymax, ix, iy
      real*4 dxGrid(lmGrid, lmGrid), dyGrid(lmGrid, lmGrid), avg, sd, vmax, vec
      real*8 sum8, sumsq8
c       
      sum8 = 0.
      sumsq8 = 0.
      vmax = -1.
      do iy = 1, nyGrid
        do ix = 1, nxGrid
          vec = sqrt(dxGrid(ix, iy)**2 + dygrid(ix, iy)**2)
          if (vec .gt. vmax) then
            vmax = vec
            ixmax = ix
            iymax = iy
          endif
          sum8 = sum8 + vec
          sumsq8 = sumsq8 + vec**2
        enddo
      enddo
      call sums_to_avgsd8(sum8, sumsq8, nxGrid, nyGrid, avg, sd)
      return
      end

      subroutine errorexit(message)
      implicit none
      character*(*) message
      print *
      print *,'ERROR: IDFPROCESS - ',message
      call exit(1)
      end
