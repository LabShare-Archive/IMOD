c****   SOLVEXYZD obtains estimates for the underlying (real) x,y,z
c       coordinates of each point and for the delta X and Y of each view,
c       for the given values of tilt, rotation, mag and compression.
c       
c       The equations relating the parameters are:
c       _    xproj = a*x + b*y + c*z + dx(view)
c       _    yproj = d*x + e*y + f*z + dy(view)
c       where a = mag * cos (tilt) * cos (rot)
c       _     b = - mag * sin (rot)
c       _     c = mag * comp * sin (tilt) * cos (rot)
c       where d = mag * cos (tilt) * sin (rot)
c       _     e = mag * cos (rot)
c       _     f = mag * comp * sin (tilt) * sin (rot)
c       
c       5/18/12: Replaced old solve_xyzd and init_dxy routines
c
c       $Id$
c       

      subroutine solveXyzd(xx,yy,isecview,irealstr, nview,nrealpt, tilt,rot,gmag,
     &    comp,xyz,dxy, rotinc, work, maxwork, error, ierr)
      implicit none
      real*4 xx(*),yy(*),tilt(*),rot(*),gmag(*),comp(*) ,xyz(3,*),dxy(2,*),work(*),rotinc
      integer*4 isecview(*),irealstr(*),nview,nrealpt,ierr,maxwork,nr
      real*8 error
c       
c       Size for real*4/int*4 array; doubles need double that
      nr = 3 * nrealpt
      if (nr**2 + 12 * nr .gt. maxwork) call errorExit(
     &    'TEMPORARY ARRAY NOT LARGE ENOUGH FOR XYZ INITIALIZATION ROUTINE')
      call solveXyzdWithArrays(xx,yy,isecview,irealstr, nview,nrealpt, tilt,rot,gmag,
     &    comp,xyz,dxy, rotinc, work, work(2*nr), work(4*nr), work(6*nr), work(8*nr),
     &    work(10*nr), work(11*nr), work(12*nr), error, ierr)
      return
      end

      subroutine solveXyzdWithArrays(xx,yy,isecview,irealstr, nview,nrealpt,tilt,rot,gmag,
     &    comp,xyz,dxy, rotinc, sx, xml, sdl, bl, baseRow, valRow, indOnView, sprod,
     &    error, ierr)
      implicit none
      real*4 xx(*),yy(*),tilt(*),rot(*),gmag(*),comp(*) ,xyz(3,*),dxy(2,*), rotinc
      integer*4 isecview(*),irealstr(*),nview,nrealpt,ierr
      real*8 error
      real*4 valRow(*), baseRow(3*nrealpt, 2)
      real*8 sprod(*), sx(*), xml(*), sdl(*), bl(*)
      integer*4 indOnView(*), indqk(4),j
      logical bigBase(2), bigRow
      real*4 ad(2),be(2),cf(2), fac, xybar(2), xyzcen(3)
      integer*4 numCols, numRows, iv, jpt, i, ixy, icol, ipt, numOnView
      real*8       wallStart, wallTime, addTime
      integer*4 packedIndex
c       
c       Load data matrix, loop on views then real points projecting on each
      numCols = 3 * nrealpt - 2
      numRows = 0
      ierr = 1
      addTime = 0
      if (nrealpt .lt. 2) then
        ierr = 0
        xyz(1:3,1) = 0.
        return
      endif
      sx(1:numCols) = 0.
      sprod(1: (numCols + 1) * numCols / 2) = 0.
      do iv=1,nview
c         
c         Set up ad, be, cf, and get indexes to points on view and get xybar
        call geometricCoeffsAndIndices(iv)
c           
c         Set up the terms for the dxy: if last point is not on view, subtract
c         a/n etc from the columns for all points on view; if last point is on
c         view, add a/n to the columns for all points not in the view
        do ixy = 1, 2
          bigBase(ixy) = .false.
          baseRow(1:numCols, ixy) = 0.
          do ipt=1,nrealpt - 1
            if (indOnView(ipt) .gt. 0 .and. indOnView(nrealpt) .eq. 0 .or. 
     &          indOnView(ipt) .eq. 0 .and. indOnView(nrealpt) .gt. 0) then
              fac = 1.
              if (indOnView(nrealpt) .eq. 0) fac = -1.
              icol = 3 * ipt - 2
              baseRow(icol, ixy) = fac * ad(ixy) / numOnView
              baseRow(icol + 1, ixy)  = fac * be(ixy) / numOnView
              baseRow(icol + 2, ixy)  = fac * cf(ixy) / numOnView
c               
c               Keep track of whether this vector is non-zero
              bigBase(ixy) = .true.
            endif
          enddo
        enddo
c       
c         Loop on points again and set up the equations for their projections
        do jpt=1,nrealpt
          if (indOnView(jpt) .gt. 0) then
            do ixy = 1, 2
              valRow(1:numCols) = baseRow(1:numCols, ixy)
              bigRow = .false.
              if (jpt < nrealpt) then
                icol = 3 * jpt - 2
                valRow(icol) = valRow(icol) + ad(ixy)
                valRow(icol + 1) = valRow(icol + 1) + be(ixy)
                valRow(icol + 2) = valRow(icol + 2) + cf(ixy)
              else
c                 
c                 Last point is negative sum of all the rest, and makes a big row of data
                do ipt = 1, nrealpt - 1
                  icol = 3 * ipt - 2
                  valRow(icol) = valRow(icol) - ad(ixy)
                  valRow(icol + 1) = valRow(icol + 1) - be(ixy)
                  valRow(icol + 2) = valRow(icol + 2) - cf(ixy)
                enddo
                bigRow = .true.
              endif
c             
c               Get the projection position and put it in the last column
c               Adjust that position by the mean of points on view
              if (ixy .eq. 1) then
                valRow(numCols) = xx(indOnView(jpt)) - xybar(1)
              else
                valRow(numCols) = yy(indOnView(jpt)) - xybar(2)
              endif
c              write(*, '(3(2f9.4,f8.4))')(valRow(i), i = 1, numCols)
c           
c               Row is done.  Accumulate it
              wallStart = wallTime()
              if (bigRow .or. bigBase(ixy)) then
                call addRowToSums(valRow, numCols, sx, sprod)
              else
c                 
c                 Just handle non-zero columns if there are a few, it is a lot quicker
                indqk(1) = icol
                indqk(2) = icol + 1
                indqk(3) = icol + 2
                indqk(4) = numCols
                do j = 1, 4
                  sx(indqk(j)) = sx(indqk(j)) + valRow(indqk(j))
                  do i = 1, j
                    icol = packedIndex(indqk(i), indqk(j))
                    sprod(icol) = sprod(icol) + valRow(indqk(i)) * valRow(indqk(j))
                  enddo
                enddo
              endif
              addTime = addTime + wallTime() - wallStart
              numRows = numRows + 1
            enddo
          endif
        enddo
      enddo
c      print *,'Loaded matrix',numCols, numRows
      wallStart = wallTime()

      call solvePackedSums(sx, sprod, numCols, numRows, xml, sdl, bl, ierr)
c      print *,'lapack',ierr
c      write(*, '(3(2f9.4,f8.4))')(sdl(i), i = 1, numCols)
c      write(*, '(3(2f9.4,f8.4))')(bl(i), i = 1, numCols - 1)
c      write(*,'(a,f7.3, a, f7.3)')'solution time',1000.*(wallTime()-wallStart),
c     &    '   addRow time', 1000. * addTime 
c       
c       retrieve solution
      xyz(1:3, nrealpt) = 0.;
      do i = 1, nrealpt - 1
        do ixy = 1, 3
          icol = 3 * (i - 1) + ixy
          xyz(ixy, i) = bl(icol)
          xyz(ixy, nrealpt) = xyz(ixy, nrealpt) - xyz(ixy, i)
        enddo
      enddo
c       
c       compute total error, equals F reported by funct
      error = 0.
      do iv=1,nview
        call geometricCoeffsAndIndices(iv)
        xyzcen = 0.
        do jpt=1,nrealpt
          if (indOnView(jpt) .gt. 0) xyzcen = xyzcen + xyz(1:3, jpt)
        enddo
        do ixy = 1, 2
          dxy(ixy, iv) = xybar(ixy) - (ad(ixy) * xyzcen(1) + be(ixy) * xyzcen(2) +
     &        cf(ixy) * xyzcen(3)) / numOnView
        enddo
        do jpt=1,nrealpt
          i = indOnView(jpt)
          if (i .gt. 0) error = error +
     &        (ad(1) * xyz(1, jpt) + be(1) * xyz(2, jpt) + cf(1) * xyz(3, jpt) +
     &        dxy(1, iv) - xx(i))**2 +
     &        (ad(2) * xyz(1, jpt) + be(2) * xyz(2, jpt) + cf(2) * xyz(3, jpt) +
     &        dxy(2, iv) - yy(i))**2
        enddo
      enddo
c      write(*,'(a,f7.1,a,f15.5)')'Rotinc =',rotinc *180 / 3.14159,',  ERROR = ',error
      return

      CONTAINS
c       
c       Sets up the ad, be, cf for a view, counts points on view, gets indices to them
c       and computes the mean projection position
c
      subroutine geometricCoeffsAndIndices(iv)
      implicit none
      integer*4 iv
      real*4 costhet,csinthet,gcosphi,gsinphi
      costhet = cos(tilt(iv))
      csinthet = comp(iv) * sin(tilt(iv))
      gcosphi = gmag(iv) * cos(rot(iv) + rotinc)
      gsinphi = gmag(iv) * sin(rot(iv) + rotinc)
c       
      ad(1) = costhet * gcosphi
      be(1) = -gsinphi
      cf(1) = csinthet * gcosphi
      ad(2) = costhet * gsinphi
      be(2) = gcosphi
      cf(2) = csinthet * gsinphi
c         
c         Count  and get indices and get the mean projection coordinate
      numOnView = 0
      xybar = 0.
      do jpt = 1, nrealpt
        indOnView(jpt) = 0
        do i = irealstr(jpt), irealstr(jpt+1)-1
          if (iv .eq. isecview(i)) then
            indOnView(jpt) = i
            numOnView = numOnView + 1
            xybar(1) = xybar(1) + xx(i)
            xybar(2) = xybar(2) + yy(i)
          endif
        enddo
      enddo
      xybar = xybar  / numOnView
      end subroutine geometricCoeffsAndIndices
      end

c       Accumulate sums and sums of cross-products from a row of data
c
      subroutine addRowToSums(row, m, sx, sprod)
      implicit none
      real*4 row(*)
      real*8 sx(*), sprod(*)
      integer*4 m, j, ind
      integer*4 packedIndex
      sx(1:m) = sx(1:m) + row(1:m)
      do j = 1, m
        ind = packedIndex(1, j)
        sprod(ind:ind+j-1) = sprod(ind:ind+j-1) + row(1:j) * row(j)
      enddo
      return
      end

c       A subroutine for calling the linear solver, just in case this is useful elsewhere
c       
      subroutine solvePackedSums(sx, ss, mp, nrows, xm, sd, b, ierr)
      implicit none
      real*8 sx(*), ss(*), xm(*), sd(*), b(*)
      integer*4 mp, nrows, i, j, ierr, m, ind
      integer*4 packedIndex
c       
c       First get the means and SDs
      m = mp - 1
      do i = 1, mp
        xm(i) = xm(i) / nrows
        sd(i) = sqrt((ss(packedIndex(i, i)) - sx(i)**2 / nrows) / (nrows - 1.))
      enddo
c       
c       What we have now is a raw sum of squares and cross-products
c       Scale the matrix by the sd's; this scales the RHS (b) variable in the last column
c       This is what multrd does, and is not the same as multr does, which is to convert
c       the matrix to true correlation coefficients, because when there is no constant
c       term the solution involves raw sums of squares instead of deviations from mean
      do j = 1, mp
        ind = packedIndex(1, j)
        do i = 1, j
          ss(ind + i - 1) = ss(ind + i - 1) / (sd(i) * sd(j))
        enddo
      enddo
c       
c       Now we can call lapack with this matrix.  Since it is not a covariance matrix,
c       can't use the positive-definite routine, have to use the symmetric matrix one
      ind = packedIndex(1, mp)
      call dspsv('U', m, 1, ss, b, ss(ind), m, ierr)
      if (ierr .ne. 0) return
c       
c       scale and return b
      do i = 1, m
        b(i) = ss(ind + i - 1) * sd(mp) / sd(i)
      enddo
      return
      end

c       Silly function because statement functions are now obsolete
      integer*4 function packedIndex(i, j)
      integer*4 i, j
      packedIndex = i + (j - 1) * j / 2
      return
      end

