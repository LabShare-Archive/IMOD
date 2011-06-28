c       MAXJOINSIZE computes the maximum size and offsets needed to hold
c       transformed data when joining serial sections
c       
c       $Id$
c       
c       $Log$
c       Revision 3.5  2011/06/23 14:57:06  mast
c       Reorder arguments to calls for consistency
c
c       Revision 3.4  2011/06/17 05:43:40  mast
c       Modified for warping
c
c       Revision 3.3  2008/12/03 21:50:02  mast
c       Increased field width in outputs, -1000 shift in Y was unparseable
c
c       Revision 3.2  2008/11/21 20:42:09  mast
c       Increased character limits
c
c       Revision 3.1  2004/10/28 21:14:20  mast
c       Addition to package
c	
c       
      implicit none
      integer maxfiles
      parameter (maxfiles = 100000)
      real*4 f(2,3,maxfiles), dmin, dmax, dmean, deltaFirst(3)
      integer*4 nx(maxfiles), ny(maxfiles), nxyz(3), mxyz(3), mode
      integer*4 numF, numFiles, i, lineSkip, maxx, maxy, idirx, idiry
      real*4 xcen, ycen, xhalf, yhalf, xmin, xmax, ymin, ymax, xcorn, ycorn
      integer*4 ixofs, iyofs, newx, newy, ierr, ifControl, nxwarp, nywarp, ibinning
      integer*4 iflags, maxNxg, maxNyg, nxGrid, nyGrid
      real*4 warpScale, xGridStrt, yGridStrt, xGridIntrv, yGridIntrv, dx, dy, pixelSize
      integer*4, allocatable :: nControl(:)
      real*4, allocatable :: dxGrid(:,:), dyGrid(:,:)
      logical*4 warping, hasWarp
      character*320 rootname,filename, errString
      integer*4 iargc, readCheckWarpFile, findMaxGridSize, getLinearTransform
      integer*4 getSizeAdjustedGrid
c	
      ifControl = 0
      if (iargc() .eq. 0) then
        print *,'Usage: maxjoinsize number_of_sections ',
     &      'lines_to_skip root_name'
        print *,' Computes size and offsets needed to contain all ',
     &      'data from joined tomograms'
        print *,' It use transforms in root_name.tomoxg and gets ',
     &      'image filenames from'
        print *, ' root_name.info, skipping the given number of lines to',
     &      ' get to the filenames'
        call exit(0)
      endif
      call setExitPrefix('ERROR: MAXJOINSIZE - ')
      if (iargc().ne.3) call exitError('THERE MUST BE THREE ARGUMENTS')
c       
c       get number of files and lines to skip
c       
      call getarg(1, rootname)
      read(rootname, *, err=91, end = 91)numFiles
      call getarg(2, rootname)
      read(rootname, *, err=91, end = 91)lineSkip
c       
c       Get the rootname, open tomoxg and read transforms
c       
      call getarg(3, rootname)
      filename = trim(rootname)//'.tomoxg'
      ierr = readCheckWarpFile(filename, 0, 1, nxwarp, nywarp, numF,
     &    ibinning, pixelSize, iflags, errString)
      if (ierr .lt. -1) call exitError(errString)
      warping = ierr .ge. 0
      if (warping) then
        if (mod(iflags / 2, 2) .ne. 0) ifControl = 1
      else
        open(3,file=filename,form='formatted',status='old' ,err=92)

        call xfrdall2(3, f, numF, maxfiles, ierr)
        if (ierr .eq. 2) call exitError('READING TRANSFORM FILE')
        if (ierr .eq. 1) call exitError('TOO MANY TRANSFORMS IN FILE FOR TRANSFORM ARRAY')
        close(3)
      endif
      if (numF .ne. numFiles) call exitError('WRONG NUMBER OF TRANSFORMS IN FILE')
c       
c       open the info file and skip lines
c       

      filename = trim(rootname)//'.info'
      open(3,file=filename,form='formatted',status='old' ,err=94)
      do i = 1, lineSkip
        read(3, '(a)', err=94, end=94)filename
      enddo
      call ialprt(.false.)
c       
c       open image files, get sizes and max size
c       
      maxx = 0
      maxy = 0
      do i = 1, numFiles
        read(3, '(a)', err=95, end=95)filename
        call imopen(1, filename, 'ro')
        call irdhdr(1, nxyz, mxyz, mode, dmin, dmax, dmean)
        if (i .eq. 1) call irtdel(1, deltaFirst)
        call imclose(1)
        nx(i) = nxyz(1)
        ny(i) = nxyz(2)
        maxx = max(maxx, nx(i))
        maxy = max(maxy, ny(i))
      enddo
      close(3)
c       
c       Set up for warping
      if (warping) then
        warpScale = pixelSize / deltaFirst(1)
        allocate(nControl(numFiles), stat = ierr)
        call memoryError(ierr, 'ARRAY FOR NUMBER OF CONTROL POINTS')
        if (findMaxGridSize(0., maxx /warpScale, 0., maxy / warpScale, nControl, maxNxg,
     &      maxNyg, errString) .ne. 0) call exitError(errString)
        if (maxNxg * maxNyg .gt. 0) then
          allocate(dxGrid(maxNxg, maxNyg), dyGrid(maxNxg, maxNyg), stat = ierr)
          call memoryError(ierr, 'ARRAYS FOR WARPING FIELDS')
        endif
      endif
c       
c       transform the 4 corners in the coordinate system of the maximum size
c       
      xcen = maxx / 2.
      ycen = maxy / 2.
      xmin = xcen
      ymin = xcen
      xmax = xcen
      ymax = xcen
      do i = 1, numFiles
        xhalf = nx(i) / 2.
        yhalf = ny(i) / 2.
        hasWarp = .false.
        if (warping) then
          if (getLinearTransform(i, f(1,1,i)) .ne. 0) call exitError(
     &        'GETTING LINEAR TRANSFORM FROM WARP FILE')
          hasWarp = nControl(i) .gt. 2
          if (hasWarp) then
            if (getSizeAdjustedGrid(i, maxx / warpScale, maxy / warpScale, 0., 0., 1,
     &          warpScale, 1, nxGrid, nyGrid, xGridStrt, yGridStrt, xGridIntrv,
     &          yGridIntrv, dxGrid, dyGrid, maxNxg, maxNyg, errString) .ne. 0)
     &          call exitError(errString)
          endif
        endif
        do idirx = -1, 1, 2
          do idiry = -1, 1, 2
            call xfapply(f(1,1,i), xcen, ycen, xcen + idirx * xhalf,
     &          ycen +idiry * yhalf, xcorn, ycorn)
            if (hasWarp) then
c              print *,i,idirx,idiry,xcorn,ycorn
              call findInversePoint(xcorn, ycorn, dxGrid, dyGrid, maxNxg, nxgrid, nyGrid,
     &            xGridStrt, yGridStrt, yGridIntrv, yGridIntrv, xcorn, ycorn, dx,dy)
c              print *,xcorn,ycorn, dx, dy
            endif
            xmin = min(xmin, xcorn)
            ymin = min(ymin, ycorn)
            xmax = max(xmax, xcorn)
            ymax = max(ymax, ycorn)
          enddo
        enddo
      enddo
c       
c       Compute and return numbers
c       
      ixofs = nint(0.5*(xmax+xmin) - xcen)
      iyofs = nint(0.5*(ymax+ymin) - ycen)
      newx = 2 * nint(0.5*(xmax - xmin))
      newy = 2 * nint(0.5*(ymax - ymin))
      write(*,101)newx,newy,ixofs,iyofs
101   format('Maximum size required:',2i9,/,'Offset needed to center:',
     &    2i8)
      call exit(0)
c       
91    call exitError('READING NUMBER OF FILES OR LINES TO SKIP')
92    call exitError('OPENING .tomoxg FILE')
93    call exitError('READING TRANSFORMS FROM .tomoxg FILE')
94    call exitError('OPENING .info FILE')
95    call exitError('READING FILENAME FROM .info FILE')
      end

      
