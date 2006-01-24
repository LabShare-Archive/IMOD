c       This file contains modules for reading distortion field files
c       or mag gradient files
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 1.2  2004/03/20 15:36:09  mast
c       Added readMagGradients
c	
c       Revision 1.1  2003/12/27 19:23:55  mast
c       Addition to library
c	
c       !
c       readDistortions will read a distortion field from a file.  If there
c       are multiple fields in the file, only the first is returned
c       ^ [idfFile] specifies name of file with field
c       ^ [fieldDx] and [fieldDy] are arrays in which to place the grid, 
c       dimensioned to [lmGrid] x [lmGrid]
c       ^ [ifdNx] and [idfNy] are the image size from which field was measured
c       ^ [idfBinning] is the binning of those images
c       ^ [pixelIdf] is their pixel size in Angstroms
c       ^ [ixGridStrt], [iyGridStrt] are coordinates at which grid starts
c       ^ [xGridIntrv], [yGridIntrv] are the spacing between grid points in
c       X & Y
c       ^ [nxGrid], [nyGrid] are the number of grid points in X & Y
c       !
      subroutine readDistortions(idfFile, fieldDx, fieldDy, lmGrid, idfNx,
     &    idfNy, idfBinning, pixelIdf, ixGridStrt, xGridIntrv, nxGrid,
     &    iyGridStrt, yGridIntrv, nyGrid)
      implicit none
      character*(*) idfFile
      integer*4  idfBinning, idfNx, idfNy, lmGrid, idfNz
      integer*4 ixGridStrt, iyGridStrt, nxGrid, nyGrid
      real*4 xGridIntrv, yGridIntrv, pixelIdf
      real*4 fieldDx(lmGrid, lmGrid), fieldDy(lmGrid, lmGrid)
c       
      integer*4 idfVersion, i, j
c       
      call dopen(14, idfFile, 'ro', 'f')
      read(14, *) idfVersion
      if (idfVersion .eq. 1) then
        read(14, *)idfNx, idfNy, idfBinning, pixelIdf
      else if (idfVersion .eq. 2) then
        read(14, *)idfNx, idfNy, idfNz, idfBinning, pixelIdf
      else
        print *
        print *,'ERROR: readDistortions - version',idfVersion,
     &      ' of idf file not recognized'
        call exit(1)
      endif
      read(14, *)ixGridStrt, xGridIntrv, nxGrid, iyGridStrt,
     &    yGridIntrv, nyGrid
      if (nxGrid .gt. lmGrid .or. nyGrid .gt. lmGrid) then
        print *
        print *,'ERROR: readDistortions - too many grid points for arrays'
        call exit(1)
      endif
      do j = 1, nyGrid
        read(14, *)(fieldDx(i, j), fieldDy(i,j), i = 1, nxGrid)
      enddo
      close(14)
      return
      end

c       !
c       loadDistortions will read all distortion fields from a file, return
c       the characteristics of each in the various arrays, and return the 
c       fields packed into 1-D arrays
c       ^ [idfFile] specifies name of file with field(s)
c       ^ [fieldDx] and [fieldDy] are arrays in which to place the grids
c       ^ [lmAllGrid] is the dimension of the grid arrays
c       ^ [ifdNx] and [idfNy] are the image size from which fields were
c       measured
c       ^ [idfNz] is the number of fields in the file
c       ^ [idfBinning] is the binning of those images
c       ^ [pixelIdf] is their pixel size in Angstroms
c       ^ [ixGridStrt], [iyGridStrt] are arrays returned with the coordinates
c       at which each grid starts
c       ^ [xGridIntrv], [yGridIntrv] are arrays returned with the spacing
c       between grid points in X & Y for each grid
c       ^ [nxGrid], [nyGrid] are arrays returned with the number of grid
c       points in X & Y for each grid
c       ^ [lmSec] is the dimension of the grid characteristic arrays
c       !
      subroutine loadDistortions(idfFile, fieldDx, fieldDy, lmAllGrid, idfNx,
     &    idfNy, idfNz, idfBinning, pixelIdf, ixGridStrt, xGridIntrv, nxGrid,
     &    iyGridStrt, yGridIntrv, nyGrid, lmSec)
      implicit none
      character*(*) idfFile
      integer*4  idfBinning, idfNx, idfNy, lmAllGrid, idfNz, lmSec
      integer*4 ixGridStrt(*), iyGridStrt(*), nxGrid(*), nyGrid(*)
      real*4 xGridIntrv(*), yGridIntrv(*), pixelIdf
      real*4 fieldDx(*), fieldDy(*)
c       
      integer*4 idfVersion, i, j, indBase, iz
c       
      call dopen(14, idfFile, 'ro', 'f')
      read(14, *) idfVersion
      if (idfVersion .eq. 1) then
        read(14, *)idfNx, idfNy, idfBinning, pixelIdf
        idfNz = 1
      else if (idfVersion .eq. 2) then
        read(14, *)idfNx, idfNy, idfNz, idfBinning, pixelIdf
      else
        print *
        print *,'ERROR: loadDistortions - version',idfVersion,
     &      ' of idf file not recognized'
        call exit(1)
      endif

      if (idfNz .gt. lmSec) then
        print *
        print *,'ERROR: loadDistortions - too many distortion fields ',
     &      'for arrays'
        call exit(1)
      endif
c       
c       Read each distortion field sequentially into field arrays
c       
      indBase = 0
      do iz = 1, idfNz
        read(14, *)ixGridStrt(iz), xGridIntrv(iz), nxGrid(iz), iyGridStrt(iz),
     &      yGridIntrv(iz), nyGrid(iz)
        if (indBase + nxGrid(iz) * nyGrid(iz) .gt. lmAllGrid) then
          print *
          print *,'ERROR: loadDistortions - too many grid points for arrays'
          call exit(1)
        endif
        do j = 1, nyGrid(iz)
          read(14, *)(fieldDx(indBase + i), fieldDy(indBase + i),
     &        i = 1, nxGrid(iz))
          indBase = indBase + nxGrid(iz)
        enddo
      enddo
      close(14)
      return
      end


c       !
c       getSecDistortions will return one distortion field out of the
c       collection already read from a file with [loadDistortions].  It will
c       check that the field fits in the arrays supplied but not that the 
c       field number is within bounds
c       ^ [izSec] is field number, numbered from 1
c       ^ [allDx] and [allDy] are 1-D arrays into which fields were read
c       ^ [nxGrid], [nyGrid] are the arrays containing the number of grid
c       points in X & Y for each field
c       ^ [fieldDx] and [fieldDy] are arrays in which to place the grid, 
c       dimensioned to [lmGrid] x [lmGrid]
c       !
      subroutine getSecDistortions(izSec, allDx, allDy, nxGrid, nyGrid,
     &    fieldDx, fieldDy, lmGrid)
      implicit none
      integer*4  izSec, lmGrid
      integer*4 nxGrid(*), nyGrid(*)
      real*4 fieldDx(lmGrid, lmGrid), fieldDy(lmGrid, lmGrid)
      real*4 allDx(*), allDy(*)
c       
      integer*4 i, j, indBase
c       
      if (nxGrid(izSec) .gt. lmGrid .or. nyGrid(izSec) .gt. lmGrid) then
        print *
        print *,'ERROR: getSecDistortions - too many grid points for arrays'
        call exit(1)
      endif

      indBase = 0
      do i = 1, izSec - 1
        indBase = indBase + nxGrid(i) * nyGrid(i)
      enddo
      do j = 1, nyGrid(izSec)
        do i = 1, nxGrid(izSec)
          fieldDx(i, j) = allDx(indBase + i)
          fieldDy(i, j) = allDy(indBase + i)
        enddo
        indBase = indBase + nxGrid(izSec)
      enddo
      return
      end

c       !
c       readMagGradients will read from a file with mag gradients and tilt
c       angles for each view, as prepared by extractmagrad
c       ^ [magFile] specifies name of file with gradient information
c       ^ [maxVals] is the maximum number of values that are allowed 
c       ^ [pixelSize] is the pixel size in Angstroms
c       ^ [axisRot] is the rotation of the tilt axis from vertical
c       ^ [tilt] is an array to receive tilt angles
c       ^ [dmagPerUm] is an array to receive percent mag per micron
c       ^ [rotPerUm] is an array to receive degrees of rotation per micron
c       ^ [numVals] is returned with the number of values
c       !
      subroutine readMagGradients(magFile, maxVals, pixelSize,
     &    axisRot, tilt, dmagPerUm, rotPerUm, numVals)
      implicit none
      character*(*) magFile
      integer*4 magVersion, numVals, maxVals, i
      real*4 pixelSize, axisRot, tilt(*), dmagPerUm(*), rotPerUm(*)
c       
      call dopen(14, magFile, 'ro', 'f')
      read(14, *) magVersion
      if (magVersion .eq. 1) then
        read(14, *)numVals, pixelSize, axisRot
        if (numVals .gt. maxVals) then
          print *
          print *,'ERROR: readMagGradients - too many values for arrays'
          call exit(1)
        endif
        do i = 1, numVals
          read(14, *) tilt(i), dmagPerUm(i), rotPerUm(i)
        enddo

      else
        print *
        print *,'ERROR: readMagGradients - version',magVersion,
     &      ' of gradient file not recognized'
        call exit(1)
      endif
      close(14)
      return
      end
