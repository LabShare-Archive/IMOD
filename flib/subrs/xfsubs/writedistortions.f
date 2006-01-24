c       This file contains modules for writing distortion field files
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c	
c       !
c       idfStartFile will open a new file for writing distortion fields and
c       write the information supplied at the start of the file.
c       ^ [outfile] specifies the name of the output file
c       ^ [iunit] specifies a free fortran unit number
c       ^ [numFields] is the number of fields that will be written
c       ^ [nx] and [ny] are the image size from which the field was measured
c       ^ [iBinning] is the binning of those images
c       ^ [pixelSize] is their pixel size in Angstroms
c       !
      subroutine idfStartFile(outfile, iunit, nx, ny, numFields, iBinning,
     &    pixelSize)
      implicit none
      character*(*) outfile
      integer*4 iunit, nx, ny, numFields, iBinning, iVersion
      real*4 pixelSize
      iVersion = 2
      call dopen(iunit, outfile, 'new', 'f')
      write(iunit,'(i5)')iVersion
      write(iunit,'(2i7,2i3,f10.4)')nx, ny, numFields, iBinning, pixelSize
      return
      end

c       !
c       idfWriteField writes one distortion field to a file
c       ^ [iunit] specifies the fortran unit number of the file
c       ^ [ixFieldStrt], [iyFieldStrt] are coordinates at which grid starts
c       ^ [xFieldIntrv], [yFieldIntrv] are the spacing between grid points in
c       X & Y
c       ^ [nxField], [nyField] are the number of grid points in X & Y
c       ^ [fieldDx] and [fieldDy] are arrays containing the field, 
c       dimensioned to [idim] x [idim]
c       !
      subroutine idfWriteField(iunit, ixFieldStrt, xFieldIntrv, nxField,
     &    iyFieldStrt, yFieldIntrv, nyField, fieldDx, fieldDy, idim)
      implicit none
      integer*4 iunit, ixFieldStrt, xFieldIntrv, nxField,iyFieldStrt
      integer*4 yFieldIntrv, nyField,idim, ix, iy
      real*4 fieldDx(idim, idim), fieldDy(idim, idim)
      write(iunit,'(2(i6,f8.2,i6))')ixFieldStrt, xFieldIntrv, nxField,
     &    iyFieldStrt, yFieldIntrv, nyField
      do iy = 1, nyField
        write(iunit,'(8f8.2)')(fieldDx(ix, iy), fieldDy(ix, iy),
     &      ix = 1, nxField)
      enddo
      return
      end
