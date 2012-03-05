c       IALSIZ_SAM_CEL modifies the header size, sampling and cell size
c       for the MRC file open on unit IUNIT, given dimensions NX, NY, NZ.

      subroutine ialsiz_sam_cel(iunit,nx,ny,nz)
      integer*4 nxyz(3),nxyzst(3)
      real*4 cell(6)
      data cell/0.,0.,0.,90.,90.,90./
      data nxyzst/0,0,0/
c       
      nxyz(1)=nx
      nxyz(2)=ny
      nxyz(3)=nz
      cell(1)=nx
      cell(2)=ny
      cell(3)=nz
      call ialsiz(iunit,nxyz,nxyzst)
      call ialsam(iunit,nxyz)
      call ialcel(iunit,cell)
      return
      end
