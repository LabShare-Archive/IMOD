*       * * * * * * * TAPERVOLEDGE * * * * * * * *
c       
c       TAPERVOLEDGE will cut a subset out of an image volume, taper the
c       intensity down to the mean value at the edge of the volume over
c       a specified range of pixels, and embed this subset into a larger
c       volume with specified borders.  The resulting volume will have
c       dimensions suitable for taking the FFT.  As a special case, a single
c       X/Y plane may be excised and tapered at its edges.
c       
c       Inputs to the program:
c       
c       Name of input file
c       
c       Name of output file
c       
c       Dimensions of the block to cut out, in X, Y and Z.  The default is
c       the whole volume.
c       
c       The index coordinates (ix, iy, iz) of the center of the block.  The
c       default is the center of the input volume.
c       
c       The width of the borders, in X, Y, and Z, with which to pad the
c       image block.  Borders will be made larger as necessary to make the
c       output image size suitable for taking an FFT.  If the dimension being
c       excised in Z is 1, then padding must be zero in Z.
c       
c       The extent in X, Y, and Z over which to taper the image intensity
c       down to the mean at the edge.  For example, if an extent of 3 is
c       used for the X direction, the last three pixels on a line would be
c       attenuated toward the mean value by 1/4, 1/2, and 3/4.
c       
c       The program will stop with an error message if the subset or the
c       output image is too large, or if the subset is not entirely within
c       the input volume.
c       
c       $Author$
c
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 3.1  2006/05/13 00:04:27  mast
c       Redimensioned, added declarations and error exits
c
C
c       David Mastronarde, 7/22/97; 2/2/01: taper a single slice.
      implicit none
      integer idim
      parameter (idim=400)
      integer*4 NX,NY,NZ
      COMMON //NX,NY,NZ
C       
      integer*4 NXYZ(3),MXYZ(3),NXYZ2(3),MXYZ2(3)
      real*4 ARRAY(idim*idim*idim),TITLE(20), CELL2(6),delta(3)
C       
      integer*4 mode,nxbox,nybox,nzbox,ixcen,iycen,izcen,ixlo,ixhi,iylo
      integer*4 iyhi,izlo,izhi,npadx,npady,npadz,nx3,ny3,nz3,nxtap,nytap
      integer*4 nztap,kti,indbase,iz
      real*4 DMIN2,DMAX2,DMEAN2,dmin,dmax,dmean,tmin,tmean,tmpmn,tmax
      common /bigarr/ array
      CHARACTER*160 FILIN,FILOUT,filpoint,plfile
      character*9 dat
      character*8 tim
      integer*4 niceframe
c       
c       7/7/00 CER: remove the encode's; titlech is the temp space
c       
      character*80 titlech
C       
      EQUIVALENCE (NX,NXYZ)
C       
      call setExitPrefix('ERROR: TAPERVOLEDGE - ')
      write(*,'(1x,a,$)')'Image input file: '
      READ(5,101)FILIN
101   format(a)
      CALL IMOPEN(1,FILIN,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
C       
      write(*,'(1x,a,$)')'Output file: '
      READ(5,101)FILOUT
c       
      nxbox=nx
      nybox=ny
      nzbox=nz
      write(*,'(1x,a,$)')'Size of image block in X, Y, and Z (/ '//
     &    'for whole volume): '
      read(5,*)nxbox,nybox,nzbox
      if(nxbox*nybox*nzbox.gt.idim**3) call exiterror(
     &    'BLOCK TOO LARGE FOR ARRAY')
c       
      ixcen=nx/2
      iycen=ny/2
      izcen=nz/2
      write(*,'(1x,a,$)')'Center index coordinates of block '//
     &    '(/ for center of volume): '
      read(5,*)ixcen,iycen,izcen
c       
      ixlo=ixcen-nxbox/2
      ixhi=ixlo+nxbox-1
      iylo=iycen-nybox/2
      iyhi=iylo+nybox-1
      izlo=izcen-nzbox/2
      izhi=izlo+nzbox-1
      if(ixlo.lt.0.or.ixhi.ge.nx.or.iylo.lt.0.or.iyhi.ge.ny
     &    .or.izlo.lt.0.or.izhi.ge.nz) call exiterror(
     &    'BLOCK NOT ALL INSIDE VOLUME')
c       
      write(*,'(1x,a,$)')'Width of pad borders in X, Y, and Z: '
      read(5,*)npadx,npady,npadz
      nx3=niceframe(2*((nxbox+1)/2+npadx),2,19)
      ny3=niceframe(2*((nybox+1)/2+npady),2,19)
      if (nzbox.eq.1) then
        if (npadz.ne.0)
     &      call exiterror ('NO PADDING IS ALLOWED IN Z FOR SINGLE SECTION')
        nz3 = 1
      else
        nz3=niceframe(2*((nzbox+1)/2+npadz),2,19)
      endif
      if(nx3*ny3*nz3.gt.idim**3) call exiterror(
     &    'PADDED BLOCK TOO LARGE FOR ARRAY')
      write(*,'(1x,a,$)')
     &    'Width to taper image to mean at edge in X, Y, Z: '
      read(5,*)nxtap,nytap,nztap
c       
      nxtap=max(0,min(nxtap,nxbox/2))
      nytap=max(0,min(nytap,nybox/2))
      nztap=max(0,min(nztap,nzbox/2))
      CALL IMOPEN(2,FILOUT,'NEW')
      call irtdel(1,delta)
      NXYZ2(1)=NX3
      NXYZ2(2)=NY3
      NXYZ2(3)=nz3
      MXYZ2(1)=NX3
      MXYZ2(2)=NY3
      MXYZ2(3)=nz3
      CELL2(1)=NX3*delta(1)
      CELL2(2)=NY3*delta(2)
      CELL2(3)=nz3*delta(3)
      CELL2(4)=90.
      CELL2(5)=90.
      CELL2(6)=90.
C       
      call time(tim)
      call date(dat)
c       
c       7/7/00 CER: remove the encodes
c       
c       ENCODE(80,301,TITLE)dat,tim
      write(titlech,301) dat,tim
      read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
301   FORMAT('TAPERVOLEDGE: taper at edge of excised volume',t57,a9,2x,a8)
      CALL ICRHDR(2,NXYZ2,MXYZ2,mode,TITLE,0)
      call itrlab(2,1)
      CALL IALCEL(2,CELL2)
      dmin=1.e10
      dmax=-1.e10
      tmean=0.
c       
      indbase=1
      do iz=izlo,izhi
        call imposn(1,iz,0)
        call irdpas(1,array(indbase),nxbox,nybox,ixlo,ixhi,iylo,iyhi, *99)
        indbase=indbase+nxbox*nybox
      enddo
c       
      if(nz3.gt.1)then
        call taperinvol(array,nxbox,nybox,nzbox,array,nx3,nx3,ny3,
     &      nz3, nxtap,nytap,nztap)
      else
        call taperinpad(array,nxbox,nybox,array,nx3,nx3,ny3,nxtap,
     &      nytap)
      endif
c       
      indbase=1
      do iz=1,nz3
        call iclden(array(indbase),nx3,ny3,1,nx3,1,ny3,tmin,tmax,
     &      tmpmn)
        dmin=min(dmin,tmin)
        dmax=max(dmax,tmax)
        tmean=tmean+tmpmn
        call iwrsec(2,array(indbase))
        indbase=indbase+nx3*ny3
      enddo
      dmean=tmean/nz3
      CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
      CALL IMCLOSE(2)
      call exit(0)
99    call exiterror('READING FILE')
      end
