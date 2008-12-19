*       * * * * * * * TAPEROUTVOL * * * * * * * *
c       
c       TAPEROUTVOL will cut a subset out of an image volume, pad it into a
c       larger volume, and taper the intensity down to the mean value of the
c       volume over the extent of the padding region, i.e., from the edge of
c       the actual excised pixels to the edge of the new volume.  None of
c       the original excised pixels are attenuated by this method.  The
c       resulting volume will have dimensions suitable for taking the FFT.
c       
c       See man page for details.
c       
c       David Mastronarde, 3/1/01
c       
c       $Id$
c       Log at end
c       
      implicit none
      integer idim,NX,NY,NZ
      parameter (idim=8200)
      COMMON //NX,NY,NZ
C       
      integer*4 NXYZ(3),MXYZ(3),NXYZST(3),NXYZ2(3),MXYZ2(3)
      real*4 ARRAY(idim*idim),TITLE(20), CELL2(6),delta(3)
C       
      CHARACTER*160 FILIN,FILOUT,filpoint,plfile
      character*9 dat
      character*8 tim
      character*80 titlech
C       
      EQUIVALENCE (NX,NXYZ)
C       
      integer*4 ixlo,iylo,izlo,ixhi,izhi,iyhi,nxbox,nybox,nzbox
      integer*4 nx3,ny3,nz3,npadx,npady,npadz,izst,iznd,mode,kti
      integer*4 iz,izread,i
      real*4 dmin2,dmax2,dmean2,dmin,dmax,dmean,tmin,tmax,tmean
      real*4 atten,base,tmpmn,origx,origy,origz
      integer*4 niceframe
      common /bigarr/array
      DATA NXYZST/0,0,0/
c       
      call setExitPrefix('ERROR: TAPEROUTVOL - ')
      write(*,'(1x,a,$)')'Image input file: '
      READ(5,101)FILIN
101   format(a)
      CALL IMOPEN(1,FILIN,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
C       
      write(*,'(1x,a,$)')'Output file: '
      READ(5,101)FILOUT
c       
      ixlo=0
      iylo=0
      izlo=0
      ixhi=nx-1
      iyhi=ny-1
      izhi=nz-1
      write(*,'(1x,a,/,a,$)')'Starting and ending X, then Y, then '//
     &    'Z index coordinates to extract',' (/ for whole volume): '
      read(5,*)ixlo,ixhi,iylo,iyhi,izlo,izhi
c       
      if(ixlo.lt.0.or.ixhi.ge.nx.or.iylo.lt.0.or.iyhi.ge.ny
     &    .or.izlo.lt.0.or.izhi.ge.nz)call exitError(
     &    'BLOCK NOT ALL INSIDE VOLUME')
      nxbox=ixhi+1-ixlo
      nybox=iyhi+1-iylo
      nzbox=izhi+1-izlo

      if(nxbox*nybox.gt.idim**2) call exitError('BLOCK TOO LARGE')
c       
      write(*,'(1x,a,$)')'Width of pad/taper borders in X, Y, and Z: '
      read(5,*)npadx,npady,npadz
      nx3=niceframe(2*((nxbox+1)/2+npadx),2,19)
      ny3=niceframe(2*((nybox+1)/2+npady),2,19)
      nz3 = nzbox
      if (nz3 .gt. 1 .or. npadz .gt. 0)
     &    nz3=niceframe(2*((nzbox+1)/2+npadz),2,19)
c       
      if(nx3*ny3.gt.idim**2) call exitError('PADDED BLOCK TOO LARGE')
c       
      CALL IMOPEN(2,FILOUT,'NEW')
      call irtdel(1,delta)
      call irtorg(1, origx, origy, origz)
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
      origx = origx - delta(1) * (ixlo - (nx3 - nxbox) / 2)
      origy = origy - delta(2) * (iylo - (ny3 - nybox) / 2)
      origz = origz - delta(3) * (izlo - (nz3 - nzbox) / 2)
C       
      call time(tim)
      call date(dat)
      write(titlech,301) dat,tim
      read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
301   FORMAT('TAPEROUTVOL: Taper outside of excised volume',t57,a9,2x,a8)
      CALL ICRHDR(2,NXYZ2,MXYZ2,mode,TITLE,0)
      call itrlab(2,1)
      CALL IALCEL(2,CELL2)
      call ialorg(2, origx, origy, origz)
      dmin=1.e30
      dmax=-1.e30
      tmin=0.
c       
      izst=izlo-(nz3-nzbox)/2
      iznd=izst+nz3-1
      do iz=izst,iznd
        izread=max(izlo,min(izhi,iz))
        call imposn(1,izread,0)
        call irdpas(1,array,nxbox,nybox,ixlo,ixhi,iylo,iyhi, *99)
        call taperoutpad(array,nxbox,nybox,array,nx3,nx3,ny3,1,dmean2)
        if(iz.lt.izlo.or.iz.gt.izhi)then
          if(iz.lt.izlo)then
            atten=float(iz-izst)/(izlo-izst)
          else
            atten=float(iznd-iz)/(iznd-izhi)
          endif
          base=(1.-atten)*dmean2
          do i=1,nx3*ny3
            array(i)=base+atten*array(i)
          enddo
        endif
        call iclden(array,nx3,ny3,1,nx3,1,ny3,tmin,tmax, tmpmn)
        dmin=min(dmin,tmin)
        dmax=max(dmax,tmax)
        tmean=tmean+tmpmn
        call iwrsec(2,array)
      enddo
      dmean=tmean/nz3
      CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
      CALL IMCLOSE(2)
      call exit(0)
99    call exitError('READING FILE')
      end
c
c       $Log$
c       Revision 3.3  2008/12/19 15:00:00  mast
c       If one slice requested without padding, do not pad in Z
c
c       Revision 3.2  2007/10/04 00:41:53  mast
c       Made it set origin to preserve coordinate system
c
c       Revision 3.1  2002/07/31 20:06:21  mast
c       Made it preserve pixel size.  Also standardized error output and
c       made declarations for implicit none.
c       
