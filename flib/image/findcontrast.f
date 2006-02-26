*       * * * * * * FINDCONTRAST * * * * * *
c       
c       FINDCONTRAST finds the black and white contrast settings that are
c       used when converting an image file to bytes with newstack or mrcbyte.
c       It computes a histogram of pixel values within a selected volume,
c       which it uses to determine the contrast settings that would truncate
c       the values of a specified, small number of pixels in the volume.
c       
c       See man page for details
c
c       David Mastronarde, 1/1/00
c
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       
      implicit none
      integer idim, limden
      parameter (idim=10000*500)
      parameter (limden=100000)
C       
      integer*4 NXYZ(3),MXYZ(3), nx, ny, nz
      real*4 array(idim)
      integer*4 ihist(-limden:limden)
      real*4 DMIN,DMAX,DMEAN,areafac
      integer*4 mode,iylo,iyhi,ixlo,ixhi,izlo,izhi,ntrunclo,ntrunchi,nxt,nyt
      integer*4 maxlines,nchunk,ivmin,ivmax,iz,ichunk,iyend,iychunk,ival
      integer*4 ntrunc,ilo,ihi,iclo,ichi,i,nylines
      EQUIVALENCE (NX,NXYZ)
      COMMON //NX,NY,NZ
      character*160 filbig

C       
C	Open image files.
C       
      write(*,'(1x,a,$)')'Name of image file: '
      read(5,50)filbig
50    format(A)
c       
      CALL IMOPEN(1,filbig,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C       
      if (mode.ne.1.and.(dmin.lt.-limden.or.dmax.gt.limden)) call errorexit(
     &    '- DATA VALUES HAVE TOO LARGE A RANGE FOR ARRAYS')

      if (nx .gt. idim) call errorexit('IMAGES TOO LARGE IN X FOR ARRAYS')
c       
10    write(*,'(1x,a,/,a,$)')'First and last slice (Imod section #'//
     &    ' in flipped volume)','  to include in analysis: '
      read(5,*)iylo,iyhi
      if (iylo.le.0.or.iyhi.gt.ny.or.iylo.gt.iyhi)then
        print *,'Illegal values, try again'
        go to 10
      endif
      iylo=iylo-1
      iyhi=iyhi-1
c       
20    ixlo=nx/10
      ixhi=nx-1-ixlo
      izlo=nz/10
      izhi=nz-1-izlo
      write(*,'(1x,a,/,a,4i7,a,$)')'Lower & upper X, lower & upper'
     &    //' Y (in flipped volume) to include in analysis',
     &    '  (/ for ',ixlo,ixhi,izlo,izhi,'): '
      read(5,*)ixlo,ixhi,izlo,izhi
      if(ixlo.lt.0.or.ixhi.ge.nx.or.ixlo.ge.ixhi.or.
     &    izlo.lt.0.or.izhi.ge.nz.or.izlo.gt.izhi)then
        print *,'Illegal values, try again'
        go to 20
      endif
c       
      areafac=max(1.,nx*ny*1.e-6)
      ntrunclo=areafac*(iyhi+1-iylo)
      ntrunchi=ntrunclo
      write(*,'(1x,a,/,a,2i8,a,$)')'Maximum numbers of pixels to '//
     &    'truncate at black and white in analyzed volume',
     &    '  (/ for ',ntrunclo,ntrunchi,'): '
      read(5,*)ntrunclo,ntrunchi
c       
      do i=-limden,limden
        ihist(i)=0
      enddo
      nxt=ixhi+1-ixlo
      nyt=iyhi+1-iylo
      maxlines = idim / nxt
      nchunk = (nyt + maxlines - 1) / maxlines

      ivmin=limden
      ivmax=-limden
      do iz=izlo,izhi
        iychunk = iylo
        do ichunk = 1, nchunk
          iyend = min(iyhi, iychunk + maxlines - 1)
          nylines = iyend + 1 - iychunk
          call imposn(1,iz,0)
          call irdpas(1,array,nxt,nylines,ixlo,ixhi,iychunk,iyend)
          do i=1,nxt*nylines
            ival=nint(array(i))
            ihist(ival)=ihist(ival)+1
            ivmin=min(ivmin,ival)
            ivmax=max(ivmax,ival)
          enddo
          iychunk = iyend + 1
        enddo
      enddo
c       
      ntrunc=0
      ilo=ivmin
      do while(ntrunc.le.ntrunclo.and.ilo.lt.ivmax)
        ntrunc=ntrunc+ihist(ilo)
        ilo=ilo+1
      enddo
c       
      ntrunc=0
      ihi=ivmax
      do while(ntrunc.le.ntrunchi.and.ihi.gt.ivmin)
        ntrunc=ntrunc+ihist(ihi)
        ihi=ihi-1
      enddo
c	write(*,'(i7,9i8)')(ihist(i),i=ivmin,ivmax)
      iclo=255*(ilo-dmin)/(dmax-dmin)
      ichi=255*(ihi-dmin)/(dmax-dmin)+0.99
      write(*,101)ivmin,ivmax,ilo,ihi,iclo,ichi
101   format('Min and max density levels in the analyzed volume are',
     &    i7,' and',i7,/,
     &    'Min and max density levels with truncation are',
     &    i7,' and',i7,/,'Implied black and white contrast levels are'
     &    ,i4,' and',i4)
      call exit(0)
      END

      subroutine errorexit(message)
      character*(*) message
      print *
      print *,'ERROR: FINDCONTRAST - ',message
      call exit(1)
      end
