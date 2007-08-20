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
c       Revision 3.3  2006/06/28 23:37:23  mast
c       Fixed bug in computing number of pixels to truncate
c
c       Revision 3.2  2006/03/07 20:05:18  mast
c       Converted to PIP and made it take unflipped coordinates with option
c       for flipped.
c
c       Revision 3.1  2006/02/26 20:21:19  mast
c       Made it read sections in chunks, standardize error exit
c
c       
      implicit none
      integer idim, limden
      parameter (idim=10000*500)
      parameter (limden=1000000)
C       
      integer*4 NXYZ(3),MXYZ(3), nx, ny, nz
      real*4 array(idim)
      integer*4 ihist(-limden:limden)
      real*4 DMIN,DMAX,DMEAN,areafac,histScale,rlo,rhi
      integer*4 mode,iylo,iyhi,ixlo,ixhi,izlo,izhi,ntrunclo,ntrunchi,nxt,nyt
      integer*4 maxlines,nchunk,ivmin,ivmax,iz,ichunk,iyend,iychunk,ival
      integer*4 ntrunc,ilo,ihi,iclo,ichi,i,nylines, ierr, izlim, iylim
      logical*4 flipped
      EQUIVALENCE (NX,NXYZ)
      COMMON //NX,NY,NZ
      character*160 filbig

      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetTwoIntegers,PipGetLogical
      integer*4 PipGetInOutFile
c	  
c       fallbacks from ../../manpages/autodoc2man -2 2  findcontrast
c
      integer numOptions
      parameter (numOptions = 7)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@slices:SlicesMinAndMax:IP:@'//
     &    'xminmax:XMinAndMax:IP:@yminmax:YMinAndMax:IP:@'//
     &    'flipyz:FlipYandZ:B:@truncate:TruncateBlackAndWhite:IP:@'//
     &    'help:usage:B:'
c	  
c       Pip startup: set error, parse options, check help, set flag if used
c
      call PipReadOrParseOptions(options, numOptions, 'findcontrast',
     &    'ERROR: FINDCONTRAST - ', .true., 1, 1, 0, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0
      
      if (PipGetInOutFile('InputFile', 1, 'Name of image file', filbig)
     &    .ne. 0) call exitError('NO INPUT FILE SPECIFIED')
C       
C	Open image file
C       
      CALL IMOPEN(1,filbig,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C       
      histScale = 1.
      if (mode.ne.1.and.(dmin.lt.-limden.or.dmax.gt.limden)) then
        if (dmin.lt.-limden) histScale = -limden/dmin
        if (dmax.gt.limden) histScale = min(histScale, limden/dmax)
      endif

      if (nx .gt. idim) call exitError('IMAGES TOO LARGE IN X FOR ARRAYS')

      flipped = .not.pipinput
      if (pipinput) ierr = PipGetLogical('FlipYandZ', flipped)
c       
c       Set up default limits
c
      ixlo=nx/10
      ixhi=nx-1-ixlo
c      
      if (flipped) then
        izlim = ny
        iylim = nz
      else
        izlim = nz
        iylim = ny
      endif
      izlo = 1
      izhi = izlim
      iylo=iylim/10
      iyhi=iylim-1-iylo
c       
      if (pipinput) then
        ierr = PipGetTwoIntegers('SlicesMinAndMax', izlo, izhi)
        ierr = PipGetTwoIntegers('XMinAndMax', ixlo, ixhi)
        ierr = PipGetTwoIntegers('YMinAndMax', iylo, iyhi)
      else
        write(*,'(1x,a,/,a,$)')'First and last slice (Imod section #'//
     &      ' in flipped volume)','  to include in analysis: '
        read(5,*)izlo,izhi
        write(*,'(1x,a,/,a,4i7,a,$)')'Lower & upper X, lower & upper'
     &      //' Y (in flipped volume) to include in analysis',
     &      '  (/ for ',ixlo,ixhi,iylo,iyhi,'): '
        read(5,*)ixlo,ixhi,iylo,iyhi
      endif
      if (izlo.le.0.or.izhi.gt.izlim.or.izlo.gt.izhi)call exitError(
     &      'SLICE NUMBERS OUTSIDE RANGE OF IMAGE FILE')
      izlo=izlo-1
      izhi=izhi-1
c       
      if(ixlo.lt.0.or.ixhi.ge.nx.or.ixlo.ge.ixhi.or.
     &    iylo.lt.0.or.iyhi.ge.iylim.or.iylo.gt.iyhi) call exitError(
     &    'X OR Y VALUES OUTSIDE RANGE OF VOLUME')
c       
      areafac=max(1.,nx*iylim*1.e-6)
      ntrunclo=areafac*(izhi+1-izlo)
      ntrunchi=ntrunclo
      if (pipinput) then
        ierr = PipGetTwoIntegers('TruncateBlackAndWhite', ntrunclo,ntrunchi)
      else
        write(*,'(1x,a,/,a,2i8,a,$)')'Maximum numbers of pixels to '//
     &      'truncate at black and white in analyzed volume',
     &      '  (/ for ',ntrunclo,ntrunchi,'): '
        read(5,*)ntrunclo,ntrunchi
      endif
c       
c       Flip coordinates
c
      if (flipped) then
        ierr = izlo
        izlo = iylo
        iylo = ierr
        ierr = izhi
        izhi = iyhi
        iyhi = ierr
      endif
      print *,'Analyzing X:',ixlo,ixhi,'  Y:',iylo, iyhi,'  Z:',izlo,izhi
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
            ival=nint(histScale*array(i))
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
      rlo = ilo / histScale
      rhi = ihi / histScale
      iclo=255*(rlo-dmin)/(dmax-dmin)
      ichi=255*(rhi-dmin)/(dmax-dmin)+0.99
      write(*,101)ivmin/histScale,ivmax/histScale,rlo,rhi,iclo,ichi
101   format('Min and max densities in the analyzed volume are',
     &    g13.5,' and',g13.5,/,
     &    'Min and max densities with truncation are',
     &    g13.5,' and',g13.5,/,'Implied black and white contrast levels are'
     &    ,i4,' and',i4)
      call exit(0)
      END

