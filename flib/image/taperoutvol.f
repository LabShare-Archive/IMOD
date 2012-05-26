*       * * * * * * * TAPEROUTVOL * * * * * * * *
c       
c       TAPEROUTVOL will cut a subset out of an image volume, pad it into a
c       larger volume, and taper the intensity down to the mean value of the
c       volume over the extent of the padding region, i.e., from the edge of
c       the actual excised pixels to the edge of the new volume.  None of
c       the original excised pixels are attenuated by this method.
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
      COMMON //NX,NY,NZ
C       
      integer*4 NXYZ(3),MXYZ(3),NXYZ2(3),MXYZ2(3)
      real*4 TITLE(20), CELL2(6),tilt(3),origtilt(3)
      real*4, allocatable :: ARRAY(:)
C       
      CHARACTER*320 FILIN,FILOUT
      character*9 dat
      character*8 tim
      character*80 titlech
C       
      EQUIVALENCE (NX,NXYZ)
C       
      integer*4 ixlo,iylo,izlo,ixhi,izhi,iyhi,nxbox,nybox,nzbox
      integer*4 nx3,ny3,nz3,izst,iznd,mode,kti
      integer*4 iz,izread,i,ierr
      real*4 dmin2,dmax2,dmean2,dmin,dmax,dmean,tmin,tmax,tmean
      real*4 atten,base,tmpmn,origx,origy,origz
      logical*4 pipinput, noFFT
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  taperoutvol
c       
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetTwoIntegers,PipGetThreeIntegers
      integer*4 PipGetInOutFile, PipGetLogical
      integer numOptions
      parameter (numOptions = 9)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@output:OutputFile:FN:@xminmax:XMinAndMax:IP:@'//
     &    'yminmax:YMinAndMax:IP:@zminmax:ZMinAndMax:IP:@'//
     &    'taper:TaperPadsInXYZ:IT:@nofft:NoFFTSizes:B:@'//
     &    'param:ParameterFile:PF:@help:usage:B:'
c       
      noFFT = .false.
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'taperoutvol',
     &    'ERROR: TAPEROUTVOL - ', .true., 2, 1, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0

      if (PipGetInOutFile('InputFile', 1, 'Name of image input file', filin)
     &    .ne. 0) call exiterror('NO INPUT FILE SPECIFIED')
c       
      CALL IMOPEN(1,FILIN,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
C       
      if (PipGetInOutFile('OutputFile', 2, 'Name of output file', filout)
     &    .ne. 0) call exiterror('NO OUTPUT FILE SPECIFIED')
c       
      if (pipinput) ierr = PipGetLogical('NoFFTSizes', noFFT)
      call taperprep(pipinput, noFFT, nxyz, ixlo, ixhi, iylo, iyhi, izlo,
     &    izhi, nxbox, nybox, nzbox, nx3, ny3, nz3, nxyz2, mxyz2, cell2,
     &    origx, origy, origz)
c
      allocate(array(nx3*ny3), stat = ierr)
      if (ierr .ne. 0) call exitError('ALLOCATING ARRAY FOR IMAGE PLANE')
c       
      CALL IMOPEN(2,FILOUT,'NEW')
      call time(tim)
      call b3ddate(dat)
      write(titlech,301) dat,tim
      read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
301   FORMAT('TAPEROUTVOL: Taper outside of excised volume',t57,a9,2x,a8)
      CALL ICRHDR(2,NXYZ2,MXYZ2,mode,TITLE,0)
      call itrlab(2,1)
      CALL IALCEL(2,CELL2)
      call ialorg(2, origx, origy, origz)
      call irttlt(1, tilt)
      call irttlt_orig(1, origtilt)
      call ialtlt(2, tilt)
      call ialtlt_orig(2, origtilt)
      dmin=1.e30
      dmax=-1.e30
      tmean=0.
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
c       Revision 3.6  2010/06/26 18:15:53  mast
c       Fixed initialization of mean sum
c
c       Revision 3.5  2010/01/08 19:06:00  mast
c       Converted to PIP, added nofft option
c
c       Revision 3.4  2008/12/19 15:02:21  mast
c       Oops need to pad single slice if padding requested
c
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
