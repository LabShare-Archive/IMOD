*       * * * * * * XCORRSTACK * * * * * *
*       
*       XCORRSTACK cross-correlates each of the sections in one image stack
c       with the first section in a second image file.  A subset of sections
c       may be done.  For details see man page.
c
c       $Id$
c       Log at end of file
*       
c       David Mastronarde 4/26/89
c       
      implicit none
      integer idim
      parameter (idim=6000*6000)
C       
      integer*4 NXYZ(3),MXYZ(3),nxyzs(3),mxyzs(3),NX,NY,NZ,nxs,nys,nzs
      real*4 ctfb(8193)
      complex array(idim/2),brray(idim/2)
C       
      COMMON //NX,NY,NZ,nxs,nys,nzs
      EQUIVALENCE (NX,NXYZ),(nxs,nxyzs)
      common /bigarr/array, brray
c       
      character*160 filbig,filsmall,filout
      character*9 dat
      character*8 tim
      character*80 titlech
      integer*4 mode,nxpad,nypad,nxdim,izst,iznd,ierr,iffill,modeout,ifSplit
      real*4 sigma1, sigma2, radius1, radius2, DMIN,DMAX,DMEAN,fill,deltab
      real*4 delta(3),origx, origy, origz, cell(6), dmsum, DMAX2,DMEAN2,DMIN2
      integer*4 ixlo, iylo, kk, jx, MODESM, nxyzst(3)
      real*4 DMINS,DMAXS,DMEANS
      data nxyzst/0,0,0/
      integer*4 niceframe

      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetString,PipGetInteger,PipGetFloat,PipGetLogical
      integer*4 PipGetInOutFile,PipGetBoolean, PipGetTwoIntegers
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  xcorrstack
c       
      integer numOptions
      parameter (numOptions = 12)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'stack:StackInputFile:FN:@single:SingleInputFile:FN:@'//
     &    'output:OutputFile:FN:@sections:StartingEndingSections:IP:@'//
     &    'radius1:FilterRadius1:F:@radius2:FilterRadius2:F:@'//
     &    'sigma1:FilterSigma1:F:@sigma2:FilterSigma2:F:@'//
     &    'split:SplitIntoCorners:B:@fill:FillValue:F:@'//
     &    'param:ParameterFile:PF:@help:usage:B:'

      sigma1 = 0.;
      sigma2 = 0.;
      radius1 = 0.
      radius2 = 0.
      ifSplit = 0
c
c       PIP startup
      call PipReadOrParseOptions(options, numOptions, 'xcorrstack',
     &    'ERROR: XCORRSTACK - ', .false., 3, 2, 1, numOptArg, numNonOptArg)
c
c       Get input/output files
      if (PipGetInOutFile('StackInputFile', 1, ' ', filbig)
     &    .ne. 0) call exitError('NO INPUT IMAGE STACK SPECIFIED')
      if (PipGetInOutFile('SingleInputFile', 2, ' ', filsmall)
     &    .ne. 0) call exitError('NO SINGLE IMAGE FILE SPECIFIED')
      if (PipGetInOutFile('OutputFile', 3, ' ', filout)
     &    .ne. 0) call exitError('NO OUTPUT FILE SPECIFIED')
C       
C       Open image files.
C       
c       
      CALL IMOPEN(1,filbig,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
      nxpad = niceframe(nx, 2, 19)
      nypad = niceframe(ny, 2, 19)
      nxdim = nxpad + 2
C       
      IF (nxdim*NYpad.GT.idim)
     &    call exiterror('IMAGES TOO LARGE FOR ARRAYS')
C       
      CALL IMOPEN(2,filsmall,'RO')
      CALL IRDHDR(2,NXYZS,MXYZS,MODESM,DMINS,DMAXS,DMEANS)
C       
      IF (NXs.gt.nx.or.nYs.gt.ny) call exitError(
     &    'SINGLE IMAGE FILE MUST NOT HAVE X OR Y SIZE BIGGER THAN STACK')
c       
      CALL IMOPEN(3,filout,'NEW')
c       
      CALL ITRHDR(3,1)
c       
      izst=0
      iznd=nz-1
      ierr = PipGetTwoIntegers('StartingEndingSections', izst, iznd)
      if(izst.lt.0.or.iznd.gt.nz-1.or.izst.gt.iznd)call exitError(
     &    'STARTING AND ENDING SECTIONS ARE OUT OF RANGE OR OUT OF ORDER')
      ierr = PipGetFloat('FilterRadius1', radius1)
      ierr = PipGetFloat('FilterRadius2', radius2)
      ierr = PipGetFloat('FilterSigma1', sigma1)
      ierr = PipGetFloat('FilterSigma2', sigma2)
      iffill = 1 - PipGetFloat('FillValue', fill)
      ierr = PipGetBoolean('SplitIntoCorners', ifSplit)
      modeout = mode
      ierr = PipGetInteger('ModeOfOutput', modeout)
      if (modeout .lt. 0 .or. (modeout / 3 .ne. 0 .and. modeout .ne. 6))
     &    call exitError('IMPROPER MODE VALUE')
      call ialmod(3, modeout)
      call PipDone()
        
      call setctfwsr(sigma1,sigma2,radius1,radius2,ctfb,nxpad,nypad,deltab)
c       
c       if single image is same size or if not splitting, use taperoutpad
c       otherwise, split it up in brray
c       
      call irdsec(2,array,*99)
      if((nxs.eq.nx.and.nys.eq.ny) .and. ifSplit .eq. 0)then
        call taperoutpad(array, nxs, nys, brray, nxdim, nxpad, nypad,
     &      iffill, fill)
      else
        call splitfill(array,nxs,nys,brray,nxdim,nxpad,nypad, iffill, fill)
      endif
C       
c       get fft of single image, apply filter if desired
c       
      call todfft(brray,nxpad,nypad,0)
      if(deltab.ne.0.)call filterpart(brray,brray,nxpad,nypad,ctfb,deltab)
c       
c       manage header pixel size and origin: REUSING NXYZS
      nxs = nx
      nys = ny
      nzs=iznd+1-izst
      call irtdel(1, delta)
      call irtorg(1, origx, origy, origz)
      call irtcel(1, cell)
      origz = origz - delta(3) * izst
      cell(3) = nzs * cell(3) / mxyz(3)
      call ialsiz(3, nxyzs, nxyzst)
      call ialsam(3, nxyzs)
      call ialcel(3, cell)
      call ialorg(3, origx, origy, origz)
c       
c       Loop over the images
      dmsum=0.
      dmax=-1.e10
      dmin=1.e10
      DO KK=izst,iznd
C         
        print *,'Working on section',kk
        call imposn(1,kk,0)
        call irdsec(1,array, *99)
        call taperoutpad(array, nx, ny, array, nxdim, nxpad, nypad,
     &      iffill, fill)
C         
c         print *,'taking fft'
        call todfft(array,nxpad,nypad,0)
c         
c         multiply array by complex conjugate of brray, put back in array
c         
c         print *,'multiplying arrays'
        do jx=1,nypad*nxdim/2
          array(jx)=array(jx)*conjg(brray(jx))
        enddo
c         
c         print *,'taking back fft'
        call todfft(array,nxpad,nypad,1)
        
c         print *,'repack, set density, write'
c         TODO: check that extra is on right
        ixlo = (nxpad - nx) / 2
        iylo = (nypad - ny) / 2
        call irepak(array,array,nxdim,nypad,ixlo,ixlo+nx-1,iylo,iylo+ny-1)
        if (modeout .eq. 2) then
          CALL iclden(array,NX,NY,1,NX,1,NY,DMIN2,DMAX2,DMEAN2)
        else
          CALL isetdn(array,NX,NY,MODEout,1,NX,1,NY,DMIN2,DMAX2,DMEAN2)
        endif
C         
C         Write out lattice.
C         
        CALL IWRSEC(3,array)
C         
        DMAX = max(dmax,dmax2)
        DMIN = min(dmin,dmin2)
        DMsum = dmsum + dmean2
      enddo
c       
      dmean=dmsum/nzs
c       
      CALL DATE(DAT)
      CALL TIME(TIM)
c       
      write(titlech,1500)dat,tim
1500  FORMAT('XCORR: Stack correlated with single image',t57,A9,2X,A8)
      CALL IWRHDRc(3,TITLEch,1,DMIN,DMAX,DMEAN)
      call imclose(3)
      CALL IMCLOSE(2)
      CALL IMCLOSE(1)
C       
      call exit(0)
99    call exitError('READING IMAGE FILE')
      END

c       
c       $Log$
c       Revision 3.1  2007/10/29 22:09:51  mast
c       Pip conversion, rename, adjust origin, rationalize filter
c
c
