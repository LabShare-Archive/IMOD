*************MTFFILTER.F********************************************
c       
c       Mtffilter can restore contrast in CCD camera images by multiplying
c       them by the inverse of the camera's modulation transfer function (
c       MTF).  It can also apply a low pass filter to reduce high frequency
c       noise.  One, the other, or both of these filters may be applied.
c       
c       For more details see the man page
c       
c       David Mastronarde, 3/30/04
c       
c       $Id$
c       Log at end of file
c       
      implicit none
      integer idim,limmtf,limstockcurves,limstockpoints
      parameter (idim=8300*8300,limmtf=8193)
      parameter (limstockcurves=1,limstockpoints=36)
      COMMON //NX,NY,NZ
C       
      integer*4 nx,ny,nz,NXYZ(3),MXYZ(3),title(20),nxyzst(3)
      real*4 ctfa(8193),xmtf(limmtf),ymtf(limmtf),cell(6),pixel(3),ctfb(8193)
      complex array(idim/2)
      common /bigarr/ array
      integer*4 nptstock(limstockcurves)
      real*4 stock(2,limstockpoints)
      data nptstock/36/
      data stock/ 0.0085,  0.98585,
     &    0.0221, 0.94238,
     &    0.0357, 0.89398,
     &    0.0493, 0.83569,
     &    0.0629, 0.76320,
     &    0.0765, 0.69735,
     &    0.0901, 0.63647,
     &    0.1037, 0.56575,
     &    0.1173, 0.49876,
     &    0.1310, 0.43843,
     &    0.1446, 0.38424,
     &    0.1582, 0.34210,
     &    0.1718, 0.30289,
     &    0.1854, 0.26933,
     &    0.1990, 0.23836,
     &    0.2126, 0.21318,
     &    0.2262, 0.18644,
     &    0.2398, 0.15756,
     &    0.2534, 0.14863,
     &    0.2670, 0.12485,
     &    0.2806, 0.11436,
     &    0.2942, 0.09183,
     &    0.3078, 0.08277,
     &    0.3214, 0.07021,
     &    0.3350, 0.05714,
     &    0.3486, 0.04388,
     &    0.3622, 0.03955,
     &    0.3759, 0.03367,
     &    0.3895, 0.02844,
     &    0.4031, 0.02107,
     &    0.4167, 0.02031,
     &    0.4303, 0.01796,
     &    0.4439, 0.00999,
     &    0.4575, 0.01103,
     &    0.4711, 0.00910,
     &    0.4898, 0.00741/

C       
      EQUIVALENCE (NX,NXYZ)
      DATA NXYZST/0,0,0/
c       
      character*120 filbig,filout
      character*9 dat
      character*8 tim
      character*70 titstr
      character*80 titlech
      integer*4 mode,npad,nxpad,nypad,imfilout,izst,iznd,nzdo,kti
      integer*4 izoutbase,nsize,istock,nmtf,i,im,kk,ixst,iyst,modout,nsize2
      real*4 dminin,dmaxin,dmean,dmin,dmax,asize,xfac,scalefac,delta
      real*4 ctfinvmax,radius1,sigma1,radius2,sigma2,s,dmsum,dmeanin
      real*4 DMIN2,DMAX2,DMEAN2,atten,beta1,deltap,delx, dely, delz,xa,ya,za
      real*4 zasq,yasq, sigma1b, radius1b
      integer*4 ind,j,ierr, indf,ix, iy
      logical fftInput
      integer*4 niceframe
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetBoolean,PipGetTwoIntegers
      integer*4 PipGetString,PipGetFloat,PipGetInOutFile,PipGetTwoFloats
      integer*4 PipGetNonOptionArg
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  mtffilter
c       
      integer numOptions
      parameter (numOptions = 14)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@output:OutputFile:FN:@'//
     &    'zrange:StartingAndEndingZ:IP:@lowpass:LowPassRadiusSigma:FP:@'//
     &    'highpass:HighPassSigma:F:@radius1:FilterRadius1:F:@'//
     &    'mtf:MtfFile:FN:@stock:StockCurve:I:@maxinv:MaximumInverse:F:@'//
     &    'invrolloff:InverseRolloffRadiusSigma:FP:@xscale:XScaleFactor:F:@'//
     &    'denscale:DensityScaleFactor:F:@param:ParameterFile:PF:@'//
     &    'help:usage:B:'

      filout = ' '
      filbig = ' '
      radius1 = 0.12
      sigma1 = 0.05
      radius2 = 1.
      sigma2 = 0.
      sigma1b = 0.
      radius1b = 0.
      scalefac = 1.
      xfac = 1.
      ctfinvmax = 4.
c       
c       Pip startup: set error, parse options, do help output
c       
      call PipReadOrParseOptions(options, numOptions, 'mtffilter',
     &    'ERROR: MTFFILTER - ', .false., 2, 1, 1, numOptArg,
     &    numNonOptArg)
C       
C       Open image files.
C       
      if (PipGetInOutFile('InputFile', 1, ' ', filbig)
     &    .ne. 0) call exitError('NO INPUT FILE SPECIFIED')
      
      ierr = PipGetInOutFile('OutputFile', 2, ' ', filout)
c       
      if (filout .eq. ' ') then
        CALL IMOPEN(1,filbig,'OLD')
      else
        CALL IMOPEN(1,filbig,'RO')
      endif
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMINin,DMAXin,DMEANin)
      fftInput = mode .eq. 3 .or. mode .eq. 4
c       
C       get padded size and make sure it will fit
c       
      if (fftInput) then
        nxpad = (nx - 1) / 2
        nypad = ny
      else
        npad = 40
        nxpad = niceframe(nx + npad, 2, 19)
        nypad = niceframe(ny + npad, 2, 19)
      endif

      IF (((NXpad+2)*NYpad.GT.idim)) call exitError(
     &    'IMAGES TOO LARGE FOR ARRAYS')
C       
      imfilout = 1
      if (filout.ne.' ')then
        imfilout = 3
        CALL IMOPEN(3,filout,'NEW')
c         
        CALL ITRHDR(3,1)
      endif
c       
      izst=1
      iznd=nz
      ierr = PipGetTwoIntegers('StartingAndEndingZ', izst, iznd)
      if (fftInput .and. ierr .eq. 0) call exitError(
     &    'FFT INPUT FILE IS ASSUMED TO BE 3D FFT, NO Z RANGE ALLOWED')
      if (izst.gt.iznd .or. izst.lt.1 .or. iznd.gt.nz) call
     &    exitError('ILLEGAL STARTING OR ENDING Z VALUES TO FILTER')
      nzdo=iznd+1-izst
      izoutbase = 1
c       
c       take care of header if writing a new file
c       
      if (imfilout.eq.3) then
        izoutbase = izst
        call irtcel(1,cell)
        call irtdel(1,pixel)
c         
c         change mxyz if it matches existing nz; set cell size to keep
c         pixel spacing the same
c         
        if (mxyz(3) .eq. nz) mxyz(3) = nzdo
        cell(3) = mxyz(3) * pixel(3)
        nz=nzdo
        call ialsiz(3,nxyz,nxyzst)
        call ialsam(3,mxyz)
        call ialcel(3,cell)
      endif
c       
c       set up the ctf scaling as usual: but delta needs to be maximum
c       frequency over ctf array size
c       
      NSIZE = MIN(8192,MAX(1024,2*MAX(NXpad,NYpad)))
      ASIZE = NSIZE
      NSIZE = NSIZE + 1
      DELTA = 0.71 / ASIZE
      if (fftInput) DELTA = 0.87 / ASIZE
c       
c       set default null mtf curve
c       
      istock = 0
      filout = ' '
      nmtf = 3
      do i = 1, 3
        xmtf(i) = (i - 1) * 0.25
        ymtf(i) = 1.
      enddo
      ierr = PipGetString('MtfFile', filout)
      if (PipGetInteger('StockCurve', istock) .eq. 0) then
        if (istock.le.0 .or. istock.gt.limstockcurves) call exitError(
     &      'ILLEGAL NUMBER ENTERED FOR STOCK CURVE')
        if (filout .ne. ' ') call exitError('YOU CANNOT ENTER BOTH'//
     &      'AN MTF FILE AND A STOCK CURVE #')
        
c         
c         if stock curve requested, find index and copy values
c         
        ind = 1
        do i = 1,istock-1
          ind = ind + nptstock(i)
        enddo
        do i = 1, nptstock(istock)
          xmtf(i) = stock(1, ind)
          ymtf(i) = stock(2, ind)
          ind = ind + 1
        enddo
        nmtf = nptstock(istock)
      endif
c       
      if (filout .ne. ' ') then
c         
c         or read from file if one provided
c         
        call dopen(2,filout,'ro','f')
        nmtf=0
10      i=nmtf+1
        read(2,*,end=20)xmtf(i),ymtf(i)
        nmtf=i
        go to 10
20      close(2)
      endif
c       
      ierr = PipGetFloat('XScaleFactor', xfac)
      do i=1,nmtf
        xmtf(i)=xmtf(i)*xfac
c         print *,xmtf(i),ymtf(i)
      enddo
c       
      ctfa(1)=1.
      im=1
      s=0.
      do j=2,nsize
        s=s+delta
        if(im.lt.nmtf-1.and.s.gt.xmtf(min(im+1,nmtf)))im=im+1
        ctfa(j)=max(0.,ymtf(im)+(ymtf(im+1)-ymtf(im))*(s-xmtf(im))
     &      /(xmtf(im+1)-xmtf(im)))
      enddo

c       write (*,'(10f7.4)')(ctfa(j), j=1,nsize)

      ierr = PipGetTwoFloats('InverseRolloffRadiusSigma', radius1, sigma1)
      ierr = PipGetTwoFloats('LowPassRadiusSigma', radius2, sigma2)
      ierr = PipGetFloat('MaximumInverse', ctfinvmax)
      ierr = PipGetFloat('DensityScaleFactor', scalefac)
      ierr = PipGetFloat('FilterRadius1', radius1b)
      ierr = PipGetFloat('HighPassSigma', sigma1b)
      call setctfnoscl(sigma1b,sigma2,radius1b,radius2,ctfb,nxpad,nypad,deltap,
     &    nsize2)

      beta1 = 0.0
      if (sigma1.gt.1.e-6) beta1 = -0.5/sigma1**2

      modout = 0.025 / delta
      s = 0.
      print *
      print *,'The overall filter being applied is:'
      print *,'radius  multiplier'
      do j=1,nsize
        atten=1.
        if (s.gt.radius1) atten = exp(beta1*(s - radius1)**2)
        if (ctfa(j).lt.0.01) then
          ctfa(j) = 1.
        else
          ctfa(j)=1.+atten*(min(ctfinvmax,1./ctfa(j))-1.)
        endif
        if (deltap .ne. 0) then
          indf = s / deltap + 1.
          xa = s / deltap + 1. - indf
          ctfa(j) = ctfa(j) * ((1.-xa) * ctfb(indf) + xa * ctfb(indf+1))
        endif
        if (mod(j,modout).eq.1.and.s.le.0.5)write(*,'(2f8.4)')s,ctfa(j)
        s=s+delta
        ctfa(j)=scalefac*ctfa(j)
      enddo
      titstr='MTFFILTER: Filtered by inverse of MTF'
      if (istock .eq. 0 .and. filout .eq. ' ') titstr =
     &    'MTFFILTER: Frequency filtered'
c       
      dmsum=0.
      dmax=-1.e10
      dmin=1.e10
      delx=0.5/(nx-1.)
      dely=1./ny
      delz=1./nz
      DO KK=izst,iznd
C         
c         print *,'reading section',kk
        call imposn(1, kk - 1, 0)
        call irdsec(1,array,*99)
        if (fftInput) then
c           
c           Filter the 3D fft - assuming the usual ordering
c           
          ind=1
          za=delz*(kk-1.)-0.5
          zasq = za**2
          do iy=1,ny
            ya=dely*(iy-1.)-0.5
            yasq=ya**2
            do ix=1,nx
              xa=delx*(ix-1.)
              S = sqrt(xa**2 + yasq + zasq)
              indf = s / delta + 1.5
              array(ind) = array(ind) * ctfa(indf)
              ind = ind + 1
            enddo
          enddo
          CALL iclcdn(array,nx,ny,1,nx,1,ny,dmin2,dmax2,dmean2)
        else
c           
c           Do ordinary 2D filter with padding/tapering, FFT, filter, repack
c           
          call taperoutpad(array,nx,ny,array,nxpad+2,nxpad,nypad,0,0.)
C           
c           print *,'taking fft'
          call todfft(array,nxpad,nypad,0)
          call filterpart(array,array,nxpad,nypad,ctfa,delta)
c           
c           print *,'taking back fft'
          call todfft(array,nxpad,nypad,1)
          
c           print *,'repack, set density, write'
          ixst = (nxpad - nx) / 2
          iyst = (nypad - ny) / 2
          call irepak(array,array,nxpad+2,nypad, ixst,ixst + nx-1,iyst,
     &        iyst+ny-1)
          CALL IclDeN(array,NX,NY,1,NX,1,NY,DMIN2,DMAX2,DMEAN2)
        endif
C         
C         Write out lattice.
C         
        call imposn(imfilout, kk - izoutbase, 0)
        CALL IWRSEC(imfilout,array)
C         
        DMAX = max(dmax,dmax2)
        DMIN = min(dmin,dmin2)
        DMsum = dmsum + dmean2
      enddo
c       
      dmean=dmsum/nzdo
      CALL DATE(DAT)
      CALL TIME(TIM)
c       
c       
      write(titlech,1500) titstr,DAT,TIM
      read(titlech,'(20a4)')(title(kti),kti=1,20)
c       ENCODE(80,1500,TITLE) titstr,DAT,TIM
1500  FORMAT(A,t57,A9,2X,A8)

      if (imfilout.eq.1 .and. nzdo.lt.nz) then
c         
c         if writing a subset back to same file, use the extreme of existing
c         and new data, and just use old mean
c         
        dmin=min(dmin,dminin)
        dmax=max(dmax,dmaxin)
        CALL IWRHDR(1,TITLE,1,DMIN,DMAX,DMEANin)
      else
        CALL IWRHDR(imfilout,TITLE,1,DMIN,DMAX,DMEAN)
      endif
      CALL IMCLOSE(1)
      if(imfilout.eq.3)call imclose(imfilout)
C       
      WRITE(6,500)
500   FORMAT(' PROGRAM EXECUTED TO END.')
      call exit(0)
99    call exitError('READING IMAGE FILE')
      END

c       
c       $Log$
c       Revision 1.3  2005/08/15 16:09:53  mast
c       Increased array size to 8K x 8K
c       
c       Revision 1.2  2004/06/19 21:12:54  mast
c       Added ability to filter a 3D FFT, with no new options
c       
c       Revision 1.1  2004/03/30 18:55:42  mast
c       Addition to package
c       
