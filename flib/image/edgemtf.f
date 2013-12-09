*       * * * * * MTFFILTER * * * * *
*       
c       This program computes an MTF curve from an image of an edge
c       
c       See man page for details
c       
c       David Mastronarde  6/2/11
c       
c       $Id$
c       
      implicit none
      integer idim
      parameter (idim=3000)
C       
      integer*4 NXYZ(3),MXYZ(3),nx,ny,nz
      real*4 ring(500),ringp(500)
      integer*4 ninr(500),ireflin(0:idim)
      real*4 array(idim*idim),brray(idim*idim),ref(idim),deriv(idim)
      real*4 ftmag(idim),shifts(idim)
      integer*4 ncollvals(4)/3,2,1,1/
      integer*4 nlinvals(4)/2,2,2,1/
      integer*4 normvals(4)/4,2,2,1/
C       
      EQUIVALENCE (NX,NXYZ(1)),(NY,NXYZ(2)),(NZ,NXYZ(3))
      integer*4 ncollapse, nlinavg, nring, navgnorm, mode,izst, iznd, ierr,i,nxlim
      integer*4 nxfull, nxo2, nx21, nyfull, navgring,lenroot,istr,iring,iend,kk,ix,iy
      integer*4 ibinning
      real*4 cross, DMIN,DMAX,DMEAN, delx, zeroshift, ringsum,avg1,fourlas,avglas,fcrs
      real*4 avg,four,x
      integer*4 niceFrame
c       
      character*320 inFile,mtfFile,outFile,rootname
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetFloat
      integer*4 PipGetInteger, PipGetTwoIntegers, PipGetIntegerArray,PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2 edgemtf
      integer numOptions
      parameter (numOptions = 13)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@rootname:RootNameOfOutput:FN:@'//
     &    'sections:SectionsStartAndEnd:IP:@points:NumberOfPoints:I:@'//
     &    'sum:SummingOfLines:I:@average:AveragingForReference:I:@'//
     &    'lines:LinesForReference:IA:@components:NormalizationComponents:I:@'//
     &    'binning:BinningOfImages:I:@cross:CrossingValue:F:@zero:ZeroDerivative:I:@'//
     &    'param:ParameterFile:PF:@help:usage:B:'

      nring = 20
      cross = 0.5
      ibinning = 1

      call PipReadOrParseOptions(options, numOptions, 'edgemtf',
     &    'ERROR: EDGEMTF - ', .false., 2, 1, 1, numOptArg, numNonOptArg)
C       
C       Open image file
      if (PipGetInOutFile('InputFile', 1, ' ', inFile) .ne. 0)
     &    call exitError('NO INPUT FILE SPECIFIED')
C       
      CALL IMOPEN(1,inFile,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C       
      IF (((NX+2)*NY.GT.idim**2)) call exitError('IMAGE TOO BIG FOR ARRAYS')
C       
      if (PipGetInOutFile('InputFile', 2, ' ', rootname) .ne. 0)
     &    call exitError('NO OUTPUT FILE SPECIFIED')
      write(*,'(1x,a,$)')'Output file for curves: '
      outFile = trim(rootname)//'.out'
      call dopen(2,outFile,'new','f')
c       
      ierr = PipGetInteger('BinningOfImages', ibinning)
      ibinning = max(1, min(4,ibinning))
      ncollapse = ncollvals(ibinning)
      nlinavg = nlinvals(ibinning)
      navgnorm = normvals(ibinning)
c       
      izst=0
      iznd=nz-1
      ierr = PipGetTwoIntegers('SectionsStartAndEnd', izst, iznd)
      ierr = PipGetInteger('SummingOfLines', ncollapse)
      ierr = PipGetInteger('AveragingForReference', nlinavg)
c       
      do i=izst,iznd
        ireflin(i)=ny/2
      enddo
      ierr = PipGetIntegerArray('LinesForReference', ireflin, iznd+1-izst, idim)
      do i=izst,iznd
        ireflin(i)=ireflin(i)/ncollapse
      enddo
c       
      nxlim=nx
      ierr = PipGetInteger('NumberOfPoints', nring)
      ierr = PipGetInteger('NormalizationComponents', navgnorm)
      ierr = PipGetInteger('ZeroDerivative', nxlim)
      ierr = PipGetFloat('CrossingValue', cross)
c       
c       adjust nx downward to allow FFT
      nxfull = nx
      nx = 2 * (nx / 2)
      do while (nx .lt. niceFrame(nx, 2, 19))
        nx = nx - 2
      enddo
      NXO2 = NX/2
      NX21 = NXO2 + 1
      DELX = 1.0/NX
      navgring=nxo2/nring
      nyfull=ny
      DO KK=izst,iznd
C         
c         print *,'reading section',kk
        lenroot = len_trim(rootname)
        mtfFile = trim(rootname)//'-'
        call int_iwrite(mtfFile(lenroot+2:),kk, ierr)
        mtfFile = trim(mtfFile)//'.mtf'
        call dopen(3, mtfFile, 'new', 'f')
        call imposn(1,kk,0)
        call irdpas(1,array,nx,nyfull,0,nx-1,0,nyfull-1,*99)
        call collapse(array,nx,nyfull,ncollapse,ny)
        zeroshift=0.
        do i=1,ny
          shifts(i)=0.
        enddo
        call copylines(array,nx,ny,brray,nx+2,shifts)
        call sumlines(brray((nx+2)*(ireflin(kk)-nlinavg/2)+1),nx+2, nlinavg, ref)
        call derivative(brray,nx+2,ny)
        call derivative(ref,nx+2,1)
        call refmeanzero(ref,nx)
        call odfft(brray,nx,ny,0)
        call odfft(ref,nx,1,0)
        call multfft(brray,nx21,ny,ref)
        call odfft(brray,nx,ny,1)
        call findshift(brray,nx+2,ny,shifts)
        call copylines(array,nx,ny,brray,nx+2,shifts)
        call sumlines(brray,nx+2,ny,ref)
        call derivative(brray,nx+2,ny)
        call derivative(ref,nx+2,1)
        call refmeanzero(ref,nx)
        call odfft(brray,nx,ny,0)
        call odfft(ref,nx,1,0)
        call multfft(brray,nx21,ny,ref)
        call odfft(brray,nx,ny,1)
        call findshift(brray,nx+2,ny,shifts)
        call copylines(array,nx,ny,brray,nx+2,shifts)
        call sumlines(brray,nx+2,ny,ref)
c         
        do i=1,nx-1
          if(i.lt.nxlim)then
            deriv(i)=ref(i+1)-ref(i)
          else
            deriv(i)=0.
          endif
          brray(i)=deriv(i)
        enddo
        brray(nx)=0.
        call odfft(brray,nx,1,0)
        call cmags(brray,nx21,ftmag)
C         
        ringsum=0.
        do i=1,navgnorm
          ringsum=ringsum+ftmag(i+1)
        enddo
        avg1=ringsum/navgnorm
C         
        fourlas=0.
        istr=2
        fcrs=-99.
        avglas = 1.
        do iring=1,nring
          iend=istr+navgring-1
          if(iring.eq.nring)iend=nx21
          four=(0.5*(iend+istr)-1)*delx
          ringsum=0.
          do i=istr,iend
            ringsum=ringsum+ftmag(i)
          enddo
          avg=ringsum/(avg1*(iend+1-istr))
          write(2,'(i5,f7.4,f9.5)')kk,four,avg
          write(3,'(f7.4,f9.5)')four,avg
          istr=iend+1
          if(avglas.gt.cross.and.avg.le.cross.and.fcrs.eq.-99.)then
            fcrs=fourlas+(cross-avglas)*(four-fourlas)/(avg-avglas)
            write(*,'(a,f6.2,a,f8.4,a)')'MTF =',cross,' at frequency',fcrs,'/pixel'
          endif
          fourlas=four
          avglas=avg
        enddo
C         
        X=DELX
        DO IX = 2,NX21
          avg=ftmag(ix)/avg1
          write(2,'(i5,f7.4,f9.5)')kk+100,x,avg
          X = X + DELX
        enddo     
c         
        do ix=1,nx
          write(2,'(i5,f6.1,f12.5)')kk+200,ix-1.,ref(ix)
        enddo
        do ix=1,nx-1
          write(2,'(i5,f6.1,f12.5)')kk+300,ix-0.5,deriv(ix)
        enddo
        do iy=1,ny
          write(2,'(i5,f6.1,f8.2)')kk+400,iy-1.,shifts(iy)
        enddo
        close(3)
      enddo
      close(2)
      call imclose(1)
      call exit
c       
99    CALL exiterror(' END OF IMAGE WHILE READING')
      END


      subroutine copylines(array,nx,ny,brray,nx2,shifts)
      real*4 array(nx,ny),brray(nx2,ny),shifts(*)
      do iy=1,ny
        do ix=1,nx
          xp=ix-shifts(iy)
          ixp=nint(xp)
          if(ixp.le.1)then
            brray(ix,iy)=array(1,iy)
          elseif(ixp.ge.nx)then
            brray(ix,iy)=array(nx,iy)
          else
            DX = XP - IXP
C             
C             Set up terms for quadratic interpolation
C             
            V4 = ARRAY(IXP-1, IY)
            V5 = ARRAY(IXP, IY)
            V6 = ARRAY(IXP+1, IY)
            vmax=max(v4,v5,v6)
            vmin=min(v4,v5,v6)
C             
            A = (V6 + V4)*.5 - V5
            C = (V6 - V4)*.5
C             
            dennew = A*DX*DX + C*DX + V5
c             limit density to between the min and max of original points
            if(dennew.gt.vmax)dennew=vmax
            if(dennew.lt.vmin)dennew=vmin
            brray(ix,iy)=dennew
          endif
        enddo
      enddo
      return
      end


      subroutine multfft(brray,nx,ny,ref)
      complex brray(nx,ny),ref(nx)
      do iy=1,ny
        do ix=1,nx
          brray(ix,iy)=brray(ix,iy)*conjg(ref(ix))
        enddo
      enddo
      return
      end


      subroutine refmeanzero(ref,nx)
      real*4 ref(*)
      sum=0.
      do i=1,nx
        sum=sum+ref(i)
      enddo
      avg=sum/nx
      do i=1,nx
        ref(i)=ref(i)-avg
      enddo
      return
      end


      subroutine findshift(brray,nx2,ny,shifts)
      real*4 brray(nx2,ny),shifts(*)
      nx=nx2-2
      do iy=1,ny
        ixmax=1
        bmax=brray(1,iy)
        do ix=2,nx
          if(bmax.lt.brray(ix,iy))then
            bmax=brray(ix,iy)
            ixmax=ix
          endif
        enddo
        ixleft=ixmax-1
        if(ixleft.eq.0)ixleft=nx
        ixright=ixmax+1
        if(ixright.gt.nx)ixright=1
c         
c         simply fit a parabola to the two adjacent points in X
c         
        cx=0.
        y1=brray(ixleft,iy)
        y2=bmax
        y3=brray(ixright,iy)
        denom=2.*(y1+y3-2.*y2)
        if(abs(denom).gt.-1.e6)cx=(y1-y3)/denom
        if(abs(cx).gt.0.5)cx=sign(0.5,cx)
c         print *,'X',y1,y2,y3,cx
        xpeak=ixmax+cx-1.
        if(xpeak.gt.nx/2)xpeak=xpeak-nx
        shifts(iy)=shifts(iy)-xpeak
      enddo
      return
      end


      subroutine sumlines(brray,nx,ny,ref)
      real*4 brray(nx,ny),ref(*)
      do ix=1,nx-2
        ref(ix)=0.
      enddo
      do iy=1,ny
        do ix=1,nx-2
          ref(ix)=ref(ix)+brray(ix,iy)
        enddo
      enddo
      do ix=1,nx-2
        ref(ix)=ref(ix)/ny
      enddo
      return
      end


      subroutine cmags(brray,nx,ftmag)
      complex brray(*)
      real*4 ftmag(*)
      do ix=1,nx
        ftmag(ix)=cabs(brray(ix))
      enddo
      return
      end


      subroutine derivative(brray,nx2,ny)
      real*4 brray(nx2,ny)
      nx=nx2-2
      do iy=1,ny
        blast=brray(1,iy)
        do ix=2,nx-1
          bthis=brray(ix,iy)
          brray(ix,iy)=0.5*(brray(ix+1,iy)-blast)
          blast=bthis
        enddo
        brray(1,iy)=.3333*brray(nx-1,iy)+.6667*brray(2,iy)
        brray(nx,iy)=.6667*brray(nx-1,iy)+.3333*brray(2,iy)
      enddo
      return
      end


      subroutine collapse(array,nx,nyfull,ncollapse,ny)
      real*4 array(nx,nyfull)
      ny=nyfull/ncollapse
      if(ncollapse.eq.1)return
      iyst=1
      do iyn=1,ny
        iynd=iyst+ncollapse-1
        do ix=1,nx
          array(ix,iyn)=array(ix,iyst)
        enddo
        do iy=iyst+1,iynd
          do ix=1,nx
            array(ix,iyn)=array(ix,iyn)+array(ix,iy)
          enddo
        enddo
        do ix=1,nx
          array(ix,iyn)=array(ix,iyn)/ncollapse
        enddo
        iyst=iynd+1
      enddo
      return
      end

