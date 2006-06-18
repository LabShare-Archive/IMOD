******************XFSIMPLEX*******************************************
*       
*       This program searches for the best general linear transform between
*       a pair of images by varying either the six formal parameters of the
*       transform, the six "semi-natural" parameters underlying such a
c       transform, or restricted subsets of those semi-natural parameters.
*       
c       See man page for details
c
*       David Mastronarde 4/5/91 (adapted from XFSEARCH)
*       
************************************************************************
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 3.8  2006/06/08 18:31:15  mast
c       Changed to read in data binned and to handle 4x2K after binning
c
c       Revision 3.7  2006/05/14 03:13:15  mast
c       Output normalized difference and distance measures, standardize
c       error outputs
c
c       Revision 3.6  2005/05/26 04:34:52  mast
c       Made sums args for iclavgsd real*8
c	
c       Revision 3.5  2003/12/24 19:07:27  mast
c       Moved amoeba subroutine to a library
c	
c       Revision 3.4  2002/08/20 19:23:48  mast
c       Didn't change both calls to iclavgsd
c	
c       Revision 3.3  2002/08/18 23:13:05  mast
c       Changed to call iclavgsd in library
c	
c       Revision 3.2  2002/05/20 15:47:33  mast
c       Made the DIFF function put out a very high value when the number of
c       pixels evaluated falls below 1% of total pixels, to keep it from
c       running away into impossible shifts.  Also increased dimensions of
c       input array to allow 4Kx4K images.
c	
c       Revision 3.1  2002/04/29 16:18:37  mast
c       Added test to keep it from failing when images match perfectly
c	
*       
      parameter (idima=4100*2100, lenTemp = 32000*64)
      parameter (isub=idima/3,limspir=1000)
      parameter (isubp1=isub+1, isubt2=2*isub+1, isubt25=2.5*isub+2)
      COMMON //NX,NY,NZ
      integer*4 NXYZ(3),MXYZ(3),NXYZST(3)
      EQUIVALENCE (NX,NXYZ)
C       
      CHARACTER*160 FILIN1,FILIN2,DATOUT,xfinfile
C       
      real*4 temp(lenTemp)
      DATA NXYZST/0,0,0/
      real*4 a(6),da(6),amat(2,2),anat(6),danat(6),acall(6)
      real*4 pp(7,7),yy(7),ptmp(6),ptol(6)
c       if doing formal params, the a(i) are DX, DY, a11,a12,a21, and a22
c       for natural paramas, the a(i) are DX and DY, Global rotation, global
c       stretch, difference between Y&X-axis stretch, and difference
c       between Y and X axis rotation.
c       the da are the step sizes for the respective a's
      logical trace
c       starting values of a, and da, for formal search
      data a/0.,0.,1.,0.,0.,1./
      data da/1.,1.,.025,.025,.025,.025/
c       starting values of a and da for natural search
      data anat/0.,0.,0.,1.,0.,0./
      data danat/1.,1.,2.,.02,.02,2./
      real*8 tsum, tsumsq
c       
      integer*4 ihist(0:1000),idxspir(limspir),idyspir(limspir)
      integer*2 ixcomp(isub),iycomp(isub)
      real*4 distspir(limspir),denlo(isub),denhi(isub),range(10,2)
     &    ,ranlo(10),ranhi(10),percen(10,2),pclo(10),pchi(10)
      equivalence (ranlo(1),range(1,1)),(ranhi(1),range(1,2))
      equivalence (pclo(1),percen(1,1)),(pchi(1),percen(1,2))
c       array for second image if doing diffs, using same storage as
c       all the arrays for doing distances
      real*4 brray(idima),ARRAY(idima)
      external func
      equivalence (brray(1),denlo(1)),(brray(isubp1),denhi(1))
     &    ,(brray(isubt2),ixcomp(1)),(brray(isubt25),iycomp(1))
      common /funcom/ array,brray,idxspir,idyspir,distspir,mattx,
     &    matty, interp,natural,ncompare,nspiral,ifdist,iftrace,rds,
     &    ntrial, deltmin,sd1,ivend,acall
C       
c       default values for potentially input parameters
      data ftol1/5.e-4/,ptol1/.02/,ftol2/5.e-3/,ptol2/.2/
      data delfac/2/
      data iflmean/1/,ibinning/2/
      data idredun/0/,radius/4/,difflim/0.05/,nrange/2/
      data pclo/0.,92.,8*0./,pchi/8.,100.,8*0./
c       
c       default values for parameters in common
c       
      iftrace=0
      fracmatt=0.05
      ifdist=0
      natural=0
      interp=0
      call setExitPrefix('ERROR: XFSIMPLEX - ')
c       
      write(*,'(1x,a,$)')'First image file: '
      READ (5,40)FILIN1
      write(*,'(1x,a,$)')'Second image file: '
      READ (5,40)FILIN2
      write(*,'(1x,a,$)')'Transform output file: '
      READ (5,40)DATOUT
      write(*,'(1x,a,$)')
     &    'File with starting transform, or Return if none: '
      READ (5,40)xfinfile
40    FORMAT (A)
c       
c       open file now to get sizes and try to adjust defaults
c       
      call ialprt(.false.)
      CALL IMOPEN(1,FILIN1,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
c       
      if(nx.le.128.and.ny.le.128)then
        ftol1=2.*ftol1
        ptol1=2.*ptol1
      endif
c       
      write(*,'(1x,a,/,a,/,a,/,a,f6.4,f5.2,f7.4,f5.2,f4.1,i2,a,$)')
     &    'Enter fractional tolerances in difference measure and'//
     &    ' in parameter values','   for terminating final search,'//
     &    ' corresponding tolerances for initial search',
     &    '   (or 0,0 for only one search), factor for initial'//
     &    ' step size, and 1 or 2 for','    trace output ['
     &    ,ftol1,ptol1,ftol2,ptol2,delfac,iftrace,']: '
      read(5,*)ftol1,ptol1,ftol2,ptol2,delfac,iftrace
      trace=iftrace.gt.0
C       
13    write(*,'(1x,a,i1,a,$)')'0 to search 6 formal variables,'//
     &    ' or # of natural variables to search [',natural,']: '
      read(*,*)natural
      if(natural.lt.0.or.natural.gt.6)go to 13
      ivst=1
      if(natural.eq.0)then			!all six formal params
        ivend=6
      else
        ivend=natural				!or selected # of natural
        do i=1,6
          a(i)=anat(i)
          da(i)=danat(i)
        enddo
      endif
c       
c       if reading in a transform, get it and convert to natural if necessary
c       
      if(xfinfile.ne.' ')then
        call dopen(1,xfinfile,'old','f')
        read(1,*)((amat(ii,jj),jj=1,2),ii=1,2),a(1),a(2)
        if(natural.eq.0)then
          a(3)=amat(1,1)
          a(4)=amat(1,2)
          a(5)=amat(2,1)
          a(6)=amat(2,2)
        else
          call amat_to_rotmag(amat,a(3),a(6),a(4),a(5))
        endif
      endif
c       
      do i=1,6
        acall(i)=a(i)
      enddo
c       
      write(*,'(1x,a,f4.2,a,$)')'edge fraction to ignore ['
     &    ,fracmatt,']: '
      read(*,*)fracmatt
c       
      write(*,'(1x,a,i1,a,$)')'0 to float to range,'//
     &    ' 1 to float to mean&sd, -1 no float [',iflmean,']: '
      read(*,*)iflmean
c       
      write(*,'(1x,a,i1,a,$)')'Binning to apply to image ['
     &    ,ibinning,']: '
      read(*,*)ibinning
      ibinning = max(1, ibinning)
c       
      write(*,'(1x,a,i1,a,$)')'0 for difference,'//
     &    ' 1 for distance measure [',ifdist,']: '
      read(*,*)ifdist
c       
      if(ifdist.eq.0)then
        write(*,'(1x,a,i1,a,$)')'1 to use interpolation ['
     &      ,interp,']: '
        read(*,*)interp
      else
c         
c         change defaults based on image size and reduction by 2
c         
        npixel=nx*ny
        if(ibinning.gt.1)then
          npixel=npixel/(ibinning**2)
          radius=4.
        else
          radius=5.
        endif
c         
        if(npixel.gt.480*360)then
          idredun=2
        elseif(npixel.gt.240*180)then
          idredun=1
        else
          idredun=0
        endif
c         
c         run percentile range from 8 down to 5 as go from 320x240 to 640x480
c         
        pcrange=min(8.,max(5.,8.-3.*(npixel-320*240)/
     &      (640*480-320*240)))
        pchi(1)=pcrange
        pclo(2)=100.-pcrange
c         
        write(*,'(1x,a,i1,a,$)')'distance to search for and'//
     &      ' eliminate redundancy, 0 not to [',idredun,']: '
        read(*,*)idredun
c         
c         get density window and search radius
        write(*,'(1x,a,f3.1,a,$)')'radius to search for match ['
     &      ,radius,']: '
        read(*,*)radius
c         
        write(*,'(1x,a,f4.2,a,$)')'max density difference for match'
     &      //' as fraction of range [',difflim,']: '
        read(*,*)difflim
c         
        write(*,'(1x,a,i1,a,$)')'number of percentile ranges ['
     &      ,nrange,']: '
        read(*,*)nrange
c         
c         get percentile ranges
        write(*,'(1x,a,6f6.1)')'lower and upper percentiles'//
     &      ' in ranges - defaults:',(pclo(i),pchi(i),i=1,nrange)
        read(*,*)(pclo(i),pchi(i),i=1,nrange)
      endif
C       
      call ialprt(.true.)
      call imclose(1)
      CALL IMOPEN(1,FILIN1,'RO')
C       NOTE: ABSOLUTELY NEED TO READ HEADER AGAIN
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C       
      CALL IMOPEN(2,FILIN2,'RO')
      CALL IRDHDR(2,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C       
C       Open data file for transform
C       
      CALL DOPEN(4,DATOUT,'NEW','F')
C       
      nx = nx / ibinning
      ny = ny / ibinning
      if (nx*ny.gt.idima) goto 94
C       just get second section to work with 
      call irdbinned(2,0, array,nx, ny, 0, 0, ibinning, nx, ny, temp, lentemp,
     &    ierr)
      if (ierr .ne. 0) goto 99
c       
      rds=ibinning
      da(1)=da(1)/rds
      da(2)=da(2)/rds
      a(1)=a(1)/rds
      a(2)=a(2)/rds

c       check reduced size against size of B

c       Just deal with points in central portion
      if(fracmatt.ge.1.)then
        mattx=nint(fracmatt/rds)
        matty=mattx
      else
        mattx=max(0,NINT(FLOAT(NX)*fracmatt))
        matty=max(0,NINT(FLOAT(NY)*fracmatt))
      endif	  
      NX1 = 1+mattx 
      NX2 = NX-mattx
      NY1 = 1+matty 
      NY2 = NY-matty
C       
      CALL ICLavgsd(ARRAY,nx,ny,NX1,NX2,NY1,NY2
     &    ,DMIN2,DMAX2,tsum,tsumsq,DMEAN2,sd2)
C       
      if(ifdist.eq.0)then
c         if doing simple difference measure, move array into brray
        do ixy=1,ny*nx
          brray(ixy)=array(ixy)
        enddo
      else
c         if doing distance measure, make histogram of intensities
        do i=0,1000
          ihist(i)=0
        enddo
        histscal=1000./(dmax2-dmin2)
        DO J=NY1,NY2
          ibase=nx*(j-1)
          DO I=NX1,NX2
C             
            ind=(ARRAY(I+ibase)-dmin2)*histscal
            ihist(ind)=ihist(ind)+1
C             
          ENDDO
        ENDDO
c         convert to cumulative histogram
        DO i=1,1000
          ihist(i)=ihist(i)+ihist(i-1)
        enddo
c         find density corresponding to each percentile limit
        do iran=1,nrange
          do lohi=1,2
            ncrit=(nx2+1-nx1)*(ny2+1-ny1)*percen(iran,lohi)/100.
            i=0
            do while(i.le.1000.and.ncrit.gt.ihist(i))
              i=i+1
            enddo
            range(iran,lohi)=(i/histscal)+dmin2
          enddo
        enddo
c         find all points in central part of array within those density
c         limits
        ncompare=0
        do iy=ny1,ny2
          ibase=nx*(iy-1)
          do ix=nx1,nx2
            val=array(ix+ibase)
            do iran=1,nrange
              if(val.ge.ranlo(iran).and.val.le.ranhi(iran))then
c                 redundancy reduction: look for previous nearby points in
c                 list
                if(idredun.gt.0)then
                  ixm=ix-idredun
                  iym=iy-idredun
                  ixp=ix+idredun
                  do ic=ncompare,1,-1
                    ixcm=ixcomp(ic)
                    iycm=iycomp(ic)
c                     if point in list is nearby and within same density
c                     range, skip this one
                    if(ixcm.ge.ixm.and.ixcm.le.ixp.and.iycm.ge.iym
     &                  .and.denlo(ic).ge.ranlo(iran).and.denlo(ic)
     &                  .le.ranhi(iran))go to 80
c                     if gotten back far enough on list, take this point
                    if(iycm.lt.iym.or.(iycm.eq.iym.and.ixcm.lt.ixm))
     &                  go to 78
                  enddo
                endif
78              ncompare=ncompare+1
                ixcomp(ncompare)=ix
                iycomp(ncompare)=iy
c                 just store density temporarily here
                denlo(ncompare)=val
                go to 80
              endif
            enddo
80        enddo
        enddo
        print *,ncompare,' points for comparison'
      endif
C       Now get first section
      call irdbinned(1,0, array,nx, ny, 0, 0, ibinning, nx, ny, temp, lentemp,
     &    ierr)
      if (ierr .ne. 0) goto 99
C       
      CALL ICLavgsd(ARRAY,nx,ny,NX1,NX2,NY1,NY2
     &    ,DMIN1,DMAX1,tsum,tsumsq,DMEAN1,sd1)
C       
c       get scale factor for floating second array densities to match that
c       of first
      scale=1.
      dadd=0.
      if(iflmean.eq.0)then
        SCALE=(DMAX1-dmin1)/(DMAX2 - dmin2)
        dadd=dmin1-scale*dmin2
      elseif(iflmean.gt.0)then
        scale=sd1/sd2
        dadd=dmean1-scale*dmean2
      endif
      if(ifdist.eq.0)then
c         for simple difference, rescale whole array
        do ixy=1,nx*ny
          brray(ixy)=SCALE*brray(ixy)+dadd
        enddo
        CALL DIFF(DELMIN,ARRAY,BRRAY,sd1,A,NX,NY,MATTX,MATTY
     &      ,interp,natural)
      else
c         otherwise, for distance,
c         rescale list of densities and add lower and upper window
        window=SCALE*0.5*difflim*(dmax2-dmin2)
        do i=1,ncompare
          valscl=scale*denlo(i)+dadd
          denlo(i)=valscl-window
          denhi(i)=valscl+window
        enddo
c         find points within search radius
        limdxy=radius+1
        nspiral=0
        do idx=-limdxy,limdxy
          do idy=-limdxy,limdxy
            dstnc=sqrt(float(idx**2+idy**2))
            if(dstnc.le.radius)then
              nspiral=nspiral+1
              distspir(nspiral)=dstnc
              idxspir(nspiral)=idx
              idyspir(nspiral)=idy
            endif
          enddo
        enddo
c         order them by distance
        do i=1,nspiral
          do j=i+1,nspiral
            if(distspir(i).gt.distspir(j))then
              tmp=distspir(i)
              distspir(i)=distspir(j)
              distspir(j)=tmp
              itmp=idxspir(i)
              idxspir(i)=idxspir(j)
              idxspir(j)=itmp
              itmp=idyspir(i)
              idyspir(i)=idyspir(j)
              idyspir(j)=itmp
            endif
          enddo
        enddo
C         
        CALL DIST(DELMIN,ARRAY,A,NX,NY,ixcomp,iycomp,
     &      denlo, denhi,ncompare,idxspir,idyspir,distspir,nspiral
     &      ,natural)
c         
      endif
      ntrial=0
      deltmin=1.e30
c       
c       DNM 4/29/02: search fails if images match perfectly, so skip if so
c       
      if (delmin.gt.0.) then
        ptfac=ptol1
        if(ftol2.gt.0.or.ptol2.gt.0)ptfac=ptol2
        call amoebaInit(pp, yy, 7, ivend, delfac, ptfac, a, da, func, ptol)
        if(ftol2.gt.0.or.ptol2.gt.0)then
          call amoeba(pp,yy,7,ivend,ftol2,func,iter,ptol,jmin)
          if(trace)print *,'restarting'
          deltmin=1.e30
          do i=1,ivend
            a(i)=pp(jmin,i)
          enddo
          call amoebaInit(pp, yy, 7, ivend, delfac, ptol1, a, da, func, ptol)
        endif
        call amoeba(pp,yy,7,ivend,ftol1,func,iter,ptol,jmin)
C         
        do i=1,ivend
          a(i)=pp(jmin,i)
        enddo
      endif
c       
      PRINT *,' FINAL VALUES'
      write(*,72)ntrial,yy(jmin),(a(ii),ii=3,6),rds*a(1),rds*a(2)
72    format(i5,f14.7,4f10.5,2f10.3)
C       
      if(natural.ne.0)then 
        call rotmag_to_amat(a(3),a(6),a(4),a(5),amat)
        WRITE(*,70)((amat(ii,jj),jj=1,2),ii=1,2),rds*a(1),rds*a(2)
        WRITE(4,70)((amat(ii,jj),jj=1,2),ii=1,2),rds*a(1),rds*a(2)
      else
        write(4,70)(a(ii),ii=3,6),rds*a(1),rds*a(2)
      endif
70    FORMAT(4f12.7,2f12.3)
C       
      CALL IMCLOSE(1)
      CALL IMCLOSE(2)
C       
      call exit(0)
C       
94    call exiterror('IMAGE TOO BIG FOR ARRAYS - USE HIGHER BINNING')
99    call exiterror('READING FILE')
      END
C       
*********************************************************************
*       
*       FIND MISFIT OF IMAGES
*       
*******************************************************************
C       
      subroutine DIST(DELTA,ARRAY,A,NX,NY,
     &    ixcomp,iycomp,denlo,denhi,ncompare,
     &    idxspir,idyspir,distspir,nspiral,natural)
C       
      DIMENSION ARRAY(nx,ny),A(6),amat(2,2)
      integer*2 ixcomp(*),iycomp(*)
      dimension denlo(*),denhi(*),idxspir(*),idyspir(*),distspir(*)
C       
      XCEN=FLOAT(NX)*0.5+1
      YCEN=FLOAT(NY)*0.5+1
C       
      DELTA = 0.
C       
      xadd=xcen+a(1)+0.5
      yadd=ycen+a(2)+0.5
      if(natural.eq.0)then
        amat(1,1)=a(3)
        amat(1,2)=a(4)
        amat(2,1)=a(5)
        amat(2,2)=a(6)
      else
        call rotmag_to_amat(a(3),a(6),a(4),a(5),amat)
      endif
c	print *,((amat(i,j),j=1,2),i=1,2)
      distmax=distspir(nspiral)+1.
      do icomp=1,ncompare
        FJ=iycomp(icomp) - YCEN
        FI=ixcomp(icomp) - XCEN
C         
        IX=amat(1,1)*FI + amat(1,2)*FJ + xadd
        IY= amat(2,1)*FI + amat(2,2)*FJ + yadd
C         
        dstnc=distmax
        critlo=denlo(icomp)
        crithi=denhi(icomp)
        do ispir=1,nspiral
          ixa=min(nx,max(1,ix+idxspir(ispir)))
          iya=min(ny,max(1,iy+idyspir(ispir)))
          den1=array(ixa,iya)
          if(den1.ge.critlo.and.den1.le.crithi)then
            dstnc=distspir(ispir)
            go to 40
          endif
        enddo
40      DELTA=DELTA + dstnc
C         
      ENDDO
C       
c       5/13/06: Normalize to per comparison point to match normalization
c       of difference measure
c       
      delta=delta/ncompare
      RETURN
C       
      END
C       
      SUBROUTINE DIFF(DELTA,ARRAY,BRRAY,sd,A,NX,NY,MATTX,MATTY
     &    ,interp,natural)
C       
      DIMENSION ARRAY(nx,ny),BRRAY(nx,ny),A(6),amat(2,2)
C       
      real*4 deltalast/0./
      save deltalast

      DELTA=0.
      XCEN=FLOAT(NX)*0.5+1			!use +1 to be consistent
      YCEN=FLOAT(NY)*0.5+1			!with qdinterp usage
C       
C       
      NX1 = 1+mattx 
      NX2 = NX-mattx
      NY1 = 1+matty 
      NY2 = NY-matty
C       
      if(natural.eq.0)then
        amat(1,1)=a(3)
        amat(1,2)=a(4)
        amat(2,1)=a(5)
        amat(2,2)=a(6)
      else
        call rotmag_to_amat(a(3),a(6),a(4),a(5),amat)
      endif
c	print *,((amat(i,j),j=1,2),i=1,2)
      npix=0
      if(interp.eq.0)then
        xadd=xcen+a(1)+0.5			!the 0.5 here gets nearest int
        yadd=ycen+a(2)+0.5
        DO J=NY1,NY2
          FJ=FLOAT(J) - YCEN
C           
          DO I=NX1,NX2
            FI=FLOAT(I) - XCEN
C	      
            IX=amat(1,1)*FI + amat(1,2)*FJ + xadd
            IY= amat(2,1)*FI + amat(2,2)*FJ + yadd
C             
            if(ix.ge.1.and.ix.le.nx.and.iy.ge.1.and.iy.le.ny)then
C               
              delta = delta + abs(array(ix,iy)- brray(I,J))
              npix=npix+1
            endif
C             
          ENDDO
C	    
        ENDDO

      else
        xadd=xcen+a(1)
        yadd=ycen+a(2)
        DO J=NY1,NY2
          FJ=FLOAT(J) - YCEN
C           
          DO I=NX1,NX2
            FI=FLOAT(I) - XCEN
C	      
            X= amat(1,1)*FI + amat(1,2)*FJ + xadd
            Y= amat(2,1)*FI + amat(2,2)*FJ + yadd
C             
            ix=x
            iy=y
            if(ix.ge.1.and.ix.lt.nx.and.iy.ge.1.and.iy.lt.ny)then
c               IX = MIN(NX-1,MAX(IX,1))
c               IY = MIN(NY-1,MAX(IY,1))
              ix1=ix+1
              iy1=iy+1
              fx=1+ix-x
              fx1=1.-fx
              fy=1+iy-y
              fy1=1.-fy
              den=array(ix,iy)*fx*fy+array(ix1,iy)*fx1*fy+
     &            array(ix,iy1)*fx*fy1+array(ix1,iy1)*fx1*fy1
C               
              delta = delta + abs(den- brray(I,J))
              npix=npix+1
            endif
c	      
C             
          ENDDO
C	    
        ENDDO
      endif
C       
c       DNM 5/17/02: if the number of pixels falls below a small threshold
c       return a big delta; otherwise adjust for # of pixels and save value
c       5/13/06: Normalize to # of Sds difference per pixel
c       
      if (npix.gt.0.02*(nx2+1-nx1)*(ny2+1-ny1))then
        delta=delta/(npix * sd)
        deltalast=delta
      else
        delta = 10.*deltalast
      endif
      RETURN
C       
      END
      


c       Function to be called by minimization routine
c       
      subroutine func(x, error)
      real*4 x(*),a(6), error
      parameter (limspir=1000,idima=4100*2100)
      parameter (isub=idima/3)
      parameter (isubp1=isub+1, isubt2=2*isub+1, isubt25=2.5*isub+2)
      COMMON //NX,NY,NZ
      real*4 array(idima),brray(idima)
      integer*4 idxspir(limspir),idyspir(limspir)
      integer*2 ixcomp(isub),iycomp(isub)
      real*4 distspir(limspir),denlo(isub),denhi(isub)
      equivalence (brray(1),denlo(1)),(brray(isubp1),denhi(1))
     &    ,(brray(isubt2),ixcomp(1)),(brray(isubt25),iycomp(1))
      common /funcom/ array,brray,idxspir,idyspir,distspir,mattx,matty
     &    ,interp,natural,ncompare,nspiral,ifdist,iftrace,rds,ntrial,
     &    deltmin,sd1,ivend,a
      character*1 starout
c       
      do i=1,ivend
        a(i)=x(i)
      enddo
c       
      if(ifdist.eq.0)then
        CALL DIFF(DELTA,ARRAY,BRRAY,sd1,A,NX,NY,MATTX,MATTY,interp,
     &      natural)
      else
        CALL DIST(DELTA,ARRAY,A,NX,NY,ixcomp,iycomp,denlo,denhi,
     &      ncompare,idxspir,idyspir,distspir,nspiral,natural)
      endif
      error=delta
      ntrial=ntrial+1
      if(iftrace.ne.0)then
        starout=' '
        if(delta.lt.deltmin)then
          starout='*'
          deltmin=delta
        endif
        if(iftrace.eq.1.or.delta.lt.deltmin) write(*,72)
     &      starout,ntrial,delta,(a(ii),ii=3,6),rds*a(1),rds*a(2)
72      format(1x,a1,i3,f14.7,4f10.5,2f10.3)
      endif
      return
      end
