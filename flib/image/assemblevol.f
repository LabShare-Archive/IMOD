*       * * * * * * * ASSEMBLEVOL * * * * * * * *
c       
c       Assemblevol will assemble a single MRC file from separate files
c       that form an array of subvolumes in X, Y, and Z.  In effect, it can
c       take montaged images and compose them into single images, just as
c       Reducemont can, but its advantage is that it can take the images
c       from multiple files.  Its primary use is for reassembling a tomogram
c       after it has been chopped into pieces, using the coordinates output
c       by Tomopieces.
c       
c       See man page for details.
c
c       David Mastronarde, 3/1/01
c       
c       $Id$
c       
      implicit none
      integer idimin,idimout,limfiles,limran,limopen
      parameter (limopen=19)
      integer*4 nx,ny,nz
      COMMON //NX,NY,NZ
C       
      integer*4 NXYZ(3),MXYZ(3),NXYZST(3),NXYZ2(3),MXYZ2(3)
      real*4 TITLE(20), CELL2(6),delta(3),tilt(3),origtilt(3)
      real*4, allocatable :: ARRAY(:),brray(:)
C       
      integer*4, allocatable :: ixlo(:),ixhi(:),iylo(:),iyhi(:)
      integer*4, allocatable :: izlo(:),izhi(:)
      CHARACTER*320 FILOUT
      character*320, allocatable :: files(:)
      character*9 dat
      character*8 tim
      character*80 titlech
C       
      EQUIVALENCE (NX,NXYZ)
C       
      integer*4 nfx,nfy,nfz,nx3,ny3,nz3,maxx,maxy,i,ifile,ix,iy,iz
      real*4 dmin2,dmax2,dmean2,dmin,dmax,dmean,tmin,tmax,tmean,tmpmn
      real*4 origx,origy,origz
      integer*4 mode,mode1,kti,izf,iunit,iyofs,ixofs,nybox,nxbox
      integer*4 ixf,iyf,layerFile
      logical openLayer
      DATA NXYZST/0,0,0/
c       
      call setExitPrefix('ERROR: ASSEMBLEVOL - ')

      write(*,'(1x,a,$)')'Output file for assembled volume: '
      READ(5,101)FILout
101   format(a)
c       
      write(*,'(1x,a,$)')'Numbers of input files in X, Y, and Z: '
      read(5,*)nfx,nfy,nfz
      if (nfx .lt. 0 .or. nfy .lt. 0 .or. nfz .lt. 0)
     &    call exitError('NUMBER OF FILES MUST BE POSITIVE')
      limran = max(nfx, nfy, nfz)
      limfiles = nfx*nfy*nfz
      allocate(ixlo(limran),ixhi(limran),iylo(limran),iyhi(limran), izlo(limran),
     &    izhi(limran), files(limfiles), stat = ixf)
      call memoryError(ixf, 'ARRAYS FOR RANGES OR FILENAMES')

      openLayer = nfx * nfy .le. limopen
c       
      print *,'Enter the starting and ending index coordinates '//
     &    'for the pixels to extract', ' from the files at successive'//
     &    ' positions in each dimension (0,0 for full extent)'
      nx3=0
      ny3=0
      nz3=0
      maxx=0
      maxy=0
      
      do i=1,nfx
        write(*,'(1x,a,i3,a,$)')'X coordinates for files at position #'
     &      ,i,' in X: '
        read(5,*)ixlo(i),ixhi(i)
        if (ixlo(i).lt.0 .or. ixhi(i) .lt. ixlo(i))call exitError(
     &      'ILLEGAL X COORDINATE LESS THAN ZERO OR OUT OF ORDER')
        if (nx3.gt.0 .and. ixlo(i).eq.0 .and. ixhi(i).eq.0) call exitError(
     &      'IF YOU ENTER 0,0 FOR ANY X COORDINATES, ALL ENTRIES MUST BE 0,0')
        if (ixlo(i).ne.0 .or. ixhi(i).ne.0) then
          nx3=nx3+ixhi(i)+1-ixlo(i)
          maxx=max(maxx,ixhi(i)+1-ixlo(i))
        endif
      enddo
      do i=1,nfy
        write(*,'(1x,a,i3,a,$)')'Y coordinates for files at position #',
     &      i,' in Y: '
        read(5,*)iylo(i),iyhi(i)
        if (iylo(i).lt.0 .or. iyhi(i) .lt. iylo(i))call exitError(
     &      'ILLEGAL Y COORDINATE LESS THAN ZERO OR OUT OF ORDER')
        if (ny3.gt.0 .and. iylo(i).eq.0 .and. iyhi(i).eq.0) call exitError(
     &      'IF YOU ENTER 0,0 FOR ANY Y COORDINATES, ALL ENTRIES MUST BE 0,0')
        if (iylo(i).ne.0 .or. iyhi(i).ne.0) then
          ny3=ny3+iyhi(i)+1-iylo(i)
          maxy=max(maxy,iyhi(i)+1-iylo(i))
        endif
      enddo
      do i=1,nfz
        write(*,'(1x,a,i3,a,$)')'Z coordinates for files at position #',
     &      i,' in Z: '
        read(5,*)izlo(i),izhi(i)
        if (izlo(i).lt.0 .or. izhi(i) .lt. izlo(i))call exitError(
     &      'ILLEGAL Z COORDINATE LESS THAN ZERO OR OUT OF ORDER')
        if (nz3.gt.0 .and. izlo(i).eq.0 .and. izhi(i).eq.0) call exitError(
     &      'IF YOU ENTER 0,0 FOR ANY Z COORDINATES, ALL ENTRIES MUST BE 0,0')
        if (izlo(i).ne.0 .or. izhi(i).ne.0) nz3=nz3+izhi(i)+1-izlo(i)
      enddo
c       
      print *,'Enter the input file names at successive positions '//
     &    'in X, then Y, then Z'
      ifile=1
      call ialprt(.false.)
      do iz=1,nfz
        do iy=1,nfy
          do ix=1,nfx
            write(*,'(1x,a,3i4,a,$)')'Name of file at',ix,iy,iz,': '
            read(5,101)files(ifile)
            call imopen(2,files(ifile),'ro')
            CALL IRDHDR(2,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
            if (ix .eq. 1 .and. iy .eq. 1 .and. iz .eq. 1) then
              call irtdel(2,delta)
              call irtorg(2,origx, origy, origz)
              call irttlt(2, tilt)
              call irttlt_orig(2, origtilt)
            endif
            if(ifile.eq.1) mode1=mode
            if(mode.ne.mode1)call exitError( 'MODE MISMATCH FOR THIS FILE')
c             
c             Collect coordinates if they are not defined yet
            if (ixlo(ix).eq.0 .and. ixhi(ix).eq.0) then
              ixlo(ix) = 0
              ixhi(ix) = nx - 1
              nx3 = nx3 + nx
              maxx = max(maxx, nx)
            endif
            if (iylo(iy).eq.0 .and. iyhi(iy).eq.0) then
              iylo(iy) = 0
              iyhi(iy) = ny - 1
              ny3 = ny3 + ny
              maxy = max(maxy, ny)
            endif
            if (izlo(iz).eq.0 .and. izhi(iz).eq.0) then
              izlo(iz) = 0
              izhi(iz) = nz - 1
              nz3 = nz3 + nz
            endif
            if(ixhi(ix).ge.nx.or.iyhi(iy).ge.ny.or.izhi(iz).ge.nz)
     &          call exitError('UPPER COORDINATE TOO HIGH FOR THIS FILE')
            call imclose(2)
            ifile=ifile+1
          enddo
        enddo
      enddo
C       
      idimout = nx3 * ny3 + 10
      idimin = maxx*maxy + 10
      allocate(ARRAY(idimin),brray(idimout), stat = ixf)
      call memoryError(ixf, 'ARRAYS FOR IMAGE DATA')
c
      CALL IMOPEN(1,FILOUT,'NEW')
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
      origx = origx - ixlo(1) * delta(1)
      origy = origy - iylo(1) * delta(2)
      origz = origz - izlo(1) * delta(3)
C       
      call time(tim)
      call b3ddate(dat)
      write(titlech,301) dat,tim
      read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
301   FORMAT('ASSEMBLEVOL: Reassemble a volume from pieces',t57,a9,2x,a8)
      CALL ICRHDR(1,NXYZ2,MXYZ2,mode,TITLE,0)
      CALL IALCEL(1,CELL2)
      call ialorg(1,origx, origy, origz)
      call ialtlt(1, tilt)
      call ialtlt_orig(1, origtilt)
      dmin=1.e30
      dmax=-1.e30
      tmean=0.
c       
      ifile=1
      do izf=1,nfz
c         
c         open the files on this layer if possible
c         
        if (openLayer) then
          do i=1,nfy*nfx
            call imopen(i+1,files(ifile),'ro')
            CALL IRDHDR(i+1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
c	      
c             DNM 7/31/02: transfer labels from first file
c             
            if (ifile.eq.1)call itrlab(1,i+1)
            ifile=ifile+1
          enddo
        endif
c         
c         loop on the sections to be composed
c         
        do iz=izlo(izf),izhi(izf)
          iunit=2
          iyofs=0
c           
c           loop on the files in X and Y
c           
          layerFile = ifile
          do iyf=1,nfy
            ixofs=0
            nybox=iyhi(iyf)+1-iylo(iyf)
            do ixf=1,nfx
c               
c               open files one at a time if necessary
c               
              if (.not.openLayer) then
                call imopen(2, files(layerFile),'ro')
                CALL IRDHDR(2,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
                if (ixf .eq. 1 .and. iyf .eq. 1 .and. izf .eq. 1 .and.
     &              iz .eq. izlo(izf)) call itrlab(1, 2)
                layerFile = layerFile + 1
              endif
c               
c               read the section, and insert into big array
c               
              call imposn(iunit,iz,0)
              nxbox=ixhi(ixf)+1-ixlo(ixf)
              call irdpas(iunit,array,nxbox,nybox,ixlo(ixf),ixhi(ixf),
     &            iylo(iyf),iyhi(iyf), *99)
              call insert_array(array,nxbox,nybox,brray,nx3,ny3,ixofs,
     &            iyofs)
              ixofs=ixofs+nxbox
              if (openLayer) then
                iunit=iunit+1
              else
                call imclose(2)
              endif
            enddo
            iyofs=iyofs+nybox
          enddo
c           
c           section done, get density and write it
c           
          call iclden(brray,nx3,ny3,1,nx3,1,ny3,tmin,tmax, tmpmn)
          dmin=min(dmin,tmin)
          dmax=max(dmax,tmax)
          tmean=tmean+tmpmn
          call iwrsec(1,brray)
        enddo
c         
c         close layer files if opened; otherwise set file number for next
c         layer
c         
        if (openLayer) then
          do i=1,nfy*nfx
            call imclose(i+1)
          enddo
        else
          ifile = layerFile
        endif
      enddo
      dmean=tmean/nz3
      CALL IWRHDR(1,TITLE,1,DMIN,DMAX,DMEAN)
      CALL IMCLOSE(1)
      write(*,'(/,i6,a)')nfx * nfy * nfz, ' files reassembled'
      call exit(0)
99    call exitError('READING FILE')
      end

      subroutine insert_array(array,nxbox,nybox,brray,nx3,ny3,ixofs,
     &    iyofs)
      implicit none
      integer*4 nxbox,nybox,nx3,ny3,ixofs,iyofs,ix,iy
      real*4 array(nxbox,nybox),brray(nx3,ny3)
      do iy=1,nybox
        do ix=1,nxbox
          brray(ix+ixofs,iy+iyofs)=array(ix,iy)
        enddo
      enddo
      return
      end
