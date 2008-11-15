*       * * * * * FILLTOMO * * * * * *
c       
c       FILLTOMO improves a combined tomogram from a two-axis
c       tilt series by replacing pixels in locations where the "matching"
c       tomogram had no data with the values from the tomogram that was 
c       matched to.  It determines a linear scaling between the latter
c       tomogram and the combined tomogram so that the intensities will
c       match as well as possible.
c       
c       See man page for details
c       
c       David Mastronarde, November 1995
c       
c       $Id$
c       
c       $Log$
c       Revision 3.1  2003/12/24 19:05:25  mast
c       Changed to fit new form of get_nxyz
c	
c       
      parameter (idim=4096,maxsamp=100000)
      COMMON //NX,NY,NZ
C       
      DIMENSION NXYZ(3),MXYZ(3),NXYZST(3),
     &    ARRAY(idim*idim),BRRAY(idim*idim),TITLE(20)
C       
      CHARACTER*320 FILIN,FILOUT
C       
      real*4 cxyzin(3),cxyzout(3),dxyzin(3)
      integer*4 nxyzin(3),mxyzin(3),nxyzout(3),indcen(3)
      common /xyz/nxin,nyin,nzin,nxout,nyout,nzout,cxin,cyin,czin
     &    ,cxout,cyout,czout
      equivalence (nxyzin(1),nxin),(nxyzout(1),nxout)
      equivalence (cxyzin(1),cxin),(cxyzout(1),cxout)
      real*4 mfor(3,3),minv(3,3),mold(3,3),mnew(3,3),moldinv(3,3)
      EQUIVALENCE (NX,NXYZ)
      real*4 avg(2),sd(2)
      character dat*9,tim*8
      common /bigarr/array,brray
c       
c       7/7/00 CER: remove the encode's; titlech is the temp space
c       
      character*80 titlech
c       
      call setExitPrefix('ERROR: FILLTOMO - ')
      write(*,'(1x,a,$)')'Name of combined tomogram file: '
      read(5,'(a)')filin
      call imopen(1,filin,'old')
      call irdhdr(1,nxyzout,mxyz,mode,dminout,dmaxout,dmeanout)
      write(*,'(1x,a,$)')'Name of tomogram that was matched TO: '
      read(5,'(a)')filin
      call imopen(2,filin,'ro')
      call irdhdr(2,nxyz,mxyz,mode,dmin,dmax,dmean)
      if(nx.ne.nxout.or.ny.ne.nyout.or.nz.ne.nzout)
     &    call exitError ('File sizes do not match')
      if(nx*ny.ge.idim*idim)call exitError('IMAGE TOO LARGE FOR FILLTOMO')
c       
      print *,'Enter either the X, Y and Z dimensions of the tomogram '
     &    ,'that was','transformed to match, or the name of that file'
      call get_nxyz(.false., ' ', 'FILLTOMO', 5,nxyzin)
c       
      print *,'Enter name of file containing inverse transformation ',
     &    'used by MATCHVOL'
      read(5,'(a)')filin
      call dopen(1,filin,'ro','f')
      read(1,*)((minv(i,j),j=1,3),cxyzin(i),i=1,3)
      close(1)
      do i=1,3
        cxyzin(i)=cxyzin(i)+nxyzin(i)/2.
        cxyzout(i)=nxyzout(i)/2.
      enddo
c       
      nleftfill=0
      nrightfill=0
      nbotfill=0
      ntopfill=0
      write(*,'(1x,a,/,a,$)')'#s of pixels on left, right, bottom,'//
     &    ' and top ',
     &    '(Y in flipped tomogram) to fill regardless: '
      read(5,*)nleftfill,nrightfill,nbotfill,ntopfill
c       
c       sample each volume to find mean and SD
c       
      do iun=1,2
        idsamp=((float(nx*ny)*nz)/(8.*maxsamp))**.3333 + 1.
        nsx=(nx/2)/idsamp+1
        ixs=nx/2-0.5*nsx*idsamp
        ixs=max(0,ixs)
        nsy=(ny/2)/idsamp+1
        iys=ny/2-0.5*nsy*idsamp
        iys=max(0,iys)
        nsz=(nz/2)/idsamp+1
        izs=nz/2-0.5*nsz*idsamp
        izs=max(0,izs)
        ndat=0
        do jz=1,nsz
          iz=izs+(jz-1)*idsamp
          do jy=1,nsy
            iy=iys+(jy-1)*idsamp
            call imposn(iun,iz,iy)
            call irdlin(iun,array,*99)
            do jx=1,nsx
              ix=1+ixs+(jx-1)*idsamp
              ndat=ndat+1
              brray(ndat)=array(ix)
            enddo
          enddo
        enddo
        call avgsd(brray,ndat,avg(iun),sd(iun),sem)
        write(6,103)iun,avg(iun),sd(iun)
103     format(' Volume',i2,': mean =',f12.4,',  SD =',f12.4)
      enddo
c       
c       scale second volume to match first	  
c       
      scl=sd(1)/sd(2)
      fadd=avg(1)-avg(2)*scl
c       
      nsecr=0
      nptr=0
      do iz=0,nz-1
        ifin=0
        zcen=iz-czout
        do iy=0,ny-1
          ycen=iy-cyout
          do ix=0,nx-1
            xcen=ix-cxout
            xp=minv(1,1)*xcen+minv(1,2)*ycen+minv(1,3)*zcen+cxin
            yp=minv(2,1)*xcen+minv(2,2)*ycen+minv(2,3)*zcen+cyin
            zp=minv(3,1)*xcen+minv(3,2)*ycen+minv(3,3)*zcen+czin
            if(xp.lt.0..or.xp.gt.nxin-1.or.yp.lt.0..or.yp.gt.nyin-1
     &          .or. zp.lt.0..or.zp.gt.nzin-1
     &          .or.ix.lt.nleftfill.or.nx-ix.le.nrightfill
     &          .or.iz.lt.nbotfill.or.nz-iz.le.ntopfill)then
              if(ifin.eq.0)then
                call imposn(1,iz,0)
                call irdsec(1,array,*99)
                call imposn(2,iz,0)
                call irdsec(2,brray,*99)
                ifin=1
                nsecr=nsecr+1
              endif
              ind=1+ix+iy*nx
              val=scl*brray(ind)+fadd
              array(ind)=val
              dminout=min(dminout,val)
              dmaxout=max(dmaxout,val)
              nptr=nptr+1
            endif
          enddo
        enddo
        if(ifin.ne.0)then
          call imposn(1,iz,0)
          call iwrsec(1,array)
        endif
      enddo
      call date(dat)
      call time(tim)
c       
c       7/7/00 CER: remove the encodes
c       
c       encode ( 80, 3000, title ) dat, tim
      write(titlech,3000) dat, tim
      read(titlech,'(20a4)')(title(kti),kti=1,20)
      call iwrhdr(1,title,1,dminout,dmaxout,dmeanout)
      call imclose(1)
      print *,nptr,' points replaced on',nsecr,' sections'
      call exit(0)
3000  format ( 'FILLTOMO: Replacing parts of combined tomogram',t57,
     &    a9,2x, a8)
99    call exitError('READING FILE')
      end
