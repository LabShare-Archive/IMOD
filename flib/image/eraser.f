****    ERASER.FOR
c       
c       This program replaces deviant pixels with interpolated values from
c       surrounding pixels.  To use it, one first prepares a point list file
c       (e.g. with WIMP) containing a point for each area that one wishes to
c       erase.  Best results will be obtained if the point is the most
c       extreme value in the area.  (For elongated areas, one might want to
c       to enter more than one point).
c       
c       The program works by building up a list of points around the
c       specified point that need to be replaced.  The list starts with just
c       the specified point.  A rectangular box is defined by a placing a
c       border around the points on the list; e.g. for a border size of 3,
c       there will be at least 3 pixels between any point on the list and
c       the edge of the box.  Then the mean and standard deviation of density
c       is computed for pixels that are in the box but are not on the list
c       or adjacent to a point on the list.  Then, all of the adjacent points
c       are examined, and they are added to the list if they deviate from the
c       mean by more than the specified criterion number of standard
c       deviations (and in the same direction from the mean as the original
c       specified point).  This process is repeated until no further points
c       get added to the list, or until the box reaches a limiting size.
c       After the list is stable, the pixels in the box and not on the list
c       are fit to a polynomial function of x and y, and the pixels on the
c       list are replaced by values computed from the polynomial.
c       
c       The inputs are:
c       Input image file
c       Output image file, or <Return> to place modified sections back into
c       .     the input file.  USE REPLACEMENT OPTION WITH CAUTION
c       Point list file
c       Criterion # of standard deviations for adding a pixel to the list
c       .     of points needing replacement  (try 2)
c       Border size around points on list (default 3 pixels)
c       Maximum # of pixels to include in box (default 400)
c       Order of polynomial (default=2, terms in x, y, x**2, y**2 and x*y)
c       
c       At each position, the program outputs a map of the pixels in the box,
c       with the initial specified point marked by a 2 and other replaced
c       points marked by a 1.
c       
c       This program doesn't always do a good job.  It could be improved, but
c       it is better to keep dirt off of theimages in the first place.
c       
c       David Mastronarde 2/27/89
c       
      parameter (imsiz=4100,limpt=100000)
      real*4 array(imsiz*imsiz),title(20)
      integer*4 nxyz(3),mxyz(3)
      equivalence (nx,nxyz(1)),(ny,nxyz(2)),(nz,nxyz(3))
      character*80 infile,outfile,ptfile
      integer*4 ixfix(limpt),iyfix(limpt),izfix(limpt)
      common /bigarr/array
c       
c       7/7/00 CER: remove the encode's; titlech is the temp space
c       
      character*80 titlech
c       
      write(*,'(1x,a,$)')'Input image file: '
      read(*,'(a)')infile
      call imopen(1,infile,'old')
      call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
      if(nx*ny.gt.imsiz**2)stop 'IMAGE TOO LARGE'
c       
      print *,'Enter output file name (Return to put modified'//
     &    ' sections back in input file)'
      read(*,'(a)')outfile
c       
      imfilout=1
      if(outfile.ne.' ')then
        imfilout=2
        call imopen(2,outfile,'new')
        call itrhdr(2,1)
      endif
c       
      write(*,'(1x,a,$)')'Point file: '
      read(*,'(a)')ptfile
      call dopen(3,ptfile,'ro','f')
c       
      npoint=0
10    i=npoint+1
      if(i.ge.limpt)stop ' - TOO MANY POINTS FOR ARRAYS'
      read(3,*,end=15)ixfix(i),iyfix(i),izfix(i)
      npoint=i
      go to 10
c       
15    write(*,'(1x,a,$)')'criterion # of standard deviations: '
      read(*,*)sdcrit
c       
      nborder=3
      write(*,'(1x,a,i2,a,$)')'border size [',nborder,']: '
      read(*,*)nborder
c       
      maxpixel=400
      write(*,'(1x,a,i4,a,$)')'maximum pixels in correction box ['
     &    ,maxpixel,']: '
      read(*,*)maxpixel
c       
      iorder=2
      write(*,'(1x,a,i2,a,$)')'order of 2-D polynomial fit ['
     &    ,iorder,']: '
      read(*,*)iorder
c       
      tmin=1.e10
      tmax=-1.e10
      tsum=0.
c       
c       start looping on sections; need to read regardless
c       
      do izsect=0,nz-1
        call irdsec(1,array,*99)
        nfix=0
c         
c         scan through points to see if any need fixing
c         
        do ipnt=1,npoint
          if(izfix(ipnt).eq.izsect)then
            if(nfix.eq.0)print *,'fixing points on section #',izsect
            nfix=nfix+1
            call cleanarea(array,nx,ny,nx,ny,ixfix(ipnt)+1
     &          ,iyfix(ipnt)+1, sdcrit, maxpixel,nborder,iorder)
          endif
        enddo
c         
        call iclden(array,nx,ny,1,nx,1,ny,dmint,dmaxt,dmeant)
        tmin=min(tmin,dmint)
        tmax=max(tmax,dmaxt)
        tsum=tsum+dmeant
c         
c         write out if any changes or if new output file
c         
        if(nfix.gt.0.or. imfilout.eq.2)then
          call imposn(imfilout,izsect,0)
          call iwrsec(imfilout,array)
        endif
      enddo
c       
      tmean=tsum/nz
c       
c       7/7/00 CER: remove the encodes
c       
c       encode(80,109,title)
      write(titlech,109) 
109   format('ERASER: bad points replaced with interpolated values')
      read(titlech,'(20a4)')(title(kti),kti=1,20)
      call iwrhdr(imfilout,title,1,tmin,tmax,tmean)
      call imclose(imfilout)
      call exit(0)
99    stop 'error'
      end



      subroutine cleanarea(array,ixdim,iydim,nx,ny,ixcen,iycen,sdcrit,
     &    maxpixel,nborder,iorder)
c       
      parameter (mxd=50)
      real*4 array(ixdim,iydim)
      logical*1 inlist(-mxd:mxd,-mxd:mxd),adjacent(-mxd:mxd,-mxd:mxd)
      logical nearedge
      parameter (isdim=1000)
      include 'statsize.inc'
      real*4 xr(msiz,isdim), xm(msiz), sd(msiz), ssd(msiz,msiz), b1(msiz),vect(msiz)
c       
c       initialize list
c       
      do j=-mxd,mxd
        do i=-mxd,mxd
          inlist(i,j)=.false.
          adjacent(i,j)=.false.
        enddo
      enddo
      inlist(0,0)=.true.
      do i=-1,1
        do j=-1,1
          adjacent(i,j)=.true.
        enddo
      enddo
      minxlist=ixcen
      minylist=iycen
      maxxlist=ixcen
      maxylist=iycen
      ninlist=1
      nbordm1=nborder-1
      nadded=1
c       
c       loop until no more points get added to list
c       
      do while(nadded.gt.0)
        ixbordlo=max(1,minxlist-nborder)
        ixbordhi=min(nx,maxxlist+nborder)
        iybordlo=max(1,minylist-nborder)
        iybordhi=min(ny,maxylist+nborder)
c         total pixels in current area
        npixel=(iybordhi+1-iybordlo)*(ixbordhi+1-ixbordlo)
c         worst case # of pixels that could be included in regression if
c         go another cycle now
        maxpixnext=(iybordhi+3-iybordlo)*(ixbordhi+3-ixbordlo)
     &      -(ninlist+4)
        nadded=0
        if(npixel.le.maxpixel.and.maxpixnext.le.isdim)then
c           
c           get mean and sd of points: exclude points in adjacent area unless
c           they are near the borders; exclude points in list
c           
          sum=0.
          sumsq=0.
          nsum=0
          do iy=iybordlo,iybordhi
            do ix=ixbordlo,ixbordhi
              nearedge= ix-ixbordlo .lt. nbordm1 .or.
     &            ixbordhi-ix .lt. nbordm1 .or.
     &            iy-iybordlo .lt. nbordm1 .or.
     &            iybordhi-iy .lt. nbordm1
              if(nearedge .or. .not.(adjacent(ix-ixcen,iy-iycen).or.
     &            inlist(ix-ixcen,iy-iycen)))then
                sum=sum+array(ix,iy)
                sumsq=sumsq+array(ix,iy)**2
                nsum=nsum+1
              endif
            enddo
          enddo
          denmean=sum/nsum
          densd=sqrt((sumsq-nsum*denmean**2)/(nsum-1))
c           
c           now look at all adjacent points not on list (and not near
c           borders) to see if they exceed criterion deviation from mean, in
c           the same direction as the central point does
c           
          signdev=sign(1.,array(ixcen,iycen)-denmean)
          do iy=iybordlo+nbordm1,iybordhi-nbordm1
            do ix=ixbordlo+nbordm1,ixbordhi-nbordm1
              ixofs=ix-ixcen
              iyofs=iy-iycen
              if(adjacent(ixofs,iyofs) .and.
     &            .not.inlist(ixofs,iyofs))then
                if(signdev*(array(ix,iy)-denmean)/densd.gt.sdcrit)then
c                   
c                   add point to list, mark adjacent points
c                   
                  nadded=nadded+1
                  ninlist=ninlist+1
                  inlist(ixofs,iyofs)=.true.
                  do j=iyofs-1,iyofs+1
                    do i=ixofs-1,ixofs+1
                      adjacent(i,j)=.true.
                    enddo
                  enddo
                  minxlist=min(minxlist,ix)
                  minylist=min(minylist,iy)
                  maxxlist=max(maxxlist,ix)
                  maxylist=max(maxylist,iy)
                endif
              endif
            enddo
          enddo
        endif
      enddo
c       
c       list of points is complete: fill regression matrix with data for all
c       points outside the list.  the ixbordlo etc. should be correct
c       
      npnts=0
      nindep=iorder*(iorder+3)/2
      do iy=iybordlo,iybordhi
        do ix=ixbordlo,ixbordhi
          ixofs=ix-ixcen
          iyofs=iy-iycen
          if(.not.inlist(ixofs,iyofs))then
            npnts=npnts+1
            call polyterm(ixofs,iyofs,iorder,xr(1,npnts))
            xr(nindep+1,npnts)=array(ix,iy)
          endif
        enddo
      enddo
c       
c       do regession
c       
      call multRegress(xr,msiz,1,nindep,npnts,1,0,b1,msiz,c1,xm,sd,ssd)
c       
c       replace points on list with values calculated from fit
c       
      write(*,'(a,i4,a,2i4,/)')' Replacing',ninlist,' pixels at',
     &    ixcen, iycen
      do iy=iybordhi-nbordm1,iybordlo+nbordm1,-1
        do ix=ixbordlo+nbordm1,ixbordhi-nbordm1
          ixofs=ix-ixcen
          iyofs=iy-iycen
          imap=0
          if(inlist(ixofs,iyofs))then
            imap=1
            if(ixofs.eq.0.and.iyofs.eq.0)imap=2
            call polyterm(ixofs,iyofs,iorder,vect)
            xsum=c1
            do i=1,nindep
              xsum=xsum+b1(i)*vect(i)
            enddo
            array(ix,iy)=xsum
          endif
          write(*,'(a,i1,$)')char(0),imap
        enddo
        write(*,'($)')
      enddo
      write(*,*)
c       
      return
      end
