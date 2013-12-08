c       SECTORIZE assigns pixels to sectors for averaging
c       the first sector is a circle of radius RZERO; DELR is the width of the
c       next ring out, and the width increases by DELDELR for successive rings
c       up to NRING rings.  NSECT specifies the number of sectors per ring.
c       IFOFSET nonzero indicates that the matrix of points is to be offset by
c       (0.5,0.5) relative to the set of rings.
c       The routine returns NSECTOT, total # of sectors, NINSECT, # of points
c       in a sector, and IDX and IDY, the offsets of the points from thw central
c       pixel.
      subroutine sectorize(rzero,delr,deldelr,nring,nsect,ifofset
     &    ,nsectot,ninsect,idx,idy,maxdev)
      dimension idx(40,*),idy(40,*),ninsect(*),rvals(0:30)
c       compute inclusive outer r values for each ring
      rvals(0)=rzero
      maxdev=0
      do i=1,nring
        rvals(i)=rzero+i*delr+(i-1)**2*deldelr
      enddo
c       zero the lists
      nsectot=nsect*nring+1
      do i=1,nsectot
        ninsect(i)=0
      enddo
      ofset=0.
      if(ifofset.ne.0)ofset=0.5
      limwin=rvals(nring)+1
c       scan each point in somewhat larger box
      do ix=-limwin,limwin
        do iy=-limwin,limwin
          dx=ix+ofset
          dy=iy+ofset
c           use distance from possibly offset center
          rad=sqrt(dx**2+dy**2)
          iring=-1
c           find what ring it's in
          do i=0,nring
            if(iring.lt.0.and.rad.le.rvals(i))iring=i
          enddo
          if(iring.ge.0)then
            maxdev=max(maxdev,abs(ix),abs(iy))
            isect=1
c             if not in center, find what sector
            if(iring.gt.0)then
              ang=atan2d(dy,dx)
              if(ang.lt.0.)ang=ang+360.
              idiv=ang/(360./nsect)+1.
              if(idiv.gt.nsect)idiv=nsect
              isect=1+(iring-1)*nsect+idiv
            endif
            ninsect(isect)=ninsect(isect)+1
            idx(ninsect(isect),isect)=iy
            idy(ninsect(isect),isect)=ix
          endif
        enddo
      enddo
c       eliminate null sectors from list!
      nlim=nsectot
      do isec=nlim,1,-1
        if(ninsect(isec).eq.0)then
          do i=isec+1,nsectot
            ninsect(i)=ninsect(i+1)
            do j=1,ninsect(i)
              idx(j,i)=idx(j,i+1)
              idy(j,i)=idy(j,i+1)
            enddo
          enddo
          nsectot=nsectot-1
        endif
      enddo
      return
      end

c       SECTSUM forms the sector sums for a given place ixc,iyc in the array
      subroutine sectsum(array,nx,ny,ixc,iyc
     &    ,nsectot,ninsect,idx,idy,sums)
      dimension idx(40,*),idy(40,*),ninsect(*),sums(*),array(nx,ny)
      do isec=1,nsectot
        sum=0.
        do ipix=1,ninsect(isec)
          sum=sum+array(ixc+idx(ipix,isec),iyc+idy(ipix,isec))
        enddo
        sums(isec)=sum
      enddo
      return
      end


c       WINDSUM forms sums within a circular radius around each pixel in
c       ARRAY and puts the sum in the corresponding place in BRRAY
c       NWIND, IXWIND, and IYWIND actually specify index offsets to all of
c       the pixels to be summed.  IFEATUR is a feature radius, which limits
c       the addresses in the array to be summed over.  It returns the min,
c       max, sum and sum of squares of the window sums, and fills the 1000-
c       element array PCTILE with the window sum corresponding to the i/10th
c       percentile of sums.
c       
      subroutine windsum(array,brray,ixdim,iydim
     &    ,nx,ny,nwind,ixwind,iywind,ifeatur
     &    ,sumsum,smin,smax,sumsq,nsum,pctile,npctl)
      dimension array(ixdim,iydim),brray(ixdim,iydim),pctile(*)
      integer*2 ixwind(*),iywind(*)
      integer*4 imhist(10001)
c       
c       limits for summation and search, a little conservative
c       
      sumsum=0.
      smax=-1.e20
      smin=1.e20
      sumsq=0.
      ix0=ifeatur+1
      ix1=nx-ifeatur
      iy0=ifeatur+1
      iy1=ny-ifeatur
c       
c       do sums
c       
      if(nwind.gt.1)then
        do iy=iy0,iy1
          do ix=ix0,ix1
            sum=0.
            do iwin=1,nwind
              sum=sum+array(ix+ixwind(iwin),iy+iywind(iwin))
            enddo
            brray(ix,iy)=sum
            sumsum=sumsum+sum
            if(sum.gt.smax)smax=sum
            if(sum.lt.smin)smin=sum
            sumsq=sumsq+sum**2
          enddo
        enddo
      else
        do iy=iy0,iy1
          do ix=ix0,ix1
            sum=array(ix,iy)
            brray(ix,iy)=sum
            sumsum=sumsum+sum
            if(sum.gt.smax)smax=sum
            if(sum.lt.smin)smin=sum
            sumsq=sumsq+sum**2
          enddo
        enddo
      endif
      nsum=(ix1+1-ix0)*(iy1+1-iy0)
c       
c       build histogram in 10000 bins from smin to smax
c       
      dhist=10000./(smax-smin)
      conhist=1.-dhist*smin
      do i=1,10001
        imhist(i)=0
      enddo
      do iy=iy0,iy1
        do ix=ix0,ix1
          ind=conhist+dhist*brray(ix,iy)
          imhist(ind)=imhist(ind)+1
        enddo
      enddo
c       
c       return array with npctl percentile limits
c       
      pctile(1)=smin
      pctile(npctl)=smax
      nexthst=0
      ncumul=0
      fracdes=float(nsum)/(npctl-1.)
      fracpct=(smax-smin)/10000.
      do i=2,npctl-1
        ncumdes=nint((i-1)*fracdes)
c         
c         add up bins until get ncumul bigger than desired ncumdes
c         
        do while (ncumul.lt.ncumdes.and.nexthst.lt.10000)
          nexthst=nexthst+1
          ncumul=ncumul+imhist(nexthst)
        enddo
c         
c         interpolate to binno=bin number in middle of bin
c         
        if(imhist(nexthst).eq.0)then
          binno=nexthst
        else
          binno=nexthst-(ncumul-ncumdes)/imhist(nexthst)
        endif
        pctile(i)=smin+binno*fracpct
      enddo
      return
      end




c       READTEACH reads in a file of teaching points, figures out which
c       points are the corners of new zones, returns descriptive parameters
c       of the zones and allocates indexes for each of the zones that will
c       be read in.
c       
      subroutine readteach(nxim,nyim,ixpclist,iypclist,izpclist,
     &    npclist,nreal,nzone,nxzon,nyzon,izzon,indzon,indcur
     &    ,ixzon0,ixzon1,iyzon0,iyzon1,ixreal,iyreal,izreal,ixpc,iypc,
     &    izpc,lmzon)
c       
      dimension ixzon0(*),ixzon1(*),nxzon(*),izzon(*)
     &    ,iyzon0(*),iyzon1(*),nyzon(*),indzon(*)
     &    ,ixpclist(*),iypclist(*),izpclist(*)
c       
      integer*4 ixreal(*),iyreal(*),izreal(*),ixpc(*),iypc(*),izpc(*)
      logical error
c       
      nreal=0
      nzone=0
      indcur=1
c       
c       read next point: loop back to here
c       
5     read(3,'(3i6)',end=7)ixtmrl1,iytmrl1,iztmrl1
      call lookup_piece(ixpclist,iypclist,izpclist, npclist,nxim,nyim,
     &    ixtmrl1,iytmrl1,iztmrl1,ixtmpc1,iytmpc1,iztmpc1)
c       
c       if no zones yet or it's outside any zone, start a new zone
      izon=nzone
      do while(izon.gt.0)
        if (.not.(iztmpc1.ne.izzon(izon).or.
     &      ixtmpc1.lt.ixzon0(izon).or.ixtmpc1.gt.ixzon1(izon).or.
     &      iytmpc1.lt.iyzon0(izon).or.iytmpc1.gt.iyzon1(izon))) exit
        izon=izon-1
      enddo
      if(izon.eq.0)then
c         
c         read other corner of zone
        read(3,'(3i6)',end=7)ixtmrl2,iytmrl2,iztmrl2
        call lookup_piece(ixpclist,iypclist,izpclist, npclist,nxim,
     &      nyim, ixtmrl2,iytmrl2,iztmrl2,ixtmpc2,iytmpc2,iztmpc2)
c         
c         point must have same z value
        error=iztmpc1.ne.iztmpc2
c         
c         check that point is not in any previous zone
        do i=1,nzone
          if(iztmpc2.eq.izzon(i)
     &        .and.ixtmpc2.ge.ixzon0(i).and.ixtmpc2.le.ixzon1(i)
     &        .and.iytmpc2.ge.iyzon0(i).and.iytmpc2.le.iyzon1(i))
     &        error=.true.
        enddo
        if(error)then
          print *,'box definition error at point #',nreal+1
          stop
        endif
c         
c         OK, define new zone
c         
        nzone=nzone+1
        if(nzone.gt.lmzon)go to 7
        ixzon0(nzone)=min(ixtmpc1,ixtmpc2)      !corner coordinates
        ixzon1(nzone)=max(ixtmpc1,ixtmpc2)
        iyzon0(nzone)=min(iytmpc1,iytmpc2)
        iyzon1(nzone)=max(iytmpc1,iytmpc2)
        nxzon(nzone)=ixzon1(nzone)+1-ixzon0(nzone) !nx and ny
        nyzon(nzone)=iyzon1(nzone)+1-iyzon0(nzone)
        izzon(nzone)=iztmpc1
        indzon(nzone)=indcur                    !index to packed image array 
        indcur=indcur+nxzon(nzone)*nyzon(nzone)
      else
c         
c         if in some zone, add point to list of real points
c         
        nreal=nreal+1
        ixreal(nreal)=ixtmrl1
        iyreal(nreal)=iytmrl1
        izreal(nreal)=iztmrl1
        ixpc(nreal)=ixtmpc1
        iypc(nreal)=iytmpc1
        izpc(nreal)=iztmpc1
      endif
      go to 5
7     return
      end


      subroutine isetsd(array,ixdim,iydim,nx,ny,desirmean,desirsd)
      real*4 array(ixdim,iydim)
      sum=0.
      sumsq=0.
      do iy=1,ny
        smtm=0.
        smtmsq=0.
        do ix=1,nx
          smtm=smtm+array(ix,iy)
          smtmsq=smtmsq+array(ix,iy)**2
        enddo
        sum=sum+smtm
        sumsq=sumsq+smtmsq
      enddo
      nsum=nx*ny
      avg=sum/nsum
      sd=sqrt((sumsq-sum**2/nsum)/(nsum-1))
      scl=desirsd/sd
      add=desirmean-scl*avg
      do iy=1,ny
        do ix=1,nx
          array(ix,iy)=array(ix,iy)*scl+add
        enddo
      enddo
      return
      end
