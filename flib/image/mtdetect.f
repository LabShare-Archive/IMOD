*       * * * * * MTDETECT * * * * *
c       
c       This program is a companion to MTTEACH, the interactive program for
c       defining a discriminant analysis for feature detection.  On each
c       section, it follows the procedures of that program: it looks at
c       either single pixels or sums of pixels within circular windows,
c       seeking points with values beyond a threshold value, then applies
c       the discriminant function derived in MTTEACH to classify the points
c       as features or non-features.  However, it also reads in the teaching
c       points used by MTTEACH and uses them, as well as the points
c       identified by the procedure just described, as a basis for searching
c       from one section to the next for additional points.  On the first
c       pass through the sections, it applies the procedure just described,
c       then it examines the vicinity of known points on adjacent sections.
c       For each such position, it looks for points within a specified
c       radius that have values greater than a relaxed threshold, and forms
c       discriminant scores for those points; if there are points with
c       scores better than the criterion (which may be relaxed by a
c       specified amount), then the best such point is taken as a new
c       feature point. On a second pass through the sections, it does only
c       the checking from section to section.  If the images being examined
c       are unaligned but an alignment is available, one can enter a file of
c       g transforms to improve the section-to-section correspondence for
c       the search.
c       
c       ENTRIES to the program:
c       
c       Image file name
c       Name of file of piece coordinates if image is a montage, otherwise
c       .  Return
c       Name of file of teaching points
c       Name of output file for list of detected points
c       Name of file of discriminant analysis parameters and solutions
c       .  produced by MTTEACH
c       Name of file of g transforms that align the whole data stack, or
c       .  Return if none is available
c       
c       IF you enter a file of g transforms, enter 4 lines:
c       .  0 if images are unaligned, or 1 if images are aligned via these
c       .       transforms
c       .  0 if teaching points were picked on unaligned images, or 1 if
c       .       points were picked on aligned images
c       .  0 to output points that would match unaligned images, or 1 to
c       .       output points that match aligned images
c       .  X and Y center coordinates of the transforms, or / to take the
c       .     default of the center coordinates of the image file
c       
c       Minimum distance between centers of adjacent features
c       
c       Radius to search around the position of a feature for features in
c       .  adjacent sections
c       
c       Amount to relax criterion discriminant score for the search in
c       .  adjacent sections.  This is expressed as the number of standard
c       .  deviations of the scores for true features; the program will first
c       .  print out what the criterion is already in terms of the number of
c       .  S.D.'s below the mean score for true features.
c       
c       Amount to relax threshold for choosing peaks in the search for
c       .  features in adjacent sections.  Use the same units in which the
c       .  threshold was specified in MTTEACH, i.e. an absolute or a relative
c       .  value.
c       
c       Starting and ending section to analyze, or / to do all sections
c       
c       link with MTSUBS and the usual libraries
c       
c       David Mastronarde, May 1989
c       4/29/90 - add ability to set range of sections to analyze
c       
      parameter (ixdim=2100,iydim=2100)
      DIMENSION NXYZ(3),MXYZ(3),ARRAY(ixdim*iydim),brray(ixdim*iydim)
      logical*1 consid(ixdim*iydim)
      COMMON //NX,NY,NZ
      EQUIVALENCE (NX,NXYZ)
      character*80 filin
C       
C       Open image file
C       
      write(*,'(1x,a,$)')'Enter input image file name: '
      read(5,50)filin
50    format(A)
      CALL IMOPEN(1,filin,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C       
      IF (nx*ny.gt.ixdim*iydim) STOP 'Input image too large'
c       
      call mtmainsub(array,brray,consid,nx,ny,nz)
      end
c       
      subroutine mtmainsub(array,brray,consid,nx,ny,nz)
      real*4 array(nx,ny),brray(nx,ny)
      parameter (lmzon=50,limpek=100000,npctl=1000,lmteach=3000,
     &    limpcl=50000,limsec=1000)
      include 'statsize.inc'
C       
      logical*1 consid(nx,ny)
      integer*4 ixwind(200),iywind(200)
      dimension ixlo(2),iylo(2),ixhi(2),iyhi(2)
      dimension ninsect(msiz+10)
     &    ,isdx(40,msiz+10),isdy(40,msiz+10),dvector(msiz)
     &    ,bvector(msiz),pctile(npctl)
C       
      integer*4 ixreal(limpek),iyreal(limpek),izreal(limpek),
     &    ixpc(limpek),iypc(limpek),izpc(limpek)
      integer*4 jxpc(lmteach),jypc(lmteach),jzpc(lmteach),
     &    jxreal(lmteach),jyreal(lmteach),jzreal(lmteach),
     &    ixpclist(limpcl),iypclist(limpcl),izpclist(limpcl),
     &    listz(limpcl)
      dimension ixzon0(lmzon),ixzon1(lmzon),nxzon(lmzon),izzon(lmzon)
     &    ,iyzon0(lmzon),iyzon1(lmzon),nyzon(lmzon),indzon(lmzon)
      integer*4 linkflag(limpek)
      logical notnear,doxforms,anyxforms,skipxforms,nowherenear
      logical b3dxor
c       
      character*80 filout,fildat,filteach,xfile,filpcl

      integer*4 isecorder(limsec),indfl(limsec),indptinsec(2,100)
      real*4 flist(2,3,limsec),finv(2,3,limsec),ff(2,3)
c       
      real*4 desirmean/100./,desirsd/20./
C       
C       Open other files
C       
      write(*,'(1x,a,$)')
     &    'Piece list file if image is a montage, otherwise Return: '
      read(*,50)filpcl
50    format(A)
      call read_piece_list(filpcl,ixpclist,iypclist,izpclist,npclist)
c       
c       if no pieces, set up mocklist
      if(npclist.eq.0)then
        do i=1,nz
          ixpclist(i)=0
          iypclist(i)=0
          izpclist(i)=i-1
        enddo
        npclist=nz
      endif
c       make ordered list of z values 
      call fill_listz(izpclist,npclist,listz,nlistz)

      izrange=listz(nlistz)+1-listz(1)
      call checklist(ixpclist,npclist,1,nx,minxpiece
     &    ,nxpieces,nxoverlap)
      call checklist(iypclist,npclist,1,ny,minypiece
     &    ,nypieces,nyoverlap)
      xcen=minxpiece+(nx+(nxpieces-1)*(nx-nxoverlap))/2.
      ycen=minypiece+(ny+(nypieces-1)*(ny-nyoverlap))/2.
c       
      write(*,'(1x,a,$)')'Teaching point file name: '
      read(5,50)filout
c       
c       get points and zone definitions
c       
      call dopen(3,filout,'ro','f')
      call readteach(nx,ny,ixpclist,iypclist,izpclist,npclist,nreal,
     &    nzone,nxzon,nyzon,izzon,indzon,indcur ,ixzon0,ixzon1,iyzon0,
     &    iyzon1,jxreal,jyreal,jzreal,jxpc,jypc,jzpc,lmzon)
      close(3)
c       
c       mark points as not searched, copy them into int*2 arrays
c       
      do i=1,nreal
        linkflag(i)=0
        ixpc(i)=jxpc(i)
        iypc(i)=jypc(i)
        izpc(i)=jzpc(i)
        ixreal(i)=jxreal(i)
        iyreal(i)=jyreal(i)
        izreal(i)=jzreal(i)
      enddo

      write(*,'(1x,a,$)')'Output point file name: '
      read(5,50)filout
      print *,'Enter name of file of discriminant analysis'//
     &    ' parameters and solutions'
      read(5,50)fildat
c       
C       GET all the parameters from the discriminant analysis
c       
      call dopen(4,fildat,'ro','f')
      READ(4,*)winrad,ifofset
      READ(4,*)icontig
      READ(4,*)ifeatur
      READ(4,*)thetaglob,cmpsign
      READ(4,*)rzero,delr,deldelr,nring,nsect,ifsecofs,nsectot
      READ(4,*)(bvector(i),i=1,nsectot)
      READ(4,*)critmin,critsign,scoreavg,scoresd
c       
c       get transforms if any
c       
      print *,'Enter name of file of g transforms that align'
     &    //' whole data stack, if available'
      write(*,'(1x,a,$)')'    (Return if none available): '
      read(*,50)xfile
      anyxforms=xfile.ne.' '
      if(anyxforms)then
c         
        write(*,'(1x,a,$)')'0 if images are unaligned, 1 if images'//
     &      ' were aligned via these transforms: '
        read(*,*)ifimalign
c         
        write(*,'(1x,a,$)')'0 or 1 if points were picked on'//
     &      ' unaligned or aligned images: '
        read(*,*)ifptalign
c         
        write(*,'(1x,a,$)')'0 or 1 to output points that match'//
     &      ' coordinates of raw or aligned images: '
        read(*,*)ifoutalign
c         
c         allow entry of true center coordinates for transforms, default is
c         to use the center of the image area
c         
        write(*,'(1x,a,/,a,f6.1,a,f6.1,a,$)')
     &      'Enter true center coordinates of the transforms,',
     &      '    or / for the default',xcen,',',ycen,
     &      ', the center of the image area: '
        read(*,*)xcen,ycen
c         
c         if images, points and output all aligned, don't need xforms
c         
        anyxforms=ifimalign*ifptalign*ifoutalign.eq.0
        doxforms=anyxforms.and.ifimalign.eq.0
        if(anyxforms)then
          call dopen(3,xfile,'ro','f')
          call xfrdall(3,flist,nflist,*98)
          if(nflist.lt.nlistz)stop 'TOO FEW TRANSFORMS IN FILE'
          skipxforms=.false.
          if(nlistz.lt.izrange)then
            if(nflist.eq.nlistz)then
              print *,'Looks like there are transforms only for '//
     &            'the sections that exist in file'
            elseif(nflist.eq.izrange)then
              print *,'There seem to be transforms for each z value,'
     &            //' including ones missing from file'
              skipxforms=.true.
            else
              print *, 'Cannot tell how transforms match up to '//
     &            'sections, because of missing sections'
              stop
            endif
          endif
c           
c           set up index to transforms that will be used, taking into account
c           whether any are being skipped.  Invert ones to be used
c           
          do i=1,nlistz
            indind=listz(i)+1-listz(1)
            indf=i
            if(skipxforms)indf=indind
            indfl(indind)=indf
            call xfinvert(flist(1,1,indf),finv(1,1,indf))
          enddo
          close(3)
c           
c           if alignment of points and images don't agree, need to fix
c           
          if(b3dxor(ifimalign.eq.0, ifptalign.eq.0))then
            do i=1,nreal
              xx=ixreal(i)
              yy=iyreal(i)
              indf=indfl(izreal(i)+1-listz(1))
              if(ifptalign.eq.0)then
                call xfcopy(flist(1,1,indf),ff) !xform points to images
              else
                call xfcopy(finv(1,1,indf),ff)  !or back xform them
              endif
              call xfapply(ff,xcen,ycen,float(ixreal(i))
     &            ,float(iyreal(i)),xx,yy)
              ixreal(i)=nint(xx)
              iyreal(i)=nint(yy)
              ixpc(i)=ixreal(i)-ixpclist(izpc(i)+1)
              iypc(i)=iyreal(i)-iypclist(izpc(i)+1)
            enddo
          endif
        else
          print *,'Aha, transforms are not really needed after all'
        endif
      endif
c       
      write(*,'(1x,a,$)')
     &    'Minimum distance between centers of adjacent features: '
      read(*,*)sepmin
c       
      write(*,'(1x,a,$)')'Radius to search around feature for'//
     &    ' features in neighboring sections: '
      read(*,*)searchrad
c       
      critsd=abs(critmin-scoreavg)/scoresd
      write(*,'(1x,a,f5.2,a)')'Criterion discriminant score is'
     &    ,critsd,' SD''s below mean score for true features'
c       
      write(*,'(1x,a,$)')'Enter # of SD''s to relax criterion for'//
     &    ' search in neighboring sections: '
      read(*,*)relax
c       
      write(*,'(1x,a,f10.4,a,$)')'Amount to relax threshold of'
     &    ,thetaglob,' for search in neighbor: '
      read(*,*)deltheta
c       
c       with current logic, should only need two passes, and only one if
c       neither criterion is relaxed
c       
      limpass=2
      if(relax.eq.0..and.deltheta.eq.0.)limpass=1
c       write(*,'(1x,a,$)')'Limit on number of passes through stack: '
c       read(*,*)limpass
c       
c       find out what sections to do
c       
      izstrt=listz(1)
      izend=listz(nlistz)
      write(*,'(1x,a,$)')
     &    'Starting and ending sections to analyze, or / to do all: '
      read(*,*)izstrt,izend
c       
      minlistz=1
      do while (listz(minlistz).lt.izstrt.and.minlistz.lt.nlistz)
        minlistz=minlistz+1
      enddo
c       
      maxlistz=nlistz
      do while (listz(maxlistz).gt.izend.and.maxlistz.gt.1)
        maxlistz=maxlistz-1
      enddo
      minlistz=min(minlistz,maxlistz)
c       
      call dopen(3,filout,'new','f')
c       
c       make list of index offsets for the summation window
c       
      nwind=0
      ofset=0.
      if(ifofset.ne.0)ofset=0.5
      limwin=winrad+1
      do i=-limwin,limwin
        do j=-limwin,limwin
          if(sqrt((i+ofset)**2+(j+ofset)**2).le.winrad)then
            nwind=nwind+1
            ixwind(nwind)=i
            iywind(nwind)=j
          endif
        enddo
      enddo
c       
c       limits for summation and search, a little conservative
      ix0=ifeatur+1
      ix1=nx-ifeatur
      iy0=ifeatur+1
      iy1=ny-ifeatur
c       
c       set up sectors
c       
      call sectorize(rzero,delr,deldelr,nring,nsect,ifsecofs
     &    ,nsectot,ninsect,isdx,isdy,maxdev)
c       
c       set up section order from the section with the most points
c       first find section with most points
c       
      isecbig=1
      maxpnts=0
      do il=1,nlistz
        npnts=0
        do i=1,nreal
          if(izreal(i).eq.listz(il))npnts=npnts+1
        enddo
        if(npnts.gt.maxpnts)then
          maxpnts=npnts
          isecbig=il
        endif
      enddo
c       
c       store the z values from listz in the array for section order
c       
      ind=1
      do i=max(minlistz,isecbig),maxlistz
        isecorder(ind)=i
        ind=ind+1
      enddo
      do i=min(maxlistz,isecbig-1),minlistz,-1
        isecorder(ind)=i
        ind=ind+1
      enddo
      numsecor=maxlistz+1-minlistz
c       
c       set up loop to do passes
      needcheck=1
      ipass=1
      nwritten=0
      do while(ipass.le.limpass.and.needcheck.ne.0)
c         
c         loop on all sections in list
c         
        do indsec=1,numsecor
c           
c           set up section # and number of last and next section
c           
          ilistz=isecorder(indsec)
          izsec=listz(ilistz)
          lastsec=-10000
          nextsec=-10000
          if(izsec.ne.listz(minlistz))lastsec=listz(ilistz-1)
          if(izsec.ne.listz(maxlistz))nextsec=listz(ilistz+1)
c           
c           set up list of starting, ending indexes of points already in
c           section
c           
          ifinsect=0
          nptranges=0
          do i=1,nreal
            if(izreal(i).eq.izsec)then
              if(ifinsect.eq.0)then
                ifinsect=1
                nptranges=nptranges+1
                indptinsec(1,nptranges)=i       !start of range
              endif
              indptinsec(2,nptranges)=i         !end of range
            else
              ifinsect=0
            endif
          enddo
          nptranges=nptranges+1                 !set up new range at end
          indptinsec(1,nptranges)=nreal+1
          indptinsec(2,nptranges)=nreal
          
c           
c           loop through the pieces, work on ones in this section
c           
          do indpc=1,npclist
            if(izpclist(indpc).eq.izsec)then
c               
c               but on later passes, check that piece is needed
c               
              if(ipass.gt.1)then
                needcheck=0
                do i=1,nreal
                  if(((izreal(i).eq.lastsec.and.
     &                mod(linkflag(i),2).eq.0).or.
     &                (izreal(i).eq.nextsec.and.
     &                linkflag(i)/2.eq.0)) .and.
     &                ixreal(i).ge.ixpclist(indpc)+ifeatur .and.
     &                ixreal(i).lt.ixpclist(indpc)+nx-ifeatur .and.
     &                iyreal(i).ge.iypclist(indpc)+ifeatur .and.
     &                iyreal(i).lt.iypclist(indpc)+ny-ifeatur)
     &                needcheck=needcheck+1
                enddo
              endif
              if(needcheck.gt.0)then
                write(*,'(a,i5,a,2i7)')' Working on section #',izsec,
     &              ', piece at',ixpclist(indpc),iypclist(indpc)
                if(ipass.gt.1)print *,needcheck,' need checking'
                call imposn(1,indpc-1,0)
                call irdsec(1,array,*99)
c                 
c                 set it to a standard mean and sd
c                 
                call isetsd(array,nx,ny,nx,ny,desirmean,desirsd)
C                 
c                 do summation and get min, mean, max, sd
c                 
                call windsum(array,brray,nx,ny
     &              ,nx,ny,nwind,ixwind,iywind,ifeatur
     &              ,sumsum,smin,smax,sumsq,nsum,pctile,npctl)
                smean=sumsum/nsum
                sd=sqrt((sumsq-nsum*smean**2)/(nsum-1.))
c                 write(*,'(1x,a,4f10.2)')
c                 &                   'min, max, mean, sd of sums:',smin,smax,smean,sd
c                 
c                 set threshold absolute or relative
c                 
                thetarelax=thetaglob-cmpsign*deltheta
                if(thetaglob.gt.1.)then
                  theta=thetaglob
                else
                  realind=1.+(npctl-1)*thetaglob
                  intind=min(npctl-1,int(realind))
                  frac=realind-intind
                  theta=(1.-frac)*pctile(intind)
     &                +frac*pctile(intind+1)
                  realind=1.+(npctl-1)*thetarelax
                  intind=min(npctl-1,int(realind))
                  frac=realind-intind
                  thetarelax=(1.-frac)*pctile(intind)
     &                +frac*pctile(intind+1)
                endif
c                 
c                 on the first pass only, do standard scan for extreme points
c                 and discrimination on them
c                 
                if(ipass.eq.1)then
c                   
c                   set considered array to .false. except in a zone
c                   
                  do iy=iy0,iy1
                    do ix=ix0,ix1
                      consid(ix,iy)=.false.
                    enddo
                  enddo
                  do izone=1,nzone
                    if(izzon(izone).eq.indpc-1)then
                      do iy=iyzon0(izone),iyzon1(izone)
                        do ix=ixzon0(izone),ixzon1(izone)
                          consid(ix,iy)=.true.
                        enddo
                      enddo
                    endif
                  enddo
c                   
                  npeaks=0
                  nselect=0
c                   
c                   scan for points above threshold, eliminate contiguity
c                   
                  do iysc=iy0,iy1
                    do ixsc=ix0,ix1
                      if(.not.consid(ixsc,iysc))then
                        consid(ixsc,iysc)=.true.
                        if(cmpsign*(brray(ixsc,iysc)-theta).ge.0.)then
                          curmax=brray(ixsc,iysc)
c                           
c                           set up point as current max and set up limits of
c                           nonexistent last search
c                           
                          ixmax=ixsc
                          iymax=iysc
                          lasxcen=ixsc
                          lasytop=iysc-1
c                           
c                           repeat until no change in region to search
c                           
                          do while(ixmax.ne.lasxcen.or.lasytop.ne.
     &                        iymax+icontig)
c                             
c                             limits for strip on top
c                             
                            ixlo(1)=(ixmax-icontig)
                            ixhi(1)=(ixmax+icontig)
                            iylo(1)=lasytop+1
                            iyhi(1)=(iymax+icontig)
c                             
c                             limits for strip on side
c                             
                            iylo(2)=(iymax-icontig)
                            iyhi(2)=lasytop
                            if(ixmax.gt.lasxcen)then
                              ixlo(2)=lasxcen+icontig+1
                              ixhi(2)=ixmax+icontig
                            else
                              ixlo(2)=ixmax-icontig
                              ixhi(2)=lasxcen-icontig-1
                            endif
                            lasxcen=ixmax
                            lasytop=iyhi(1)
c                             
c                             search each strip for new max
c                             
                            do iset=1,2
                              do iy=max(iy0,iylo(iset))
     &                            ,min(iy1,iyhi(iset))
                                do ix=max(ix0,ixlo(iset))
     &                              ,min(ix1,ixhi(iset))
                                  if(.not.consid(ix,iy))then
                                    consid(ix,iy)=.true.
                                    if(cmpsign*(brray(ix,iy)-curmax)
     &                                  .gt.0.) then
                                      ixmax=ix
                                      iymax=iy
                                      curmax=brray(ix,iy)
                                    endif
                                  endif
                                enddo
                              enddo
                            enddo
                          enddo
c                           
c                           have a stable maximum now; get discriminant
c                           score if point is not too near edges or an
c                           existing point
c                           
                          npeaks=npeaks+1
                          call testlimit(ixmax,iymax,ixpclist(indpc),
     &                        iypclist(indpc),izsec,maxdev,nx,ny,
     &                        ixreal,iyreal,izreal,nreal,indptinsec,
     &                        nptranges,sepmin, notnear)
c                           
                          if(notnear)then
                            call sectsum(array,nx,ny,ixmax,iymax
     &                          ,nsectot,ninsect,isdx,isdy,dvector)
                            score=0.
                            do j=1,nsectot
                              score=score+dvector(j)*bvector(j)
                            enddo
c                             
c                             if score is ok, add to list
                            if(critsign*(score-critmin).ge.0)then
                              nreal=nreal+1
                              ixpc(nreal)=ixmax-1
                              iypc(nreal)=iymax-1
                              izpc(nreal)=indpc-1
                              ixreal(nreal)=ixpclist(indpc)+ixmax-1
                              iyreal(nreal)=iypclist(indpc)+iymax-1
                              izreal(nreal)=izsec
                              linkflag(nreal)=0
                              nselect=nselect+1
                              indptinsec(2,nptranges)=nreal
                            endif
                          endif
                        endif
                      endif
                    enddo
                  enddo
                  print *,npeaks,' peaks,',nselect,' selected'
                endif                           !end of scan on pass 1
c                 
c                 now look through list of points for ones on neighboring
c                 sections that haven't been checked on this section
c                 
                nadded=0
                do ireal=1,nreal
c                   on prev sec and flag = 0 or 2 means need search forward
c                   on next sec and flag = 0 or 1 means need search backward
c                   
                  if(((izreal(ireal).eq.lastsec.and.
     &                mod(linkflag(ireal),2).eq.0).or.
     &                (izreal(ireal).eq.nextsec.and.
     &                linkflag(ireal)/2.eq.0)) .and.
     &                ixreal(ireal).ge.ixpclist(indpc)+ifeatur .and.
     &                ixreal(ireal).lt.ixpclist(indpc)+nx-ifeatur .and.
     &                iyreal(ireal).ge.iypclist(indpc)+ifeatur .and.
     &                iyreal(ireal).lt.iypclist(indpc)+ny-ifeatur)then
c                     
c                     Now transform coordinates onto this section if need to
c                     
                    ixsrch=ixpc(ireal)
                    iysrch=iypc(ireal)
                    if(doxforms)then
                      indfor=indfl(izreal(ireal)+1-listz(1))
                      indbak=indfl(izsec+1-listz(1))
c                       
c                       apply f of adjacent sec then f inverse of current sec
c                       
                      call xfapply(flist(1,1,indfor),xcen,ycen,float
     &                    (ixreal(ireal)),float(iyreal(ireal)),xx,yy)
                      call xfapply(finv(1,1,indbak),xcen,ycen,xx,yy,xx,yy)
                      ixsrch=nint(xx)-ixpclist(indpc)
                      iysrch=nint(yy)-iypclist(indpc)
                    endif
c                     
c                     loop on points within the search radius, look for best
c                     score among points above relaxed threshold
c                     
                    isrchrad=searchrad+1
                    srchradsq=searchrad**2
                    ixmax=-1
                    iymax=-1
                    ifnear=0
c                     
c                     see if anywhere near an edge or another point; if so,
c                     will need to test this for each test point
c                     
                    call testlimit(ixsrch+1,iysrch+1,
     &                  ixpclist(indpc),iypclist(indpc),
     &                  izsec,maxdev+isrchrad, nx,ny, ixreal,iyreal,
     &                  izreal,nreal,indptinsec,nptranges,
     &                  sepmin+isrchrad,nowherenear)
                    do idx=-isrchrad,isrchrad
                      do idy=-isrchrad,isrchrad
                        if(idx**2+idy**2.le.srchradsq)then
                          ixtest=ixsrch+1+idx
                          iytest=iysrch+1+idy
                          if(ixtest.ge.ix0.and.ixtest.le.ix1.and.
     &                        iytest.ge.iy0.and.iytest.le.iy1)then
                            if(cmpsign*(brray(ixtest,iytest)
     &                          -thetarelax).ge.0.)then
                              if(.not.nowherenear)
     &                            call testlimit(ixtest,iytest,
     &                            ixpclist(indpc),iypclist(indpc),
     &                            izsec,maxdev, nx,ny, ixreal,iyreal,
     &                            izreal,nreal,indptinsec,nptranges,
     &                            sepmin,notnear)
                              if(nowherenear.or.notnear)then
c                                 
                                call sectsum(array,nx,ny,ixtest,
     &                              iytest ,nsectot,ninsect,isdx,isdy,
     &                              dvector)
                                score=0.
                                do j=1,nsectot
                                  score=score+dvector(j)*bvector(j)
                                enddo
                                if(ixmax.eq.-1)bestscore=score
                                if(critsign*score.ge.critsign
     &                              *bestscore)then
                                  bestscore=score
                                  ixmax=ixtest
                                  iymax=iytest
                                endif
                              else
                                ifnear=1
                              endif
                            endif
                          endif
                        endif
                      enddo
                    enddo
                    rlx=0.
                    if(ifnear.eq.0)rlx=-critsign*relax
c                     
c                     if got any scores, and best is better than relaxed
c                     criterion, take the best point
c                     
                    if(ixmax.ne.-1.and.
     &                  critsign*(bestscore-(critmin+rlx)).ge.0)then
                      nreal=nreal+1
                      ixpc(nreal)=ixmax-1
                      iypc(nreal)=iymax-1
                      izpc(nreal)=indpc-1
                      ixreal(nreal)=ixpclist(indpc)+ixmax-1
                      iyreal(nreal)=iypclist(indpc)+iymax-1
                      izreal(nreal)=izsec
                      nadded=nadded+1
                      indptinsec(2,nptranges)=nreal
c                       
c                       set flag for new point to avoid having to check
c                       back toward old point that was used to get new one
c                       
                      if(lastsec.eq.izreal(ireal))then
                        linkflag(nreal)=2
                      else
                        linkflag(nreal)=1
                      endif
                    endif
c                     
c                     set the linkflag that this direction has been checked
c                     add 1 if checking forward, 2 if checking backward
c                     
                    if(lastsec.eq.izreal(ireal))then
                      linkflag(ireal)=linkflag(ireal)+1 
                    else
                      linkflag(ireal)=linkflag(ireal)+2
                    endif
                  endif
                enddo         
                print *,nadded,' points added by neighbor section checking'
              endif
            endif
          enddo                                 !end of loop on pieces
c           
c           write out new points after each section
c           
          do i=nwritten+1,nreal
            ixout=ixreal(i)
            iyout=iyreal(i)
            izout=izreal(i)
            if(anyxforms.and.b3dxor(ifimalign.eq.0, ifoutalign.eq.0))then
              indf=indfl(izreal(i)+1-listz(1))
              if(ifimalign.eq.0)then
                call xfcopy(flist(1,1,indf),ff) !xform points for output
              else
                call xfcopy(finv(1,1,indf),ff)  !or back xform them
              endif
              call xfapply(ff,xcen,ycen,float(ixout)
     &            ,float(iyout),xx,yy)
              ixout=nint(xx)
              iyout=nint(yy)
            endif
            write(3,'(3i6)')ixout,iyout,izout
          enddo
          nwritten=nreal
        enddo                                   !end of loop on sections
c         
c         count number of points that need checking
c         
        needcheck=0
        do i=1,nreal
          if(.not.(linkflag(i).eq.3.or.
     &        (linkflag(i).eq.2.and.izreal(i).eq.listz(maxlistz)).or.
     &        (linkflag(i).eq.1.and.izreal(i).eq.listz(minlistz))))
     &        needcheck=needcheck+1
        enddo
c         
c         reverse the order of section usage
c         
        do i=1,numsecor/2
          itmp=isecorder(i)
          isecorder(i)=isecorder(numsecor+1-i)
          isecorder(numsecor+1-i)=itmp
        enddo
        print *,'pass',ipass,needcheck,' points need checking'
        ipass=ipass+1
      enddo
      stop
c       
99    WRITE(6,450)
450   FORMAT(' END OF IMAGE WHILE READING')
      STOP
98    print *,'error reading xforms'
      stop
      END



c       TESTLIMIT tests whether given array indexes are within the limits of
c       the testable image and whether it is too close to an existing point
c       
      subroutine testlimit(ixmax,iymax,ixpcofs,iypcofs,izsec,maxdev,
     &    nx,ny, ixreal,iyreal,izreal,nreal,indptinsec,nptranges,
     &    sepmin,notnear)
      integer*2 ixreal(*),iyreal(*),izreal(*)
      integer*4 indptinsec(2,*)
      logical notnear
      notnear=ixmax.gt.maxdev.and.iymax.gt.maxdev.and.
     &    ixmax.le.nx-maxdev.and.iymax.le.ny-maxdev
      isepmin=sepmin+1
      sepminsq=sepmin**2
      ixmxreal=ixmax+ixpcofs-1
      iymxreal=iymax+iypcofs-1
      do irange=1,nptranges
        i=indptinsec(1,irange)  
        do while(i.le.indptinsec(2,irange).and.notnear)
          if(izsec.eq.izreal(i))then
            idx=abs(ixreal(i)-ixmxreal)
            idy=abs(iyreal(i)-iymxreal)
            if(idx.le.isepmin.and.idy.le.isepmin.and.
     &          idx**2+idy**2.lt.sepminsq)notnear=.false.
          endif
          i=i+1
        enddo
      enddo
      return
      end
