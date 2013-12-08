*       * * * * * MTTEACH * * * * *
*       
c       This program is used to develop criteria for distinguishing
c       microtubules (or other small features) from other image features.
c       First one prepares a file of teaching points.  This file is built on
c       a series of areas and contains a point for each real feature in each
c       area.  The first two points in each area are two diagonal corner
c       points of the area, NOT features; the rest of the points for that
c       area should all be inside these points.  The program has modest
c       ability to check for errors in the teaching file.  It is permissible
c       to add a point to the teaching point list that falls in ANY of the
c       zones defined earlier in the list, not necessarily in the last
c       defined zone.
c       
c       Given the zones described by the areas with teaching points,
c       the program reads in the images contained in these zones (these
c       images would typically already be filtered, or correlated with the
c       average feature).  The program then allows one to set up a two-step
c       procedure for identifying features.  In the first step, the program
c       finds all of the points where the pixel value is more extreme than a
c       threshold value.  One can adjust this threshold so as to find as
c       many of the true features as is feasible, at the expense of finding
c       many false features as well.  In the second step, the program uses
c       discriminant analysis on the pixels surrounding the extreme peaks to
c       find a discriminant function (a function of those surropunding
c       pixels) that best distinguishes the peaks that are true features from
c       peaks that are not.
c       
c       It is possible either to examine single pixels to look for peaks, or
c       to form a simple sum of pixels within a circular window around each
c       pixel position, and look for peaks in those "window sums". In
c       practice this seems to be useless, so the instructions below will
c       refer to examining pixels rather than "window sums".
c       
c       When a prompt prints a number in [ ], that is a default value which
c       can be selected by entering "/"
c       
c       ENTRIES to the program:
c       
c       Image file name
c       Name of file of piece coordinates if image is a montage, otherwise
c       .  Return
c       Name of teaching point file
c       
c       0 use individual pixel values to look for peaks,
c       .  or 1 to use sums of pixels within a circular window.
c       
c       IF you select sums of pixels, then enter two lines:
c       .  Radius of the window in pixels (which need not be an integer)
c       .  0 to center window on a pixel, 1 to offset it by (0.5,0.5)
c       
c       Distance to search from a peak to eliminate nearby extreme points.
c       .  When the program finds a pixel more extreme than the threshold
c       .  (specified below), it will search up to this distance away for
c       .  other points above threshold, and record only a single peak at the
c       .  most extreme pixel.
c       
c       Number of pixels at edges of zones to leave out of search for peaks.
c       .  Typically one would enter the feature radius here.
c       
c       At this point, one may repeatedly enter threshold values. The
c       threshold can be either an absolute value (if>1) or a relative
c       value (between 0 and 1).  The program will search for pixels greater
c       or less than the threshold depending on whether an absolute
c       threshold is greater or less than the mean, or a relative one is
c       greater or less than 0.5.  If one enters a relative threshold, then
c       within each separate zone, the program will look for a fixed
c       fraction of extreme pixels; i.e. a threshold of 0.99 or 0.01 will
c       select the brightest or dimmest 1% of pixels for further
c       consideration.  For each threshold value, the program outputs the
c       number of teaching points that have been found and the number of
c       extreme points (after contiguity elimination) that do not correspond
c       to teaching points.
c       
c       Instead of selecting a threshold, one may enter:
c       .  -3 to output points above threshold to a file - useful only for
c       .       diagnostic purposes (one needs to specify a threshold again)
c       .  -2 to loop back and respecify whether to do window sums, the
c       .       search radius for eliminating contiguous peaks, and the edges
c       .  -1 to do discriminant analysis, if there are fewer than 1000 peaks
c       
c       When discriminant analysis is selected, if there are any true
c       features that are not peaks above threshold, then the program will ask
c       whether to include those points as true features in the analysis.
c       Enter 0 to exclude them or 1 to include them.
c       
c       Next specify a division of a circular region into a series of
c       sectored rings, by entering:
c       
c       Radius of the entire circular region
c       Radius of central circle, which is not divided into sectors
c       Annular thickness (outer minus inner radius) of the innermost
c       .  ring, the one outside the central, undivided circle
c       Number of concentric rings (excluding central circle)
c       Number of sectors to divide rings into
c       0 to center the rings on a pixel, 1 to offset by (0.5,0.5)
c       
c       The thickness of the rings will change (typically increase)
c       progressively from the specified thickness of the innermost ring so
c       as to fit the given number of rings into the desired total circular
c       area.
c       
c       The program makes a variable out of the sum of pixels in each sector,
c       then finds a discriminat function, a linear combination of these
c       variables, that best distinguishes the true peaks from the false
c       ones. It then finds a criterion discriminant score that minimizes
c       the total number of extreme points that would be misclassified.
c       
c       The choices after a discriminant analysis are:
c       
c       1 to scan a range of discriminant scores, finding the criterion in
c       .  that range that minimizes misclassified peaks.  Enter a lower and
c       .  upper criterion score and the number of scores to try in the range
c       2 to redo discriminant analysis after respecifying sectored rings
c       3 to store analysis parameters and solutions in a file for use by
c       .  MTDETECT (the last "best criterion score" found by scanning is the
c       .  value that will be stored in the file)
c       4 to loop back to setting thresholds for peak selection
c       5 to store the discriminant function in an image file as a
c       .  coefficient for each pixel
c       6 to exit
c       
c       Link program with MTSUBS and the usual libraries.
c       
c       David Mastronarde, May 1989
c       
      parameter (ixdim=2100,iydim=2100,limpek=10000,lmzon=50,
     &    lmreal=3000,limpcl=50000)
      include 'statsize.inc'
      COMMON //NX,NY,NZ
C       
      DIMENSION NXYZ(3),MXYZ(3),ARRAY(ixdim*iydim),brray(ixdim*iydim)
      logical*1 consid(ixdim,iydim),matchrl(limpek),matchpk(limpek)
      integer*2 ixpeak(limpek),iypeak(limpek),ixwind(200),iywind(200)
     &    ,izonpeak(limpek)
      integer*4 ixind(lmreal),iyind(lmreal),izind(lmreal),
     &    ixreal(lmreal),iyreal(lmreal),izreal(lmreal),
     &    ixpclist(limpcl),iypclist(limpcl),izpclist(limpcl)

      parameter (npctl=1000)
      real*4 pctile(npctl,lmzon)
      dimension dmat(msiz,limpek),ssd(msiz,msiz)
      dimension ning(2),nstr(2),nend(2),ninsect(msiz+10)
     &    ,isdx(40,msiz+10),isdy(40,msiz+10),dvector(msiz)
     &    ,bvector(msiz),score(limdat),scavg(2),scsd(2),nbad(2)
c       
c       parameters defining the coordinates and storage locations of zones
      dimension ixzon0(lmzon),ixzon1(lmzon),nxzon(lmzon),izzon(lmzon)
     &    ,iyzon0(lmzon),iyzon1(lmzon),nyzon(lmzon),indzon(lmzon)
      dimension indunmatch(lmreal),izonunmat(lmreal)
C       
      EQUIVALENCE (NX,NXYZ)
c       
      character*80 filin,filout,label,filpcl
      character*12 pixwind
      character*5  truefalse(2)/'false','real '/
      data icontig/5/,ifeatur/8/,ifsecofs/0/
      data radmax/10./,rzero/0.5/,delr/1./,nring/6/,nsect/8/
      real*4 desirmean/100./,desirsd/20./
      logical b3dxor
C       
C       Open image file and point file
C       
      write(*,'(1x,a,$)')'Image input file name: '
      read(5,50)filin
50    format(A)
c       
      CALL IMOPEN(1,filin,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C       
      IF ((NX*NY.gt.ixdim*iydim)) GOTO 94
c       
      write(*,'(1x,a,$)')
     &    'Piece list file if image is a montage, otherwise Return: '
      read(*,50)filpcl
      call read_piece_list(filpcl,ixpclist,iypclist,izpclist,npclist)
c       
      write(*,'(1x,a,$)')'Teaching point file name: '
      read(5,50)filout
c       
c       get points and zone definitions
c       
      call dopen(3,filout,'ro','f')
      call readteach(nx,ny,ixpclist,iypclist,izpclist,npclist,nreal,
     &    nzone,nxzon,nyzon,izzon,indzon,indcur ,ixzon0,ixzon1,iyzon0,
     &    iyzon1,ixind,iyind,izind,ixreal,iyreal,izreal,lmzon)
      close(3)
      if(indcur.gt.ixdim*iydim+1)stop 'TOO MANY ZONES'
      print *,nreal,' actual points'
c       
c       read in the needed sections and pack zones into array
c       
      do izsec=0,nz-1
        needed=0
        do i=1,nzone
          if(izzon(i).eq.izsec)needed=needed+1
        enddo
        if(needed.gt.0)then
          call imposn(1,izsec,0)
          call irdsec(1,brray,*99)
c           
c           set it to a standard mean and sd
c           
          call isetsd(brray,nx,ny,nx,ny,desirmean,desirsd)
c           
c           repack zones into array
c           
          do izn=1,nzone
            if(izzon(izn).eq.izsec)
     &          call irepak(array(indzon(izn)),brray,nx,ny,
     &          ixzon0(izn),ixzon1(izn),iyzon0(izn),iyzon1(izn))
          enddo
        endif
      enddo
c       
c       get parameters for window sums
c       
13    print *,'Enter 0 use individual pixel values to look for peaks,'
      write(*,'(1x,a,$)')'   or 1 to use sums of pixels within'//
     &    ' a circular window: '
      read(*,*)ifwindow
      if(ifwindow.eq.0)then
        winrad=0.1
        ifofset=0
        pixwind='pixels:'
      else
        write(*,'(1x,a,$)')'Window radius in pixels: '
        read(*,*)winrad
        write(*,'(1x,a,$)')'0 for window centered on a pixel '//
     &      'or 1 to offset window by .5,.5: '
        read(*,*)ifofset
        pixwind='window sums:'
      endif
c       
      write(*,'(1x,a,i2,a,$)')'Distance to search from a peak to '//
     &    'eliminate nearby extreme points [',icontig,']: '
      read(*,*)icontig
c       
      ifeatur=max(ifeatur,int(windrad)+1)
      write(*,'(1x,a,/,a,i2,a,$)')'Enter # of pixels at edges of'//
     &    ' zones to leave out of search for peaks',
     &    '   (approximately the feature radius) [',ifeatur,']: '
      read(*,*)ifeatur
C       
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
c       do summation and get grand min, mean, max, sd
c       window sums are packed into brray, percentile distributions for each
c       zone are in pctile
c       
      sumsum=0.
      smax=-1.e20
      smin=1.e20
      sumsq=0.
      nsum=0
      do izn=1,nzone
        call windsum(array(indzon(izn)),brray(indzon(izn))
     &      ,nxzon(izn),nyzon(izn),nxzon(izn),nyzon(izn),nwind,ixwind,
     &      iywind,ifeatur ,sumsumzn,sminzn,smaxzn,sumsqzn,nsumzn,
     &      pctile(1,izn), npctl)
        sumsum=sumsum+sumsumzn
        smin=min(smin,sminzn)
        smax=max(smax,smaxzn)
        sumsq=sumsq+sumsqzn
        nsum=nsum+nsumzn
      enddo
c       
      smean=sumsum/nsum
      sd=sqrt((sumsq-nsum*smean**2)/(nsum-1.))
      write(*,'(1x,a,a,4f10.2)')
     &    'min, max, mean, sd of ',pixwind,smin,smax,smean,sd
c       
      ifdiscok=0
15    write(*,*)
      print *,'Enter -2 to respecify window, search, & edges;'//
     &    ' -3 to output points to file,'
      if(ifdiscok.ne.0)print *,'   or -1 to do discriminant'//
     &    ' analysis to separate real from false peaks,'
      write(*,'(1x,a,$)') '   or threshold (1 to max for '//
     &    'absolute, or 0 to 1 for relative): '
      read(*,*)thetain
      iopt=nint(thetain)
      if(iopt.eq.-2)go to 13
      if(iopt.eq.-1.and.ifdiscok.ne.0)go to 49
      iftypeout=0
      if(iopt.eq.-3)then
        iftypeout=1
        print *,'output file for points'
        read(5,50)filout
        call dopen(4,filout,'new','f')
        write(*,'(1x,a,$)')'Threshold for real now: '
        read(*,*)thetain
      endif
      if(thetain.le.0.)go to 15
      theta=thetain
c       
c       allow positive or negative comparisons
      cmpsign=1.
      if((theta.gt.1..and.theta.lt.smean).or.
     &    (theta.le.1..and.theta.lt.0.5))cmpsign=-1.
c       
c       scan through each zone, picking window sums greater than threshold
c       and eliminating contiguous ones.
c       
      npeaks=0
      do izn=1,nzone
        if(theta.gt.1.)then
          realtheta=theta
        else
          realind=1.+(npctl-1)*theta
          intind=min(npctl-1,int(realind))
          frac=realind-intind
          realtheta=(1.-frac)*pctile(intind,izn)
     &        +frac*pctile(intind+1,izn)
        endif
        call scanpeak(brray(indzon(izn)),consid,nxzon(izn),nyzon(izn)
     &      ,ifeatur,icontig,ixzon0(izn),iyzon0(izn),izn
     &      ,ixpeak,iypeak,izonpeak,npeaks
     &      ,limpek,realtheta,cmpsign)
      enddo
c       
      print *,npeaks,' peaks'
      npeaks=min(npeaks,limpek)
      if(iftypeout.ne.0)then
        do i=1,npeaks
          izpk=izzon(izonpeak(i))
          write(4,109)ixpeak(i),iypeak(i),izpk
109       format(3i6)
        enddo
        close(4)
      endif
c$$$    17          write(*,'(1x,a,$)')'criterion distance: '
c$$$    read(*,*)cmpcrt
c$$$    if(cmpcrt.le.0.)go to 15
      cmpcrt=5
      icrit=cmpcrt
c       
c       match up the set of picked points with the teaching points; cmpcrit
c       is the maximum distance for a match
c       
      do ireal=1,nreal
        matchrl(ireal)=.false.
      enddo
      nmatrl=0
      nmatpk=0
      do ipk=1,npeaks
        matchpk(ipk)=.false.
        ixpk=ixpeak(ipk)
        iypk=iypeak(ipk)
        do ireal=1,nreal
          if(izreal(ireal).eq.izzon(izonpeak(ipk)))then
            idx=ixreal(ireal)-ixpk
            idy=iyreal(ireal)-iypk
            if(abs(idx).le.icrit.and.abs(idy).le.icrit)then
              if(sqrt(float(idx**2+idy**2)).le.cmpcrt)then
                if(matchrl(ireal))then
                  print *,'two peaks match real point #',ireal
                else
                  nmatrl=nmatrl+1
                  matchrl(ireal)=.true.
                endif
                matchpk(ipk)=.true.
                nmatpk=nmatpk+1
                go to 45
              endif
            endif
          endif
        enddo
45    enddo
c       
c       now count up number of reals actually inside a zone as # of matched, plus
c       unmatched reals that are actually inside a zone minus matt
c       
      nunmatch=0
      do ireal=1,nreal
        if(.not.matchrl(ireal))then
          ixrl=ixreal(ireal)
          iyrl=iyreal(ireal)
          do izon=1,nzone
            if(izreal(ireal).eq.izzon(izon).and.
     &          (ixrl.ge.ixzon0(izon)+ifeatur.and.
     &          ixrl.le.ixzon1(izon)-ifeatur).and.
     &          (iyrl.ge.iyzon0(izon)+ifeatur.and.
     &          iyrl.le.iyzon1(izon)-ifeatur))then
              nunmatch=nunmatch+1
              indunmatch(nunmatch)=ireal
              izonunmat(nunmatch)=izon
              go to 47
            endif
          enddo
        endif
47    enddo
      nrealin=nmatrl+nunmatch
c       
c       express # to fix etc in terms of nrealin
c       
      nfalsepos=npeaks-nmatpk
      ntofix=nfalsepos+nunmatch
      falsepos=(100.*nfalsepos)/npeaks
      truepos=100.-falsepos
      falseneg=(100.*nunmatch)/nrealin
      write(*,111)nmatpk,npeaks,nmatrl,
     &    nrealin,ntofix,truepos,nfalsepos,falsepos,nunmatch,falseneg
111   format(' matched',i4,' of',i4,' peaks,',i4,' of',i4,
     &    ' reals,  # to fix:',i4,/
     &    ,' true pos =',f5.1,'%, ',i4,' false pos =',f5.1,
     &    '%, ',i4,' false neg =' ,f5.1,'%')
c       
c       don't allow discrim if too many points
c       
      ifdiscok=1
      if(npeaks.gt.limpek)ifdiscok=0
      go to 15
c       
c       define sectors to make sums in
49    ifaddunmat=1
      if(npeaks+nunmatch.le.limpek.and.nunmatch.gt.0)then
        write(*,'(1x,a,$)')'0 or 1 to exclude or include'//
     &      ' unmatched reals (false negs) in analysis: '
        read(*,*)ifaddunmat
      endif
      write(*,'(a,i4,a)')' Specify how to add up pixels in a set '//
     &    'of sectored rings with <=',msiz,' segments...'
51    write(*,'(1x,a,f6.2,a,$)')
     &    'Total radius to include in analysis [',radmax,']: '
      read(*,*)radmax
      write(*,'(1x,a,f6.2,a,$)')
     &    'Radius of central, undivided circle [',rzero,']: '
      read(*,*)rzero
      write(*,'(1x,a,f6.2,a,$)')
     &    'Annular thickness of innermost ring [',delr,']: '
      read(*,*)delr
      write(*,'(1x,a,i2,a,$)')
     &    'Number of concentric rings [',nring,']: '
      read(*,*)nring
      write(*,'(1x,a,i2,a,$)')
     &    'Number of sectors to divide ring into [',nsect,']: '
      read(*,*)nsect
      write(*,'(1x,a,i2,a,$)')
     &    '1 to offset center by 0.5,0.5 [',ifsecofs,']: '
      read(*,*)ifsecofs
      if(nring.gt.1)deldelr=(radmax-rzero-nring*delr)/(nring-1)**2
      call sectorize(rzero,delr,deldelr,nring,nsect,ifsecofs
     &    ,nsectot,ninsect,isdx,isdy,maxdev)
      print *,nsectot,' total sectors;',maxdev,' maximum deviation'
      if(nsectot.gt.msiz)go to 51
      ndat=0
c       
c       put non-matched peaks in group 1 and matched peaks in group 2
c       
      do igrp=1,2
        nstr(igrp)=ndat+1
        do ipk=1,npeaks
          if(b3dxor(igrp.eq.1, matchpk(ipk)))then
            izon=izonpeak(ipk)
            ixc=ixpeak(ipk)+1-ixzon0(izon)
            iyc=iypeak(ipk)+1-iyzon0(izon)
            if(ixc.gt.maxdev.and.iyc.gt.maxdev
     &          .and.ixc.le.nxzon(izon)-maxdev
     &          .and.iyc.le.nyzon(izon)-maxdev)then
              ndat=ndat+1
              call sectsum(array(indzon(izon)),nxzon(izon),nyzon(izon)
     &            ,ixc,iyc,nsectot,ninsect,isdx,isdy,dvector)
              do i=1,nsectot
                dmat(i,ndat)=dvector(i)
              enddo
            endif
          endif
        enddo
c         
c         add unmatched reals to group 2 if flag set
c         
        if(igrp.eq.2.and. ifaddunmat.ne.0)then
          do iunm=1,nunmatch
            ireal=indunmatch(iunm)
            ixc=ixreal(ireal)
            iyc=iyreal(ireal)
            izon=izonunmat(iunm)
            if(ixc.gt.maxdev.and.iyc.gt.maxdev
     &          .and.ixc.le.nxzon(izon)-maxdev
     &          .and.iyc.le.nyzon(izon)-maxdev)then
              ndat=ndat+1
              call sectsum(array(indzon(izon)),nxzon(izon),nyzon(izon)
     &            ,ixc,iyc,nsectot,ninsect,isdx,isdy,dvector)
              do i=1,nsectot
                dmat(i,ndat)=dvector(i)
              enddo
            endif
          enddo
        endif
        nend(igrp)=ndat
        ning(igrp)=ndat+1-nstr(igrp)
      enddo
c       
c       do discriminant
c       
      call eigenv(dmat,nsectot,-2,ning,nstr,nend,ssd,1)
c       
c       save the vector
c       
      do i=1,nsectot
        bvector(i)=ssd(i,1)
      enddo
c       
c       compute scores for the points
c       
      do i=1,ndat
        sum=0.
        do j=1,nsectot
          sum=sum+dmat(j,i)*bvector(j)
        enddo
        score(i)=sum
      enddo
      write(*,*)
      do i=1,2
        call avgsd(score(nstr(i)),ning(i),scavg(I),scsd(I),scsem)
        write(*,'(1x,i5,1x,a,a,2f13.5)')ning(i),truefalse(i),
     &      ' points, mean & S.D. discriminant score:'
     &      ,scavg(i),scsd(i)
      enddo
      critsign=1.
      if(scavg(1).gt.scavg(2))critsign=-1.
c       
c       find score that leaves least # to fix
c       
      critlo=min(scavg(1)+0.5*scsd(1),scavg(2)+0.5*scsd(2))
      crithi=max(scavg(1)-0.5*scsd(1),scavg(2)-0.5*scsd(2))
      ncritry=1000
24    needmin=ndat+1
      do icrit=1,ncritry
        crit=critlo
        if(icrit.gt.1)
     &      crit=critlo+(icrit-1)*(crithi-critlo)/(ncritry-1)
        call fixcount(score,nstr,nend,crit,critsign,nbad,needfix)
        if(needfix.lt.needmin)then
          needmin=needfix
          critmin=crit
          nbadpos=nbad(1)
          nbadneg=nbad(2)
        endif
      enddo
      falsepos=(100.*nbadpos)/(ning(2)+nbadpos-nbadneg)
      falseneg=(100.*nbadneg)/ning(2)
      write(*,113)critmin,nbadpos,falsepos,nbadneg,falseneg,needmin
113   format(' Best criterion at',f11.5,' would give:',/,i6,
     &    ' false pos:',f5.1,'%,',i6,' false neg:',f5.1,
     &    '%,   # to fix:',i4)
c       
      if(ifaddunmat.eq.0.and. nunmatch.gt.0)then
        ntbadneg=nbadneg+nunmatch
        tfalseneg=(100.*ntbadneg)/nrealin
        write(*,114)ntbadneg,tfalseneg,needmin+nunmatch
114     format(' Including reals lost on peak search,',i4,
     &      ' false neg:',f5.1,'%, total # to fix:',i4)
      endif
23    write(*,*)
      print *,'Enter 1 to scan range of criterion scores,',
     &    '      2 to redo discriminant analysis',
     &    '      3 to store analysis parameters in file,',
     &    '   4 to go back & set new threshold'
      write(*,'(1x,a,$)')'      5 to store discriminant in image'//
     &    ' file,    6 to exit: '
      iopt=0
      read(*,*)iopt
      if(iopt.eq.1)then
        write(*,'(1x,a$)')'Lower and upper criterion scores,'//
     &      ' # to try in that range: '
        read(*,*)critlo,crithi,ncritry
        go to 24
      elseif(iopt.eq.2)then
        go to 49
      elseif(iopt.eq.6)then
        call exit(0)
      elseif(iopt.eq.4)then
        go to 15
      elseif(iopt.eq.3)then
c         
c         write info to file
        write(*,'(1x,a,$)')
     &      'File to store discriminant parameters and solutions: '
        read(5,50)filout
        call dopen(7,filout,'new','f')
        write(7,*)winrad,ifofset
        write(7,*)icontig
        write(7,*)ifeatur
        write(7,*)theta,cmpsign
        write(7,*)rzero,delr,deldelr,nring,nsect,ifsecofs,nsectot
        write(7,*)(bvector(i),i=1,nsectot)
        write(7,*)critmin,critsign,scavg(2),scsd(2)
        close(7)
      elseif(iopt.eq.5.and.
     &      (2*maxdev+1)**2.lt.ixdim*iydim-indcur)then
        write(*,'(1x,a,$)')
     &      'Image file to store discriminant function: '
        read(5,50)filout
        call imwrite(filout,isdx,isdy,ninsect,nsectot,bvector,
     &      array(indcur),2*maxdev+1)
      elseif(iopt.eq.5)then
        print *,'not enough image array space'
      endif
      go to 23
c       
99    WRITE(6,450)
450   FORMAT(' END OF IMAGE WHILE READING')
      STOP
94    WRITE (6,660)
660   FORMAT(' Input cell .GT. idim,idim .')
      STOP
      END


c       FIXCOUNT takes a criterion discriminant score and counts up how many
c       points would be misclassified by that score.
c       
      subroutine fixcount(score,nstr,nend,crit,critsign,nbad,needfix)
      dimension score(*),nstr(*),nend(*),nbad(*)
      do ig=1,2
        nbad(ig)=0
        cmpsign=2*(1.5-ig)*critsign
        do i=nstr(ig),nend(ig)
          if(cmpsign*(score(i)-crit).ge.0.)nbad(ig)=nbad(ig)+1
        enddo
      enddo
      needfix=nbad(1)+nbad(2)
      return
      end


c       SCANPEAK looks through a zone for points beyond the threshold and
c       then looks around (a distance specified by ICONTIG) for other points
c       beyond threshold, and adds the one such point to the list IXPEAK etc.
c       
      subroutine scanpeak(brray,consid,nx,ny
     &    ,ifeatur,icontig,ixzon,iyzon,izon,ixpeak,iypeak
     &    ,izonpeak,npeaks,limpek,theta,cmpsign)
      dimension brray(nx,ny),ixlo(2),iylo(2),ixhi(2),iyhi(2)
      integer*2 ixpeak(*),iypeak(*),izonpeak(*)
      logical*1 consid(nx,ny)
      ix0=ifeatur+1
      ix1=nx-ifeatur
      iy0=ifeatur+1
      iy1=ny-ifeatur
c       set considered array to .false.
      do iy=iy0,iy1
        do ix=ix0,ix1
          consid(ix,iy)=.false.
        enddo
      enddo
c       scan for points above threshold
      do iysc=iy0,iy1
        do ixsc=ix0,ix1
          if(.not.consid(ixsc,iysc))then
            consid(ixsc,iysc)=.true.
            if(cmpsign*(brray(ixsc,iysc)-theta).ge.0.)then
              curmax=brray(ixsc,iysc)
c               set up point as current max and set up limits of nonexistent last search
              ixmax=ixsc
              iymax=iysc
              lasxcen=ixsc
              lasytop=iysc-1
c               repeat until no change in region to search
              do while(ixmax.ne.lasxcen.or.lasytop.ne.iymax+icontig)
c                 limits for strip on top
                ixlo(1)=(ixmax-icontig)
                ixhi(1)=(ixmax+icontig)
                iylo(1)=lasytop+1
                iyhi(1)=(iymax+icontig)
c                 limits for strip on side
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
c                 search each strip for new max
                do iset=1,2
                  do iy=max(iy0,iylo(iset)),min(iy1,iyhi(iset))
                    do ix=max(ix0,ixlo(iset)),min(ix1,ixhi(iset))
                      if(.not.consid(ix,iy))then
                        consid(ix,iy)=.true.
                        if(cmpsign*(brray(ix,iy)-curmax).gt.0.)then
                          ixmax=ix
                          iymax=iy
                          curmax=brray(ix,iy)
                        endif
                      endif
                    enddo
                  enddo
                enddo
              enddo
c               have a stable maximum now; save it
              npeaks=npeaks+1
              if(npeaks.le.limpek)then
                ixpeak(npeaks)=ixmax+ixzon-1
                iypeak(npeaks)=iymax+iyzon-1
                izonpeak(npeaks)=izon
              else
                if(npeaks.eq.limpek+1)print *,'TOO MANY PEAKS!!!'
              endif
            endif
          endif
        enddo
      enddo
      return
      end


      subroutine imwrite(filout,isdx,isdy,ninsect,nsectot,bvector,
     &    array,nx)
      character*(*) filout
      integer*4 isdx(40,*),isdy(40,*),ninsect(*)
      real*4 array(nx,nx),bvector(*)
      integer*4 nxyz(3)/0,0,1/
      real*4 cell(6)/0.,0.,1.,90.,90.,90./,title(20)
c       
c       7/7/00 CER: remove the encode's; titlech is the temp space
c       
      character*80 titlech
c       
      call imopen(5,filout,'NEW')
c       
c       7/7/00 CER: remove the encodes
c       
c       encode(80,'(a)',title)'DISCRIMINANT FUNCTION'
      write(titlech,3000)
3000  FORMAT('DISCRIMINANT FUNCTION')
      read(titlech,'(20a4)')(title(kti),kti=1,20)
      nxyz(1)=nx
      nxyz(2)=nx
      cell(1)=nx
      cell(2)=nx
      call icrhdr(5,nxyz,nxyz,2,title,0)
      call ialcel(5,cell)
      do iy=1,ny
        do ix=2,nx
          array(ix,iy)=0.
        enddo
      enddo
      midpix=(nx+1)/2
      do isect=1,nsectot
        do j=1,ninsect(isect)
          array(midpix+isdx(j,isect),midpix+isdy(j,isect))
     &        =bvector(isect)
        enddo
      enddo
      call iclden(array,nx,nx,1,nx,1,nx,dmin,dmax,dmean)
      call iwrsec(5,array)
      call iwrhdr(5,title,1,dmin,dmax,dmean)
      call imclose(5)
      return
      end
