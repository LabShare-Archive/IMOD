*       * * * * SUBIMANOVA * * * * *
c       
c       SUBIMANOVA subtracts one set of average images from another set and
c       uses a nested analysis of variance (ANOVA) to find the statistical
c       significance of the difference at each pixel.  It then sets to zero
c       all differences less significant than a specified level.  The program
c       can output either actual differences or pixel values that reflect
c       the level of significance.  In order to do the ANOVA, it must have a
c       standard deviation or variance image corresponding to each average
c       image.
c       
c       The average and standard deviation/variance images can be ones
c       produced by IMAVGSTAT or by other means.  When one starts the
c       program, one designates a pair of A files (with average and S.D./VAR
c       images) and a pair of B files.  One can then subtract any set of
c       sections in B from any set of sections in A; A and B may be the same
c       pair of files.
c       
c       The user is responsible for keeping track of how many samples were
c       used in making each average, and informing this program of those
c       numbers.  The program needs these numbers to do the ANOVA.
c       
c       Entries to the program:
c       
c       Average image file A
c       Standard deviation or variance image file A
c       Average image file B, or Return if same as file for A
c       Standard deviation or variance image file B, or Return if same as
c       .  file for A
c       Output image file to store differences in
c       
c       0 to use a simple mean when combining the average images of one set,
c       .  or 1 to form a weighted mean, where each average image would be
c       .  weighted by the number of samples combined to form that average.
c       .  In the latter case, the mean would be identical to the average
c       .  image that could be obtain by combining ALL of the samples of
c       .  that set.
c       
c       0 if the files have standard deviations in them, or 1 if the files
c       .  have variances
c       
c       Number of differences to compute
c       
c       For each difference, enter:
c       
c       .  List of section numbers in file A, where ranges are allowed
c       .     (e.g. 0-2,4,7-8).
c       
c       .  List of section numbers in file B, where ranges are allowed
c       
c       .  Number of samples making up those averages for each section in A
c       
c       .  Number of samples making up those averages for each section in B
c       
c       .  Significance level (e.g. 0.05, 0.01, etc).  Differences with less
c       .     than this significance will be set to zero.  Enter a
c       .     negative value to have significant pixels values set to 
c       .     the negative of the log of the probability, or to the positive
c       .     log for negative differences.  For example, positive and
c       .     negative differences with a P of 0.01 would be output as
c       .     2 and -2, respectively.
c       
c       
c       The infamous Satterthwaite approximation will be used whenever the
c       criteria for its application are satisfied.  An entry to make this
c       optional is commented out in the code below.
c       
c       David Mastronarde  4/23/90
c       4/12/95 chnaged to use local subroutines instead of NAG ones
c       
      parameter (ixdim=2100,iydim=2100,maxset=200)

      real array(ixdim,maxset), brray(ixdim,maxset),
     &    asdray(ixdim,maxset),
     &    bsdray(ixdim,maxset), crray(ixdim,iydim), title(20), cell(6)
      integer   nxyz(3), mxyz(3), nxyzst(3), asec(maxset), bsec(maxset)
      equivalence (nxyz,nx)
      common//nx,ny,nz
      character*80 afile, bfile,asdfile,bsdfile, cfile
      character dat*9, tim*8
c       
c       7/7/00 CER: remove the encode's; titlech is the temp space
c       
      character*80 titlech
      data nxyzst/0,0,0/
      data cell /0.,0.,0.,90.,90.,90./
c       
      integer*4 dfgroup,dfsubgr,dfwithin,nsum
      real*4 mswithin,msgroup,mssubgr,sswithin,sssubgr,ssgroup,rn0,
     &    rn0p,rnb0,fgroup,fpgroup,satter,dfpsubgr
      common /anova/nsum,rn0,rn0p,rnb0,dfgroup,dfsubgr,dfwithin,
     &    ssgroup,sssubgr,sswithin,msgroup,mssubgr,mswithin,fgroup,
     &    fpgroup,satter,dfpsubgr,rcrit
      real*8 g01bbf,g01cbf
c       
      parameter (limgrp=40,limsub=100)
      integer*4 n(limgrp,limsub),nb(limgrp),ntot(limgrp)
      real*4 xb(limgrp,limsub),sd(limgrp,limsub)
      fprob(ndf1,ndf2,f)=betai(0.5*ndf2,0.5*ndf1,ndf2/(ndf2+ndf1*f))
c       
c       ---------------------------------
c       --- initialize and input info ---

      print *,'This program will subtract sections of file B',
     &    ' from sections of file A,',' retaining only',
     &    ' differences that are statistically significant'
      write (*,'(1x,a,$)')'Name of average file A : '
      read  (*, '( a )' )       afile
      write (*,'(1x,a,$)')
     &    'Name of standard deviation or variance file A : '
      read  (*, '( a )' )       asdfile
      write (*,'(1x,a,$)')
     &    'Name of average file B (Return if same as A): '
      read  ( *, '( a )' )      bfile
      write (*,'(1x,a,$)')
     &    'Name of SD or var file B (Return if same as A): '
      read  ( *, '( a )' )      bsdfile


      write ( *,'(1x,a,$)')'Name of file to place differences in: '
      read  ( *, '( a )' )      cfile

      call imopen(1,afile,'ro')
      call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
      if (nx.gt.ixdim.or.iy.gt.iydim)
     &    stop '- IMAGES TOO BIG FOR ARRAYS'
      call imopen(2,asdfile,'ro')
      call irdhdr(2,nxyz,mxyz,mode,dmin,dmax,dmean)
c       
      if(bfile.ne.' ')then
        call imopen(3,bfile,'ro')
        imbavg=3
        call irdhdr(3,nxyz,mxyz,mode,dmin,dmax,dmean)
      else
        imbavg=1
      endif
c       
      if(bsdfile.ne.' ')then
        call imopen(4,bsdfile,'ro')
        imbsd=4
        call irdhdr(4,nxyz,mxyz,mode,dmin,dmax,dmean)
      else
        imbsd=2
      endif
c       
      call imopen(5,cfile,'new')
      call itrhdr(5,1)
c       
      write(*,'(1x,a,$)')'0 to weight means equally, 1 to weight'//
     &    ' them by sample size: '
      read(*,*)ifweight
c       
      write(*,'(1x,a,$)')
     &    '0 if files have SDs, 1 if they have variances: '
      read(5,*)ifvarnc
c       
      write(*,'(1x,a,$)')'Number of differences to compute: '
      read(*,*)nz
      dmin=1.e10
      dmax=-1.e10
      dsum=0.
c       
      do idiff=1,nz
        print *,'Enter list of section numbers for A sets (Ranges ok)'
        call rdlist(5,asec,nb(1))
c         
        print *,'Enter list of section numbers for B sets (Ranges ok)'
        call rdlist(5,bsec,nb(2))
c         
        do i=1,2
          write(*,'(1x,a,$)')
     &        'Number of samples in the '//char(64+i)//' averages: '
          read(*,*)(n(i,j),j=1,nb(i))
          ntot(i)=0
          do j=1,nb(i)
            ntot(i)=ntot(i)+n(i,j)
          enddo
        enddo
c         
        call dfcalc(2,nb,n)

        satter=0.
        if(dfsubgr.lt.100.and.dfsubgr.lt.2*dfwithin)then
c           write(*,'(1x,a,$)')
c           &           '1 to use Satterthwaite approximation, 0 not to: '
c           read(*,*)ifsatter
c           if(ifsatter.ne.0)then
c           
c           get criterion for Satterthwaite approximation
c           
          satter=fvalue(0.975,dfwithin,dfsubgr)
          satter=satter*fvalue(0.5,dfsubgr,dfwithin)
c           endif
        endif
c         
        write(*,'(1x,a,$)')'Desired significance level (e.g. 0.05)'//
     &      ' for setting differences to zero: '
        read(*,*)signif
        ifsigout=0
        if(signif.lt.0)then
          ifsigout=1
          signif=-signif
        endif
        fcrit0=fvalue(1.-signif,dfgroup,dfsubgr)
        if(satter.ne.0.)then
          fcritm=fvalue(1.-signif,dfgroup,dfsubgr-1)
          fcritp=fvalue(1.-signif,dfgroup,dfsubgr+1)
        endif
c         print *,satter,fcritm,fcrit0,fcritp
c         
        do ilin=1,ny
          do jj=1,nb(1)
            call imposn(1,asec(jj),ilin-1)
            call irdlin(1,array(1,jj),*96)
            call imposn(2,asec(jj),ilin-1)
            call irdlin(2,asdray(1,jj),*97)
            if(ifvarnc.ne.0)then
              do ix=1,nx
                asdray(ix,jj)=sqrt(asdray(ix,jj))
              enddo
            endif
          enddo
          do jj=1,nb(2)
            call imposn(imbavg,bsec(jj),ilin-1)
            call irdlin(imbavg,brray(1,jj),*98)
            call imposn(imbsd,bsec(jj),ilin-1)
            call irdlin(imbsd,bsdray(1,jj),*99)
            if(ifvarnc.ne.0)then
              do ix=1,nx
                bsdray(ix,jj)=sqrt(bsdray(ix,jj))
              enddo
            endif
          enddo
          do ix=1,nx
            do jj=1,nb(1)
              xb(1,jj)=array(ix,jj)
              sd(1,jj)=asdray(ix,jj)
            enddo
            do jj=1,nb(2)
              xb(2,jj)=brray(ix,jj)
              sd(2,jj)=bsdray(ix,jj)
            enddo
            call sscalc(2,nb,n,xb,sd)
            diff=0.
            fcrit=fcrit0
            if(satter.gt.0..and.rcrit.gt.satter)then
              if(dfpsubgr.lt.dfsubgr)then
                fcrit=fcrit0+(dfsubgr-dfpsubgr)*(fcritm-fcrit0)
              else
                fcrit=fcrit0+(dfpsubgr-dfsubgr)*(fcritp-fcrit0)
              endif
            endif
            if(fpgroup.ge.fcrit)then
              if(ifweight.eq.0)then
                asum=0.
                do jj=1,nb(1)
                  asum=asum+xb(1,jj)
                enddo
                bsum=0.
                do jj=1,nb(2)
                  bsum=bsum+xb(2,jj)
                enddo
                diff=asum/nb(1)-bsum/nb(2)
              else
                asum=0.
                do jj=1,nb(1)
                  asum=asum+n(1,jj)*xb(1,jj)
                enddo
                bsum=0.
                do jj=1,nb(2)
                  bsum=bsum+n(2,jj)*xb(2,jj)
                enddo
                diff=asum/ntot(1)-bsum/ntot(2)
              endif
              if(ifsigout.eq.1)then
                if(satter.gt.0..and.rcrit.gt.satter)then
                  intdfp=dfpsubgr
                  pbelow=fprob(dfgroup,intdfp,fpgroup)
                  pabove=fprob(dfgroup,intdfp+1,fpgroup)
                  pgroup=pbelow+(pabove-pbelow)*(dfpsubgr-intdfp)
                else
                  pgroup=fprob(dfgroup,dfsubgr,fgroup)
                endif
                diff=sign(alog10(pgroup),diff)
              endif
            endif
            crray(ix,ilin)=diff
          enddo
        enddo
c         
        call irepak(crray,crray,ixdim,iydim,0,nx-1,0,ny-1)
        call iclden(crray,nx,ny,1,nx,1,ny,tmin,tmax,tmean)
        dmin=min(dmin,tmin)
        dmax=max(dmax,tmax)
        dsum=dsum+tmean
c         
        call iwrsec(5,crray)
      enddo
c       
      cell(1) = float(nx)
      cell(2) = float(ny)
      cell(3) = float(nz)
      call ialsiz(5,nxyz,nxyzst)
      call ialsam(5,nxyz)
      call ialcel(5,cell)
      call date(dat)
      call time(tim)
c       
c       7/7/00 CER: remove the encodes
c       
c       encode ( 80, 3000, title ) dat, tim
      write(titlech,3000) dat,tim
      read(titlech,'(20a4)')(title(kti),kti=1,20)
      dmean=dsum/nz
      call iwrhdr(5,title,1,dmin,dmax,dmean)

      call imclose(5)

      call exit(0)
96    continue
97    continue
98    continue
99    stop 'error reading file'
3000  format ( 'SUBIMSTAT: Subtract section B from section A.',t57,
     &    a9, 2x, a8 )

      end

