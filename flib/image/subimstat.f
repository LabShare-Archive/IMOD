*       * * * * SUBIMSTAT * * * * *
c       
c       SUBIMSTAT subtracts one average image from another and uses
c       standard deviation or variance images to find the statistical
c       significance of the difference at each pixel, as evaluated by a t-
c       statistic.  It then sets to zero all differences that are less
c       significant than the specified level of significance.  It can also
c       output pixel values reflecting the level of significance rather than
c       the difference.
c       
c       See man page for details.
c       David Mastronarde  1/27/90
c
c       $Id$
c       Log at end of file
c       
      implicit none
      integer limlist
      parameter (limlist = 100000)
      real*4 cell(6), title(20)
      real*4, allocatable :: array(:), brray(:),asdray(:), bsdray(:), crray(:)
      integer   nxyz(3), mxyz(3), nxyzst(3), asec, bsec, nxyzb(3), maxarr,nx,ny,nz
      integer*4 listsecs(limlist), nxyzsda(3), nxyzsdb(3), nza, nlist
      equivalence (nxyz,nx)
      common//nx,ny,nz
      character*320 afile, bfile,asdfile,bsdfile, cfile
      character dat*9, tim*8
      character*80 titlech
      integer*4 i, mode,imbavg,imbsd,ifvarnc,iflist,idiff,nsa,nsb,ntdf,ifsigout,ifail
      real*4 dmin,dmax,dmean,psignif,tcrit,varfac,tcritfac,diff,denomcrit,denom,absdiff
      real*4 tt, pval,tmin,tmax,tmean,t,dsum
      integer*4 nsam1,nsbm1,kti,ndf
c       
      data nxyzst/0,0,0/
      data cell /0.,0.,0.,90.,90.,90./
      real*4 tprob,betai,tvalue
      tprob(ndf,t)=1.-betai(0.5*ndf,0.5,ndf/(ndf+t**2))/2.

c       ---------------------------------
c       --- initialize and input info ---
      call setExitPrefix('ERROR: SUBIMSTAT - ')

      print *,'This program will subtract a section of file B',
     &    ' from a section of file A,',' retaining only',
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
      maxarr = nx * ny
      nza = nz
      allocate(array(maxarr), brray(maxarr),asdray(maxarr), bsdray(maxarr),
     &    crray(maxarr), stat = i)
      call memoryError(i, 'ARRAYS FOR IMAGE DATA')
      call imopen(2,asdfile,'ro')
      call irdhdr(2,nxyzsda,mxyz,mode,dmin,dmax,dmean)
      if (nxyzsda(1) .ne. nx .or. nxyzsda(2) .ne. ny)
     &    call exitError('ALL IMAGES MUST BE THE SAME SIZE IN X AND Y')
c       
      if(bfile.ne.' ')then
        call imopen(3,bfile,'ro')
        imbavg=3
        call irdhdr(3,nxyzb,mxyz,mode,dmin,dmax,dmean)
        if (nxyzb(1) .ne. nx .or. nxyzb(2) .ne. ny)
     &      call exitError('ALL IMAGES MUST BE THE SAME SIZE IN X AND Y')
      else
        imbavg=1
        nxyzb = nxyz
      endif
c       
      if(bsdfile.ne.' ')then
        call imopen(4,bsdfile,'ro')
        imbsd=4
        call irdhdr(4,nxyzsdb,mxyz,mode,dmin,dmax,dmean)
        if (nxyzsdb(1) .ne. nx .or. nxyzsdb(2) .ne. ny)
     &      call exitError('ALL IMAGES MUST BE THE SAME SIZE IN X AND Y')
      else
        imbsd=2
        nxyzsdb = nxyzsda
      endif
c       
      call imopen(5,cfile,'new')
      call itrhdr(5,1)
c       
      write(*,'(1x,a,$)') '0 if files have SDs, 1 if they have variances: '
      read(5,*)ifvarnc
c       
      write(*,'(1x,a,/a,$)')'Enter 0 to specify each pair of sections separately,',
     &    '   or 1 to do a list of corresponding sections all the same way: '
      read(5,*)iflist
      if (iflist .eq. 0) then
        write(*,'(1x,a,$)')'Number of differences to compute: '
        read(*,*)nz
      else 
        do i = 1, nz
          listsecs(i) = i - 1
        enddo
        nlist = nz
        write(*,'(1x,a,$)')'Enter list of sections to do, or / for all: '
        call rdlist2(5, listsecs, nlist, limlist)
        nz = nlist
      endif
      dmin=1.e10
      dmax=-1.e10
      dsum=0.
c       
      do idiff=1,nz
20      if (iflist .eq. 0) then
          write(*,'(1x,a,$)') 'Section #''s for A and B, to compute (A-B): '
          read(*,*)asec,bsec
        else
          asec = listsecs(idiff)
          bsec = asec
        endif
        if (asec .lt. 0 .or. asec .ge. nza .or. asec .gt. nxyzsda(3) .or.
     &      bsec .lt. 0 .or. bsec .ge. nxyzb(3) .or. bsec .gt. nxyzsdb(3)) then
          write(*,'(a,i6,a,i6,a)')'Section number', asec, ' for A or', bsec,
     &        ' is out of range in one of the input files'
          if (iflist .eq. 0) goto 20
          call exitError('SECTION NUMBER OUT OF RANGE FOR ONE OF THE FILES')
        endif
c         
        if (iflist .eq. 0 .or. idiff .eq. 1) then
          write(*,'(1x,a,$)') 'Number of samples in averages for A and B: '
          read(*,*)nsa,nsb
c           
          ntdf=nsa+nsb-2
          write(*,'(1x,a,$)')'Significance level (e.g. 0.05) for '//
     &        'setting differences to zero: '
          read(*,*)psignif
          ifsigout=0
          if(psignif.lt.0)then
            ifsigout=1
            psignif=-psignif
          endif
          ifail=0
          tcrit=tvalue(1.-psignif,ntdf)  
          write(*,'(a,f8.3,a,i5,a)')' T criterion is',tcrit,' with',
     &        ntdf,' degrees of freedom'
        endif
c
        call imposn(1,asec,0)
        call irdsec(1,array,*99)
        call imposn(2,asec,0)
        call irdsec(2,asdray,*99)
        call imposn(imbavg,bsec,0)
        call irdsec(imbavg,brray,*99)
        call imposn(imbsd,bsec,0)
        call irdsec(imbsd,bsdray,*99)
        if(ifvarnc.eq.0)then
c           print *,'squaring arrays'
          do i=1,nx*ny
            asdray(i)=asdray(i)**2
            bsdray(i)=bsdray(i)**2
          enddo
        endif
c         
c         ooh- this is pretty convoluted now.  The test was whether
c         abs of diff / sqrt (varfac * (nsam1*sd1**2+nsam2*sd2**2) was
c         less than tcrit.  So instead, see if diff**2 is .lt. tcrit**2
c         times the denominator (the argument of the sqrt)  Quicker, doesn't
c         blow up.
c         
        varfac=(1./nsa+1./nsb)/ntdf
        tcritfac=tcrit**2*varfac
        nsam1=nsa-1
        nsbm1=nsb-1
        do i=1,nx*ny
          diff=array(i)-brray(i)
          denomcrit=tcritfac*(nsam1*asdray(i)+nsbm1*bsdray(i))
          if(diff**2.lt.denomcrit)then
            diff=0.
          elseif(ifsigout.eq.1)then
            denom=sqrt(max(0.,
     &          varfac*(nsam1*asdray(i)+nsbm1*bsdray(i))))
            absdiff=abs(diff)
            tt=absdiff/(max(0.05*absdiff,denom))
            pval=max(0.000001,1.-tprob(ntdf,tt))
            diff=sign(alog10(pval),diff)
          endif
          crray(i)=diff
        enddo
c         
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
      write(titlech,3000) dat,tim
      read(titlech,'(20a4)')(title(kti),kti=1,20)
      dmean=dsum/nz
      call iwrhdr(5,title,1,dmin,dmax,dmean)

      call imclose(5)

      call exit(0)
99    call exitError('READING FILE')
3000  format ( 'SUBIMSTAT: Subtract section B from section A.',t57,
     &    a9, 2x, a8 )

      end

c       $Log$
c
c       4/23/90 - have program look up criterion t value.
c       4/12/95 - use local subroutines for t-value
