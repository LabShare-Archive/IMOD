*       * * * * CHECKXFORMS * * * * * *
c       
c       CHECKXFORMS will examine a list of transformations to determine the
c       maximum image rotation and the maximum image displacement specified
c       by those transforms.  It reports the maximum rotation in degrees and
c       the section number at which that maximum occurs, and the maximum
c       displacement in pixels and the section number at which that maximum
c       occurs.
c       
c       After starting the program, just enter the names of desired transform
c       files, one after another.  Type Return to exit the program when done.
c       
c       David Mastronarde 1/2/92
c       
      real*4 f(2,3,1000)
      character*80 file
      print *,'Enter names of files of transforms one after another;',
     &    '   Just type Return to exit when done'
      write(*,101)
101   format(/,22x,'# of      maximum     at        maximum      at',/
     &    ,20x,'sections    rotation  section    X/Y offset  section')
10    read(5,'(a)')file
      if(file.eq.' ')stop
      close (1)
      open(1,file=file,status='old',err=98)
      call xfrdall(1,f,nf,*99)
      dxymax=0.
      rotmax=0.
      isecdxy=0
      isecrot=0
      do i=1,nf
        call amat_to_rotmagstr(f(1,1,i),theta,smag,str,phi)
        if(abs(theta).gt.rotmax)then
          rotmax=abs(theta)
          isecrot=i-1
        endif
        do j=1,2
          if(abs(f(j,3,i)).gt.dxymax)then
            dxymax=abs(f(j,3,i))
            isecdxy=i-1
          endif
        enddo
      enddo
      write(*,102)nf,rotmax,isecrot,dxymax,isecdxy
102   format(i25,f13.1,i8,f14.1,i9)
      go to 10
98    print *,'error opening file'
      go to 10
99    print *,'error reading transforms'
      go to 10
      end
