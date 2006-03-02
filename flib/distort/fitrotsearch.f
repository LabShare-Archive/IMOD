c       FITROTSEARCH fits a 3-dimensional polynomial to the difference measures
c       output by Matchrotpairs and reports the minimum of the fit.
c       
c       See man page for details
c
c       David Mastronarde, 3/2/06
c       
c       $Author$
c
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c
      implicit none
      integer idim
      parameter (idim=1000)
      include 'statsize.inc'
      real*4 xr(msiz,idim), sx(msiz), xm(msiz), sd(msiz)
     &    , ss(msiz,msiz), ssd(msiz,msiz), d(msiz,msiz), r(msiz,msiz)
     &    , b(msiz), b1(msiz)
c
      real*4 freinp(100)
      real*4 av(idim),bv(idim),rot(idim),diff(idim)
      integer*4 numeric(100)
      character*160 filnam
      character*1024 line
c
      integer*4 ndat, nplus, iscl, nfields, nfit, norder, nindep, nav, nbv
      integer*4 i, j, k, ierr, ivaStart, ivaEnd, ivbStart, ivbEnd, ivamin
      integer*4 ivbmin
      real*4 avmin, avmax, bvmin, bvmax, diffmin, avatmin, bvatmin, avsrch
      real*4 bvsrch, difsum, c1, rsq, fra
c
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetTwoIntegers, PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  fitrotsearch
c       
      integer numOptions
      parameter (numOptions = 5)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@aviews:AViewsStartAndEnd:IP:@'//
     &    'bviews:BViewsStartAndEnd:IP:@order:OrderOfPolynomial:I:@'//
     &    'help:usage:B:'
c
      norder = 0
      ivaStart = 0
      ivaEnd = 10000
      ivbStart = 0
      ivbEnd = 10000
c
      call PipReadOrParseOptions(options, numOptions, 'fitrotsearch',
     &    'ERROR: FITROTSEARCH - ', .false., 1, 1, 0, numOptArg,
     &    numNonOptArg)

      if (PipGetInOutFile('InputFile', 1, ' ', filnam) .ne. 0)
     &    call errorexit('NO INPUT FILE SPECIFIED')
      ierr = PipGetTwoIntegers('AViewsStartAndEnd', ivaStart, ivaEnd)
      ierr = PipGetTwoIntegers('BViewsStartAndEnd', ivbStart, ivbEnd)
      ierr = PipGetInteger('OrderOfPolynomial', norder)
      if (norder .ne. 0 .and. (norder .lt. 2 .or. norder .gt. 4))
     &    call errorexit('ORDER MUST BE IN RANGE FROM 2 TO 4')

      call dopen(1, filnam, 'old', 'f')
      ndat = 0
      nplus = 0
10    read(1, '(a)', err=40, end=20) line
      call frefor2(line, freinp, numeric, nfields, 100)
      iscl = nfields / 4
      if ((line(1:1) .eq. 'A' .and. nfields / 2 .eq. 4 .and. numeric(2) +
     &    numeric(4) + numeric(6) + numeric(8) .eq. 4 .and.
     &    abs(abs(freinp(6)) - 90.) .lt.0.1) .or.
     &    (nfields .eq. 4 .and. numeric(1) + numeric(2) + numeric(3) +
     &      numeric(4) .eq. 4 .and. abs(abs(freinp(3)) - 90.) .lt.0.1)) then
        ndat = ndat + 1
        av(ndat) = freinp(1 * iscl)
        bv(ndat) = freinp(2 * iscl)
        rot(ndat) = freinp(3 * iscl)
        diff(ndat) = freinp(4 * iscl)
        if (rot(ndat) .gt. 0) nplus = nplus + 1
      endif
      go to 10
20    nfit = 0
      avmin = 10000
      avmax = -10000
      bvmin = 10000
      bvmax = -10000
      diffmin = 1.e20
      do i = 1, ndat
        if ((rot(i) .gt. 0 .and. nplus .ge. ndat / 2 .or. rot(i) .lt. 0 .and.
     &      nplus .lt. ndat / 2) .and.
     &      nint(av(i)) .ge. ivaStart .and. nint(av(i)) .le. ivaEnd .and.
     &      nint(bv(i)) .ge. ivbStart .and. nint(bv(i)) .le. ivbEnd) then
          nfit = nfit + 1
          av(nfit) = av(i)
          bv(nfit) = bv(i)
          rot(nfit) = rot(i)
          diff(nfit) = diff(i)
          avmin=min(avmin, av(nfit))
          bvmin=min(bvmin, bv(nfit))
          avmax=max(avmax, av(nfit))
          bvmax=max(bvmax, bv(nfit))
          if (diff(nfit) .lt. diffmin) then
            diffmin = diff(nfit)
            avatmin = av(nfit)
            bvatmin = bv(nfit)
          endif
        endif
      enddo

      ivamin = nint(avatmin)
      ivbmin = nint(bvatmin)
      write(*, 101)diffmin, ivamin, ivbmin
101   format('Minimum in data = ', f12.0,'  at A',i2,'    B',i2)
      if (norder .eq. 0) then
        norder = 3
        if (nfit .le. 25) norder = 2
      endif
      nindep=norder*(norder+3)/2                !# of independent variables
      do i=1,nfit
        call polytermReal(av(i),bv(i),norder,xr(1,i))
        xr(nindep+1,i)=diff(i)
      enddo
      call multr(xr,nindep+1,nfit,sx,ss,ssd,d,r,xm,sd,b,b1, c1, rsq ,fra)
      write(*,100) nfit, nindep, norder, rsq
100   format(i3,' measurements in fit to',i3,' variables (order',i2,
     &    '), R squared =',f7.4)

      diffmin = 1.e20
      nav = 10. * (avmax -avmin)
      nbv = 10. * (bvmax -bvmin)
      do i = 0, nav
        avsrch = avmin + 0.1 * i
        do j = 0, nbv
          bvsrch = avmin + 0.1 * i
          call polytermReal(avsrch, bvsrch, norder, freinp)
          difsum = c1
          do k = 1, nindep
            difsum = difsum + b1(k) * freinp(k)
          enddo
          if (difsum .lt. diffmin) then
            diffmin = difsum
            avatmin = avsrch
            bvatmin = bvsrch
          endif
        enddo
      enddo

      write(*, 102)diffmin, avatmin, bvatmin
102   format('Minimum in fit  = ', f12.0,'  at A',f4.1,'  B',f4.1)
      if (ivamin .ne. nint(avatmin) .or. ivbmin .ne. nint(bvatmin))
     &    write(*, 103)nint(avatmin), nint(avatmin),nint(bvatmin),nint(bvatmin)
103   format('You should rerun matchrotpairs with -za ',i1,',',i1,' and -zb ',
     &    i1,',',i1)
      call exit(0)
40    call errorexit('Reading file')
      end

        
      subroutine errorexit(message)
      character*(*) message
      print *
      print *,'ERROR: FITROTSEARCH - ',message
      call exit(1)
      end
