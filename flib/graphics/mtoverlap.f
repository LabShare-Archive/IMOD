*       * * * * * MTOVERLAP * * * * *
c       
c       MTOVERLAP allows one to display sets of "bundles" of microtubules
c       (MT's) and to compute overlap between MT's coming from the two
c       different directions.  It has a lot of flexibility but one can select
c       a default, standard display format fairly easily.
c       See man page for details.
c       
c       David Mastronarde  10/3/90
c       2/21/92: changes to scale data into microns, add line fits
c       5/1/92: implemented simple distance-dependent overlap
c       6/9/92: implemented mapping of types
c       11/5/94: fixed interset and intergraph spacing, aligned interleaves
c       .        at the bottom of each set to obviate need to invert drawing
c       6/14/96: added plotting output
c       4/28/97: changes for IMOD models
c       
c       $Id$
c       
      call plax_initialize('mtoverlap')
      call exit(0)
      end

      subroutine realgraphicsmain()
      parameter (limbun=200,limmt=10000,limdsp=2000,limtyp=60)
      parameter (limpt=1000000,limgrf=60)
      real*4 zstrt(limmt),zend(limmt),center(limbun),datdisp(3,limdsp)
      integer*4 itype(limmt),indbundle(limbun),ninbundle(limbun)
      integer*4 listbund(limbun),itypcena(limtyp),itypcenb(limtyp)
      integer*4 itypovra(limtyp),itypovrb(limtyp),itypdisp(limtyp)
      integer*4 icoldisp(limtyp),iorder(limtyp),ipos(limtyp)
      integer*4 indtyp(limtyp,limgrf),nintyp(limtyp,limgrf)
      integer*4 itypset(limtyp,limbun),igrfbund(limbun),lstmp(limbun)
      integer*4 ntypset(limtyp),numov(4,limbun),listcalc(limbun)
      integer*4 maxinset(limtyp,limgrf)
      real*4 ovsum(4,limbun),ovsumsq(4,limbun),xmt(limpt),ymt(limpt)
      real*4 zmt(limpt),tiltzstrt(2000),remapz(2000),costilt(2000)
      integer*4 indxymt(limmt),nfitslp(3),ntilts(limbun),ifile(limbun)
      integer*4 mapfrom(limtyp),mapto(limtyp),indtlt(limbun)
      real*4 slopinv(3),fitpast(3),zptscal(limbun),xyscal(limbun)

      logical changeone,typeonlist,outopen,doslope,drawtwice,interlace
      logical doplot
      character*80 modelfile,comfile
      character*4 dummy4
      character*80 xlabel
c       
      zscalin=-1280.
      ytotin=-1024.
      spacrelin=.5
      linthickin=1
      marglft=25
      margrt=25
      margtop=15
      margbot=40
      margax=20
      ioutnm=6
      ioutov=6
      outopen=.false.
      doslope=.false.
      pctlo=20.
      pcthi=80.
      icolax=251
      icollab=251
      icolfit=252
      idxtxt=0
      idytxt=0
      isiztxt=8
      majorlen=15
      minorlen=7
      iaxthick=1
      labthick=1
      majormod=5
      labelmod=10
      yinterset=2
      yintergrf=5
      doplot=.false.
      pltsizx=7.5
      pltsizy=6.
      pltofsx=0.
      pltofsy=1.
      ifpage=0
c       
      call plax_open()
      call build_lut(0,240)
c       
      call opencomfile
      call getmappings(mapfrom,mapto,nmap)
c       
c       initialize list of bundles
c       
10    nbundles=0
      indfree=0
      indxyfree=0
      modelfile=' '
      nfile=0
      indtlt(1)=1
c       
c       get list of bundles
c       
15    write(*,'(1x,a,$)')'# of bundles to specify (or 0 to take'//
     &    ' specifications from a file): '
      read(5,*)nbunspec
16    inunit=5
      if(nbunspec.le.0)then
17      write(*,*)'Enter name of file with specifications, or'//
     &      ' Return to take input from keyboard'
        read(5,'(a)')comfile
        if(comfile.eq.' ')go to 15
        close(4)
c         
c         7/20/00 CER remove shared, readonly for gnu
c         
        open(4,file=comfile,status='old',err=17)
        inunit=4
        read(4,*)nbunspec
      endif
c       
      do ibun=1,nbunspec
        write(*,'(a,i4)')' Enter specifications for bundle #',
     &      nbundles+1
        nfp=nfile+1
        intl=indtlt(nfp)
        call read_model(modelfile,inunit,xyscal(nfp),zptscal(nfp),
     &      tiltzstrt(intl),remapz(intl),costilt(intl),
     &      ntilts(nfp),nfile)
        if(nfile.eq.nfp)indtlt(nfile+1)=indtlt(nfile)+ntilts(nfile)
        intl=indtlt(nfile)
        call get_bundle(zstrt,zend,itype,indbundle,ninbundle,
     &      nbundles,indfree,xmt,ymt,zmt,indxymt,indxyfree,inunit,
     &      xyscal(nfile),zptscal(nfile),tiltzstrt(intl),
     &      remapz(intl),costilt(intl),ntilts(nfile))
        ifile(nbundles)=nfile
      enddo
c       
      changeone=.false.
      write(*,'(1x,a,/,a,$)')'Enter 0 to specify everything, or 1'//
     &    ' (for IMOD models) or 2 (for WIMP models)',
     &    ' to use defaults for types, colors, size: '
      read(5,*)ifdefault
c       
c       specify centering of bundles
c       
20    print *,'Enter list of bundles to display or calculate overlap'
     &    ,' for (ranges OK)'
      call rdlist(5,listbund,nlistbund)
c       
c       revise list of bundles to be legal
c       
      ibun=1
      do while (ibun.le.nlistbund)
        if(listbund(ibun).le.0.or.listbund(ibun).gt.nbundles)then
          do imov=ibun+1,nlistbund
            listbund(imov-1)=listbund(imov)
          enddo
          nlistbund=nlistbund-1
        else
          ibun=ibun+1
        endif
      enddo
c       
      write(* ,117)
117   format(' Enter 1 to have each bundle''s center computed',
     &    ' separately,',/,'       2 to have one center computed',
     &    ' for all bundles together,',/,
     &    '       3 to specify one center for all bundles, or',/,
     &    '       4 to have combinations of centers specified/',
     &    'computed together/separate')
      read(5,*)icenopt
c       
      if(icenopt.le.3)then
        if(icenopt.le.1)then
          cenval=0.
        elseif(icenopt.eq.2)then
          cenval=-1.
        elseif(icenopt.eq.3)then
          write(*,'(1x,a,$)')
     &        'Center Z value (a section #) for all bundles: '
          read(5,*)cenval
        endif
        do ibun=1,nlistbund
          center(ibun)=cenval
        enddo
      else
        write(*,118)
118     format(' Enter one value per bundle: either a specific center',
     &      ' Z value as a section #,',/,'  or the negative of a',
     &      ' specific center Z value in microns,',/,
     &      '  or 0 to have the center ',
     &      'computed separately for that bundle,',/,'  or - a number',
     &      ' >100 to have center computed together with other'//
     &      ' bundles',/, '   having the same negative number')
        read(5,*)(center(i),i=1,nlistbund)
      endif
c       
c       get types to use for calculating center
c       
      if(ifdefault.le.0)then
        print *,'Enter list of types coming from low Z to base center',
     &      ' calculation on'
        call rdlist(5,itypcena,ntypcena)
        print *,'Enter list of types coming from high Z to base center'
     &      ,' calculation on'
        call rdlist(5,itypcenb,ntypcenb)
      else
        ntypcena=1
        ntypcenb=1
        if(ifdefault.eq.1)then
          itypcena(1)=1
          itypcenb(1)=2
        else
          itypcena(1)=6
          itypcenb(1)=5
        endif
      endif
c       
c       scan through list to find ones that need center calculation, but
c       first convert the ones that don't from section #'s to Z values
c       
      do ibun=1,nlistbund
        ifi=ifile(listbund(ibun))
        intl=indtlt(ifi)
        if(center(ibun).gt.0.)center(ibun)=scalez(center(ibun),
     &      zptscal(ifi),tiltzstrt(intl),remapz(intl),
     &      costilt(intl),ntilts(ifi))
        if(center(ibun).lt.0.and.center(ibun).gt.-100.)
     &      center(ibun)=-center(ibun)
      enddo
      do ibun=1,nlistbund
        thiscen=center(ibun)
        ifi=ifile(listbund(ibun))
        intl=indtlt(ifi)
        if(thiscen.le.0.)then
          aendsum=0.
          naend=0
          bstrtsum=0.
          nbstrt=0
          ibcend=ibun
          if(thiscen.lt.0.)ibcend=nlistbund
          do ibcalc=ibun,ibcend
            if(center(ibcalc).eq.thiscen)then
c               
c               if this bundle is one to include in calculation
c               
              indstrt=indbundle(listbund(ibcalc))
              indend=indstrt+ninbundle(listbund(ibcalc))-1
              do ind=indstrt,indend
                if(typeonlist(ityperemap(itype(ind),mapfrom,mapto,
     &              nmap),itypcena,ntypcena))then
                  aendsum=aendsum+zend(ind)
                  naend=naend+1
                endif
                if(typeonlist(ityperemap(itype(ind),mapfrom,mapto,
     &              nmap),itypcenb,ntypcenb))then
                  bstrtsum=bstrtsum+zstrt(ind)
                  nbstrt=nbstrt+1
                endif
              enddo
            endif
          enddo
c           
c           calculate center value and store it for bundles included in calc
c           
          cenval=(aendsum/naend+bstrtsum/nbstrt)/2.
          ntmp=0
          do ibcalc=ibun,ibcend
            if(center(ibcalc).eq.thiscen)then
              center(ibcalc)=cenval
              ntmp=ntmp+1
              lstmp(ntmp)=ibcalc
            endif
          enddo
c           
c           convert cenval back to Z index
c           
          zz=ifix(1.7*cenval/zptscal(ifi))
          do while(zz.ge.1..and.scalez(zz,zptscal(ifi),
     &        tiltzstrt(intl),remapz(intl),costilt(intl),
     &        ntilts(ifi)) .gt.cenval)
            zz=zz-1
          enddo
          zabove=scalez(zz+1,zptscal(ifi),tiltzstrt(intl),
     &        remapz(intl),costilt(intl),ntilts(ifi))
          zbelow=scalez(zz,zptscal(ifi),tiltzstrt(intl),
     &        remapz(intl),costilt(intl),ntilts(ifi))
          zz=zz+(cenval-zbelow)/(zabove-zbelow)
          write(*,107)cenval,zz,(lstmp(i),i=1,ntmp)
107       format(' Center =',f8.3,' (section',f7.2,
     &        ') for bundles:',(8i4))
        endif
      enddo
      if(changeone)go to 45
c       
c       specify types to compute overlap from
c       
30    if(ifdefault.eq.0)then
        print *,'Enter list of types coming from low Z to compute',
     &      ' overlap from'
        call rdlist(5,itypovra,ntypovra)
        print *,'Enter list of types coming from high Z to compute',
     &      ' overlap from'
        call rdlist(5,itypovrb,ntypovrb)
      else
        ntypovra=1
        ntypovrb=1
        if(ifdefault.eq.1)then
          itypovra(1)=1
          itypovrb(1)=2
        else
          itypovra(1)=6
          itypovrb(1)=5
        endif
      endif
      itypslpst=itypovrb(1)
      itypslpnd=itypovra(1)
      write(*,'(1x,a,/,a,/,a,/,a,$)')'Enter 0 for simple overlap '//
     &    'calculation independent of distance in X/Y,',
     &    '       1 to count only MTs within a certain distance'//
     &    ' in X/Y,','       2 for 3-D with inverse power decay with'
     &    //' distance in X/Y,',
     &    '    or 3 for 3-D with exponential decay: '
      read(5,*)iovertype
      if(iovertype.gt.0)then
        write(*,'(1x,a,$)')'0 to compute average or 1 to compute '//
     &      'sum of overlap quantities per MT: '
        read(5,*)ifoversum
        write(*,'(1x,a,/,a,$)')'Enter distance in X/Y plane below'//
     &      ' which overlap quantity will equal 1',
     &      '  (typically this should be the preferred MT spacing): '
        read(5,*)rzero
        rnull=rzero
        if(iovertype.gt.1)then
          write(*,'(1x,a,$)')'Distance beyond which overlap will'//
     &        ' be counted as zero: '
          read(5,*)rnull
          if(iovertype.eq.2)then
            write(*,'(1x,a,$)')'Power to apply (a positive integer): '
            read(5,*)ipower
          else
            write(*,'(1x,a,$)')'Space constant for exponential decay: '
            read(5,*)xlambda
          endif
        endif
      else
        ifoversum=0
      endif
      if(changeone)go to 60
c       
c       specify types for display and how to display them
c       
40    if(ifdefault.eq.0)then
        print *,'Enter list of types to display (ranges ok)'
        call rdlist(5,itypdisp,ntypdisp)
        if(ntypdisp.gt.0)then
          do i=1,ntypdisp
            icoldisp(i)=256-abs(itypdisp(i))
          enddo
          print *,'Enter list of colors to display as, or / for',
     &        ' standard colors (ranges OK)'
          call rdlist(5,icoldisp,ncoldisp)
          ncoldisp=ntypdisp
c           
          write(*,119)
119       format(' For each type, enter a positive number to order',
     &        ' from low to high',/,
     &        '    or negative number to order from high to low.',/,
     &        ' Enter: 1 or -1 to order by starts, 2 or -2 to',
     &        ' order by ends,',/,' or 3 or -3 to order by lengths.')
          read(5,*)(iorder(i),i=1,ntypdisp)
c           
          write(*,120)
120       format(' Enter position (numbered from top down) in which',
     &        ' to plot each type;',/, ' (types with the same',
     &        ' position number will be interleaved)')
          read(5,*)(ipos(i),i=1,ntypdisp)
          call reduce_list(ipos,ntypdisp,ndispsets)
        endif
      else
        ntypdisp=4
        if(ifdefault.eq.1)then
          itypdisp(1)=4
          itypdisp(2)=1
          itypdisp(3)=2
          itypdisp(4)=3
        else
          itypdisp(1)=1
          itypdisp(2)=6
          itypdisp(3)=5
          itypdisp(4)=4
        endif
        icoldisp(1)=255
        icoldisp(2)=250
        icoldisp(3)=251
        icoldisp(4)=252
        iorder(1)=3
        iorder(2)=2
        iorder(3)=-1
        iorder(4)=3
        ipos(1)=1
        ipos(2)=2
        ipos(3)=2
        ipos(4)=3
        ndispsets=3
      endif
      if(changeone)go to 60
c       
c       specify graphs to plot bundles in
c       
45    write(*,'(1x,a,/,a,$)')'Enter 1 to plot all bundles in same'//
     &    ' graph,',' 2 to plot each bundle in separate graph, or'//
     &    ' 3 to specify a combination: '
      read(5,*)igrfopt
      if(igrfopt.le.1)then
        do i=1,nlistbund
          igrfbund(i)=1
        enddo
      elseif(igrfopt.eq.2)then
        do i=1,nlistbund
          igrfbund(i)=i
        enddo
      else
        print *,
     &      'Enter graph # (numbered from top down) for each bundle'
        read(5,*)(igrfbund(i),i=1,nlistbund)
      endif
      call reduce_list(igrfbund,nlistbund,ngrftot)
      if(changeone)go to 60
c       
c       specify size of display and spacings factor
c       
50    if(ifdefault.eq.0)then
        if(ntypdisp.gt.0)then
          write(*,'(1x,a,/,a,f9.2,a,$)')'Enter either minus the total'
     &        //' X size of display in pixels','     or the # of '//
     &        'pixels per unit of Z (/ for',zscalin,'): '
          read(5,*)zscalin
          write(*,'(1x,a,/,a,f9.2,a,$)')'Enter either minus the total'
     &        //' Y size of display in pixels','     or the # of '//
     &        'pixels for interleaved types (/ for',ytotin,'): '
          read(5,*)ytotin
          write(*,'(1x,a,/,a,/,a,/,a,f6.3,5i3,a,$)')'Enter line'//
     &        ' spacing in interleave relative to spacing in'//
     &        ' non-interleaved','  regions (enter negative value'//
     &        ' to draw from bottom up),'
     &        ,'  line, axis, and label thickness, and '//
     &        'major and minor tick lengths','  (/ for ',spacrelin,
     &        linthickin,iaxthick,labthick,majorlen, minorlen,'): '
          read(5,*)spacrelin,linthickin,iaxthick,labthick,majorlen,
     &        minorlen
          write(*,'(1x,a,/,a,/,a,3i4,5i3,a,$)')'Enter colors for '//
     &        'axes, labels, and fitted lines, label size, extra '//
     &        'amounts',' to shift labels left and down, intervals'//
     &        ' at which to have major ticks and labels',' (/ for ',
     &        icolax,icollab,icolfit,isiztxt,idxtxt,idytxt,majormod,
     &        labelmod,'): '
          read(5,*)icolax,icollab,icolfit,isiztxt,idxtxt,idytxt,
     &        majormod,labelmod
        endif
      endif
c       
c       display bundles
c       
60    if(ntypdisp.gt.0)then
        call plax_box(0,0,0,1279,1023)
c         
c         for each type, build list of starts and ends relative to center
c         
        linthick=linthickin
        drawtwice=linthickin.eq.2
        if(drawtwice)linthick=1
        spacrel=abs(spacrelin)
        idirdraw=sign(1.,spacrelin)
        inddisp=0
        zrelmin=1.e10
        zrelmax=-1.e10
        ninterlv=0
        noninterlv=0
        suminterlv=0.
        ntotsets=0
        do igrf=1,ngrftot
          do itd=1,ntypdisp
            indtyp(itd,igrf)=inddisp+1
            do ibun=1,nlistbund
              if(igrfbund(ibun).eq.igrf)then
                indstrt=indbundle(listbund(ibun))
                indend=indstrt+ninbundle(listbund(ibun))-1
                do ind=indstrt,indend
                  if(ityperemap(itype(ind),mapfrom,mapto,nmap)
     &                .eq.itypdisp(itd))then
                    inddisp=inddisp+1
                    datdisp(1,inddisp)=zstrt(ind)-center(ibun)
                    datdisp(2,inddisp)=zend(ind)-center(ibun)
                    datdisp(3,inddisp)=zend(ind)-zstrt(ind)
                    zrelmin=min(zrelmin,datdisp(1,inddisp))
                    zrelmax=max(zrelmax,datdisp(2,inddisp))
                  endif
                enddo
              endif
            enddo
            nintyp(itd,igrf)=1+inddisp-indtyp(itd,igrf)
c             
c             now need to order them by whatever
c             
            idir=-sign(1,iorder(itd))
            indor=max(1,min(3,abs(iorder(itd))))
            do ii=indtyp(itd,igrf),inddisp-1
              do jj=ii+1,inddisp
c                 
c                 DNM 3/22/01: on the PC with optimization 2 or 3, the
c                 original simple if test failed; so switch to explicit
c                 compound test.
c                 
                if((idir.gt.0.and.
     &              datdisp(indor,ii).lt.datdisp(indor,jj)).or.
     &              (idir.lt.0.and.
     &              datdisp(indor,ii).gt.datdisp(indor,jj)))then
c                   if(idir*(datdisp(indor,ii)-datdisp(indor,jj)).lt.0.)
c                   &                 then
                  do kk=1,3
                    tmp=datdisp(kk,ii)
                    datdisp(kk,ii)=datdisp(kk,jj)
                    datdisp(kk,jj)=tmp
                  enddo
                endif
              enddo
            enddo
          enddo
c           
c           now make table of display positions
c           
          do iset=1,ndispsets
            ninset=0
            maxset=0
            sumset=0.
            do itd=1,ntypdisp
              if(ipos(itd).eq.iset)then
                ninset=ninset+1
                itypset(iset,ninset)=itd
                maxset=max(maxset,nintyp(itd,igrf))
                sumset=sumset+nintyp(itd,igrf)
              endif
            enddo
            ntypset(iset)=ninset
            maxinset(iset,igrf)=maxset
            if(maxset.gt.0)then
              ntotsets=ntotsets+1
              if(ninset.eq.1)then
                noninterlv=noninterlv+maxset-1
              else
                ninterlv=ninterlv+maxset*ninset-1
c                 if(maxset.eq.nintyp(ntypdisp,igrf))ninterlv=ninterlv+1
                suminterlv=suminterlv+sumset
              endif
            endif
          enddo
        enddo
c         
c         compute scaling, make axes
c         
c         ngapunits=noninterlv+ngrftot*ndispsets+nint(spacrel*ninterlv)
c         &           +3*(ngrftot-1)
        ngapunits=noninterlv+yinterset*(ntotsets-ngrftot)+
     &      nint(spacrel*ninterlv)+yintergrf*(ngrftot-1)
        if(ytotin.lt.0..or.suminterlv.eq.0.)then
          nytot=min(1024.,-ytotin)
          spacenon=float(nytot-margtop-majorlen-margbot-margax)/
     &        ngapunits
        else
          spacenon=min((1024.-margtop-majorlen-margbot-margax)/
     &        ngapunits,ytotin/(spacrel*suminterlv))
          nytot=spacenon*ngapunits+margtop+majorlen+margbot+margax
        endif
        spaceinter=spacrel*spacenon
        if(suminterlv.gt.0.)then
          yinter=suminterlv*spaceinter
          write(*,'(a,f7.2,a)')' Average vertical extent of'//
     &        ' interleaved region is',yinter,' pixels'
          if(ytotin.gt.0..and.ytotin-yinter.gt.1.)
     &        print *,'This extent is less than the amount requested or'
     &        //' the amount in the last display'
          ytotin=yinter
        endif
c         
        if(zscalin.lt.0.)then
          nxtot=min(1280.,-zscalin)
        else
          nxtot=min(1280.,zscalin*(zrelmax-zrelmin)+margrt+marglft)
        endif
        zscale=(nxtot-margrt-marglft)/(zrelmax-zrelmin)
        zadd=marglft+640-nxtot/2-zscale*zrelmin
        write(*,'(a,f7.2,a)')' Horizontal scaling is',zscale,
     &      ' pixels per unit of Z'
        if(zscalin.gt.0..and.(zscalin-zscale)/zscale.gt.2./nxtot)
     &      print *,'This scale is less than the amount requested or'
     &      //' the amount in the last display'
        zscalin=zscale
c         
        if(zptscal(ifile(listbund(1))).eq.1.)then
          izaxfac=1
          labmod=5
          if(zrelmax-zrelmin.gt.60.)labmod=10
        else
          izaxfac=10
          labmod=labelmod
        endif
        jsize=1.2*isiztxt
        iyaxtop=511+nytot/2
        iyaxbot=512-nytot/2+margbot
        print *,zrelmin,zrelmax,izaxfac
c         ix0=zrelmin*zscale+zadd
c         ix1=zrelmax*zscale+zadd
        ix0=-int(-izaxfac*zrelmin)*zscale/izaxfac + zadd
        ix1=int(izaxfac*zrelmax)*zscale/izaxfac + zadd
        yfree=iyaxtop-margtop-majorlen
        if(idirdraw.lt.0)yfree=iyaxbot+margax
        call plax_vectw(iaxthick,icolax,ix0,iyaxtop,ix1,iyaxtop)
        call plax_vectw(iaxthick,icolax,ix0,iyaxbot,ix1,iyaxbot)
        if(doplot)then
          call psSetup(iaxthick,c1,c2,c3,0)
          yaxbot=iyaxbot*pltscly+pltofsy
          yaxtop=iyaxtop*pltscly+pltofsy
          x0=ix0*pltsclx+pltofsx
          x1=ix1*pltsclx+pltofsx
          call psMoveAbs(x0,yaxbot)
          call psVectAbs(x1,yaxbot)
          call psMoveAbs(x0,yaxtop)
          call psVectAbs(x1,yaxtop)
        endif
        do iz=-int(-izaxfac*zrelmin),int(izaxfac*zrelmax)
          ixx=iz*zscale/izaxfac+zadd
          ilen=minorlen
          if(mod(iz,majormod).eq.0)then
            ilen=majorlen
            if(mod(iz,labmod).eq.0)then
              write(dummy4,'(i4)')iz/izaxfac
              call plax_sctext(labthick,isiztxt,isiztxt,icollab,
     &            ixx-46-idxtxt,512-nytot/2-idytxt,dummy4)
              if(doplot)then
                call int_iwrite(dummy4,iz/izaxfac,nchr)
                xlab=ixx*pltsclx+pltofsx
                ylab=(512-nytot/2-idytxt)*pltscly+pltofsy+.005*jsize
                call psWriteText(xlab,ylab,dummy4(1:nchr),jsize,0,0)
              endif
            endif
          endif
          if(ilen.gt.0)then
            call plax_vectw(iaxthick,icolax,ixx,iyaxtop,ixx,iyaxtop-ilen)
            call plax_vectw(iaxthick,icolax,ixx,iyaxbot,ixx,iyaxbot-ilen)
            if(doplot)then
              ylen=ilen*pltscly
              xx=ixx*pltsclx+pltofsx
              call psMoveAbs(xx,yaxtop)
              call psVectAbs(xx,yaxtop+ylen)
              call psMoveAbs(xx,yaxbot)
              call psVectAbs(xx,yaxbot-ylen)
            endif
          endif
        enddo
        if(doplot.and.xlabel.ne.' ')then
          lbllen=len_trim(xlabel)
          xlab=(0.5*(zrelmin+zrelmax)*zscale+zadd)*pltsclx+pltofsx
          ylab=(512-nytot/2-idytxt)*pltscly+pltofsy-.012*jsize
          call psWriteText(xlab,ylab,xlabel(1:lbllen),jsize,0,0)
        endif
c         
c         finally, plot the data sets
c         
        if(doslope)then
          do iun=6,ioutov
            write(iun,104)
104         format(' Graph   Slope (% tubes/unit of Z)   50%-point',
     &          ' past center     # tubes in fit',/,'          ',
     &          'starts    ends     mean    starts   ends    mean',
     &          '   starts  ends  total')
          enddo
        else
          do iun=6,ioutnm
            write(iun,102)
102         format(' Graph Type   #   mean - length - s.d.')
          enddo
        endif
        if(doplot)call psSetup(linthick,c1,c2,c3,0)
        do igrf=1,ngrftot
          do i=1,3
            nfitslp(i)=0
            slopinv(i)=0.
            fitpast(i)=0.
          enddo
          do iset=1,ndispsets
            ydel=spacenon
            if(ntypset(iset).gt.1)ydel=ntypset(iset)*spaceinter
            ymin=yfree
            do itinset=1,ntypset(iset)
              itd=itypset(iset,itinset)
              yy=yfree-idirdraw*((itinset-1)*spaceinter+
     &            (maxinset(iset,igrf)-nintyp(itd,igrf))*ydel)
              indstr=indtyp(itd,igrf)
              sumlen=0.
              sumsq=0.
              indslp=0
              nslp=0
              if(doslope)then
                if(itypdisp(itd).eq.itypslpst)indslp=1
                if(itypdisp(itd).eq.itypslpnd)indslp=2
              endif
              do ind=indstr,indstr+nintyp(itd,igrf)-1
                xx0=datdisp(1,ind)*zscale+zadd
                ix0=xx0
                xx1=datdisp(2,ind)*zscale+zadd
                ix1=xx1
                iyy=yy
                call plax_vectw(linthick,icoldisp(itd),ix0,iyy,ix1,iyy)
                if(drawtwice)
     &              call plax_vectw(1,icoldisp(itd),ix0,iyy-1,ix1,iyy-1)
                if(doplot)then
                  yplt=yy*pltscly+pltofsy
                  call psMoveAbs(xx0*pltsclx+pltofsx,yplt)
                  call psVectAbs(xx1*pltsclx+pltofsx,yplt)
                endif
                sumlen=sumlen+datdisp(3,ind)
                sumsq=sumsq+datdisp(3,ind)**2
                if(indslp.ne.0)then
                  ypct=100.*(ind+0.5-indtyp(itd,igrf))
     &                /nintyp(itd,igrf)
                  if(ypct.ge.pctlo.and.ypct.le.pcthi)then
                    nslp=nslp+1
                    xmt(indxyfree+nslp)=datdisp(indslp,ind)
                    ymt(indxyfree+nslp)=ypct
                    yyend=iyy
                    if(nslp.eq.1)yystr=iyy
                  endif
                endif
                ymin=idirdraw*min(idirdraw*ymin,idirdraw*yy)
                yy=yy-idirdraw*ydel
              enddo
              if(doslope)then
                if(nslp.gt.1)then
                  call lsfit(ymt(indxyfree+1),xmt(indxyfree+1),nslp,
     &                slop,bint,ro)
                  xxstr=(ymt(indxyfree+1)*slop+bint)*zscale+zadd
                  xxend=(ymt(indxyfree+nslp)*slop+bint)*zscale+zadd
                  ixstr=xxstr
                  ixend=xxend
                  iystr=yystr
                  iyend=yyend
                  call plax_vectw(linthick,icolfit,ixstr,iystr,ixend,
     &                iyend)
                  if(doplot)then
                    call psMoveAbs(xxstr*pltsclx+pltofsx,yystr*pltscly+
     &                  pltofsy)
                    call psVectAbs(xxend*pltsclx+pltofsx,yyend*pltscly+
     &                  pltofsy)
                  endif
                  nfitslp(indslp)=nslp
                  slopinv(indslp)=1/max(abs(slop),1./9999.)
                  fitpast(indslp)=(2*indslp-3)*(50.*slop+bint)
                endif
              else
                call sums_to_avgsd(sumlen,sumsq,nintyp(itd,igrf),
     &              avglen, sdlen)
                do iun=6,ioutnm
                  write(iun,101)igrf,itypdisp(itd),nintyp(itd,igrf),
     &                avglen, sdlen
101               format(i4,i7,i5,2f10.3)
                enddo
              endif
            enddo
            if(maxinset(iset,igrf).gt.0)
     &          yfree=ymin-idirdraw*yinterset*spacenon
c             if(idirdraw*ymin.le.idirdraw*yfree)
c             &           yfree=ymin-idirdraw*2*spacenon
          enddo
          yfree=yfree-idirdraw*(yintergrf-yinterset)*spacenon
          if(doslope)then
            if(nfitslp(1).gt.0.and.nfitslp(2).gt.0)then
              nfitslp(3)=nfitslp(1)+nfitslp(2)
              slopinv(3)=(slopinv(1)+slopinv(2))/2.
              fitpast(3)=(fitpast(1)+fitpast(2))/2.
            endif
            do iun=6,ioutov
              write(iun,105)igrf,(slopinv(i),i=1,3),
     &            (fitpast(i),i=1,3),(nfitslp(i),i=1,3)
105           format(i4,3x,3f9.2,3f8.2,2x,3i6)
            enddo
          endif
        enddo
        call plax_flush()
      endif
c       
c       calculate overlap factors
c       
      if(ntypovra*ntypovrb.gt.0.and..not.doslope)then
        do ibun=1,nlistbund
          do i=1,3
            ovsum(i,ibun)=0.
            ovsumsq(i,ibun)=0.
            numov(i,ibun)=0
          enddo
          indstrt=indbundle(listbund(ibun))
          indend=indstrt+ninbundle(listbund(ibun))-1
          do ind=indstrt,indend
            if(typeonlist(ityperemap(itype(ind),mapfrom,mapto,nmap)
     &          ,itypovra,ntypovra))then
c               
c               if it's a tube from left, calculate how far its end is
c               past center
c               
              past=zend(ind)-center(ibun)
              ovsum(1,ibun)=ovsum(1,ibun)+past
              ovsumsq(1,ibun)=ovsumsq(1,ibun)+past**2
              numov(1,ibun)=numov(1,ibun)+1
              ntmp=0
              stmp=0.
c               
c               look for tubes that overlap it and compute an overlap
c               quantity
c               
              do indb=indstrt,indend
                if((indb.ne.ind).and.typeonlist(ityperemap(itype(indb)
     &              ,mapfrom,mapto,nmap),itypovrb,ntypovrb))then
                  zovstrt=max(zstrt(ind),zstrt(indb))
                  zovend=min(zend(ind),zend(indb))
                  overlap=zovend-zovstrt
                  if(overlap.gt.0.)then
                    if(iovertype.gt.0)then
                      ipta=indxymt(ind)
                      iptb=indxymt(indb)
                      do while (zmt(ipta).lt.zovstrt)
                        ipta=ipta+1
                      enddo
                      do while (zmt(iptb).lt.zovstrt)
                        iptb=iptb+1
                      enddo
                      delz=overlap
                      overlap=0.
                      do while (zmt(ipta).lt.zovend .and.
     &                    ipta.lt.indxymt(ind+1))
                        if(ipta+1.lt.indxymt(ind+1))
     &                      delz=zmt(ipta+1)-zmt(ipta)
                        if(iptb+1.lt.indxymt(indb+1))
     &                      delz=zmt(iptb+1)-zmt(iptb)
                        delx=xmt(ipta)-xmt(iptb)
                        if(delx.le.rnull)then
                          dely=ymt(ipta)-ymt(iptb)
                          if(dely.le.rnull)then
                            rr=sqrt(delx**2+dely**2)
                            if(rr.le.rnull)then
                              if(iovertype.eq.1)then
                                overlap=overlap+delz
                              else
                                relr=max(1.,rr/rzero)
                                if(iovertype.eq.2)then
                                  overlap=overlap+delz*(1./relr)**ipower
                                else
                                  overlap=overlap+delz*exp((1.-relr)/
     &                                xlambda)
                                endif
                              endif
                            endif
                          endif
                        endif
                        ipta=ipta+1
                        iptb=iptb+1
                      enddo
                    endif
                    ntmp=ntmp+1
                    stmp=stmp+overlap
                  endif
                endif
              enddo
              if(ntmp.gt.0)then
                if(ifoversum.gt.0)ntmp=1
                numov(2,ibun)=numov(2,ibun)+1
                ovsum(2,ibun)=ovsum(2,ibun)+stmp/ntmp
                ovsumsq(2,ibun)=ovsumsq(2,ibun)+(stmp/ntmp)**2
              endif
            endif
c             
            if(typeonlist(ityperemap(itype(ind),mapfrom,mapto,nmap)
     &          ,itypovrb,ntypovrb))then
              past=center(ibun)-zstrt(ind)
              ovsum(1,ibun)=ovsum(1,ibun)+past
              ovsumsq(1,ibun)=ovsumsq(1,ibun)+past**2
              numov(1,ibun)=numov(1,ibun)+1
              ntmp=0
              stmp=0.
              do inda=indstrt,indend
                if(inda.ne.ind.and.typeonlist(ityperemap(itype(inda),
     &              mapfrom,mapto,nmap),itypovra,ntypovra))then
                  zovstrt=max(zstrt(ind),zstrt(inda))
                  zovend=min(zend(ind),zend(inda))
                  overlap=zovend-zovstrt
                  if(overlap.gt.0.)then
                    if(iovertype.gt.0)then
                      ipta=indxymt(inda)
                      iptb=indxymt(ind)
                      do while (zmt(ipta).lt.zovstrt)
                        ipta=ipta+1
                      enddo
                      do while (zmt(iptb).lt.zovstrt)
                        iptb=iptb+1
                      enddo
                      delz=overlap
                      overlap=0.
                      do while (zmt(ipta).lt.zovend .and.
     &                    ipta.lt.indxymt(inda+1))
                        if(ipta+1.lt.indxymt(inda+1))
     &                      delz=zmt(ipta+1)-zmt(ipta)
                        if(iptb+1.lt.indxymt(ind+1))
     &                      delz=zmt(iptb+1)-zmt(iptb)
                        delx=xmt(ipta)-xmt(iptb)
                        if(delx.le.rnull)then
                          dely=ymt(ipta)-ymt(iptb)
                          if(dely.le.rnull)then
                            rr=sqrt(delx**2+dely**2)
                            if(rr.le.rnull)then
                              if(iovertype.eq.1)then
                                overlap=overlap+delz
                              else
                                relr=max(1.,rr/rzero)
                                if(iovertype.eq.2)then
                                  overlap=overlap+delz*(1./relr)**ipower
                                else
                                  overlap=overlap+delz*exp((1.-relr)/
     &                                xlambda)
                                endif
                              endif
                            endif
                          endif
                        endif
                        ipta=ipta+1
                        iptb=iptb+1
                      enddo
                    endif
                    ntmp=ntmp+1
                    stmp=stmp+overlap
                  endif
                endif
              enddo
              if(ntmp.gt.0)then
                if(ifoversum.gt.0)ntmp=1
                numov(3,ibun)=numov(3,ibun)+1
                ovsum(3,ibun)=ovsum(3,ibun)+stmp/ntmp
                ovsumsq(3,ibun)=ovsumsq(3,ibun)+(stmp/ntmp)**2
              endif
            endif
          enddo
          numov(4,ibun)=numov(2,ibun)+numov(3,ibun)
          ovsum(4,ibun)=ovsum(2,ibun)+ovsum(3,ibun)
          ovsumsq(4,ibun)=ovsumsq(2,ibun)+ovsumsq(3,ibun)
        enddo
      endif
c       
      if(.not.doslope)then
        do iun=6,ioutov
          write(iun,103)
103       format(' bundle Dist. past center    Left overlap      ',
     &        'Right overlap      Total overlap'/,'   #',
     &        4('    mean  S.D.   #'))
        enddo
        do ibun=1,nlistbund
          call calcmeans(ovsum,ovsumsq,numov,ibun,1,ioutov,zptscal)
          listcalc(ibun)=ibun
        enddo
        call calcmeans(ovsum,ovsumsq,numov,listcalc,nlistbund,ioutov,
     &      zptscal)
      endif
c       
70    write(*,121)
121   format(' Enter 1 to combine computations for subsets of',
     &    ' bundles,',' 2 to set display size,',/,
     &    '       3 to put bundles in graphs,',
     &    ' 4/5 to specify types to display/compute,',/,
     &    '       6 to specify bundles to work with,',
     &    ' 7 to control output to file',/,
     &    '       8 or 9 to read in bundles (8 to add to existing',
     &    ' ones, 9 to replace them)',/,
     &    '      10 to take commands from file,',
     &    ' 11 to exit, 12 to fit lines to endpoints,',/,
     &    '      13 to change mapping of types,',
     &    ' 14/15/16 to do/view/print plot')
      read(5,*)iopt
      ifdefault=0
      changeone=.true.
      doslope=.false.
      doplot=.false.
      go to (80,50,45,40,30,20,95,15,10,90,99,85,88,98,197,197),iopt
      go to 70
80    print *,'Enter list of bundle numbers to combine calculations for'
      call rdlist(5,listcalc,nlist)
      call calcmeans(ovsum,ovsumsq,numov,listcalc,nlist,ioutov,zptscal)
      go to 70
90    call opencomfile
      go to 70
95    if(.not.outopen)then
        print *,'Enter name of output file to store overlap values in '
     &      ,'(Return for none)'
        read(5,'(a)')modelfile
        if(modelfile.ne.' ')then
          call dopen(7,modelfile,'new','f')
          outopen=.true.
        endif
      endif
c       
      if(outopen)then
        write(*,'(1x,a,/,a,$)')'Enter 0 for no output to file, 1'//
     &      ' to output overlap values only,','       2 to output'//
     &      ' numbers of MT''s only, or 3 to output both: '
        read(5,*)ioptout
        ioutnm=6
        ioutov=6
        if(ioptout.eq.1.or.ioptout.ge.3)ioutov=7
        if(ioptout.ge.2)ioutnm=7
      endif
      go to 70
c       
85    write(*,'(1x,a,/,a,2i4,a,$)')'Enter the type for which to fit a'
     &    //' line to starting points, and the type for',
     &    '    which to fit a line to the ending points (/ for',
     &    itypslpst,itypslpnd,'): '
      read(5,*)itypslpst,itypslpnd
      write(*,'(1x,a,/,a,2f6.1,a,$)')'Enter lower and upper '//
     &    'percentiles of the MTs to include in the line fit,',
     &    '     going down from the top of the graph (/ for',
     &    pctlo,pcthi,'): '
      read(5,*)pctlo,pcthi
      doslope=.true.
      go to 60
c       
88    call getmappings(mapfrom,mapto,nmap)
      go to 70
c       
c       97      interlace=.not.interlace
c       if(interlace)then
c       call p_ldphvi(9,33,30,360,3,3,16,480,9)
c       call p_pan(330,752)
c       else
c       call p_ldphv(6,9,12,81,1,4,37,1024,1)
c       call p_pan(0,1023)
c       endif
c       call plax_flush()
c       go to 70
c       
98    write(*,'(1x,a,/,a,/,a,4f7.2,i4,a,$)') 'Enter X and Y size'
     &    //' and lower left X,Y (in inches) of area on paper',
     &    ' corresponding to full screen, and 0 or 1 for same'//
     &    ' or new page',' [/ for',pltsizx,pltsizy, pltofsx,pltofsy,
     &    ifpage,']: '
      read(5,*)pltsizx,pltsizy,pltofsx,pltofsy,ifpage
      pltsclx=pltsizx/1280.
      pltscly=pltsizy/1024.
      doplot = .true.
      print *,'Enter text label for axis, or Return for none'
      read(5,'(a)')xlabel
      if(ifpage.ne.0)call psFrame()
      ifpage=1
      go to 60
c       
197   call pltout(16-iopt)
      ifpage=0
      go to 70
c       
99    call plax_box(0,0,0,1279,1023)
      call plax_flush()
      call plax_close()
      call psexit
      end





      subroutine read_model(modelfile,inunit,xyscal,zscal,tiltzstrt,
     &    remapz,costilt,ntilts,nfile)
      character*(*) modelfile
      real*4 tiltzstrt(*),remapz(*),costilt(*)
      character*50 newfile,tiltfile
      logical exist,readw_or_imod
      integer getimodhead
c       
91    if(inunit.eq.5.and.modelfile.ne.' ')then
        write(*,'(1x,a,$)')
     &      'Name of model file (or Return for same as last): '
      elseif(inunit.eq.5)then
        write(*,'(1x,a,$)')'Name of model file: '
      endif
      read(inunit,'(a)')newfile
      if(modelfile.ne.' '.and.newfile.eq.' ')return
c       
75    exist=readw_or_imod(newfile)
      if(.not.exist)then
        modelfile=' '
        go to 91
      else
        call scale_model(0)
        nfile=nfile+1
        modelfile=newfile
        write(*,'(1x,a,$)')
     &      'Name of file of tilt info (Return if none): '
        read(inunit,'(a)')tiltfile
        if(tiltfile.ne.' ')then
          call dopen(3,tiltfile,'ro','f')
          ntilts=0
3         i=ntilts+1
          read(3,*,end=5)tiltzstrt(i),costilt(i)
          ntilts=i
          costilt(ntilts)=cosd(costilt(ntilts))
          go to 3
5         remapz(1)=tiltzstrt(1)
          close(3)
          do i=1,ntilts-1
            remapz(i+1)=remapz(i)+(tiltzstrt(i+1)-tiltzstrt(i))
     &          /costilt(i)
          enddo
        endif
c         
        defscal=1.e6
        ierr=getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip)
        if(ierr.eq.0.and.abs(xyscal-defscal)/defscal.gt.1.e-5)then
          write(*,'(a,f10.6,a)')' Scale set from model header at',
     &        xyscal,' microns/pixel'
          zscal=xyscal*zscale
          return
        endif
        secthick=1000.
        write(*,'(1x,a,$)')'Nominal section thickness in nm, or / for'
     &      //' no scaling of Z values: '
        read(inunit,*)secthick
        zscal=secthick/1000.
c         
        xmag=1.
        umperpix=1.
        write(*,'(1x,a,/,a,$)')'Enter magnification of negatives, '//
     &      'and scale at which negatives were digitized',
     &      '   (microns/pixel from VIDS), or / for no '//
     &      'scaling of X/Y values: '
        read(inunit,*)xmag,umperpix
c         
        xyscal=umperpix/xmag
        return
      endif
      end



      subroutine get_bundle(zstrt,zend,itype,indbundle,ninbundle,
     &    nbundles,indfree,xmt,ymt,zmt,indxymt,indxyfree,inunit,
     &    xyscal, zscal,tiltzstrt, remapz,costilt,ntilts)
c       
      real*4 zstrt(*),zend(*),xmt(*),ymt(*),zmt(*)
      integer*4 itype(*),indbundle(*),ninbundle(*),indxymt(*)
      real*4 tiltzstrt(*),remapz(*),costilt(*)
      include 'model.inc'
      real*4 bx(500),by(500)
      logical*1 notgot(max_obj_num)
      logical looking,inside
c       
10    if(inunit.eq.5)write(*,'(1x,a,/,a,$)')'Enter 0 to take all'//
     &    ' objects in model, or enter the number of limiting regions'
     &    ,'   (rectangular areas or boundary contours) to specify: '
      read(inunit,*)ncoords
      do i=1,max_mod_obj
        notgot(i)=.true.
      enddo
      indbase=indfree
      do icoord=1,max(1,ncoords)
        xlo=-1.e10
        xhi=1.e10
        ylo=xlo
        yhi=xhi
        zlo=-1.e10
        zhi=1.e10
        nvert=0
        if(ncoords.gt.0)then
          if(inunit.eq.5)write(*,'(1x,a,/,a,$)')'Enter IMOD Object'//
     &        ' # and contour # of boundary contour,',
     &        '  or WIMP object # AND 0, or 0,0 to enter X/Y'//
     &        ' coordinate limits: '
          read(inunit,*)iobjboundin,icontbound
          nvert=0
          if(iobjboundin.le.0)then
            if(inunit.eq.5)write(*,'(1x,a,$)')'Lower & upper X, lower &'
     &          //' upper Y limits, or / for no limits: '
            read(inunit,*)xlo,xhi,ylo,yhi
            if(inunit.eq.5)write(*,'(1x,a,$)')'Lower and upper Z '//
     &          'limits of region, or / for no limits: '
            read(inunit,*)zlo,zhi
          else
            iobjbound=iobjfromcont(iobjboundin,icontbound)
            if(iobjbound.eq.0)then
              print *,'Non-existent object'
              go to 10
            endif
            if(npt_in_obj(iobjbound).lt.3)then
              print *,'Not enough points in that object'
              go to 10
            endif
c             
c             extract object
c             
            zz=p_coord(3,abs(object(ibase_obj(iobjbound)+1)))
            do i=1,npt_in_obj(iobjbound)
              ipnt=abs(object(ibase_obj(iobjbound)+i))
              if(p_coord(3,ipnt).ne.zz)then
                print *,'Object not all in one Z plane'
                go to 10
              endif
              if(nvert.eq.0 .or. p_coord(1,ipnt).ne.bx(max(1,nvert))
     &            .or. p_coord(2,ipnt).ne.by(max(1,nvert)))then
                nvert=nvert+1
                bx(nvert)=p_coord(1,ipnt)
                by(nvert)=p_coord(2,ipnt)
              endif
            enddo
            bx(nvert+1)=bx(1)
            by(nvert+1)=by(1)
c             
            zlo=zz-0.01
            zhi=zz+0.01
            if(inunit.eq.5)write(*,'(1x,a,$)')'Lower and upper Z '//
     &          'limits of region, or / for Z value of contour only: '
            read(inunit,*)zlo,zhi
          endif
        endif
c         
        do iobj=1,max_mod_obj
          looking=notgot(iobj)
          if(npt_in_obj(iobj).gt.1)then
            if(abs(p_coord(3,abs(object(ibase_obj(iobj)+1)))-
     &          p_coord(3,abs(object(ibase_obj(iobj)+2)))).lt.0.01)
     &          looking=.false.
          endif
          i=1
          do while(looking.and.i.le.npt_in_obj(iobj))
            ipnt=abs(object(ibase_obj(iobj)+i))
            if(p_coord(3,ipnt).ge.zlo.and.p_coord(3,ipnt).le.zhi)then
              if(nvert.eq.0)then
                looking=p_coord(1,ipnt).lt.xlo.or.p_coord(1,ipnt).gt.xhi
     &              .or.p_coord(2,ipnt).lt.ylo.or.p_coord(2,ipnt).gt.yhi
              else
                looking=.not.
     &              inside(bx,by,nvert,p_coord(1,ipnt),p_coord(2,ipnt))
              endif
              if(.not.looking)then
                indfree=indfree+1
                zstrtmp=p_coord(3,abs(object(ibase_obj(iobj)+1)))
                zendtmp=p_coord(3,abs(object(ibase_obj(iobj)+
     &              npt_in_obj(iobj))))
                itype(indfree)=256-obj_color(2,iobj)
                if(obj_color(1,iobj).eq.0)itype(indfree)=-itype(indfree)
c                 
c                 save object x/y coordinates
c                 
                indxymt(indfree)=indxyfree+1
                izst=nint(zstrtmp)
                iznd=nint(zendtmp)
c                 
c                 5/30/96 changed from -0.5 on start, +0.5 on end to 0 and 1
c                 so that entered center values would be correct
c                 
                zstrt(indfree)=scalez(zstrtmp,zscal,tiltzstrt,
     &              remapz,costilt,ntilts)
                zend(indfree)=scalez(zendtmp+1.0,zscal,tiltzstrt,
     &              remapz,costilt,ntilts)
                ii=1
                do iz=izst,iznd
                  do while(nint(p_coord(3,abs(object(ibase_obj(iobj)
     &                +ii)))).lt.iz.and.ii.lt.npt_in_obj(iobj))
                    ii=ii+1
                  enddo
                  ipnt=abs(object(ibase_obj(iobj)+ii))
                  indxyfree=indxyfree+1
                  if(nint(p_coord(3,ipnt)).eq.iz)then
                    xmt(indxyfree)=xyscal*p_coord(1,ipnt)
                    ymt(indxyfree)=xyscal*p_coord(2,ipnt)
                    ii=min(ii+1,npt_in_obj(iobj))
                  else
                    iplast=abs(object(ibase_obj(iobj)+ii-1))
                    zfac=float(iz-nint(p_coord(3,iplast)))/
     &                  (nint(p_coord(3,ipnt))-nint(p_coord(3,iplast)))
                    xmt(indxyfree)=xyscal*(zfac*p_coord(1,ipnt)+
     &                  (1.-zfac)*p_coord(1,iplast))
                    ymt(indxyfree)=xyscal*(zfac*p_coord(2,ipnt)+
     &                  (1.-zfac)*p_coord(2,iplast))
                  endif
                  zmt(indxyfree)=scalez(float(iz),zscal,tiltzstrt,
     &                remapz,costilt,ntilts)
                enddo
              endif
            endif
            i=i+1
          enddo
          notgot(iobj)=looking
        enddo
      enddo
c       
      nbundles=nbundles+1
      indbundle(nbundles)=indbase+1
      ninbundle(nbundles)=indfree-indbase
      print *,ninbundle(nbundles),' objects in bundle'
      indxymt(indfree+1)=indxyfree+1
      return
      end


c       SCALEZ will scale the Z index coordinate ZZ by first remapping
c       the Z values via the tilt remappings, then by multiplying by ZSCAL
c       
      function scalez(zz,zscal,tiltzstrt,remapz,costilt,ntilts)
      real*4 tiltzstrt(*),remapz(*),costilt(*)
      scalez=zz
      if(ntilts.gt.0)then
        if(zz.ge.tiltzstrt(1))then
          itilt=ntilts
          do while(zz.lt.tiltzstrt(itilt))
            itilt=itilt-1
          enddo
          scalez=remapz(itilt)+(zz-tiltzstrt(itilt))/costilt(itilt)
        endif
      endif
      scalez=scalez*zscal
      return
      end


c       OPENCOMFILE requests a command file name and either opens that file
c       for input instead of keyboard input, or restores keyboard input
c       if no file name is entered, or if end of file or error occurs.
c       
      subroutine opencomfile
      character*50 comfile
      logical istty/.true./
      save istty
      write(*,*) 'Enter name of file with commands,',
     &    ' or Return for input from keyboard'
      read(5,'(a)',err=10,end=10)comfile
      if(comfile.ne.' ')go to 20
10    if(istty)return
      comfile='/dev/tty'
20    close(5)
c       
c       7/20/00 CER remove shared, readonly for open
c       
      open(5,file=comfile,status='old',err=10)
      istty=comfile.eq.'/dev/tty'
      return
      end


      logical function typeonlist(itype,ityplist,ntyplist)
      integer*4 ityplist(*)
      typeonlist=.true.
      do i=1,ntyplist
        if(itype.eq.ityplist(i))return
      enddo
      typeonlist=.false.
      return
      end


      subroutine calcmeans(ovsum,ovsumsq,numov,listcalc,nlist,iout,
     &    zptscal)
      real*4 ovsum(4,*),ovsumsq(4,*)
      integer*4 numov(4,*),listcalc(*),nn(4)
      real*4 avg(4),sd(4)
      character*20 fmt
c       
      if(nlist.eq.1)then
        ibun=listcalc(1)
      else
        ibun=-nlist
      endif
c       
      do icol=1,4
        sum=0.
        sumsq=0.
        nn(icol)=0
        do il=1,nlist
          ind=listcalc(il)
          sum=sum+ovsum(icol,ind)
          sumsq=sumsq+ovsumsq(icol,ind)
          nn(icol)=nn(icol)+numov(icol,ind)
        enddo
        call sums_to_avgsd(sum,sumsq,nn(icol),avg(icol),sd(icol))
      enddo
      fmt='(i3,4(f9.1,f6.1,i4))'
      if(zptscal.ne.1.)fmt='(i3,4(f9.2,f6.2,i4))'
      do iun=6,iout
        write(iun,fmt)
     &      ibun,(avg(i),sd(i),nn(i),i=1,4)
      enddo
      return
      end


      subroutine reduce_list(list,nlist,ntot)
      integer*4 list(*),ltmp(100)
      ntot=0
      do inval=-999,999
        ifany=0
        do i=1,nlist
          if(list(i).eq.inval)then
            if(ifany.eq.0)then
              ifany=1
              ntot=ntot+1
            endif
            ltmp(i)=ntot
          endif
        enddo
      enddo
      do i=1,nlist
        list(i)=ltmp(i)
      enddo
      return
      end


      subroutine getmappings(mapfrom,mapto,nmap)
      integer*4 mapfrom(*),mapto(*)
      nto=nmap
      print *,'Enter list of types to change into new types, or',
     &    ' Return for no changes'
      call rdlist(5,mapfrom,nmap)
      if(nmap.eq.0)return
      print *,'Enter list of types to change them into'
      ngot=0
10    call rdlist(5,mapto(ngot+1),nto)
      ngot=ngot+nto
      if(ngot.ge.nmap)return
      print *,'Enter',nmap-ngot,' more types to complete the list'
      go to 10
      end
      

      function ityperemap(itype,mapfrom,mapto,nmap)
      integer*4 mapfrom(*),mapto(*)
      ityperemap=itype
      do i=1,nmap
        if(itype.eq.mapfrom(i))ityperemap=mapto(i)
      enddo
      return
      end


c       *build_lut*************************************************************
c       
c       set up look-up tables to cover intensity range imin-imax as 0-255
c       this effectively allows use of imax+1-255 for cursor and overlay
c       
      subroutine build_lut(imin,jmax)
c       
      character*10 string
      data maxval/244/
c       
      imax = min(maxval,jmax)
      del = imax - imin
      if (del .eq. 0) del = 1.
      scl = 255./del
      val = 0
      do j = 0,imin-1
        call plax_mapcolor(j,0,0,0)
      enddo
      do j = imax+1,maxval
        call plax_mapcolor(j,255,255,255)
      enddo
      do j = imin,imax
        ival = val + .5
        call plax_mapcolor(j,ival,ival,ival)
        val = val + scl
      enddo
      call plax_mapcolor(245,255,255,255)
      call plax_mapcolor(246,255,255,255)
      call plax_mapcolor(247,229,209,95)
      call plax_mapcolor(248,138,131,3)
      call plax_mapcolor(249,239,125,0)
      call plax_mapcolor(250,255,0,0)
      call plax_mapcolor(251,0,255,0)
      call plax_mapcolor(252,0,0,255)
      call plax_mapcolor(253,255,255,0)
      call plax_mapcolor(254,255,0,255)
      call plax_mapcolor(255,0,255,255)
      call plax_flush()
      return
      end

      function trnc(xx)
c       real*4 safe2(3)/.35,1.2,2.65/
      trnc = xx
c       idot=nint(xx*upi)
c       trnc=(3*(idot/3) + safe2(mod(idot,3)+1))/upi
      return
      end
