*       * * * * AVGANOVA * * * * *
c       
c       AVGANOVA will do statistical comparisons, using nested analysis of
c       variance, on the output of the program IMAVGSTAT.
c       
c       This output consists of mean, standard deviation, and standard error
c       of the mean for all of the summing areas in a series of different
c       data sets.  The summing areas were derived from a set of summing
c       regions specified by a WIMP model; each summing region was divided
c       into one or more summing areas.
c       
c       To set up a comparison, you designate one collection of data sets
c       as Group 1, and another collection of data sets as Group 2.  (The
c       ANOVA requires that each group contain more than one data set.)
c       You then specify a collection of summing regions.  The ANOVA will
c       be run separately on each summing area within those regions.
c       More generally, you can do a multiple comparison of differences among
c       more than 2 Groups.
c       
c       Each of the data sets included in a comparison may be rescaled
c       independently; i.e. a particular linear scaling may be applied to all
c       of the areas in a data set, a different scaling may be applied to
c       all areas in another set, etc.  It is also possible to apply the
c       same scaling, or the same form of scaling, to all data sets without
c       entering values for each set separately.  Scaling may be specified
c       in four ways:  1) One may directly specify a factor to multiply by
c       and a factor to add.  4)  One may specify that the values for a set
c       are all to be divided by the value for a specified area of that set.
c       3) One may specify that a given set should have its values shifted
c       (without any multiplication) so that the mean of a particular
c       collection of summing regions matches the corresponding mean for
c       some other data set.  4) One may do a least-squares linear
c       regression between the data points of the set being scaled and the
c       corresponding data points of some other set, and use the
c       coefficients of the regression to determine the scaling factors.
c       The data points used for regression are the means from the summing
c       areas within a particular collection of summing regions.
c       
c       Entries to the program are now described in order as they are first
c       encountered.  After doing one comparison, one may loop back to a
c       variety of different points in order to change different parameters.
c       
c       Name of statistics file output by IMAVGSTAT
c       
c       Number of groups of sets
c       
c       List of numbers of the sets to include in Group 1.  Sets are
c       .  numbered from 1.  You can enter ranges separated by commas,
c       .  e.g. 1-3,7-9
c       
c       List of numbers of the sets to include in Group 2.
c       
c       List of numbers of the sets to include in Group 3, if any, etc.
c       
c       List of numbers of the regions to compare.  Ranges are OK
c       
c       0 to compare the means of the summing areas, or 1 to compare the
c       .  integrals, which are the means times the number of pixels.
c       
c       List of numbers of sets to rescale - ranges may be entered, or just
c       .  Return for no rescaling, or enter / to select either all sets or
c       .  the sets selected last time, as indicated by the prompt.
c       
c       IF you select rescaling, first enter 0 to specify scaling separately
c       .  for each set, or 1 to apply the similar scaling to all sets.
c       
c       IF you select rescaling, next make the following entries for each
c       .  set that you specified for rescaling:
c       
c       .  0 to specify scaling factors directly, 999 to divide values by the
c       .    value in one area, or the number of another data set, if you
c       .    wish to regress this set against the other set, or the negative
c       .    of the number of another set, if you wish to shift this set to
c       .    have the same mean as that set.
c       
c       .  IF you entered 0, next enter the factor to multiply by, and the
c       .    amount to add after multiplication
c       
c       .  IF you entered 999, next enter the region number, and the number
c       .    of the area within that region, to divide by.
c       
c       .  BUT, IF you entered a set number, next enter a list of the numbers
c       .    of the regions to use for comparing the two data sets.
c       
c       For each comparison, the program first prints a t-statistic
c       (with significance level) for a simple comparison of the mean of the
c       means in Groups 1 and 2.  This statistic is not as valid as the one
c       from the ANOVA which follows.  However, it is a one-tailed statistic
c       and might be more suitable for evaluating whether the two groups
c       differ in a direction that was expected a priori.
c       The top line of the ANOVA table shows the significance of difference
c       between the two groups.  The second line shows the significance of
c       differences among the different data sets within each group
c       (subgroup differences).  If the conditions for the Satterthwaite
c       approximation are satisfied, then the results from that
c       approximation are printed next and should be used for comparison
c       between groups, instead of the first line of the table.
c       
c       After the comparisons, enter one of the following:
c       1 to loop back to the specification of rescaling of sets
c       2 to loop back to entering the list of regions to compare
c       3 to loop back to entering the list of data sets in the groups
c       4 to loop all the way back and read a new data file
c       5 to exit
c       
c       David Mastronarde  4/23/90

      parameter (limarea=2500,limset=500,limdat=10000,limmat=500000)
      real*4 avg(limmat),sd(limmat)
     &    ,sem(limmat),xx(limdat),yy(limdat),semadd(limdat),
     &    sclfac(limset),scladd(limset),xxft(limarea),yyft(limarea)
      integer*4 nsumarea(limarea),indregion(limarea),ngx(limdat),
     &    nsymb(500),kset(500),ktype(500),kreg(500),nregress(limset),
     &    nreguse(limset),ireguse(limarea,100),isrescl(limset),
     &    npixarea(limarea),normreg(limset),normarea(limset)
      character*50 statname
      character*23 defsetxt
c       
      parameter (limgrp=20,limsub=20)
      integer*4 nn(limgrp,limsub),nb(limgrp)
      real*4 xb(limgrp,limsub),sds(limgrp,limsub)
      real*4 sclf(limset),scla(limset),avav(limgrp),sdav(limgrp)
      integer*4 nsampl(limset)
      real*8 g01baf
      tprob(ndf,t)=1.-betai(0.5*ndf,0.5,ndf/(ndf+t**2))/2.
c       
5     write(*,'(1x,a,$)')'Name of statistics file: '
      read(*,'(a)')statname
      close(1)
      call dopen(1,statname,'ro','f')
c       
c       write(*,'(1x,a,$)')
c       &           '0 for plots on parallax, 1 for terminal only: '
c       read(*,*)iffil
c       call grfopn(iffil)
c       
      read(1,*)nregion
      read(1,*)(nsumarea(i),i=1,nregion)
c       
      ntotarea=0
      do i=1,nregion
        indregion(i)=ntotarea+1
        ntotarea=ntotarea+nsumarea(i)
      enddo
c       
      read(1,*)ntotin
      if(ntotin.ne.ntotarea)print *,
     &    'Warning - number of areas does not add up correctly'
      read(1,*)(npixarea(i),i=1,ntotin)
      read(1,*)nsets
c       
      write(*,101)nsets,nregion,ntotarea,(nsumarea(i),i=1,nregion)
101   format(i5,' data sets',/,i5,' summing regions with a total of',
     &    i4,' summing areas',/,' Number of areas in these regions:',
     &    /,(20i4))
c       
      do iset=1,nsets
        read(1,*)nsampl(iset)
        do iarea=1,ntotarea
          ind=iarea+ntotarea*(iset-1)
          read(1,*)idum,avg(ind),sd(ind),sem(ind)
        enddo
      enddo
c       
10    write(*,'(1x,a,$)')'Number of groups of sets: '
      read(5,*)ngrps
      nsetplot=0
      do igrp=1,ngrps
        write(*,'(a,i3,a)')' Enter list of numbers of sets for group'
     &      ,igrp,' (ranges ok)'
        call rdlist(5,kset(nsetplot+1),nb(igrp))
        nsetplot=nsetplot+nb(igrp)
      enddo
      nrescale=nsetplot
      defsetxt='all sets)'
      do i=1,nsetplot
        isrescl(i)=kset(i)
      enddo
c       ntytot=0
c       do while(ntytot.lt.nsetplot)
c       write(*,'(a,i3,a)')' Enter list of symbol types for',
c       &             nsetplot-ntytot, ' sets (ranges ok)'
c       call rdlist(5,ktype(ntytot+1),ntyadd)
c       ntytot=ntytot+ntyadd
c       enddo
c       
15    print *,'Enter list of numbers of regions to test (ranges ok)'
      call rdlist(5,kreg,nregplot)
c       
c       20      write(*,'(1x,a,$)')
c       &           '+ # of S.E.M. or - # of S.D. for error bars: '
c       read(*,*)facsem
c       
20    write(*,'(1x,a,$)')'0 to use means, 1 to use integrals: '
      read(*,*)integral
      print *,'Enter list of numbers of sets to rescale'
      print *,'     (ranges OK, Return for none, / for ',defsetxt
      call rdlist(5,isrescl,nrescale)
      defsetxt='same sets as last time)'
      if(nrescale.gt.0.and.(nrescale.gt.1.or.isrescl(1).ne.0))then
        write(*,'(1x,a,$)')'0 to specify each independently, 1 to '
     &      //'use same specification for all: '
        read(*,*)ifsamescl
        nspecify=nrescale
        if(ifsamescl.ne.0)nspecify=1
        do ires=1,nspecify
          write(*,'(1x,a,i3,a,/,a,/,a,$)')'For set #',isrescl(ires),
     &        ', enter 0 to specify scaling, 999 to divide by one'//
     &        ' area,','  or # of other set to compare with',
     &        '     (+ set # for regression, - set # '//
     &        'to shift to same mean): '
          read(*,*)nregress(ires)
          if(nregress(ires).eq.0)then
            write(*,'(1x,a,$)')'Factors to multiply by, then add: '
            read(*,*)sclfac(ires),scladd(ires)
          elseif(nregress(ires).eq.999)then
            write(*,'(1x,a,$)')'Region #, and # of area within'//
     &          ' region, to divide by: '
            read(*,*)normreg(ires),normarea(ires)
          else
            print *,'Enter list of regions to use for comparing sets'//
     &          ' (ranges OK)'
            call rdlist(5,ireguse(1,ires),nreguse(ires))
          endif
        enddo
      else
        nrescale=0
      endif
c       
      do iset=1,nsetplot
        jset=kset(iset)
        ires=0
        sclf(iset)=1.
        scla(iset)=0.
        do i=1,nrescale
          if(jset.eq.isrescl(i))ires=i
        enddo
        if(ires.gt.0)then
          if(ifsamescl.ne.0)ires=1
          if(nregress(ires).eq.0)then
            sclf(iset)=sclfac(ires)
            scla(iset)=scladd(ires)
          elseif(nregress(ires).eq.999)then
            scla(iset)=0.
            indar=indregion(normreg(ires))+normarea(ires)-1
            sclf(iset)=1./avg(indar+ntotarea*(jset-1))
            if(integral.ne.0)sclf(iset)=sclf(iset)/npixarea(indar)
          else
            npnts=0
            do ireg=1,nreguse(ires)
              jreg=ireguse(ireg,ires)
              do indar=indregion(jreg),indregion(jreg)+nsumarea(jreg)-1
                npnts=npnts+1
                yyft(npnts)=avg(indar+ntotarea*(abs(nregress(ires))-1))
                xxft(npnts)=avg(indar+ntotarea*(jset-1))
              enddo
            enddo
            if(npnts.gt.0)then
              if(nregress(ires).gt.0)then
                call lsfit(xxft,yyft,npnts,sclf(iset),scla(iset),rho)
              else
                sclf(iset)=1.
                call avgsd(xxft,npnts,xxavg,xxsd,xxsem)
                call avgsd(yyft,npnts,yyavg,yysd,yysem)
                scla(iset)=yyavg-xxavg
              endif
            endif
            write(*,103)jset,npnts,rho,sclf(iset),scla(iset)
103         format(' Set #',i3,', n=',i4,', r=',f6.3,', multiply by',
     &          f9.5,' and add',f7.2)
          endif
        endif
      enddo
c       
      do ireg=1,nregplot
        jreg=kreg(ireg)
        do indar=indregion(jreg),indregion(jreg)+nsumarea(jreg)-1
          write(*,107)indar+1-indregion(jreg),jreg
107       format(/,20x,'Comparison for area',i3,' in region',i3)
          ii=1
          jj=0
          do iset=1,nsetplot
            jset=kset(iset)
            jj=jj+1
            nn(ii,jj)=nsampl(jset)
            xb(ii,jj)=sclf(iset)*avg(indar+ntotarea*(jset-1))
     &          +scla(iset)
            sds(ii,jj)=sclf(iset)*sd(indar+ntotarea*(jset-1))
            if(integral.ne.0)then
              xb(ii,jj)=xb(ii,jj)*npixarea(indar)
              sds(ii,jj)=sds(ii,jj)*npixarea(indar)
            endif
            xxft(jj)=xb(ii,jj)
            if(jj.eq.nb(ii))then
              call avgsd(xxft,nb(ii),avav(ii),sdav(ii),xxsem)
              write(*,105)ii,nb(ii),avav(ii),sdav(ii),xxsem
105           format(' Group',i2,',  n=',i3,', mean of means=',f12.3
     &            ,',  SD=' ,f11.3,',  SEM=',f11.3)
              jj=0
              ii=ii+1
            endif
          enddo
          call tstat(avav(1),sdav(1),nb(1),avav(2),sdav(2),nb(2),
     &        tsta,ndft)
          ifail=0
c           pval=1.-g01baf(ndft,dble(abs(tsta)),ifail)
          pval=1.-tprob(ndft,abs(tsta))
          write(*,106)ndft,tsta,pval
106       format(/,' t(',i3,') =',f10.2,',  P =',f7.4)
          call nestanova(ngrps,nb,nn,xb,sds)
        enddo
      enddo
30    write(*,'(1x,a,/,a,$)')'Respecify scaling (1), regions (2)'//
     &    ', data sets (3), or input file (4),','    or exit (5): '
      read(*,*)iopt
      if(iopt.lt.1)go to 30
      go to (20,15,10,5,40),iopt
      go to 30
c       
40    stop
      end


      subroutine tstat(x1,s1,n1,x2,s2,n2,t,nt)
      s1s=s1**2
      s2s=s2**2
      f=s1s/s2s
      nf1=n1-1
      nf2=n2-1
      nt=n1+n2-2
      sp=sqrt((nf1*s1s+nf2*s2s)/nt)
      t=(x1-x2)/(sp*sqrt(1./n1+1./n2))
      s1n=s1s/n1
      s2n=s2s/n2
      tp=(x1-x2)/sqrt(s1n+s2n)
      v=(s1n+s2n)**2/(s1n**2/nf1+s2n**2/nf2)-2
      sxsq1=s1**2*(n1-1)+n1*x1**2
      sxsq2=s2**2*(n2-1)+n2*x2**2
      ns=n1+n2
      xt=(n1*x1+n2*x2)/ns
      st=sqrt((sxsq1+sxsq2-ns*xt**2)/(ns-1))
c       write(*,100)nf1,nf2,f,nt,t,v,tp,xt,st,ns
c       100     format("  f (",i3,",",i3,")  = ",f9.4,/" t (",i3,") =",f9.4,/
c       1,"tprime (",f6.2,") = ",f9.4,/" composite mean =",f12.5,", s.d. ="
c       2,f12.5,", n =",i4)
      return
      end
