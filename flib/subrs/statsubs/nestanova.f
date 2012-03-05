c       NESTANOVA computes a two-level nested analysis of variance with
c       unequal sample sizes, given only the means and standard deviations
c       of the subgroups rather than the individual measurements.
c       NA = number of groups
c       NB (I) = number of subgroups of Ith group
c       N (I,J) = number of individuals in Jth subgroup of Ith group
c       XB (I,J) = mean for Jth subgroup of Ith group
c       SD (I,J) = SD for Jth subgroup of Ith group
c       The Satterthwaite approximation for the F test among groups is
c       reported if criteria are satisfied
c       
c       Based on equations and terminology in Sokal and Rohlf, BIOMETRY,
c       P.293-301.
c       
      subroutine nestanova(na,nb,n,xb,sd)
      parameter (limgrp=20,limsub=20)
      integer*4 n(limgrp,limsub),nb(limgrp)
      real*4 xb(limgrp,limsub),sd(limgrp,limsub)
c       
      integer*4 dfgroup,dfsubgr,dfwithin,nsum
      real*4 mswithin,msgroup,mssubgr,sswithin,sssubgr,ssgroup,rn0,
     &    rn0p,rnb0,fgroup,fpgroup,satter,dfpsubgr
      common /anova/nsum,rn0,rn0p,rnb0,dfgroup,dfsubgr,dfwithin,
     &    ssgroup,sssubgr,sswithin,msgroup,mssubgr,mswithin,fgroup,
     &    fpgroup,satter,dfpsubgr,rcrit
      real*8 g01bbf,g01cbf
      fprob(ndf1,ndf2,f)=betai(0.5*ndf2,0.5*ndf1,ndf2/(ndf2+ndf1*f))
c       
c       calculate degrees of freedom
c       
      call dfcalc(na,nb,n)
c       
c       get criterion for Satterthwaite approximation
c       
      satter=fvalue(0.975,dfwithin,dfsubgr)
      satter=satter*fvalue(0.5,dfsubgr,dfwithin)
c       
c       calculate sums of squares and abslaue minimum of other items needed
c       to do f test on groups
c       
      call sscalc(na,nb,n,xb,sd)
c       
c       get other items for report, and probabilities of F values
c       
      fsubgr=mssubgr/mswithin
      varsubgr=(mssubgr-mswithin)/rn0
      vargroup=(msgroup-mswithin-rn0p*varsubgr)/rnb0
      vartot=mswithin+varsubgr+vargroup
      pctwithin=100.*mswithin/vartot
      pctsubgr=100.*varsubgr/vartot
      pctgroup=100.*vargroup/vartot
c       
      pgroup=fprob(dfgroup,dfsubgr,fgroup)
      psubgr=fprob(dfsubgr,dfwithin,fsubgr)
c       
      write(*,101,err=35)dfgroup,ssgroup,msgroup,vargroup,pctgroup,
     &    fgroup,dfgroup,dfsubgr,pgroup,dfsubgr,
     &    sssubgr,mssubgr,varsubgr,pctsubgr,fsubgr,dfsubgr,dfwithin,
     &    psubgr,dfwithin,sswithin,mswithin,mswithin,pctwithin
101   format(/,' Source  df        SS         MS        ',
     &    'variance  % total   F   [df1,df2]   P',/,' Group '
     &    ,i4,3g13.6,f6.1,f7.2,' [',i2,',',i4,']',f7.4,
     &    /,' Subgrp',i4,3g13.6,f6.1,f7.2,' [',i2,',',i4,']',
     &    f7.4,/,' Within',i4,3g13.6,f6.1,/)
c       
35    if(dfsubgr.lt.100.and.dfsubgr.lt.2*dfwithin.and.rcrit.gt.satter)
     &    then
c         
c         if Satterthwaite criteria satisfied
c         
        intdfp=dfpsubgr
        pbelow=fprob(dfgroup,intdfp,fpgroup)
        pabove=fprob(dfgroup,intdfp+1,fpgroup)
        ppgroup=pbelow+(pabove-pbelow)*(dfpsubgr-intdfp)
            write(*,102)fpgroup, dfgroup,dfpsubgr,ppgroup
102     format(' Conditions satisfied to use Satterthwaite',
     &      ' approximation for Groups test',/,10x,'F=' ,f8.2,
     &      ' [',i3,',',f7.2,']  P=',f7.4,/)
      endif
      return
      end


c       DFCALC calculates degrees of freedom and the n0, n0', and nb0
c       factors needed for forming proper F ratios.
c       NA = number of groups
c       NB (I) = number of subgroups of Ith group
c       N (I,J) = number of individuals in Jth subgroup of Ith group
c       
      subroutine dfcalc(na,nb,n)
      parameter (limgrp=20,limsub=20)
      integer*4 n(limgrp,limsub),nb(limgrp)
c       
      integer*4 dfgroup,dfsubgr,dfwithin,nsum
      real*4 mswithin,msgroup,mssubgr,sswithin,sssubgr,ssgroup,rn0,
     &    rn0p,rnb0,fgroup,fpgroup,satter,dfpsubgr
      common /anova/nsum,rn0,rn0p,rnb0,dfgroup,dfsubgr,dfwithin,
     &    ssgroup,sssubgr,sswithin,msgroup,mssubgr,mswithin,fgroup,
     &    fpgroup,satter,dfpsubgr,rcrit
c       
c       form the partial sums of n values needed to get the n0 factors
c       
      nsum=0
      nsqsum=0
      ns3=0
      s4=0.
      nbsum=0
      do 20 i=1,na
        nsub=0
        nsqsub=0
        do 18 j=1,nb(i)
          nsub=nsub+n(i,j)
          nsqsub=nsqsub+n(i,j)**2
18      continue
        nbsum=nbsum+nb(i)
        nsum=nsum+nsub
        nsqsum=nsqsum+nsqsub
        ns3=ns3+nsub**2
        s4=s4+float(nsqsub)/nsub
20    continue
c       
c       get df's and the factors
c       
      dfgroup=na-1
      dfsubgr=nbsum-na
      dfwithin=nsum-nbsum
      rn0p=(s4-float(nsqsum)/nsum)/dfgroup
      rn0=(nsum-s4)/dfsubgr
      rnb0=(nsum-float(ns3)/nsum)/dfgroup
c       print *,dfgroup,dfsubgr,dfwithin,rn0p,rn0,rnb0
      return
      end


c       SSCALC computes sums of squares and the F ratio for among group
c       comparisons, as well as the Satterthwaite approximation to this
c       F ratio and the degrees of freedom of subgroups, if the factor
c       SATTER is non-zero.
c       
c       NA = number of groups
c       NB (I) = number of subgroups of Ith group
c       N (I,J) = number of individuals in Jth subgroup of Ith group
c       XB (I,J) = mean for Jth subgroup of Ith group
c       SD (I,J) = SD for Jth subgroup of Ith group
c       
      subroutine sscalc(na,nb,n,xb,sd)
      parameter (limgrp=20,limsub=20)
      integer*4 n(limgrp,limsub),nb(limgrp)
      real*4 xb(limgrp,limsub),sd(limgrp,limsub),mspsubgr
c       
      integer*4 dfgroup,dfsubgr,dfwithin,nsum
      real*4 mswithin,msgroup,mssubgr,sswithin,sssubgr,ssgroup,rn0,
     &    rn0p,rnb0,fgroup,fpgroup,satter,dfpsubgr
      common /anova/nsum,rn0,rn0p,rnb0,dfgroup,dfsubgr,dfwithin,
     &    ssgroup,sssubgr,sswithin,msgroup,mssubgr,mswithin,fgroup,
     &    fpgroup,satter,dfpsubgr,rcrit
      real*8 s1,s2,s3,s2sub
c       
c       get proper sums for computing sums of squares
c       
      sswithin=0.
      s1=0.
      s2=0.
      s3=0.
      do 30 i=1,na
        s2sub=0.
        nsub=0
        do 25 j=1,nb(i)
          nn=n(i,j)
          nsub=nsub+nn
          sswithin=sswithin+(nn-1)*sd(i,j)**2
          xn=xb(i,j)*nn
          s2sub=s2sub+xn
          s1=s1+dble(xn*xb(i,j))
25      continue
        s2=s2+dble(s2sub**2/nsub)
        s3=s3+dble(s2sub)
30    continue
c       
c       get sum of squares, mean squares and F for groups
c       
      s3=s3**2/nsum
      sssubgr=s1-s2
      ssgroup=s2-s3
      msgroup=ssgroup/dfgroup
      mssubgr=sssubgr/dfsubgr
      fgroup=msgroup/mssubgr
      fpgroup=fgroup
      dfpsubgr=dfsubgr
c       
c       if doing Satterthwaite, get criterion
c       
      if(satter.ne.0.)then
        mswithin=sswithin/dfwithin
        rcrit=(rn0p/(rn0p-rn0))*mssubgr/mswithin
        if(satter.lt.0.or.rcrit.gt.satter)then
c           
c           if SATTER factor is unknown or criterion is satisfied,
c           get new F ratio and df
c           
          w2=rn0p/rn0
          w1=1.-w2
          mspsubgr=w1*mswithin+w2*mssubgr
          fpgroup=msgroup/mspsubgr
          dfpsubgr=mspsubgr**2/((w1*mswithin)**2/dfwithin+
     &        (w2*mssubgr)**2/dfsubgr)
        endif
      endif
      return
      end
