c       SUBROUTINES FOR MTK ONLY
c
c       $Id$
c       log at end
c              
c       GETBINSPEC gets a bin specification appropriate for the type of
c       graphs being done
c
      subroutine getbinspec(delr,nbins,power,limfit,padbound,fracomit,
     &    ifbundend,samplen,ifcloseg,ifscatsurf)
      parameter (limbins=1001)
      logical didonce/.false./
      save didonce
      integer*4 in5
      common /nmsinput/ in5
c       
      write(*,'(1x,a,$)')
     &    'Bin width (radial distance), number of bins: '
      read(in5,*)delr,nbins
      if(nbins.ge.limbins)print *,'# of bins truncated to',limbins-1
      nbins=min(nbins,limbins-1)
c       
      ifchange=1
      if(didonce)then
        write(*,'(1x,a,$)')'0 to keep other parameters the same,'//
     &      ' 1 to change them: '
        read(in5,*)ifchange
      endif
      didonce=.true.
      if(ifbundend.eq.0.and.ifchange.ne.0)then
        write(*,'(1x,a,$)')'Sampling length for lines, or 0 to find '
     &      //'closest approach to whole line: '
        read(in5,*)samplen
        write(*,'(1x,a,/,a,$)')'(For whole lines) Power for radial '
     &      //'weighting,','  number of points to fit over: '
        read(in5,*)power,limfit
        limfit=max(1,limfit)
        write(*,'(1x,a,/,a,$)')'(For line segments) 0 to find '//
     &      'distance from start of sample segment,',
     &      '  or 1 to find closest approach to segment: '
        read(in5,*)ifcloseg
        write(*,'(1x,a,$)')'0 to measure from center of scattered '
     &      //'points or 1 to measure from surface: '
        read(in5,*)ifscatsurf
      elseif(ifchange.ne.0)then
        write(*,'(1x,a,/,a,$)')'Enter distance to pad boundaries (- '
     &      //'if in pixels to be scaled),','   and fraction '//
     &      'of farthest points to omit from bundle: '
        read(in5,*)padbound,fracomit
        write(*,'(1x,a,$)')'0 or 1 to analyze distances to all '//
     &      'bundles or only to nearest bundle: '
        read(in5,*)limfit
      endif
      return
      end
      


c       GETGRAPHSPEC gets specifications of the types involved in each
c       desired graph.
c       NGRAPH is returned with the number of graphs
c       NxxxTYP is returned with the number of types for reference items
c       (xxx=REF), or neighboring items (xxx=NEIGH);
c       ITYPxxx is returned with the list of types for each case
c       
      subroutine getgraphspec(ngraph,itypref,
     &    nreftyp, itypneigh, nneightyp, iwhichend,ifbundend,
     &    iobjflag,limflag,irefflag,neighflag)
      parameter (limtyp=50,itypall=999)
      integer*4 nreftyp(*),nneightyp(*)         !# of types for ref and neigh
      integer*4 itypref(limtyp,*),itypneigh(limtyp,*)
      integer*4 iwhichend(limtyp,*),iobjflag(*)
      logical didonce/.false./
      save didonce
      integer*4 in5
      common /nmsinput/ in5
c       
      ifchange=1
      if(didonce)then
        write(*,'(1x,a,$)')'0 to keep same graph specifications or'//
     &      ' 1 to specify new graphs: '
        read(in5,*)ifchange
      endif
      didonce=.true.
10    if(ifchange.ne.0)then
        irefflag=-1
        neighflag=-1
        write(*,'(1x,a,$)')'Number of different graphs to compute: '
        read(in5,*)ngraph
c         
        do ii=1,ngraph
          write(*,102)ii,'from (reference'
102       format(' For graph #',i3,', enter list of objects to ',
     &        'measure distances',/,5x,a,' objects)',
     &        ' (Return for all, ranges OK)')
          call rdlist(in5,itypref(1,ii),nreftyp(ii))
          if(nreftyp(ii).eq.0)then
            nreftyp(ii)=1
            itypref(1,ii)=itypall
          endif
c           
          write(*,102)ii,'to (neighboring'
          call rdlist(in5,itypneigh(1,ii),nneightyp(ii))
          if(nneightyp(ii).eq.0)then
            nneightyp(ii)=1
            itypneigh(1,ii)=itypall
          endif
c           
          if(ifbundend.ne.0)then
            print *,'For each neighbor object, enter 0 to count ',
     &          'neither end, 1 to count the end','    at low Z, 2 to'
     &          ,' count the end at high Z, or 3 to count both ends'
            read(in5,*)(iwhichend(j,ii),j=1,nneightyp(ii))
          endif
        enddo
      endif
      call checkflags(ngraph,itypref, nreftyp, itypneigh,
     &    nneightyp, iobjflag,limflag,irefflag,neighflag)
      if(irefflag.ge.0.and.neighflag.ge.0)return
      if(ifchange.ne.0)then
        print *,'Start over with graph specifications'
      else
        print *,'You need to enter new graph specifications'
      endif
      go to 10
      end


      subroutine checkflags(ngraph,itypref, nreftyp, itypneigh,
     &    nneightyp, iobjflag,limflag,irefflag,neighflag)
      parameter (limtyp=50,itypall=999)
      integer*4 nreftyp(*),nneightyp(*)         !# of types for ref and neigh
      integer*4 itypref(limtyp,*),itypneigh(limtyp,*),iobjflag(*)
      logical feasible
      integer*4 icando(2,10)
      data icando/1,1,1,2,1,4,2,1,2,2,2,4,4,1,4,2,4,4,-1,-1/
      do ii=1,ngraph
        if(nreftyp(ii).eq.1.and.itypref(1,ii).eq.itypall)then
          do i=1,limflag
            if(iobjflag(i).ge.0)then
              if(irefflag.lt.0)then
                irefflag=iobjflag(i)
              elseif(iobjflag(i).ne.irefflag)then
                print *,'Reference object types do not all match'
                go to 10
              endif
            endif
          enddo
        else
          do i=1,nreftyp(ii)
            ityp=abs(itypref(i,ii))
            if(iobjflag(ityp).ge.0)then
              if(irefflag.lt.0)then
                irefflag=iobjflag(ityp)
              elseif(iobjflag(ityp).ne.irefflag)then
                print *,'Reference object types do not all match'
                go to 10
              endif
            endif
          enddo
        endif
        if(nneightyp(ii).eq.1.and.itypneigh(1,ii).eq.itypall)then
          do i=1,limflag
            if(iobjflag(i).ge.0)then
              if(neighflag.lt.0)then
                neighflag=iobjflag(i)
              elseif(iobjflag(i).ne.neighflag)then
                print *,'Neighboring object types do not all match'
                go to 10
              endif
            endif
          enddo
        else
          do i=1,nneightyp(ii)
            ityp=abs(itypneigh(i,ii))
            if(iobjflag(ityp).ge.0)then
              if(neighflag.lt.0)then
                neighflag=iobjflag(ityp)
              elseif(iobjflag(ityp).ne.neighflag)then
                print *,'Neighboring object types do not all match'
                go to 10
              endif
            endif
          enddo
        endif
      enddo
c       
c       check for feasibility
c       
      i=1
      feasible=.false.
      do while(icando(1,i).ge.0.and..not.feasible)
        feasible=icando(1,i).eq.irefflag.and.
     &      icando(2,i).eq.neighflag
        i=i+1
      enddo
      if(feasible)return
      print *,'Distances cannot be measured from the reference','
     &    to the neighbor types'
10    irefflag=-1
      neighflag=-1
      return
      end



c       INTEGRATE compute the integral of the items in some bins of a graph,
c       subtracting a baseline value as well.
c       GRAPHS has the values of the graph
c       NBINS is number of bins
c       INTST, INTND are starting and ending bins to integrate
c       IBAST, IBAND are starting and ending bins to compute the baseline
c       value from, or 0,0 to use thevalue supplied in BASELINE
c       SUM is returned with the integral
c       CENTROID is returned with centroid of distance
c       
      subroutine integrate(graphs,areas,nbins,delr,power,intst,intnd,
     &    ibast, iband,baseval,sum, centroid)
      implicit none
      real*4 graphs(*),areas(*),delr,power,baseval,sum, centroid
      integer*4 nbins,intst,intnd, ibast,iband
      real*4 baseline,distsum,counts,frac
      integer*4 ibin
      baseline=baseval
      if(ibast.le.iband.and.(ibast.ne.0.or.iband.ne.0))then
        baseline=0.
        do ibin=ibast,iband
          baseline=baseline+graphs(ibin)
        enddo
        baseline=baseline/(iband+1-ibast)
      endif
      sum=0.
      distsum = 0.
      centroid = 0.
      do ibin=intst,intnd
c         if(power.eq.0.)then
c         radpow=1.
c         else
c         radpow=(2.*(ibin-0.5)*delr)**power
c         endif
c         frac=radpow*delr*3.14159
        frac=areas(ibin)
        counts = (graphs(ibin)-baseline)*frac 
        sum=sum+counts
        distsum = distsum + delr * (ibin - 0.5) * counts
      enddo
      if (sum .gt. 0.) centroid = distsum / sum
      return
      end

c       LINFIT fits a straight line to the N points in arrays X and Y by the
c       method of least squares, returning SLOPE, and intercept BINT
c       
      subroutine linfit(x,y,n,slope,bint)
      dimension x(*),y(*)
      if(n.eq.2)then
        slope=(y(2)-y(1))/(x(2)-x(1))
        bint=y(1)-slope*x(1)
      else
        sx=0.
        sxsq=0.
        sy=0.
        sxy=0.
        sysq=0.
        do i=1,n
          sx=sx+x(i)
          sy=sy+y(i)
          sxy=sxy+x(i)*y(i)
          sxsq=sxsq+x(i)**2
          sysq=sysq+y(i)**2
        enddo
        d=n*sxsq-sx**2
        slope=(n*sxy-sx*sy)/d
        bint=(sxsq*sy-sx*sxy)/d
      endif
      return
      end


c       SEGMENT_DIST_Z computes the distance between segments of two
c       "z-based lines", characterized by x=A1*z+B1 and y=C1*z+D1 for z
c       between ZS1 and ZN1, and x=A2*z+B2 and y=C2*z+D2 for z between ZS2
c       and ZN2.  It returns the sqaure of the distance in DIST, and the
c       z values of the points of closest approach on the two segments
c       in Z1 and Z2.
c       
      subroutine segment_dist_z(a1,b1,c1,d1,a2,b2,c2,d2,zs1,zn1,zs2,zn2
     &    ,z1,z2,dist)
      logical out1,out2
      integer*4 iftrace/0/,ntrace/0/
      point_to_line(a,b,c,d,x,y,z)=(a*x+c*y+z-a*b-c*d)/(1.+a**2+c**2)
c       
c       first find z1,z2 position of global minimum
c       
      sq1=1+a1**2+c1**2
      sq2=1+a2**2+c2**2
      crs=-(1+a1*a2+c1*c2)
      con1=-(a1*(b1-b2)+c1*(d1-d2))
      con2=a2*(b1-b2)+c2*(d1-d2)
      den=sq1*sq2-crs**2
      z1num=con1*sq2-con2*crs
      z2num=sq1*con2-con1*crs
      if(abs(den).lt.1.e-20.or.
     &    abs(den).lt.1.e-6*max(abs(z1num),abs(z2num)))then
c         
c         parallel lines: "just" check the 4 endpoints
c         
        xs1=a1*zs1+b1
        xn1=a1*zn1+b1
        ys1=c1*zs1+d1
        yn1=c1*zn1+d1
        xs2=a2*zs2+b2
        xn2=a2*zn2+b2
        ys2=c2*zs2+d2
        yn2=c2*zn2+d2
        z2t=max(zs2,min(zn2,point_to_line(a2,b2,c2,d2,xs1,ys1,zs1)))
        dist=(a2*z2t+b2-xs1)**2+(c2*z2t+d2-ys1)**2+(z2t-zs1)**2
        z1=zs1
        z2=z2t
        z2t=max(zs2,min(zn2,point_to_line(a2,b2,c2,d2,xn1,yn1,zn1)))
        tdist=(a2*z2t+b2-xn1)**2+(c2*z2t+d2-yn1)**2+(z2t-zn1)**2
        if(tdist.lt.dist)then
          dist=tdist
          z1=zn1
          z2=z2t
        endif
        z1t=max(zs1,min(zn1,point_to_line(a1,b1,c1,d1,xs2,ys2,zs2)))
        tdist=(a1*z1t+b1-xs2)**2+(c1*z1t+d1-ys2)**2+(z1t-zs2)**2
        if(tdist.lt.dist)then
          dist=tdist
          z1=z1t
          z2=zs2
        endif
        z1t=max(zs1,min(zn1,point_to_line(a1,b1,c1,d1,xn2,yn2,zn2)))
        tdist=(a1*z1t+b1-xn2)**2+(c1*z1t+d1-yn2)**2+(z1t-zn2)**2
        if(tdist.lt.dist)then
          dist=tdist
          z1=z1t
          z2=zn2
        endif
        go to 20
      endif
      z1=z1num/den
      z2=z2num/den
      out1=z1.lt.zs1.or.z1.gt.zn1
      out2=z2.lt.zs2.or.z2.gt.zn2
      if(out1.and.out2)then
        z1=max(zs1,min(zn1,z1))
        z2t=max(zs2,min(zn2,point_to_line(a2,b2,c2,d2,
     &      a1*z1+b1,c1*z1+d1,z1)))
        z2=max(zs2,min(zn2,z2))
        z1t=max(zs1,min(zn1,point_to_line(a1,b1,c1,d1,
     &      a2*z2+b2,c2*z2+d2,z2)))
        if(z1.ne.z1t.or.z2.ne.z2t)then
          dist=(a1*z1+b1-a2*z2t-b2)**2+(c1*z1+d1-c2*z2t-d2)**2+
     &        (z1-z2t)**2
          tdist=(a1*z1t+b1-a2*z2-b2)**2+(c1*z1t+d1-c2*z2-d2)**2+
     &        (z1t-z2)**2
          if(dist.lt.tdist)then
            z2=z2t
          else
            dist=tdist
            z1=z1t
          endif
          go to 20
        endif
      elseif(out1)then
        z1=max(zs1,min(zn1,z1))
        z2=max(zs2,min(zn2,point_to_line(a2,b2,c2,d2,
     &      a1*z1+b1,c1*z1+d1,z1)))
      elseif(out2)then
        z2=max(zs2,min(zn2,z2))
        z1=max(zs1,min(zn1,point_to_line(a1,b1,c1,d1,
     &      a2*z2+b2,c2*z2+d2,z2)))
      endif
      dist=(a1*z1+b1-a2*z2-b2)**2+(c1*z1+d1-c2*z2-d2)**2+(z1-z2)**2
20    if(iftrace.eq.0)return
      ntrace=ntrace+1
      if(ntrace.lt.iftrace)return
      ntrace=0
      dmin=1.e20
      do i=0,100
        z1t=zs1+i*(zn1-zs1)/100.
        x1t=a1*z1t+b1
        y1t=c1*z1t+d1
        do j=0,100
          z2t=zs2+j*(zn2-zs2)/100.
          d=(a2*z2t+b2-x1t)**2+(c2*z2t+d2-y1t)**2+(z2t-z1t)**2
          if(d.lt.dmin)then
            dmin=d
            z1m=z1t
            z2m=z2t
          endif
        enddo
      enddo
      write(*,101)a1,b1,c1,d1,zs1,zn1,a2,b2,c2,d2,zs2,zn2,z1,z2,
     &    dist,z1m,z2m,dmin
101   format(6f10.5,/,6f10.5,/,3f8.5,/,3f8.5)
      return
      end



      subroutine get_next_sample
     &    (xmt,ymt,zmt,iref,sampfrac,limref,samplen,ifcloseg,
     &    xtmp,ytmp, ztmp,ntmp,irefend,fracend,seglen,
     &    xtmpmin,xtmpmax,ytmpmin,ytmpmax,ztmpmin, ztmpmax,
     &    refxmin,refxmax,refymin,refymax,refzmin,refzmax)
      real*4 xmt(*),ymt(*),zmt(*),xtmp(*),ytmp(*),ztmp(*)
      real*4 xtmpmin(*),ytmpmin(*),ztmpmin(*)
      real*4 xtmpmax(*),ytmpmax(*),ztmpmax(*)
c       
      seglen=0.
      ntmp=0
      if(iref.ge.limref)return
c       
c       start by putting out the interpolated first point
c       
      ntmp=1
      next=iref+1
      xtmp(1)=(1.-sampfrac)*xmt(iref)+sampfrac*xmt(iref+1)
      ytmp(1)=(1.-sampfrac)*ymt(iref)+sampfrac*ymt(iref+1)
      ztmp(1)=(1.-sampfrac)*zmt(iref)+sampfrac*zmt(iref+1)
c       
c       loop until end of line or sample length reached
c       
      do while(next.le.limref.and.seglen.lt.samplen)
c         
c         length of current segment
c         
        curlen=sqrt((xmt(next)-xtmp(ntmp))**2+
     &      (ymt(next)-ytmp(ntmp))**2+
     &      (zmt(next)-ztmp(ntmp))**2)
        if(seglen+curlen.lt.samplen)then
c           
c           if that still falls short, add point to output list and advance
c           
          ntmp=ntmp+1
          xtmp(ntmp)=xmt(next)
          ytmp(ntmp)=ymt(next)
          ztmp(ntmp)=zmt(next)
          seglen=seglen+curlen
          next=next+1
        else
c           
c           otherwise, find interpolated point, add that to list
c           
          irefend=next-1
          fracend=(samplen-seglen)/curlen
          if(irefend.eq.iref)fracend=sampfrac+(1.-sampfrac)*fracend
          seglen=samplen
          ntmp=ntmp+1
          xtmp(ntmp)=(1.-fracend)*xmt(irefend)+fracend*xmt(irefend+1)
          ytmp(ntmp)=(1.-fracend)*ymt(irefend)+fracend*ymt(irefend+1)
          ztmp(ntmp)=(1.-fracend)*zmt(irefend)+fracend*zmt(irefend+1)
        endif
      enddo
c       
c       if termination was because at end of segment, set ending values
c       
      if(seglen.lt.samplen)then
        irefend=limref
        fracend=0.
      endif
c       
c       do min's and max's as necessary
c       
      if(ifcloseg.eq.0)then
        refxmin=xtmp(1)
        refymin=ytmp(1)
        refzmin=ztmp(1)
        refxmax=xtmp(1)
        refymax=ytmp(1)
        refzmax=ztmp(1)
        ntmp=1
      else
        refxmin=1.e10     
        refymin=1.e10     
        refzmin=1.e10     
        refxmax=-1.e10    
        refymax=-1.e10    
        refzmax=-1.e10    
        do i=1,ntmp-1
          xtmpmin(i)=min(xtmp(i),xtmp(i+1))
          ytmpmin(i)=min(ytmp(i),ytmp(i+1))
          ztmpmin(i)=min(ztmp(i),ztmp(i+1))
          xtmpmax(i)=max(xtmp(i),xtmp(i+1))
          ytmpmax(i)=max(ytmp(i),ytmp(i+1))
          ztmpmax(i)=max(ztmp(i),ztmp(i+1))
          refxmin=min(refxmin,xtmpmin(i))
          refymin=min(refymin,ytmpmin(i))
          refzmin=min(refzmin,ztmpmin(i))
          refxmax=max(refxmax,xtmpmax(i))
          refymax=max(refymax,ytmpmax(i))
          refzmax=max(refzmax,ztmpmax(i))
        enddo
      endif
      return
      end


      subroutine trim_win_seg(x1,y1,z1,x2,y2,z2,s1,s2)
      if(s1.eq.0.and.s2.eq.0.)return
      untrim=sqrt((x2-x1)**2+(y2-y1)**2+(z2-z1)**2)
      f1=min(1.,s1/untrim)
      f2=min(1.,s2/untrim)
      if(f1+f2.ge.1.)then
        fs=f1+f2
        f1=f1/fs
        f2=f2/fs
      endif
      x1t=x1+f1*(x2-x1)
      x2=x2+f2*(x1-x2)
      x1=x1t
      y1t=y1+f1*(y2-y1)
      y2=y2+f2*(y1-y2)
      y1=y1t
      z1t=z1+f1*(z2-z1)
      z2=z2+f2*(z1-z2)
      z1=z1t
      return
      end

      subroutine save_connector(x1min,y1min,z1min,x2min,y2min,z2min,
     &    xmt,ymt,zmt,limxyz,indfree,iobjref,iobjneigh,iobjwin,
     &    nobjwin, ninwin,ninwtot,nrefwin,nneighwin)
      real*4 xmt(*),ymt(*),zmt(*)
      integer*4 iobjwin(*)
c       
      ninwtot=ninwtot+1
      ifrefon=0
      ifneighon=0
      do iow=1,nobjwin
        if(iobjref.eq.iobjwin(iow))ifrefon=1
        if(iobjneigh.eq.iobjwin(iow))ifneighon=1
      enddo
      if(ifrefon.eq.0)then
        nobjwin=nobjwin+1
        iobjwin(nobjwin)=iobjref
        nrefwin=nrefwin+1
      endif
      if(ifneighon.eq.0)then
        nobjwin=nobjwin+1
        iobjwin(nobjwin)=iobjneigh
        nneighwin=nneighwin+1
      endif
      if(indfree+2.gt.limxyz)return
      ninwin=ninwin+1
      xmt(indfree)=x1min
      xmt(indfree+1)=0.5*(x1min+x2min)
      xmt(indfree+2)=x2min
      ymt(indfree)=y1min
      ymt(indfree+1)=0.5*(y1min+y2min)
      ymt(indfree+2)=y2min
      zmt(indfree)=z1min
      zmt(indfree+1)=0.5*(z1min+z2min)
      zmt(indfree+2)=z2min
      indfree=indfree+3
      return
      end



c       SEGMENT_DIST measures the distance between two line segments, one
c       from XS1,YS1,ZS1 to XN1,YN1,ZN1 and one from XS2,YS2,ZS2 to
c       XN2,YN2,ZN2.  It returns the square of the distance in DIST and two
c       parameters, T1 and T2, specifying the fraction of the distance along
c       the segments where the points of closest approach are.
c       
      subroutine segment_dist(xs1,ys1,zs1,xn1,yn1,zn1
     &    ,xs2,ys2,zs2,xn2,yn2,zn2, t1,t2,dist)
      logical out1,out2
      integer*4 iftrace/0/,ntrace/0/
      save ntrace
c       
c       function returns the parameter t for the point along the line
c       closest to x,y,z
c       
      point_to_line(xs,ys,zs,a,b,c,x,y,z)=
     &    (a*(x-xs)+b*(y-ys)+c*(z-zs))/(a**2+b**2+c**2)
c       
c       first find z1,z2 position of global minimum
c       
      a1=xn1-xs1
      b1=yn1-ys1
      c1=zn1-zs1
      a2=xn2-xs2
      b2=yn2-ys2
      c2=zn2-zs2
      sq1=a1**2+b1**2+c1**2
      sq2=a2**2+b2**2+c2**2
      crs=-(a1*a2+b1*b2+c1*c2)
      con1=a1*(xs2-xs1)+b1*(ys2-ys1)+c1*(zs2-zs1)
      con2=-(a2*(xs2-xs1)+b2*(ys2-ys1)+c2*(zs2-zs1))
      den=sq1*sq2-crs**2
      t1num=con1*sq2-con2*crs
      t2num=sq1*con2-con1*crs
      if(abs(den).lt.1.e-20.or.
     &    abs(den).lt.1.e-6*max(abs(t1num),abs(t2num)))then
c         
c         parallel lines: "just" check the 4 endpoints
c         start of line 1 versus line 2
c         
        t2t=max(0.,min(1.,point_to_line
     &      (xs2,ys2,zs2,a2,b2,c2,xs1,ys1,zs1)))
        dist=(a2*t2t+xs2-xs1)**2+(b2*t2t+ys2-ys1)**2+
     &      (c2*t2t+zs2-zs1)**2
        t1=0.
        t2=t2t
c         
c         end of line 1 versus line 2
c         
        t2t=max(0.,min(1.,point_to_line
     &      (xs2,ys2,zs2,a2,b2,c2,xn1,yn1,zn1)))
        tdist=(a2*t2t+xs2-xn1)**2+(b2*t2t+ys2-yn1)**2+
     &      (c2*t2t+zs2-zn1)**2
        if(tdist.lt.dist)then
          dist=tdist
          t1=1.
          t2=t2t
        endif
c         
c         start of line 2 versus line 1
c         
        t1t=max(0.,min(1.,point_to_line
     &      (xs1,ys1,zs1,a1,b1,c1,xs2,ys2,zs2)))
        tdist=(a1*t1t+xs1-xs2)**2+(b1*t1t+ys1-ys2)**2+
     &      (c1*t1t+zs1-zs2)**2
        if(tdist.lt.dist)then
          dist=tdist
          t1=t1t
          t2=0.
        endif
c         
c         end of line 2 versus line 1
c         
        t1t=max(0.,min(1.,point_to_line
     &      (xs1,ys1,zs1,a1,b1,c1,xn2,yn2,zn2)))
        tdist=(a1*t1t+xs1-xn2)**2+(b1*t1t+ys1-yn2)**2+
     &      (c1*t1t+zs1-zn2)**2
        if(tdist.lt.dist)then
          dist=tdist
          t1=t1t
          t2=1.
        endif
        go to 20
      endif
      t1=t1num/den
      t2=t2num/den
      out1=t1.lt.0..or.t1.gt.1.
      out2=t2.lt.0..or.t2.gt.1.
      if(out1.and.out2)then
c         
c         if both closest points are out of bounds, truncate each one to
c         its segment, then find closest point on other segment to that
c         truncated point.  If this gives different answers, pick the
c         pair with the closest approach
c         
        t1=max(0.,min(1.,t1))
        t2t=max(0.,min(1.,point_to_line(xs2,ys2,zs2,a2,b2,c2,
     &      a1*t1+xs1,b1*t1+ys1,c1*t1+zs1)))
        t2=max(0.,min(1.,t2))
        t1t=max(0.,min(1.,point_to_line(xs1,ys1,zs1,a1,b1,c1,
     &      a2*t2+xs2,b2*t2+ys2,c2*t2+zs2)))
        if(t1.ne.t1t.or.t2.ne.t2t)then
          dist=(a1*t1+xs1-a2*t2t-xs2)**2+(b1*t1+ys1-b2*t2t-ys2)**2+
     &        (c1*t1+zs1-c2*t2t-zs2)**2
          tdist=(a1*t1t+xs1-a2*t2-xs2)**2+(b1*t1t+ys1-b2*t2-ys2)**2+
     &        (c1*t1t+zs1-c2*t2-zs2)**2
          if(dist.lt.tdist)then
            t2=t2t
          else
            dist=tdist
            t1=t1t
          endif
          go to 20
        endif
      elseif(out1)then
c         
c         If outside one segment but in the other, truncate the one it is
c         outside, then find closest point to other segment
c         
        t1=max(0.,min(1.,t1))
        t2=max(0.,min(1.,point_to_line(xs2,ys2,zs2,a2,b2,c2,
     &      a1*t1+xs1,b1*t1+ys1,c1*t1+zs1)))
      elseif(out2)then
        t2=max(0.,min(1.,t2))
        t1=max(0.,min(1.,point_to_line(xs1,ys1,zs1,a1,b1,c1,
     &      a2*t2+xs2,b2*t2+ys2,c2*t2+zs2)))
      endif
      dist=(a1*t1+xs1-a2*t2-xs2)**2+(b1*t1+ys1-b2*t2-ys2)**2+
     &    (c1*t1+zs1-c2*t2-zs2)**2
20    if(iftrace.eq.0)return
      ntrace=ntrace+1
      if(ntrace.lt.iftrace)return
      ntrace=0
      dmin=1.e20
      do i=0,100
        t1t=i/100.
        x1t=a1*t1t+xs1
        y1t=b1*t1t+ys1
        z1t=c1*t1t+zs1
        do j=0,100
          t2t=j/100.
          d=(a2*t2t+xs2-x1t)**2+(b2*t2t+ys2-y1t)**2+(c2*t2t+zs2-z1t)**2
          if(d.lt.dmin)then
            dmin=d
            t1m=t1t
            t2m=t2t
          endif
        enddo
      enddo
      write(*,101)xs1,ys1,zs1,xn1,yn1,zn1,
     &    xs2,ys2,zs2,xn2,yn2,zn2,t1,t2,
     &    dist,t1m,t2m,dmin
101   format(6f10.5,/,6f10.5,/,3f8.5,/,3f8.5)
      return
      end


c       INSIDE_TRIANGLE returns true if the point XT,YT is inside or on the
c       boundary of the triangle whose corners are in arrays BX and BY.
c       It is somewhat faster than INSIDE.  It is based on "Computational
c       Geometry in C" by Joseph O'Rourke, 1998.
c       
      logical function inside_triangle(bx,by,xt,yt)
      real*4 bx(*),by(*)
      logical inside
c       
c       compute signed area of each triangle between the point and an edge
c       
      area0=(bx(1)-xt)*(by(2)-yt)-(bx(2)-xt)*(by(1)-yt)
      area1=(bx(2)-xt)*(by(3)-yt)-(bx(3)-xt)*(by(2)-yt)
      area2=(bx(3)-xt)*(by(1)-yt)-(bx(1)-xt)*(by(3)-yt)

      if(area0.ne.0..and.area1.ne.0..and.area2.ne.0.)then
        inside_triangle=area0.gt.0..and.area1.gt.0..and.area2.gt.0.
     &      .or.area0.lt.0..and.area1.lt.0..and.area2.lt.0.
        return
      endif
      if(area0.eq.0..and.area1.eq.0..and.area2.eq.0.)then
        inside_triangle=inside(bx,by,3,xt,yt)
        return
      endif

      inside_triangle=area0.eq.0..and.area1.gt.0..and.area2.gt.0.
     &    .or.area0.eq.0..and.area1.lt.0..and.area2.lt.0.
     &    .or.area1.eq.0..and.area0.gt.0..and.area2.gt.0.
     &    .or.area1.eq.0..and.area0.lt.0..and.area2.lt.0.
     &    .or.area2.eq.0..and.area0.gt.0..and.area1.gt.0.
     &    .or.area2.eq.0..and.area0.lt.0..and.area1.lt.0.
     &    .or.area0.eq.0..and.area1.eq.0.
     &    .or.area0.eq.0..and.area2.eq.0.
     &    .or.area0.eq.1..and.area2.eq.0.
      return
      end




c       POINT_LINE_DIST measures the distance from the point X,Y,Z to the
c       line segment from XS,YS,ZS ato XN,YN,ZN.  It returns the square of
c       the distance in DSQR and the  parameter T specifying the position
c       along the segment of the point of closest approach (between 0 at
c       XS,YS,ZS and 1 at XN,YN,ZN).
c       
      subroutine point_line_dist(xs,ys,zs,xn,yn,zn,x,y,z,t,dsqr)
      a=xn-xs
      b=yn-ys
      c=zn-zs
      t=max(0.,min(1.,(a*(x-xs)+b*(y-ys)+c*(z-zs))/(a**2+b**2+c**2)))
      dsqr=(a*t+xs-x)**2+(b*t+ys-y)**2+(c*t+zs-z)**2
      return
      end



c       POINT_TO_TRIANGLE measures the distance of closest approach between
c       the point XP,YP,ZP and the triangle ITRI in common MTKCOM.  It
c       returns the separation in DIST, and the coordinates in the rotated
c       triangle, XROT and YROT, of the point of closest approach to the
c       triangle.
c       
      subroutine point_to_triangle(xp,yp,zp,itri,xrot,yrot,dist)
      include 'mtk.inc'
      logical inside_triangle
c       
c       rotate the point
c       
      tmp=xp*cgam(itri)-yp*sgam(itri)
      xr=tmp*cbet(itri)-zp*sbet(itri)
      yr=xp*sgam(itri)+yp*cgam(itri)
      zr=tmp*sbet(itri)+zp*cbet(itri)
      if(inside_triangle(xyrot(1,1,itri),xyrot(1,2,itri),xr,yr))then
c         
c         if x,y is inside planar triangle, that's the answer
c         
        xrot=xr
        yrot=yr
        dist=abs(zr-zrot(itri))
      else
c         
c         otherwise, get distance to each edge of triangle
c         
        dmin=1.e10
        do iv=1,3
          ivn=iv+1
          if(ivn.eq.4)ivn=1
          call point_line_dist(xyrot(iv,1,itri),xyrot(iv,2,itri),
     &        zrot(itri),xyrot(ivn,1,itri),xyrot(ivn,2,itri),zrot(itri)
     &        ,xr,yr,zr,t,dsqr)
          if(dsqr.lt.dmin)then
            ivmin=iv
            ivnmin=ivn
            dmin=dsqr
            tmin=t
          endif
        enddo     
        dist=sqrt(dmin)
        xrot=(1.-tmin)*xyrot(ivmin,1,itri)+tmin*xyrot(ivnmin,1,itri)
        yrot=(1.-tmin)*xyrot(ivmin,2,itri)+tmin*xyrot(ivnmin,2,itri)
      endif
      return
      end



c       SEGMENT_TO_TRIANGLE measures the distance of closest approach between
c       the line segment connecting XS,YS,ZS to XN,YN,ZN and the triangle
c       ITRI in common MTKCOM.  It returns the separation in DIST, the
c       parameter T specifying the position along the segment of the point of
c       closest approach (between 0 at XS,YS,ZS and 1 at XN,YN,ZN), and the
c       coordinates in the rotated triangle, XROT and YROT of the point of
c       closest approach to the triangle.
c       
      subroutine segment_to_triangle(xs,ys,zs,xn,yn,zn,itri,t,xrot,
     &    yrot,dist)
      include 'mtk.inc'
      logical inside_triangle,sin,nin,b3dxor
c       
c       rotate the endpoints
c       
      tmp=xs*cgam(itri)-ys*sgam(itri)
      xsr=tmp*cbet(itri)-zs*sbet(itri)
      ysr=xs*sgam(itri)+ys*cgam(itri)
      zsr=tmp*sbet(itri)+zs*cbet(itri)
      tmp=xn*cgam(itri)-yn*sgam(itri)
      xnr=tmp*cbet(itri)-zn*sbet(itri)
      ynr=xn*sgam(itri)+yn*cgam(itri)
      znr=tmp*sbet(itri)+zn*cbet(itri)
      sin=inside_triangle(xyrot(1,1,itri),xyrot(1,2,itri),xsr,ysr)
      nin=inside_triangle(xyrot(1,1,itri),xyrot(1,2,itri),xnr,ynr)
c       
      if(sin.and.nin)then
c         
c         if both endpoints are over triangle, then one must be closest
c         unless line passes through triangle
c         
        if(b3dxor(zsr.gt.zrot(itri), znr.gt.zrot(itri)))then
          dist=0.
          t=(zrot(itri)-zsr)/(znr-zsr)
          xrot=(1.-t)*xsr+t*xnr
          yrot=(1.-t)*ysr+t*ynr
        else
          dists=abs(zsr-zrot(itri))
          distn=abs(znr-zrot(itri))
          if(dists.lt.distn)then
            dist=dists
            xrot=xsr
            yrot=ysr
            t=0.
          else
            dist=distn
            xrot=xnr
            yrot=ynr
            t=1.
          endif
        endif
        return
      endif
c       
c       if one endpoint is over, it is a candidate
c       
      dist=1.e10
      if(sin)then
        xrot=xsr
        yrot=ysr
        dist=abs(zsr-zrot(itri))
        t=0.
      endif
      if(nin)then
        xrot=xnr
        yrot=ynr
        dist=abs(znr-zrot(itri))
        t=1.
      endif
c       
c       but still need to check each line segment
c       
      dmin=dist**2
      ivmin=0
      do iv=1,3
        ivn=iv+1
        if(ivn.gt.3)ivn=1
        call segment_dist(xyrot(iv,1,itri),xyrot(iv,2,itri),
     &      zrot(itri),xyrot(ivn,1,itri),xyrot(ivn,2,itri),
     &      zrot(itri),xsr,ysr,zsr,xnr,ynr,znr,t1,t2,dsqr)
        if(dsqr.lt.dmin)then
          ivmin=iv
          ivnmin=ivn
          dmin=dsqr
          t1min=t1
          t2min=t2
        endif
      enddo
c       
c       if a segment was it, need square root and rotated position
c       
      if(ivmin.gt.0)then
        dist=sqrt(dmin)
        xrot=(1.-t1min)*xyrot(ivmin,1,itri)+t1min*xyrot(ivnmin,1,itri)
        yrot=(1.-t1min)*xyrot(ivmin,2,itri)+t1min*xyrot(ivnmin,2,itri)
        t=t2min
      endif
      return
      end



c       TRIANGLE_TO_TRIANGLE computes the minimum distance between two
c       triangles.  It uses VERTS, INDVERT, XYROT, ZROT, SBET, CBET, SGAM,
c       and CGAM from the common MTKCOM.  ITRI1 and ITRI2 are the indices of
c       the two triangles.  The routine returns DIST, the distance between
c       triangles; ITRIR, the index of the triangle in whose rotated
c       coordinate system the closest approaching points are located;
c       XR1, YR1, ZR1, the coordinates of the point on the other triangle;
c       and XR2 and YR2, the coordinates of the closest point on the
c       triangle ITRIR.
c       
      subroutine triangle_to_triangle(itri1,itri2,xr1,yr1,zr1,xr2,yr2,
     &    itrir,dist)
      include 'mtk.inc'
      logical inside_triangle,vint(3,2),b3dxor
      real*4 xr(3,2),yr(3,2),zr(3,2)
      integer*4 jtri(2)
      jtri(1)=itri1
      jtri(2)=itri2
      distmin=1.e10
      do it=1,2
c         
c         rotate the endpoints
c         
        do iv=1,3
          ind=indvert(iv,jtri(it))
          itri=jtri(3-it)
          tmp=verts(1,ind)*cgam(itri)-verts(2,ind)*sgam(itri)
          xr(iv,it)=tmp*cbet(itri)-verts(3,ind)*sbet(itri)
          yr(iv,it)=verts(1,ind)*sgam(itri)+verts(2,ind)*cgam(itri)
          zr(iv,it)=tmp*sbet(itri)+verts(3,ind)*cbet(itri)
          vint(iv,it)=inside_triangle(xyrot(1,1,itri),xyrot(1,2,itri),
     &        xr(iv,it),yr(iv,it))
        enddo
c         
c         test for paired vertices on opposite sides of the other triangle
c         
        do iv=1,3
          ivn=mod(iv,3)+1
          if(vint(iv,it).and.vint(ivn,it).and.
     &        b3dxor(zr(iv,it).gt.zrot(itri), zr(ivn,it).gt.zrot(itri)))then
            dist=0.
            t=(zrot(itri)-zr(iv,it))/(zr(iv,it)-zr(ivn,it))
            xr1=xr(iv,it)*(1.-t)+xr(ivn,it)*t
            yr1=yr(iv,it)*(1.-t)+yr(ivn,it)*t
            zr1=zrot(itri)
            xr2=xr1
            yr2=yr1
            itrir=itri
            return
          endif
c           
c           if any one endpoint is over, it is a candidate for minimum point
c           being on the face of the triangle
c           
          dist=abs(zr(iv,it)-zrot(itri))
          if(vint(iv,it).and.dist.lt.distmin)then
            distmin=dist
            xr1=xr(iv,it)
            yr1=yr(iv,it)
            zr1=zr(iv,it)
            xr2=xr1
            yr2=yr1
            itrir=itri
          endif
          
        enddo
      enddo
c       
c       but still need to check each line segment pair, in coordinates of
c       rotated second triangle
c       
      dmin=distmin**2
      dist=distmin
      ivmin=0
      itri=itri2
      do iv=1,3
        ivn=mod(iv,3)+1
        do jv=1,3
          jvn=mod(jv,3)+1
          call segment_dist(xyrot(iv,1,itri),xyrot(iv,2,itri),
     &        zrot(itri),xyrot(ivn,1,itri),xyrot(ivn,2,itri),
     &        zrot(itri),xr(jv,1),yr(jv,1),zr(jv,1),xr(jvn,1),
     &        yr(jvn,1),zr(jvn,1),t1,t2,dsqr)
          if(dsqr.lt.dmin)then
            ivmin=iv
            ivnmin=ivn
            jvmin=jv
            jvnmin=jvn
            dmin=dsqr
            t1min=t1
            t2min=t2
          endif
        enddo
      enddo
c       
c       if a segment was it, need square root and interpolated positions
c       relative to rotated second triangle
c       
      if(ivmin.gt.0)then
        dist=sqrt(dmin)
        itrir=itri
        xr1=(1.-t2min)*xr(jvmin,1)+t2min*xr(jvnmin,1)
        yr1=(1.-t2min)*yr(jvmin,1)+t2min*yr(jvnmin,1)
        zr1=(1.-t2min)*zr(jvmin,1)+t2min*zr(jvnmin,1)
        xr2=(1.-t1min)*xyrot(ivmin,1,itri)+t1min*xyrot(ivnmin,1,itri)
        yr2=(1.-t1min)*xyrot(ivmin,2,itri)+t1min*xyrot(ivnmin,2,itri)
      endif
      return
      end


      subroutine crossproduct(avec,bvec,cvec)
      real*4 avec(*),bvec(*),cvec(*)
      cvec(1)=avec(2)*bvec(3)-avec(3)*bvec(2)
      cvec(2)=avec(3)*bvec(1)-avec(1)*bvec(3)
      cvec(3)=avec(1)*bvec(2)-avec(2)*bvec(1)
      return
      end


      function indexshift(iobj,iflag,icolor,npntobj,nmt)
      integer*4 icolor(*),npntobj(*)
      include 'mtk.inc'
      if(iflag.lt.4)then
        ityp=icolor(iobj)
      else
        ityp=iobjmesh(iobj)
      endif
      ifon=0
      do i=1,nobjshifted
        if(iobjshift(i).eq.ityp)ifon=i
      enddo
      if(ifon.eq.0)then
        indexshift=-1
        return
      endif
      indexshift=istrshift(ifon)
      if(iflag.eq.4)return
      do i=1,iobj-1
        if(icolor(i).eq.ityp)then
          if(iflag.eq.1)then
            indexshift=indexshift+1
          else
            indexshift=indexshift+npntobj(i)
          endif
        endif
      enddo     
      return
      end

c       
c       $Log$
c       Revision 3.5  2006/05/01 21:14:50  mast
c       Increased number of bins to 1001
c
c       Revision 3.4  2005/12/09 04:43:27  mast
c       gfortran: .xor., continuation, format tab continuation or byte fixes
c
c       Revision 3.3  2004/04/20 05:41:41  mast
c       fix some misprinted variables
c       
c       Revision 3.2  2003/10/26 05:33:27  mast
c       change command files to use unit 4 instead reopening 5
c       
c       Revision 3.1  2003/08/08 17:53:03  mast
c       Change terminology from points to objects
c       
