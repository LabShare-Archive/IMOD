c       FIND_SURFACES will analyze a set of NREALPT points, with coordinates
c       in XYZ, correlate the Z with the X and Y coordinates of those
c       points, and determine the angles that the points would have to be
c       tilted first around the X axis then around the Y axis in order for
c       them to lie parallel to the X-Y plane.  From these tilts, it will
c       estimate the true tilt angle of the section, TILTNEW, from the tilt
c       angle used in deriving the point coordinates, TILTMAX.  If NSURFACE
c       is 1, it will simply fit a plane to all of the points and base its
c       estimates on that fit.  If NSURFACE is 2, it will attempt to divide
c       the points into two groups occupying two surfaces, then provide
c       separate estimates of the new tilt angle based on the slope of the
c       plane through either set of points, or on the average slope.
c       IGROUP is an array returned with a value for each point of 1 if
c       on lower surface, 2 if on upper, but only if 2-surface analysis is
c       done
c       
c       $Id$
c       Log at end of file
c       
      subroutine find_surfaces(xyz,nrealpt,nsurface,tiltmax,
     &    iunit2,tiltnew,igroup,ifcomp,tiltadd, znew, imageBinned)
      implicit none
      integer maxreal
      real*4 xyz(3,*), tiltmax, tiltnew, tiltadd, bintmi, bint, znew
      integer*4 nrealpt, nsurface, iunit2, ifcomp,imageBinned
      parameter (maxreal=2000)
      real*4 xx(maxreal),yy(maxreal),zz(maxreal),zrot(maxreal)
      integer*4 igroup(*),icheck(maxreal/2)
      integer*4 i, iun, niter, iter,ipt, npntmi,npntpl,iterlim,limcheck,ncheck
      real*4 aslop,bslop,alpha,slop,resid,truepl,slopav,alphaav,theta,costh
      real*4 sinth,cosal,sinal,zmin,zmax,zp,zmiddle,aslopmi,bslopmi, alphami
      real*4 slopmi,residmi,err,resmax,res,errnew,thick,truemi
      real*4 shiftInc,shiftTot, botExtreme, topExtreme
      integer*4 iptmax,ifcheck
      logical changed

      real*4 cosd,sind,atand
c       
c       first fit a line to all of the points to get starting angle
c       
      do i=1,nrealpt
        xx(i)=xyz(1,i)
        yy(i)=xyz(2,i)
        zz(i)=xyz(3,i)
        igroup(i)=0
      enddo
      call lsfit2_resid(xx,yy,zz,nrealpt,aslop,bslop,bint,alpha,slop,
     &    resid, costh, sinth)

      do iun=6,iunit2
        write(iun,'(/,a,//,a,//,a,/,a,f8.4)') ' SURFACE ANALYSIS:',
     &      ' The following parameters are appropriate if fiducials'
     &      //' are all on one surface',
     &      ' Fit of one plane to all fiducials:',
     &      ' Adjusted slope =',slop
      enddo
c       
c       if there are supposed to be 2 surfaces, enter an iterative procedure
c       that rotates points based on the current average slope (initially
c       the slope of the fit to all points, later the average of slopes for
c       the top and bottom surfaces), finds the midpoint of the range of
c       rotated Z values, assigns points to the lower or upper surface based
c       on this midpoint, and fits lines to lower and upper surface points.
c       
      if(nsurface.gt.1)then
        call calc_tiltnew(slop,tiltmax,iunit2,truepl,ifcomp,tiltadd)
        do iun=6,iunit2
          write(iun,'(a,/,a,/)')' The following '//
     &        'parameters are provided to indicate the consistency',
     &        ' of the fit when fiducials are on two surfaces'
        enddo
        niter=6
c         iterate at least twice and until get same result twice
        changed=.true.
        iter=1
        slopav=slop
        alphaav=alpha
        do while(iter.le.niter.and.changed)
          theta=-atand(slopav)
          costh=cosd(theta)
          sinth=sind(theta)
          cosal=cosd(alphaav)
          sinal=sind(alphaav)
          changed=iter.eq.1
c           
c           rotate each point, get range and midpoint
c           
          zmin=1.e10
          zmax=-1.e10
          do ipt=1,nrealpt
            zp=xyz(3,ipt)*cosal-xyz(2,ipt)*sinal
            zrot(ipt)=xyz(1,ipt)*sinth+zp*costh
            zmin=min(zmin,zrot(ipt))
            zmax=max(zmax,zrot(ipt))
          enddo
          zmiddle=(zmin+zmax)/2.
c           
c           now assign points to groups based on Z values
c           
          do ipt=1,nrealpt
            if(zrot(ipt).le.zmiddle)then
              changed=changed.or.(igroup(ipt).eq.2)
              igroup(ipt)=1
            else
              changed=changed.or.(igroup(ipt).eq.1)
              igroup(ipt)=2
            endif
          enddo

          call two_surface_fits(xyz,igroup,nrealpt,xx,yy,zz, npntmi, aslopmi,
     &        bslopmi,bintmi, alphami,slopmi,residmi, npntpl, aslop, bslop,
     &        bint, alpha,slop,resid,slopav,alphaav, botExtreme, topExtreme)

c           write(*,101)npntmi,bintmi,slopmi,romi
c           write(*,101)npntpl,bint,slop,ro
          iter=iter+1
        enddo
c         
c         another round of iterations: moving points with large residuals
c         between groups to see if it helps
c         
        iterlim=1000
        limcheck=nrealpt/2
        ncheck=0
        iter=1
        err=npntmi*residmi+npntpl*resid
        do while(ncheck.lt.limcheck.and.iter.lt.iterlim)
c           
c           find point with maximum residual, excluding ones already checked
c           
          resmax=-1.
          do ipt=1,nrealpt
            ifcheck=0
            do i=1,ncheck
              if(ipt.eq.icheck(i))ifcheck=1
            enddo
            if(ifcheck.eq.0)then
              if(igroup(ipt).eq.1)then
                res=abs(xyz(3,ipt)-(aslopmi*xyz(1,ipt)+
     &              bslopmi*xyz(2,ipt)+bintmi))
              else
                res=abs(xyz(3,ipt)-(aslop*xyz(1,ipt)+
     &              bslop*xyz(2,ipt)+bint))
              endif
              if(res.gt.resmax)then
                resmax=res
                iptmax=ipt
              endif
            endif
          enddo
c           
c           don't move a point if the donor group has less than 4 or the
c           receiving group has less than 3
c           
          if((igroup(iptmax).eq.1.and.(npntmi.lt.4.or.npntpl.lt.3))
     &        .or.(igroup(iptmax).eq.2.and.
     &        (npntmi.lt.3.or.npntpl.lt.4)))then
            ncheck=ncheck+1
            icheck(ncheck)=iptmax
          else
c             
c             swap that point into the other group
c             
            igroup(iptmax)=3-igroup(iptmax)
            call two_surface_fits(xyz,igroup,nrealpt,xx,yy,zz, npntmi, aslopmi,
     &          bslopmi,bintmi, alphami,slopmi,residmi, npntpl, aslop, bslop,
     &          bint, alpha,slop,resid,slopav,alphaav, botExtreme, topExtreme)
            errnew=npntmi*residmi+npntpl*resid
c             
            if(errnew.lt.err)then
c               
c               if error is less, make change permanent, clear out list of
c               checked ones, increment iterations
c               
              err=errnew
              ncheck=0
              iter=iter+1
            else
c               
c               otherwise restore and refit, save this point on checked list
c               
              igroup(iptmax)=3-igroup(iptmax)
              call two_surface_fits(xyz,igroup,nrealpt,xx,yy,zz,
     &            npntmi, aslopmi,bslopmi,bintmi, alphami,slopmi,
     &            residmi, npntpl, aslop, bslop,bint, alpha,slop,
     &            resid,slopav,alphaav, botExtreme, topExtreme)
              ncheck=ncheck+1
              icheck(ncheck)=iptmax
            endif
          endif
        enddo
c         
        if(npntmi.gt.1)then
          do iun=6,iunit2
            write(iun,101)'bottom',npntmi,bintmi,slopmi,residmi,-alphami
101         format(' Fit of one plane to points on ',a,' surface:',/,
     &          ' # of points = ',i6,/,' Z axis intercept =',f8.2,/,
     &          ' Adjusted slope =' ,f10.4,/,' Mean residual =',f11.3,/,
     &          ' X axis tilt needed =',f6.2)
          enddo
          call calc_tiltnew(slopmi,tiltmax,iunit2,truemi,ifcomp,tiltadd)
        endif
      else
        npntpl=nrealpt
      endif
      if(npntpl.gt.1)then
        do iun=6,iunit2
          write(iun,101)'top',npntpl,bint,slop,resid,-alpha
        enddo
        call calc_tiltnew(slop,tiltmax,iunit2,truepl,ifcomp,tiltadd)
      endif
      if(nsurface.eq.1)then
        tiltnew=truepl
      else
c         
c         but just return distance between intercepts
c         
        thick=bint-bintmi
        do iun=6,iunit2
          print *,'The following parameters combine the last two '//
     &        'results and are','appropriate if fiducials are on'//
     &        ' two surfaces'
          write(iun,102)thick,slopav,-alphaav
102       format(/,' Thickness at Z intercepts =',f11.2,/,
     &        ' Average adjusted slope =',f14.4,/,
     &        ' Average X axis tilt needed =',f10.2)
        enddo
        call calc_tiltnew(slopav,tiltmax,iunit2,tiltnew,ifcomp,tiltadd)
c
c         Get the unbinned thickness and shifts needed to center the gold
c         The direction is opposite to expected because the positive Z points
c         come out on the bottom of the tomogram, presumably due to rotation
        thick = imageBinned * (topExtreme - botExtreme)
        shiftTot = imageBinned * (topExtreme + botExtreme) / 2.
        shiftInc = shiftTot - imageBinned * znew
        do iun=6,iunit2
          write(iun,103)thick,shiftInc,shiftTot
103       format(' Unbinned thickness needed to contain centers of all ',
     &        'fiducials =', f13.0,/,
     &        ' Incremental unbinned shift needed to center range of fi',
     &        'ducials in Z =',f8.1,/,
     &        ' Total unbinned shift needed to center range of fiducial',
     &        's in Z =',f14.1)
        enddo
      endif
      return
      end


      subroutine two_surface_fits(xyz,igroup,nrealpt,xx,yy,zz, npntmi,
     &    aslopmi,bslopmi,bintmi, alphami,slopmi,residmi, npntpl, aslop, bslop,
     &    bint, alpha,slop,resid,slopav,alphaav,botExtreme, topExtreme)
      implicit none
      real*4 xyz(3,*),xx(*),yy(*),zz(*),aslopmi,bslopmi,bintmi, alphami,slopmi
      real*4 residmi,aslop, bslop,bint, alpha,slop,resid,slopav,alphaav
      real*4 devmin, devmax, botExtreme, topExtreme
      integer*4 igroup(*),npntmi,nrealpt,npntpl
      integer*4 ipt
      real*4 xxmi,yymi,zzmi
c       
c       first fit a line to points with z below middle
c       
      npntmi=0
      do ipt=1,nrealpt
        if(igroup(ipt).eq.1)then
          npntmi=npntmi+1
          xx(npntmi)=xyz(1,ipt)
          yy(npntmi)=xyz(2,ipt)
          zz(npntmi)=xyz(3,ipt)
        endif
      enddo
      if(npntmi.gt.1)then
        call lsfit2_resid(xx,yy,zz,npntmi,aslopmi,bslopmi,bintmi,
     &      alphami,slopmi,residmi, devmin, devmax)
        botExtreme = bintmi + devmin
      else
        xxmi=xx(1)
        yymi=yy(1)
        zzmi=zz(1)
      endif
c       
c       next fit a line to points with z above cutoff
c       
      npntpl=0
      do ipt=1,nrealpt
        if(igroup(ipt).eq.2)then
          npntpl=npntpl+1
          xx(npntpl)=xyz(1,ipt)
          yy(npntpl)=xyz(2,ipt)
          zz(npntpl)=xyz(3,ipt)
        endif
      enddo
      if(npntpl.gt.1)then
        call lsfit2_resid(xx,yy,zz,npntpl,aslop,
     &      bslop,bint, alpha,slop,resid, devmin, devmax)
        topExtreme = bint + devmax
      endif
c       
c       get slop and intercept of each line, even if only 1 point
c       
      if(npntmi.eq.1)then
        slopmi=slop
        bintmi=zzmi-yymi*bslop-xxmi*aslop
        alphami=alpha
        botExtreme = bintmi
      endif
      if(npntpl.eq.1)then
        slop=slopmi
        bint=zz(1)-yy(1)*bslopmi-xx(1)*aslopmi
        alpha=alphami
        topExtreme = bint
      endif
      slopav=(npntpl*slop+npntmi*slopmi)/nrealpt
      alphaav=(npntpl*alpha+npntmi*alphami)/nrealpt
      return
      end

c       DNM 5/3/02: give the new maximum tilt angle only for compression
c       solutions
c       
      subroutine calc_tiltnew(slopmi,tiltmax,iunit2,truemi,ifcomp,
     &    tiltadd)
      implicit none
      real*4 slopmi,tiltmax,truemi,tiltadd
      integer*4 iunit2,ifcomp
      real*4 cosnew,globdelt
      integer*4 iun
      real*4 atand,acosd,cosd,sind

      cosnew=slopmi*sind(tiltmax)+cosd(tiltmax)
      globdelt=atand(-slopmi)
      do iun=6,iunit2
        write(iun,102)globdelt,globdelt+tiltadd
102     format(' Incremental tilt angle change =',f7.2,/,
     &      ' Total tilt angle change =',f13.2)
        if (ifcomp.ne.0)then
          if(abs(cosnew).le.1.)then
            truemi=sign(acosd(cosnew),tiltmax)
            write(iun,103)tiltmax,truemi
103         format('     or, change a fixed maximum tilt angle from'
     &          ,f8.2,' to',f8.2,/)
          else
            write(iun,'(a,/)') '      or. . . But cannot derive '//
     &          'implied tilt angle: |cos| > 1'
          endif
        else
          write(iun,*)
        endif
      enddo
      return
      end
      

      subroutine lsfit2_resid(x,y,z,n,a,b,c,alpha,slop,resid, devmin, devmax)
      implicit none
      real*4 x(*),y(*),z(*),a,b,c,alpha,slop,resid,ro,devmin, devmax, dev
      integer*4 n,i
      real*4 sind,cosd,atand

      if(n.gt.2)then
        call lsfit2(x,y,z,n,a,b,c)
      else
        b=0.
        call lsfit(x,z,n,a,c,ro)
      endif
      resid=0.
      devmin = 1.e10
      devmax = -1.e10
      do i=1,n
        dev = z(i)-(x(i)*a+y(i)*b+c)
        resid=resid+abs(dev)
        devmin = min(devmin, dev)
        devmax = max(devmax, dev)
      enddo
      resid=resid/n
      alpha=atand(b)
      slop=a/(cosd(alpha)-b*sind(alpha))
      return
      end
c       
c       $Log$
c       Revision 3.4  2008/12/12 00:47:21  mast
c       Add output of unbinned thickness and shift needed to center planes
c
c       Revision 3.3  2005/12/09 04:43:27  mast
c       gfortran: .xor., continuation, format tab continuation or byte fixes
c
c       Revision 3.2  2005/12/08 03:46:51  mast
c       Overdimension maxreal since it gets forgotten about
c       
c       Revision 3.1  2002/05/07 02:05:53  mast
c       Changed output to make it more understandable and readable
c       
