*       * * * * CHECKMTMOD * * * * *
c       
c       CHECKMTMOD will check a microtubule tracking model built on serial
c       sections for three kinds of errors: branch points, skipped sections,
c       and failure of the Z coordinate to increase monotonically. In
c       addition, it can search for situations where the start of one contour
c       is near the end of another contour, so that one can then check
c       whether these contours are in fact the same microtubule.
c       
c       All branch points are reported.  The Z coordinate is expected to
c       increase monotonically within each contour, so there will be a report
c       whenever Z stays the same or decreases.  If an entire contour is in
c       one Z plane, there will be just one report for that contour.
c       Z values are expected to occur in a regular progression, and gaps in
c       that progression will be reported unless they occur at missing
c       sections, at which no contours have points.
c       
c       You can specify a list of additional sections on which some points
c       may be intentionally missing.  The program will then not report the
c       contours that skip over that section.  You can enter a list with
c       ranges, such as 1-3,37,78.
c       
c       To search for starts near ends, enter a distance within the X-Y plane
c       to search, and the maximum difference in Z coordinates to search.
c       (Enter / to accept the default values for these parameters, or enter
c       0,0 to skip the search).
c       
c       Enter the scaling in Z relative to the X and Y scaling (this would
c       be the same as the scale value used to display
c       the model at the true scale).  This value is needed to compute angles
c       between trajectories of MTs with nearby ends.  If you are not
c       searching for starts near ends, it doesn't matter what you enter.
c       
c       When searching for starts near ends, the program will report
c       the starting and ending points whenever those two points are within
c       the selected distance in the X-Y plane and the Z value of the
c       starting point minus the Z value of the ending point is positive and
c       less than the selected difference in Z values.  The program also
c       fits a line to the last three points in the contour that ends, and
c       another line to the first 3 points in the contour that starts.  It
c       uses these lines to extrapolate the trajectory of each contour to the
c       position in Z midway between the ending and starting points.  It then
c       reports the distance between these extrapolated positions, and the
c       angle between the trajectories in degrees.
c       
c       For each match, the program outputs on two lines the following
c       information:
c       
c       # of contour that ends, X, Y, Z coordinates of the ending point,
c       # of contour that starts, X, Y, Z coordinates of the starting point,
c       separation between objects extrapolated to a single plane in Z,
c       and angle between the trajectories of the objects.
c       
c       This information on extrapolated separation and angle can be used
c       to zero in on the matches that need to be examined in the model;
c       most "matches" can be eliminated without examination because either
c       the angle or the separation is unreasonably large.
c       
c       David Mastronarde  1/21/90, jazzed up 4/32/93
c       modified for IMOD 4/25/97
c       
      include 'model.inc'

      logical exist,readw_or_imod,zused(10000)
      character*80 modelfile
      integer*4 izskipok(200)
      real*4 xfit(10),yfit(10),zfit(10)
      data distmax/100./,zsepmax/5./
c       
c       get model
c       
10    write(*,'(1x,a,$)')'Name of model file: '
      read(*,'(a)')modelfile
15    exist=readw_or_imod(modelfile)
      if(.not.exist)then
        print *,'no good, try again'
        go to 10
      endif
c       
      print *,'Skipped points will not be reported for sections that',
     &    ' have no points at all.'
      print *,'Enter list of additional section numbers for which',
     &    ' skipped points',' should not be',
     &    '     reported (ranges OK, Return for none)'
      call rdlist(5,izskipok,nzskipok)
c       
      write(*,'(1x,a,/,a,/,a,f4.0,a,f3.0,a,$)')'Maximum distance in'//
     &    ' X-Y plane and maximum separation in Z to search'//
     &    ' for','   starting points near ending points, or 0,0 for'
     &    //' no search','    [/ for ',distmax,',',zsepmax,']: '
      read(*,*)distmax,zsepmax
c       
      write(*,'(1x,a,$)')'Z scaling relative to X and Y (needed to'//
     &    ' compute angles): '
      read(5,*)zscal
      distsqr=distmax**2
c       
c       find min and max z
c       
      izmin=100000
      izmax=-100000
      do iobj=1,max_mod_obj
        ibase=ibase_obj(iobj)
        do i=1,npt_in_obj(iobj)
          jpnt=abs(object(i+ibase))
          iz=nint(p_coord(3,jpnt))
          izmin=min(izmin,iz)
          izmax=max(izmax,iz)
        enddo
      enddo
c       
c       find which z values are used
c       
      nz=izmax+1-izmin
      do i=1,nz
        zused(i)=.false.
      enddo
c       
      do iobj=1,max_mod_obj
        ibase=ibase_obj(iobj)
        do i=1,npt_in_obj(iobj)
          jpnt=abs(object(i+ibase))
          iz=nint(p_coord(3,jpnt))
          zused(iz+1-izmin)=.true.
        enddo
      enddo
c       
c       take out the ones that are ok to skip
c       
      do iok=1,nzskipok
        ind=izskipok(iok)+1-izmin
        if(ind.gt.0.and.ind.le.nz)zused(ind)=.false.
      enddo
c       
c       now go through objects looking for problems
c       
      write(*,110)
110   format(/,' O   2, C   20 (   65)  means IMOD object 2, ',
     &    'contour 20, WIMP object 65',/)
      do iobj=1,max_mod_obj
        call objtocont(iobj,obj_color,imodobj,imodcont)
        ibase=ibase_obj(iobj)
        ifanyz=0
        do ipt=1,npt_in_obj(iobj)
          jpnt=abs(object(ipt+ibase))
          iz=nint(p_coord(3,jpnt))
          if(ipt.gt.1)then
            if(iz.ne.izlast)ifanyz=ifanyz+1
          endif
          izlast=iz
        enddo
        if(ifanyz.gt.0.or.npt_in_obj(iobj).eq.0)then
          do ipt=1,npt_in_obj(iobj)
            jpnt=object(ipt+ibase)
            if(jpnt.gt.0)then
              iz=nint(p_coord(3,jpnt))
              if(ipt.gt.1)then
                if(iz.le.izlast)then
                  write(*,101)imodobj,imodcont,iobj,ipt,
     &                (p_coord(j,jpnt),j=1,3)
                else
                  nskip=0
                  do izchk=izlast+1,iz-1
                    if(zused(izchk+1-izmin))nskip=nskip+1
                  enddo
                  if(nskip.gt.0)write(*,102)nskip,imodobj,imodcont,
     &                iobj,ipt, (p_coord(j,jpnt),j=1,3)
                endif
              endif
              izlast=iz
            else
              write(*,103)imodobj,imodcont,iobj,ipt,(p_coord(j,-jpnt),j=1,3)
              izlast=p_coord(3,-jpnt)
            endif
          enddo
        else
          write(*,105)imodobj,imodcont,iobj,npt_in_obj(iobj),izlast
        endif
      enddo
101   format('  Z fails to increase, O',i4,', C',i5,' (',
     &    i5,') pt',i5,', XYZ',2f8.1,f6.0)
102   format(i4,' Z values skipped, O',i4,', C',i5,' (',
     &    i5,') pt',i5,', XYZ',2f8.1,f6.0)
103   format('    Branch point in O',i4,', C',i5,' (',
     &    i5,') pt',i5,', XYZ',2f8.1,f6.0)
105   format('  O',i4,', C',i5,' (',i5,') is planar;',i5,
     &    ' points, Z =',i5)
c       
      write(*,*)
      if(zsepmax.gt.0)then
        print *,'Checking for errors completed.'
        print *,'Checking for object starts near object ends;',
     &      '   Object # and X-Y-Z coordinates are reported next:'
c         
c         go through objects in model, comparing last point of each to
c         first point in all other objects
c         
        do iobj=1,max_mod_obj
          ninobj1=npt_in_obj(iobj)
          call objtocont(iobj,obj_color,imodobj,imodcont)
          if(ninobj1.gt.1)then
            jpnt=abs(object(ninobj1+ibase_obj(iobj)))
            xx=p_coord(1,jpnt)
            yy=p_coord(2,jpnt)
            zz=p_coord(3,jpnt)
            if(zz.ne.p_coord(3,object(1+ibase_obj(iobj))))then
              do job=1,max_mod_obj
                call objtocont(job,obj_color,jmodobj,jmodcont)

                if(npt_in_obj(job).gt.1.and.job.ne.iobj)then
                  ipnt2=abs(object(1+ibase_obj(job)))
                  zstrt=p_coord(3,ipnt2)
                  if(zstrt.ne.p_coord(3,abs(
     &                object(npt_in_obj(job)+ibase_obj(job)))))then
                    if(zstrt.gt.zz.and.
     &                  zstrt-zz.le.zsepmax)then
                      dx=abs(xx-p_coord(1,ipnt2))
                      dy=abs(yy-p_coord(2,ipnt2))
                      if(dx.le.distmax.and.dy.le.distmax.and.
     &                    dx**2+dy**2.le.distsqr)then
c                         
c                         got a match,
c                         
                        zmid=0.5*zscal*(zz+zstrt)
                        nfit=0
                        do ipt=max(1,ninobj1-2),ninobj1
                          jpack=abs(object(ipt+ibase_obj(iobj)))
                          nfit=nfit+1
                          xfit(nfit)=p_coord(1,jpack)
                          yfit(nfit)=p_coord(2,jpack)
                          zfit(nfit)=zscal*p_coord(3,jpack)
                        enddo
                        call lsfit(zfit,xfit,nfit,bx1,ax1,ro)
                        call lsfit(zfit,yfit,nfit,by1,ay1,ro)
                        xtrap1=ax1+bx1*zmid
                        ytrap1=ay1+by1*zmid
c                         
                        nfit=0
                        do ipt=1,min(3,npt_in_obj(job))
                          jpack=abs(object(ipt+ibase_obj(job)))
                          nfit=nfit+1
                          xfit(nfit)=p_coord(1,jpack)
                          yfit(nfit)=p_coord(2,jpack)
                          zfit(nfit)=zscal*p_coord(3,jpack)
                        enddo
                        call lsfit(zfit,xfit,nfit,bx2,ax2,ro)
                        call lsfit(zfit,yfit,nfit,by2,ay2,ro)
                        xtrap2=ax2+bx2*zmid
                        ytrap2=ay2+by2*zmid
                        septrap=sqrt((xtrap1-xtrap2)**2+
     &                      (ytrap1-ytrap2)**2)
                        cosang=(1.+bx1*bx2+by1*by2)/sqrt
     &                      ((1.+bx1**2+by1**2)*(1.+bx2**2+by2**2))
                        ang=acosd(cosang)
                        write(*,104)imodobj,imodcont,iobj,xx,yy,zz,
     &                      jmodobj,jmodcont,job,
     &                      (p_coord(i,ipnt2),i=1,3),septrap,ang
                      endif
                    endif
                  endif
                endif
              enddo
            endif
          endif
        enddo
104     format(/,' O',i4,', C',i5,' (',i5,') ends at ',2f7.0,f6.0,
     &      /,' O',i4,', C',i5,' (',i5,') starts @',2f7.0,f6.0,
     &      ' sep',f5.0,f5.0,'deg')
      endif
c       
      print *, 'CHECKING COMPLETED'
      call exit(0)
      end
