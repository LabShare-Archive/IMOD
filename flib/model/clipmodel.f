* * * * * CLIPMODEL * * * * *
c	  
c       CLIPMODEL will clip out a portion of a model and output either a
c       new model file, or a simple list of point coordinates.  It can either
c       include only points within a defined rectangular block of 3-
c       dimensional coordinate space, or exclude points within such a block.
c       This process of inclusion or exclusion may be invoked repeatedly.
c       The program can also eliminate an entire IMOD object (or all WIMP
c       objects of specified colors).
c       
c       See man page for details
c
c       $Id$
c       Log at end of file
c       
c       David Mastronarde 1/11/90
c
      include 'model.inc'
      integer limz, limobj
      parameter (limz = 100000, limobj = 100000)
      integer*4 listz(limz),icolelim(limobj),iflags(limobj)
      logical exist,readw_or_imod,inside,lastinside,getModelObjectRange
      integer*4 getimodflags,getimodhead,getimodscales,getImodObjSize
      character*320 modelfile
      character*2 inex
      character*7 pixmic

      call imodPartialMode(1)
10    write(*,'(1x,a,$)')'Name of input model file to clip: '
      read(*,'(a)')modelfile
15    exist=readw_or_imod(modelfile)
      if(.not.exist)go to 10
c       
      write(*,'(1x,a,$)')'Name of output model or point file: '
      read(*,'(a)')modelfile
c       
      write(*,'(1x,a,$)')'0 for model output, 1 for point file'//
     &    ' output (-1 for corner points also): '
      read(*,*)ifpoint
c       
      write(*,'(1x,a,/,a,$)')'0 to retain only longest included'//
     &    ' segment,','  or 1 to retain all points within limits: '
      read(5,*)ifkeepall
c       
      do i=1,limobj
        iflags(i)=0
      enddo
      ierr=getimodflags(iflags,limobj)
      if(ierr.ne.0)print *,'Error getting object types, assuming',
     &    ' all are closed contours'
      nobjTot = getImodObjSize()
      pixmic='microns'
      defscal=1.e6
c       
c       DNM 7/7/02: get image scales, and switch to exiting on error
c       
      ierr=getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip)
      if (ierr .ne. 0) then
        print *,'CLIPMODEL: Error getting model header'
        call exit(1)
      endif
      if(ierr.ne.0.or.abs(xyscal-defscal)/defscal.lt.1.e-5)then
        xyscal=1.
        pixmic='pixels'
      endif
      zscal=xyscal*zscale
      if(ifflip.eq.0)then
        indy=2
        indz=3
      else
        indz=2
        indy=3
      endif
c       
16    write(*,'(1x,a,/,a,$)') 'Enter 0 to include or 1 to exclude'//
     &    ' points in a specified coordinate block,','   or -1 or -2'
     &    //' to exclude or shorten particular objects: '
      read(*,*)ifexcl
      if(ifexcl.ge.0)then
        inex='in'
        if(ifexcl.ne.0)inex='ex'
c         
        xmin=-1.e10
        xmax=1.e10
        ymin=-1.e10
        ymax=1.e10
18      write(*,'(1x,a,$)')'Minimum and maximum X and Y to '
     &      //inex//'clude (/ for no limits): '
        read(*,*)xmin,xmax,ymin,ymax
        if(ifpoint.lt.0.and.ymax.eq.1.e10)then
          print *,'If you want corner points, then you',
     &        ' MUST enter actual min and max X & Y'
          go to 18
        endif
        write(*,'(1x,a,$)')
     &      'Minimum and maximum Z to '//inex//'clude: '
        read(*,*)zmin,zmax
        if(xmax-xmin.lt.1.e9.and.ymax-ymin.lt.1.e9)then
          volume=(zmax+1-zmin)*(xmax+1-xmin)*(ymax+1-ymin)*zscal*
     &        xyscal**2
          write(*,105)volume,pixmic
105       format(/,' Selected volume is',f20.10,' cubic ',a)
        endif
      else
        if(ifexcl.lt.-1)then
          write(*,'(1x,a,$)')'Number of points to remove from start,'
     &        //' # to remove from end: '
          read(5,*)nremstr,nremend
          print *,'Enter list of object numbers for contours to ',
     &        'shorten, with negative'
        else
          print *,'Enter list of numbers of objects to',
     &        ' eliminate, with negative'
        endif
        print *,'  numbers for WIMP objects that are turned ',
     &      'off (ranges OK)'
        call rdlist(5,icolelim,ncolelim)
      endif
c       
c       look at each object, find longest contiguous segment within area
c       Increase Z limits a bit in case there are slightly off Z values
      zmin = zmin - 0.005
      zmax = zmax + 0.005
      n_ptot=0
      n_ptotold=0
      cutsum=0.
      nonemptyold=0
      nonemptynew=0
      do imodObj = 1, nobjTot

        if (.not.getModelObjectRange(imodObj, imodObj)) then
          write(*,'(/,a,i6)') 'ERROR: CLIPMODEL - LOADING DATA FOR OBJECT #',
     &        imodobj
          call exit(1)
        endif
        call scale_model(0)
c         
        do iobj=1,max_mod_obj
          n_ptotold=n_ptotold+npt_in_obj(iobj)
        enddo
c         
        do iobj=1,max_mod_obj
          ninobj=npt_in_obj(iobj)
          if(ninobj.gt.0)then
            nonemptyold=nonemptyold+1
            if(ifexcl.ge.0)then
c              imodobj=256-obj_color(2,iobj)
              ibase=ibase_obj(iobj)
              if(ifkeepall.eq.0.and.mod(iflags(imodobj),4).ne.2)then
c               
c                 keeping only longest segment inside limits: can be used for
c                 open or closed contours but not scattered points
c               
                lastinside=.false.
                iptstbest=1
                iptndbest=0
                do ipt=1,ninobj
                  jpnt=abs(object(ipt+ibase))
                  xx=p_coord(1,jpnt)
                  yy=p_coord(indy,jpnt)
                  zz=p_coord(indz,jpnt)
c                   this could be done with .xor. but it would take you longer
c                   to figure out what was going on
                  inside=(xx.ge.xmin.and.xx.le.xmax).and.
     &                (yy.ge.ymin.and.yy.le.ymax).and.
     &                (zz.ge.zmin.and.zz.le.zmax)
                  if(ifexcl.ne.0)inside=.not.inside
                  if(inside)iptinside=ipt
                  if(inside.and..not.lastinside)then
                    iptst=ipt
                  endif
                  if(lastinside.and..not.inside .or.
     &                ipt.eq.ninobj.and.inside)then
                    if(iptndbest-iptstbest .lt. iptinside-iptst)then
                      iptndbest=iptinside
                      iptstbest=iptst
                    endif
                  endif
                  lastinside=inside
                enddo
c               
c                 truncate object and point base to new start
c                 
                npt_in_obj(iobj)=iptndbest+1-iptstbest
                ibase_obj(iobj)=ibase_obj(iobj)+iptstbest-1
              else
c               
c                 keeping all points inside limits
c               
                ipt=1
                ifcut=0
                ifstartcut = 0
                modflag=mod(iflags(imodobj),4)
                do while (ipt.le.ninobj)
                  jpnt=abs(object(ipt+ibase))
                  xx=p_coord(1,jpnt)
                  yy=p_coord(indy,jpnt)
                  zz=p_coord(indz,jpnt)
c                   this could be done with .xor. but it would take you longer
c                   to figure out what was going on
                  inside=(xx.ge.xmin.and.xx.le.xmax).and.
     &                (yy.ge.ymin.and.yy.le.ymax).and.
     &                (zz.ge.zmin.and.zz.le.zmax)
                  if(ifexcl.ne.0)inside=.not.inside
                  if(inside)then
                    if(ifcut.ne.0.and.modflag.ne.2)then
                      if(ipt.eq.1)then
c                         
c                         if last point was cut out and we are still at the
c                         start, then mark the start as cut
c                         
                        ifstartcut=1
                      else
c                         
c                         otherwise, compute distance between this & last point
c                         
                        cutsum=cutsum+sqrt(
     &                      (xyscal*(p_coord(1,jpnt)-p_coord(1,lpnt)))**2 +
     &                      (xyscal*(p_coord(indy,jpnt)-
     &                      p_coord(indy,lpnt)))**2+
     &                      (zscal*(p_coord(indz,jpnt)-p_coord(indz,lpnt)))**2)
                      endif
                    endif
                    ifcut=0
                    ipt=ipt+1
                    lpnt=jpnt
                  else
                    do i=ipt+ibase+1,1+ibase+ninobj
                      object(i-1)=object(i)
                    enddo
                    ninobj=ninobj-1
                    ifcut=1
                  endif
                enddo
                npt_in_obj(iobj)=ninobj
c                 
c                 If there are any points left, and either the start was cut
c                 or the last point was cut, and this is a closed contour,
c                 then add distance between endpoints as a cut edge
c               
                if(ninobj.gt.0.and.(ifstartcut.gt.0.or.ifcut.ne.0).and.
     &              modflag.eq.0)then
                  jpnt=abs(object(1+ibase))
                  cutsum=cutsum+sqrt(
     &                (xyscal*(p_coord(1,jpnt)-p_coord(1,lpnt)))**2 +
     &                (xyscal*(p_coord(indy,jpnt)- p_coord(indy,lpnt)))**2+
     &                (zscal*(p_coord(indz,jpnt)-p_coord(indz,lpnt)))**2)
                endif
              endif
c             
c               if getting rid of certain color, check for color
c               
            else
              icoltst=256-obj_color(2,iobj)
              if(obj_color(1,iobj).eq.0)icoltst=-icoltst
              iftreat=0
              do icl=1,ncolelim
                if(icoltst.eq.icolelim(icl))iftreat=1
              enddo
              if(iftreat.eq.1)then
                if(ifexcl.eq.-1)then
                  npt_in_obj(iobj)=0
                else
                  npt_in_obj(iobj)=max(0,ninobj-nremstr-nremend)
                  ibase_obj(iobj)=ibase_obj(iobj)+nremstr
                endif
              endif
            endif
            n_ptot=n_ptot+npt_in_obj(iobj)
            if(npt_in_obj(iobj).eq.0)then
              n_object=n_object-1
            else
              nonemptynew=nonemptynew+1
            endif
          endif
        enddo
        call scale_model(1)
        call putModelObjects()
      enddo
c       
c       write out result, offer to go back for more
c       
      write(*,102)nonemptyold,nonemptynew,n_ptotold,n_ptot
102   format(/,' Number of non-empty contours reduced from',i7,' to',
     &    i8,/, ' Number of points reduced from',i8,' to',i8,/)
c       
      if(cutsum.gt.0.)then
        write(*,103)cutsum,pixmic,cutsum*zscal,pixmic
103     format(' Total length of cut edges is',f15.5,' ',a,/,
     &      ' Approximate surface area of cut edges is',f18.8,
     &      ' square ',a,/)
      endif
c       
      write(*,'(1x,a,$)')'0 to write out results, 1 to enter new'//
     &    ' block to include or exclude: '
      read(*,*)ifloop
      if(ifloop.ne.0)go to 16
c       
c       now either just write model, or write point file
c       
      if(ifpoint.eq.0)then
        n_point = -1
77      call write_wmod(modelfile)
        print *,'CLIPPED MODEL WRITTEN'
      else
        call dopen(1,modelfile,'new','f')
        if (.not.getModelObjectRange(1, imodObj)) then
          print *
          print *, 'ERROR: CLIPMODEL - RELOADING DATA TO MAKE POINT OUTPUT'
          call exit(1)
        endif
c         
c         This should be done before point output
        call scale_model(0)
        ixmin=nint(xmin)
        iymin=nint(ymin)
        izmin=nint(zmin)
        ixmax=nint(xmax)
        iymax=nint(ymax)
        izmax=nint(zmax)
        nlistz=0
c         
c         loop through objects again
c         
        do iobj=1,max_mod_obj
          ibase=ibase_obj(iobj)
          do ipt=1,npt_in_obj(iobj)
            jpnt=abs(object(ipt+ibase))
            ixx=max(ixmin,min(ixmax,nint(p_coord(1,jpnt))))
            iyy=max(iymin,min(iymax,nint(p_coord(2,jpnt))))
            izz=max(izmin,min(izmax,nint(p_coord(3,jpnt))))
c             
c             if need corners, see if z coord is on list of z's
c             
            if(ifpoint.lt.0)then
              inlist=0
              do il=1,nlistz
                if(izz.eq.listz(il))inlist=il
              enddo
c		
c               if not, add to list of z's and write corner
c               
              if(inlist.eq.0)then
                nlistz=nlistz+1
                listz(nlistz)=izz
                write(1,101)ixmin,iymin,izz
                write(1,101)ixmax,iymax,izz
              endif
            endif
            write(1,101)ixx,iyy,izz
101         format(3i6)
          enddo
        enddo
        close(1)
        print *, 'POINT LIST WRITTEN'
      endif
      call exit(0)
      end

c       
c       $Log$
c       Revision 3.6  2010/06/23 23:47:55  mast
c       removed unused undefined variable
c
c       Revision 3.5  2008/11/29 15:40:17  mast
c       Increased z limits to make sure integer entries work
c
c       Revision 3.4  2008/10/07 02:28:35  mast
c       Changed to read one object at a time
c
c       Revision 3.3  2008/09/26 02:00:35  mast
c       Increased dimensions
c
c       Revision 3.2  2002/09/09 21:36:00  mast
c       Eliminate stat_source: and nimp_source: from all includes
c	
c       Revision 3.1  2002/07/07 20:12:28  mast
c       Modified to get scale factors from model file and use them to
c       scale to index coordinates
c	
c       modified for IMOD 4/24/97; 
c       compute cut edges and handle coordinates better, 10/24/00.
