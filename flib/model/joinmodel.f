*       * * * * JOINMODEL * * * * *
c       
c       JOINMODEL will combine two models, joining objects in the
c       SECOND model to the ends of objects in the FIRST model, if the
c       corresponding points match up sufficiently well.  Branch points are
c       allowed in either model, and should be preserved even during joining
c       of objects.  Point "marks" (numbers controlling point symbol
c       display), which are properties of individual points in the model,
c       will be preserved, but text labels in the second model will not.
c       
c       Objects will be joined together only if there is physical
c       overlap between the end of the object in the FIRST model and the
c       start of the object in the SECOND model.  If an object in the FIRST
c       model ends on one section and an object in the SECOND model starts
c       in the same location on the next section, they will not be joined.
c       
c       When two objects are joined together, the points in the object in the
c       SECOND model that overlap will be thrown away, and the rest of the
c       points in that object will be added to the end of the object in the
c       FIRST model.  The resulting object will have the same color and
c       other attributes as the original object in the FIRST model.  All
c       other objects will be carried over into the joined model without
c       modification.
c       
c       Entries to the program:
c       
c       Name of SECOND model file (YES, it needs the SECOND one first).
c       Name of FIRST model file
c       Name of output model file
c       
c       Maximum distance in the X-Y plane, and maximum separation in Z,
c       .  allowed in order for points from the two different models to be
c       .  considered matching points.  Enter / to accept the default values
c       .  in brackets.
c       
c       Maximum overlap to look for between objects in the two models.
c       .  Enter / to accept the default value in brackets.  This parameter
c       .  controls how long it takes the program to look for matches.
c       
c       
c       The program considers the end point of each object in the FIRST
c       model in turn.  It looks for a match between that end point and the
c       first, second, etc. point of each object in the SECOND model
c       (up to the number specified by the maximum overlap parameter).  When
c       it finds a match, it then tests whether each of the points from the
c       beginning of the object in the SECOND model up to that matching point
c       also match the corresponding points in the object in the FIRST model.
c       Only if there is a complete match along the entire stretch of overlap
c       will the objects be joined.
c       
c       David Mastronarde  1/10/90
c       
      include 'model.inc'
c       
c       Variable definitions for storage of second model
c       
      parameter (less_pt = max_pt/5)
      integer*4 object2(len_object)             !pointer to p_coord array
      integer*2 npt_in_obj2(max_obj_num)        !# of points in object
      integer*4 ibase_obj2(max_obj_num)         !base index of object in OBJECT
      integer*2 obj_color2(2,max_obj_num)       !ON/OFF and color indexes
      real*4 p_coord2(3,less_pt)                !model coordinates of points
      integer*1 pt_label2(less_pt)              !symbol number for point

      logical exist,readw_or_imod,extended,doesmatch,failed
     &    ,matched(max_obj_num)
      character*80 modelfile
      data maxoverlap/10/,distmax/2./,zsepmax/0.1/
c       
c       get second model first
c       
10    write(*,'(1x,a,$)')'Name of SECOND input model file: '
      read(*,'(a)')modelfile
15    exist=readw_or_imod(modelfile)
      if(.not.exist)then
        print *,'no good, try again'
        go to 10
      endif
c       
c       repack this model before storing in second arrays
c       
      call repack_mod
c       
c       store in second arrays
c       
      if (n_point.gt.less_pt) stop 'MODEL TOO LARGE FOR ARRAYS'
      nobj=n_object
      nintot=n_point
      max_obj2=max_mod_obj
c       
      do i=1,n_point
        do j=1,3
          p_coord2(j,i)=p_coord(j,i)
        enddo
        pt_label(i)=pt_label(i)
      enddo
c       
      do iobj=1,max_mod_obj
        npt_in_obj2(iobj)=npt_in_obj(iobj)
        ibase_obj2(iobj)=ibase_obj(iobj)
        obj_color2(1,iobj)=obj_color(1,iobj)
        obj_color2(2,iobj)=obj_color(2,iobj)
      enddo
c       
      do i=1,len_object
        object2(i)=object(i)
      enddo
c       
c       get model being added to
c       
20    write(*,'(1x,a,$)')'Name of FIRST input model file: '
      read(*,'(a)')modelfile
25    exist=readw_or_imod(modelfile)
      if(.not.exist)then
        print *,'no good, try again'
        go to 20
      endif
c       
c       repack this model before working with it
c       
      call repack_mod
c       
      write(*,'(1x,a,$)')'Name of output model file: '
      read(*,'(a)')modelfile
c       
      write(*,'(1x,a,/,a,f3.1,a,f3.1,a,$)')'Maximum distance in X-Y'//
     &    ' plane and maximum separation in Z allowed',
     &    '   between points to consider them matching [',
     &    distmax,',',zsepmax,']: '
      read(*,*)distmax,zsepmax
      distsqr=distmax**2
c       
      write(*,'(1x,a,i2,a,$)')'Maximum overlap to seek'//
     &    ' between ends of objects [',maxoverlap,']: '
      read(*,*)maxoverlap
c       
c       go through objects in first model, comparing last point of each to
c       points in all objects in second model
c       

      do iobj=1,max_mod_obj
        ninobj1=npt_in_obj(iobj)
        ibase=ibase_obj(iobj)
        if(ninobj1.gt.0)then
          jpnt=abs(object(ninobj1+ibase))
          xx=p_coord(1,jpnt)
          yy=p_coord(2,jpnt)
          zz=p_coord(3,jpnt)
          extended=.false.
          job=1
          do while (.not.extended.and.job.le.max_obj2)
            ibase2=ibase_obj2(job)
            ninobj2=npt_in_obj2(job)
            if(ninobj2.gt.0.and..not.matched(job))then
              ip=1
              do while (.not.extended.and.
     &            ip.le.min(maxoverlap,ninobj2))
                ipnt2=abs(object2(ip+ibase2))
                if(abs(p_coord2(3,ipnt2)-zz).le.zsepmax)then
                  dx=abs(xx-p_coord2(1,ipnt2))
                  dy=abs(yy-p_coord2(2,ipnt2))
                  if(dx.le.distmax.and.dy.le.distmax.and.
     &                dx**2+dy**2.le.distsqr)then
c                     
c                     got a match, now look back from 1 to ip-1 for match
c                     
                    if(ninobj1.ge.ip)then
                      doesmatch=.true.
                      ipm=1
                      do while (doesmatch.and.ipm.lt.ip)
                        jpnt=abs(object(ibase+ninobj1+ipm-ip))
                        ipnt2=abs(object2(ipm+ibase2))
                        doesmatch=abs(p_coord2(3,ipnt2)-
     &                      p_coord(3,jpnt)) .le.zsepmax .and.
     &                      (p_coord2(1,ipnt2)-p_coord(1,jpnt))**2+
     &                      (p_coord2(2,ipnt2)-p_coord(2,jpnt))**2
     &                      .le.distsqr
                        ipm=ipm+1
                      enddo
c                       
c                       if everything matches, now add from ip+1 to end
c                       
                      if(doesmatch)then
                        call object_mover(iobj,failed)
                        if(failed)stop 'insufficient object space'
                        if(ninobj2-ip+n_point.ge.max_pt)
     &                      stop 'insufficient point space'
                        ibase=ibase_obj(iobj)
c                         
                        do iadd=ip+1,ninobj2
                          ipnt2=object2(ibase2+iadd)
                          ninobj1=ninobj1+1
                          if(ipnt2.gt.0)then
c                             
c                             if not a branch point, just add point on end
c                             
                            n_point=n_point+1
                            ntot_in_obj=ntot_in_obj+1
                            p_coord(1,n_point)=p_coord2(1,ipnt2)
                            p_coord(2,n_point)=p_coord2(2,ipnt2)
                            p_coord(3,n_point)=p_coord2(3,ipnt2)
                            object(ibase+ninobj1)=n_point
                            pt_label(n_point)=pt_label2(ipnt2)
                          else
c                             
c                             otherwise, find positive reference at prior
c                             place in this object, and use that to set up
c                             branch reference
c                             
                            iref=0
                            look=1
                            do while (look.lt.iadd .and. iref.eq.0)
                              if(object2(ibase2+look).eq.-ipnt2)
     &                            iref=look
                              look=look+1
                            enddo
                            if(iref.gt.0)then
                              object(ibase+ninobj1)=-object(ibase+
     &                            iref-iadd+ninobj1)
                            else
                              print *,'unresolvable branch reference',
     &                            ' in second model, object',job,
     &                            ', point',iadd
                              stop
                            endif
                          endif
                        enddo
c                         
                        npt_in_obj(iobj)=ninobj1
                        ibase_free=max(ibase_free,ibase+ninobj1)
3                       matched(job)=.true.
                        extended=.true.
                      endif
                    endif
                  endif
                endif
                ip=ip+1
              enddo                             !end of loop through points
            endif
            job=job+1
          enddo                                 !end of loop thru 2nd objects
        endif
      enddo
c       
c       now take unmatched objects of 2nd model and make them new objects
c       first call the object packer, because it won't get called ever
c       when new objects are being allocated
c       
      call object_packer
      iobject=1
      do job=1,max_obj2
        if(.not.matched(job).and.npt_in_obj2(job).gt.0)then
c           
c           find first free object
c           
          do while (npt_in_obj(iobject).gt.0.and.
     &        iobject.le.max_obj_num)
            iobject=iobject+1
          enddo
          call object_mover(iobject,failed)
          if(failed)stop 'insufficient object space'
          if(n_point+npt_in_obj2(job).ge.max_pt)
     &        stop 'insufficient point space'
c           
c           take care of all the counters and pointers
c           
          n_object=n_object+1
          if(n_object.gt.max_obj_num)stop 'too many objects'
          ibase_obj(iobject)=ibase_free
          nin_order=nin_order+1
          obj_order(nin_order)=iobject
          ndx_order(iobject)=nin_order
          ibase=ibase_free
          max_mod_obj=max(max_mod_obj,iobject)
c           
c           move the points into model arrays
c           
          ibase2=ibase_obj2(job)
          do iadd=1,npt_in_obj2(job)
            ipnt2=object2(ibase2+iadd)
            if(ipnt2.gt.0)then
c               
c               if not a branch point, just add point on end
c               
              n_point=n_point+1
              ntot_in_obj=ntot_in_obj+1
              p_coord(1,n_point)=p_coord2(1,ipnt2)
              p_coord(2,n_point)=p_coord2(2,ipnt2)
              p_coord(3,n_point)=p_coord2(3,ipnt2)
              pt_label(n_point)=pt_label2(ipnt2)
              object(ibase+iadd)=n_point
            else
c               
c               otherwise, find positive reference at prior place in this
c               object, and use that to set up branch reference
c               
              iref=0
              look=1
              do while (look.lt.iadd .and. iref.eq.0)
                if(object2(ibase2+look).eq.-ipnt2) iref=look
                look=look+1
              enddo
              if(iref.gt.0)then
                object(ibase+iadd)=-object(ibase+iref)
              else
                print *,'unresolvable branch reference',
     &              ' in second model, object',job,', point',iadd
                stop
              endif
            endif
          enddo
          npt_in_obj(iobject)=npt_in_obj2(job)
          obj_color(1,iobject)=obj_color2(1,job)
          obj_color(2,iobject)=obj_color2(2,job)
          ibase_free=ibase+npt_in_obj2(job)
        endif
      enddo
c       
c       now just write model
c       
77    call write_wmod(modelfile)
      print *, 'JOINED MODEL WRITTEN'
      call exit(0)
      end
