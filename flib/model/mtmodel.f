*****   MTMODEL.FOR*********************************************
*       
*       MTMODEL builds a model using a list of unconnected points.
*       It links points from one section to the next by considering each
c       point and seeing if the x-y coordinates are within a certain radius 
c       of a point in the next section.  It can first do a scan through the
c       points with a narrow radius, then further scans with progressively
c       bigger radii (ignoring points that are already matched in a previous
c       scan).  By default, all objects will be given color 250 (red), but
c       one can have objects colored in a cycle of colors or coded for their
c       length and termination.
c       
c       One can add the points in the list to an existing model if desired.
c       Simply enter the name of this model file as the first entry in the
c       program.  The points at the ends of existing objects will be examined
c       first for their possible connections to new points.  Any new points
c       within a specified number of pixels of the existing points will be
c       discarded.  The color and other attributes of the objects in the
c       existing model will remain unchanged.
c       
c       ENTRIES:
c       
c       Name of file with old model to extend, or Return if none
c       
c       IF you are extending a model, next enter the minimum distance to 
c       .  allow between a new point and a point in the existing model.
c       
*       Name of point list file
c       Name of new model file to create
c       
c       Radius to search for matching points in first scan, radius to search
c       .  in last scan, number of scans
c       
c       1 to use colors to code for the size of objects and whether they
c       .  start at the beginning or end of the data stack
c       
c       IF you specify 1, next enter the minimum object length which should
c       .  be coded with the colors for "long" objects
c       
c       1 to change the default colors.
c       
c       IF you enter 1 here, next specify a list of colors from 247 to 255.
c       .  Ranges (e.g. 250-253) may be entered.
c       .  If you are not coding for object length/endpoints, then the
c       .  default is to color every object red.  In this case, if you change
c       .  the default, colors will be assigned to objects by cycling
c       .  repeatedly through the list that you enter.
c       .  If you are coding for colors, the default color list, and the
c       .  order in which colors should be respecified in the list, is:
c       
c       single point=magenta
c       points in every section = white
c       long from beginning section = orange
c       short from beginning section = red
c       long from end section = blue
c       short from end section = light blue (cyan)
c       long in middle only = green
c       short in middle only = yellow
c       
c       
c       Minimum number of connected points required to make an object in the
c       .  output model.  Points that would conenct into objects smaller than
c       .  this minimum number will be discarded.
c       
c       Finally, if there are gaps in the progression of Z values in the list
c       .  of points, the program will tell you how many gaps.  Enter 1 to
c       .  connect points across the gaps; otherwise all objects will end at
c       .  the gaps.
c       
C       David Mastronarde  9/8/88

      include 'model.inc'

      parameter (less_pt = max_pt/5)
      character*80 modelfile,pointfile,oldmodel
      logical moreobj,topend,botend,failed,exist,readw_or_imod
      integer*2 xpnt(less_pt),ypnt(less_pt),zpnt(less_pt),itmp
      integer*4 indsecst(1001),listcolor(9)
     &    ,indsort(less_pt),nextpnt(less_pt),modpnt(max_obj_num)
      logical*1 matched(less_pt),gap(1001)
      data nsyms/8/
      data iwinradini/3/,iwinradfin/15/,nscan/5/
      data ifmarker/0/,ifcolchng/0/
      data sametol/5./
c       
c       initialize colors
c       
      data ncolor,listcolor/8,254,248,249,250,252,255,251,253,247/
c       
c       get parameters
c       
c       
c       zero out model properties
c       
      n_point=0
      ntot_in_obj=0
      n_object=0
      nin_order=0
      ibase_free=0
      max_mod_obj=1
      ntot_in_obj=0
      n_clabel=0
      do i=1,max_pt
        pt_label(i)=0
      enddo
      do i=1,max_obj_num
        npt_in_obj(i)=0
      enddo
c       
c       find out if reading an old model and get it
c       
70    Print *,'Enter name of old model file if extending model,',
     &    ' or Return if not extending'
      read(*,'(a)')oldmodel
c       
      if(oldmodel.ne.' ')then
75      exist=readw_or_imod(oldmodel)
        if(.not.exist)go to 70
c         
c         repack this model before working with it
c         
        call repack_mod
c         
        write(*,'(1x,a,/,a,f4.1,a,$)')'Minimum distance to allow'//
     &      ' between points of old model and',
     &      '    new points on the same section [',sametol,']: '
        read(*,*)sametol
      endif
      samesqr=sametol**2
c       
      write(*,'(1x,a,$)')'Input point file: '
      read(*,'(a)')pointfile
c       
      write(*,'(1x,a,$)')'Output model file: '
      read(*,'(a)')modelfile
c       
      winradini=iwinradini
      winradfin=iwinradfin
      write(*,'(1x,a,i1,a,i2,a,i1,a,$)')'Radii to search for match'
     &    //' in first & last scans, # of scans [',iwinradini,
     &    ',',iwinradfin,',',nscan,']: '
      read(*,*)winradini,winradfin,nscan
c       
      write(*,'(1x,a,$)')
     &    '1 to use colors to code for object size and endpoints: '
      read(*,*)ifmarker
c       
      if(ifmarker.ne.0)then
        write(*,'(1x,a,$)')
     &      'Minimum length for a tube to be colored as long: '
        read(*,*)longcolor
      endif
c       
      write(*,'(1x,a,$)')'1 to change default colors or cycle'//
     &    ' through a list of colors: '
      read(*,*)ifcolchng
c       
      if(ifcolchng.ne.0)then
        print *,'Enter list of colors (247-255), ranges ok'
        call rdlist(6,listcolor,ncolor)
      endif
c       
c       if neither marking colors or list change selected, set all red
c       
      if(ifcolchng.eq.0.and.ifmarker.eq.0)then
        ncolor=1
        listcolor(1)=250
      endif
c       
c       Commented out the ability to assign point types - wimp does it fine
c       
      ityptop=0
      itypbot=0
      itypmid=0
c       write(*,'(1x,a,$)')
c       &           'symbol type for bottom, top, middle: '
c       read(*,*)itypbot,ityptop,itypmid
c       
      write(*,'(1x,a,$)')'Minimum # of connected points required'//
     &    ' to put object in output model: '
      read(*,*)ninobjlim
c       
c       read in the points, find min and max z, set match index to zero
c       
      open(2,file=pointfile,status='OLD')
      izmin=100000
      izmax=-izmin
      in_point=0
10    if(n_point+in_point.gt.less_pt)then
        print *,n_point,' +',in_point,' points is more than',less_pt
        stop
      endif
      i=in_point+1
      read(2,*,end=14,err=91)xpnt(i),ypnt(i),zpnt(i)
      matched(i)=.false.
      nextpnt(i)=0
      izmin=min(zpnt(i),izmin)
      izmax=max(zpnt(i),izmax)
      in_point=i
      go to 10
c       
c       build index to "sorted" points without sorting them
c       
14    isort=1
      ifgap=0
      do iz=izmin,izmax
c         
c         if adding to old model, make list of points in old model in
c         this section (look back from ends of objects)
c         
        if(oldmodel.ne.' ')then
          noldinsec=0
          do iobj=1,max_mod_obj
            ipt=npt_in_obj(iobj)
            do while (ipt.gt.0)
              jpnt=object(ibase_obj(iobj)+ipt)
              izmod=nint(p_coord(3,jpnt))
              if(izmod.gt.iz)then
                ipt=ipt-1
              elseif(izmod.eq.iz)then
                noldinsec=noldinsec+1
                modpnt(noldinsec)=jpnt
                ipt=0
              else              
                ipt=0
              endif
            enddo
          enddo
        endif  
c         
c         save index of start of each section
c         
        indsecst(iz+1-izmin)=isort
        ifon=0
        do i=1,in_point
          if(zpnt(i).eq.iz)then
            ifon=1
            ifclose=0
            if(oldmodel.ne.' ')then
c               
c               if adding to old model, make sure point is not too close
c               to any points on list of ones in this section
c               
              iold=1
              do while(ifclose.eq.0.and.iold.le.noldinsec)
                if((p_coord(1,modpnt(iold))-xpnt(i))**2.+
     &              (p_coord(2,modpnt(iold))-ypnt(i))**2.lt.
     &              samesqr)ifclose=1
                iold=iold+1
              enddo
            endif
            if(ifclose.eq.0)then
              indsort(isort)=i
              isort=isort+1
            endif
          endif
        enddo
        if(ifon.eq.0)ifgap=ifgap+1
        gap(iz+1-izmin)=ifon.eq.0
      enddo
      nsect=izmax+1-izmin
      indsecst(nsect+1)=isort
c       
      if(ifgap.gt.0)then
        write(*,'(1x,a,i4,a,/,a,$)')
     &      'There are',ifgap,' gaps in Z values.',
     &      ' Enter 1 to connect points across the gaps, 0 not to: '
        read(*,*)ifbridge
      endif
c       
c       if adding to old model, mark pointers for ends of objects as zeros
c       
      do i=1,max_mod_obj
        modpnt(i)=0
      enddo
c       
c       start the scan loop
c       
      do iscan=1,nscan
        winrad=winradini
        if(iscan.gt.1)winrad
     &      =winradini+(iscan-1)*(winradfin-winradini)/(nscan-1)
        iwin=winrad+1
        winradsq=winrad**2
c         
c         loop on sections and on each point in section
c         
        do isec=1,nsect-1
c           
c           but if adding to old model, first loop on unconnected endpoints
c           in this section
c           
          izsec=izmin+isec-1
          if(oldmodel.ne.' ')then
            do iobj=1,max_mod_obj
              ninobj=npt_in_obj(iobj)
              if(ninobj.gt.0)then
                jpnt=object(ninobj+ibase_obj(iobj))
                if(modpnt(iobj).eq.0.and.
     &              nint(p_coord(3,jpnt)).eq.izsec)then
c                   
c                   if point is not matched up yet, look at unmatched points in
c                   next section
c                   
                  ixx=nint(p_coord(1,jpnt))
                  iyy=nint(p_coord(2,jpnt))
c                   
c                   or in next existing section, if need to bridge a gap
c                   
                  iseclook=isec+1
                  do while(ifbridge.ne.0 .and. iseclook.lt.nsect .and.
     &                gap(iseclook))
                    iseclook=iseclook+1
                  enddo
                  jpnt=indsecst(iseclook)
c                   
                  do while(modpnt(iobj).eq.0.and.
     &                jpnt.lt.indsecst(iseclook+1))
                    jsrt=indsort(jpnt)
                    if(.not.matched(jpnt).and.abs(ixx-xpnt(jsrt)).le.
     &                  iwin.and.abs(iyy-ypnt(jsrt)).le.iwin.and.
     &                  (ixx-xpnt(jsrt))**2+ (iyy-ypnt(jsrt))**2.le.
     &                  winradsq)then
c                       
c                       got one: set it as matched and point index to it
c                       
                      matched(jpnt)=.true.
                      modpnt(iobj)=jpnt
                    endif
                    jpnt=jpnt+1
                  enddo
                endif
              endif
            enddo
          endif
c           
          do ipnt=indsecst(isec),indsecst(isec+1)-1
            if(nextpnt(ipnt).eq.0)then
c               
c               if point is not matched up yet, look at unmatched points in
c               next section
c               
              ixx=xpnt(indsort(ipnt))
              iyy=ypnt(indsort(ipnt))
c               
c               or in next existing section, if need to bridge a gap
c               
              iseclook=isec+1
              do while(ifbridge.ne.0 .and. iseclook.lt.nsect .and.
     &            gap(iseclook))
                iseclook=iseclook+1
              enddo
              jpnt=indsecst(iseclook)
c               
              do while(nextpnt(ipnt).eq.0.and.
     &            jpnt.lt.indsecst(iseclook+1))
                jsrt=indsort(jpnt)
                if(.not.matched(jpnt).and.abs(ixx-xpnt(jsrt)).le.iwin
     &              .and.abs(iyy-ypnt(jsrt)).le.iwin.and.sqrt(float
     &              ((ixx-xpnt(jsrt))**2+(iyy-ypnt(jsrt))**2))
     &              .le.winrad)then
c                   
c                   got one: set it as matched and point index to it
c                   
                  matched(jpnt)=.true.
                  nextpnt(ipnt)=jpnt
                endif
                jpnt=jpnt+1
              enddo
            endif
          enddo
        enddo
      enddo
c       
c       count how many model objects are needed: any non-matched is start of
c       object
c       
      new_object=0
      do i=1,isort-1
        if(.not.matched(i))then
          ninobj=1
          ind=i
          do while(nextpnt(ind).gt.0)
            ninobj=ninobj+1
            ind=nextpnt(ind)
          enddo
          if(ninobj.ge.ninobjlim)new_object=new_object+1
        endif
      enddo
      if(new_object+n_object.gt.max_obj_num)then
        print *,new_object+n_object,' objects is more than',max_obj_num
        stop
      endif
c       
c       get a margin of error and divide it among max # objects and
c       # pnts/obj: not needed with new model format
c       
c       maxnpoint=nsect
c       facmargin=sqrt(float(len_object)/(n_object*maxnpoint))
c       max_mod_obj=n_object*facmargin
c       max_obj_pt=len_object/max_mod_obj
c       
c       first add to end of old model
c       
      n_pointold=n_point
      if(oldmodel.ne.' ')then
        do iobj=1,max_mod_obj
          if(modpnt(iobj).gt.0)then
            call object_mover(iobj,failed)
            if(failed)stop 'insufficient object space'
            ninobj=npt_in_obj(iobj)
            ibase=ibase_obj(iobj)
            indobj=modpnt(iobj)
            do while(indobj.gt.0)
              isrt=indsort(indobj)
              ninobj=ninobj+1
              n_point=n_point+1
              ntot_in_obj=ntot_in_obj+1
              p_coord(1,n_point)=xpnt(isrt)
              p_coord(2,n_point)=ypnt(isrt)
              p_coord(3,n_point)=zpnt(isrt)
              object(ibase+ninobj)=n_point
              pt_label(n_point)=0
              indobj=nextpnt(indobj)
            enddo
            npt_in_obj(iobj)=ninobj
            ibase_free=max(ibase_free,ibase+ninobj)
          endif
        enddo
        call object_packer
      endif
c       
c       for each point that's not matched, build new object
c       
      icolor=1
      iobject=1
      do ipnt=1,isort-1
        if(.not.matched(ipnt))then
          ninobj=1
          ind=ipnt
          do while(nextpnt(ind).gt.0)
            ninobj=ninobj+1
            ind=nextpnt(ind)
          enddo
          if(ninobj.ge.ninobjlim)then
c             
c             find first free object
c             
            do while (npt_in_obj(iobject).gt.0)
              iobject=iobject+1
            enddo
            call object_mover(iobject,failed)
            if(failed)stop 'insufficient object space'
c             
c             take care of all the counters and pointers
c             
            n_object=n_object+1
            ibase_obj(iobject)=ibase_free
            nin_order=nin_order+1
            obj_order(nin_order)=iobject
            ndx_order(iobject)=nin_order
            ibase=ibase_free
            max_mod_obj=max(max_mod_obj,iobject)
c             
            ninobj=0
            indobj=ipnt
            moreobj=.true.
            do while(moreobj)
              isrt=indsort(indobj)
              ksym=itypmid
              if(ninobj.eq.0.and.zpnt(isrt).ne.izmin)ksym=itypbot
              if(nextpnt(indobj).eq.0.and.zpnt(isrt).ne.izmax)
     &            ksym=ityptop
              ninobj=ninobj+1
              n_point=n_point+1
              ntot_in_obj=ntot_in_obj+1
              p_coord(1,n_point)=xpnt(isrt)
              p_coord(2,n_point)=ypnt(isrt)
              p_coord(3,n_point)=zpnt(isrt)
              object(ibase+ninobj)=n_point
              pt_label(n_point)=ksym
              indobj=nextpnt(indobj)
              moreobj=indobj.gt.0
            enddo
            npt_in_obj(iobject)=ninobj
            obj_color(1,iobject)=1
            ibase_free=ibase+ninobj
            if(ifmarker.ne.0)then
c               
c               if doing markers, pick colors for being at one end or the
c               other
c               
              topend=zpnt(isrt).eq.izmax
              botend=p_coord(3,object(ibase+1)).eq.izmin
              lentrue=nint(zpnt(isrt)+1-p_coord(3,object(ibase+1)))
              if(lentrue.eq.1)then
                icolor=1
              elseif(lentrue.eq.nsect)then
                icolor=2
              else
                icolor=7
                if(topend.and..not.botend)icolor=5
                if(botend.and..not.topend)icolor=3
c                 and switch the color if its shorter than criterion
                if(lentrue.lt.longcolor)icolor=icolor+1
              endif
            endif
            obj_color(2,iobject)=listcolor(icolor)
            icolor=mod(icolor,ncolor)+1
          endif
        endif
      enddo
      ptperobj=float(n_point)/n_object
      write(*,102)in_point,in_point+1-isort,isort-1,n_point-n_pointold
     &    ,n_point,n_object,ptperobj
102   format(i7,' original points in point file',/,
     &    i7,' eliminated by proximity to points in existing model',/,
     &    i7,' remaining points reduced to',i6,/,i7,' total points in'
     &    ,i6,' objects, ',f7.2,' points/object on average')
c       
c       clear label arrays
c       
      n_clabel=0
c       
c       write it out
c       
      call write_wmod(modelfile)
      stop
91    print *,'error reading file'
      stop
      end
