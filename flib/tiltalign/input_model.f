c***    INPUT_MODEL reads in the model data, reads the header info from an
c       image file, sorts out the model points that are to be included in
c       the analysis, and converts the coordinates to "index" coordinates
c       with the origin at the center of the section
c       
c       $Id$
c       
      subroutine input_model(imodobj, imodcont, nprojpt,  iwhichout, xcen, ycen,xdelt,
     &    listz, maxzlist, modelfile,residualFile, pointfile, iuangle, iuxtilt,pipinput)
c       
      use alivar
      implicit none
      real*4 xcen,ycen
      integer*4 imodobj(*),imodcont(*),listz(*)
      integer*4 nprojpt, maxzlist
      integer*4 iwhichout, iuangle,iuxtilt
      character*(*) modelfile,pointfile,residualFile
c       
      character*320 solufile,anglefile
      logical exist,readSmallMod,pipinput
      integer getimodhead,getimodscales,getimodmaxes,numberInList
c       
      include 'smallmodel.inc'
c       
      integer*4 nxyz(3),mxyz(3)
      real*4 delta(3)
c       
      integer*4 nzlist,iobject,ninobj,ipt,iz,i,j,itmp,minzval,ifspecify
      integer*4 ninoutlist,ivstr,ivend,ivinc,ifonlist,ibase,nprojtmp
      integer*4 inlist,mode,ierr,ifflip,ifpip,ierr2,ierr3
      real*4 xorig,yorig,zorig,xdelt,ydelt,dmin,dmax,dmean
      real*4 xyscal,zscal,xofs,yofs,zofs
      logical residualout, resbothout, gotdot
      character*1024 listString
      integer*4 PipGetThreeIntegers
      integer*4 PipGetString,PipGetTwoIntegers, PipGetTwoFloats
      character*320 concat
      logical b3dxor
c       
c       read model in
c       
      if (pipinput) then
        ifpip = 1
        if (PipGetString('ModelFile', modelfile) .ne. 0) call errorexit(
     &      'NO INPUT FIDUCIAL MODEL FILE ENTERED', 0)
      else
        ifpip = 0
        write(*,'(1x,a,$)')'Model file: '
        read(5,'(a)')modelfile
      endif
      exist=readSmallMod(modelfile)
      if(.not.exist)call errorexit('READING MODEL FILE', 0)
c       
c       get dimensions of file and header info on origin and delta
c       
      if (pipinput) then
        modelfile = ' '
        ierr = PipGetString('ImageFile', modelfile)
      else
        print *,'Image file with matching header info'//
     &      ' (Return to enter nx,ny directly)'
        read(5,'(a)')modelfile
      endif
c       
      if(modelfile.eq.' ')then
c         
c         DNM 7/28/02: get defaults from  model header, including z size
c         
        ierr=getimodhead(xyscal,zscal,xofs,yofs,zofs,ifflip)
        ierr=getimodscales(delta(1),delta(2),delta(3))
        ierr=getimodmaxes(nxyz(1),nxyz(2),nxyz(3))
        xorig=-xofs
        yorig=-yofs
        zorig=-zofs
        if (pipinput) then
          ierr = PipGetTwoIntegers('ImageSizeXandY', nxyz(1),nxyz(2))
          ierr = PipGetTwoFloats('ImageOriginXandY', xorig,yorig)
          ierr = PipGetTwoFloats('ImagePixelSizeXandY', delta(1),delta(2))
        else
          write(*,'(1x,a,$)')
     &        'nx, ny, x origin, y origin, x delta, y delta: '
          read(5,*)nxyz(1),nxyz(2),xorig,yorig,delta(1),delta(2)
        endif
      else
        call ialprt(.false.)                    !don't want to see header!
        call imopen(1,modelfile,'ro')
        call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
        call irtorg(1,xorig,yorig,zorig)
        call irtdel(1,delta)
        call imclose(1)
      endif
      xdelt=delta(1)
      ydelt=delta(2)
      xcen=nxyz(1)/2.
      ycen=nxyz(2)/2.
c       
c       scan to get list of z values
c       
      nzlist=0
      do iobject=1,max_mod_obj
        ninobj=npt_in_obj(iobject)
        if(ninobj.gt.1)then
          do ipt=1,ninobj
            iz=nint((p_coord(3,object(ipt+ibase_obj(iobject))) +  zorig) / delta(3))
            if (numberInList(iz, listz, nzlist, 0) .eq. 0) then
              nzlist=nzlist+1
              if (nzlist .gt. maxzlist)
     &            call errorexit('TOO MANY Z VALUES IN MODEL FOR TEMPORARY ARRAYS')
              listz(nzlist)=iz
            endif
          enddo
        endif
      enddo
      if (n_point .eq. 0) call errorexit('THE FIDUCIAL MODEL IS EMPTY', 0)
      if (nzlist .eq. 0) call errorexit(
     &    'THE FIDUCIAL MODEL HAS NO CONTOURS WITH MORE THAN ONE POINT', 0)
      if (nzlist .eq. 1) call errorexit(
     &    'THE FIDUCIAL MODEL HAS POINTS ON ONLY ONE VIEW', 0)
c       
c       order list of z values
c       
      do i=1,nzlist-1
        do j=i+1,nzlist
          if(listz(i).gt.listz(j))then
            itmp=listz(i)
            listz(i)=listz(j)
            listz(j)=itmp
          endif
        enddo
      enddo
c       
c       set minimum z value if any are negative, and set number of views
c       in file to be maximum of number acquired from model or file or
c       range of z values
c       
      minzval=min(0,listz(1))
c       
c       DNM 7/12/02: now that we convert to index coordinates, we
c       use the minimum just computed.
c       # of file views should be the size of file, but make it big
c       enough to hold all z values actually found
c       
      nfileviews=max(listz(nzlist)-minzval+1,nxyz(3))
c       
c       get name of file for output model
c       
      residualout = .false.
      if (pipinput) then
        modelfile = ' '
        residualFile = ' '
        if (PipGetString('OutputModelAndResidual', modelfile) .eq. 0) then
          resbothout = .true.
        else
          resbothout = .false.
          ierr = PipGetString('OutputModelFile', modelfile)
          ierr = PipGetString('OutputResidualFile', residualFile)
        endif
      else
        residualfile = ' '
        write(*,'(1x,a,/,a)')'Enter file name for output model of'//
     &      ' solved X-Y-Z coordinates,',' or a name containing .res'//
     &      'for a list of residuals,',' or a name without an '//
     &      'extension for both outputs, or Return for neither'
        read(5,'(a)')modelfile
c         
c         7/26/02: if modelfile contains .res, output residuals
c         12/20/02: if there is no extension, output both residual and
c         model
c         
        resbothout = modelfile .ne. ' '
        gotdot = .false.
        do i = 1, len_trim(modelfile)-2
          if (gotdot .and. modelfile(i:i+2).eq.'res')residualout = .true.
          if (gotdot .and. modelfile(i:i+2).eq.'mod')resbothout = .false.
          if (modelfile(i:i).eq.'.')  gotdot = .true.
        enddo
      endif
c       
      if (residualout.or.resbothout) then
        if (residualout) then
          residualfile = modelfile
        else
          residualfile = concat(modelfile, '.resid')
        endif
c         
c         manage the model file name now; attach extension if model
c         wanted, or null it out if not
c         
        if (residualout) then
          modelfile = ' '
        else
          modelfile = concat(modelfile, '.3dmod')
        endif
      endif
c       
c       get name of files for angle list output and 3-D point output
c       
      iuangle=0
      angleFile = ' '
      pointfile = ' '
      if (pipinput) then
        ierr = PipGetString('OutputFidXYZFile', pointfile)
        ierr = PipGetString('OutputTiltFile', anglefile)
      else
        write(*,'(1x,a)')'Enter file name for ASCII list of'//
     &      ' solved X-Y-Z coordinates (Return for none)'
        read(5,'(a)')pointfile
c         
        write(*,'(1x,a)')'Enter name of output file for list of'//
     &      ' solved tilt angles (Return for none)'
        read(5,'(a)')anglefile
      endif
      if(anglefile.ne.' ')then
        call dopen(9,anglefile,'new','f')
        iuangle=9
      endif
c       
c       if iuxtilt comes in non-zero, ask about file for x axis tilts
c       
      anglefile = ' '
      if (pipinput) then
        ierr = PipGetString('OutputXAxisTiltFile', anglefile)
      else if(iuxtilt.ne.0)then
        iuxtilt=0
        write(*,'(1x,a)')'Enter name of output file for list of'//
     &      ' X-axis tilt angles (Return for none)'
        read(5,'(a)')anglefile
      endif
      if(anglefile.ne.' ')then
        call dopen(10,anglefile,'new','f')
        iuxtilt=10
      endif
c       
c       open output file to put transforms into
c       
      if (pipinput) then
        solufile = ' '
        ierr = PipgetString('OutputTransformFile', solufile)
        iwhichout = 1
      else
        write(*,'(1x,a,$)')'Output file for solutions/transforms: '
        read(5,'(a)')solufile
c         
        write(*,'(1x,a,$)')'1 to put only xforms in file,'
     &      //' -1 to put only solutions, 0 both: '
        read(5,*)iwhichout
      endif
      call dopen(7,solufile,'new','f')
c       
c       find out which z values to include; end up with a list of z values
c       
      ninoutlist = 0
      if (pipinput) then
        ifspecify = 0
        ierr = PipGetThreeIntegers('IncludeStartEndInc', ivstr,ivend,ivinc)
        ierr2 = PipGetString('IncludeList', listString)
        ierr3 = PipGetString('ExcludeList', listString)
        if (ierr + ierr2 + ierr3 .lt. 2) call errorexit(
     &      'YOU MAY ENTER ONLY ONE OF IncludeStartEndInc, '//
     &      'IncludeList, OR ExcludeList', 0)
        if (ierr .eq. 0) ifspecify = 1
        if (ierr2 + ierr3 .eq. 1) then
          call parselist(listString, imodobj,ninoutlist)
          if (ierr3 .eq. 0) ifspecify = 3
        endif
      else
c         
        write(*,'(1x,a,i3,a,/,a,/,a,/,a,$)')'Enter 0 to include all',nzlist,
     &      ' views with points'
     &      ,'    or 1 to enter start, end, and increment view numbers'
     &      ,'    or 2 to specify a list of views to include'
     &      ,'    or 3 to specify a list of views to exclude: '
        read(5,*)ifspecify
c         
        if (ifspecify.gt.0)then
          if(ifspecify.eq.1)then
            write(*,'(1x,a,$)')
     &          'Start, end, and increment of views to include: '
            read(5,*)ivstr,ivend,ivinc
          elseif(ifspecify.eq.2)then
            write(*,'(1x,a,$)')'Enter views to include (ranges ok): '
            call rdlist(5,imodobj,ninoutlist)
          else
            write(*,'(1x,a,$)')'Enter views to exclude (ranges ok): '
            call rdlist(5,imodobj,ninoutlist)
          endif
        endif
      endif
      if (ifspecify .eq. 1) then
        ninoutlist=1+(ivend-ivstr)/ivinc
        do i=1,ninoutlist
          imodobj(i)=ivstr+(i-1)*ivinc
        enddo
      endif
c       
c       go through list of z values in model and make sure they are on
c       an include list or not on an exclude list
c       
      if (ninoutlist .gt. 0) then
        i=1
        do while(i.le.nzlist)
          ifonlist=0
          do j=1,ninoutlist
            if(listz(i)+1.eq.imodobj(j))ifonlist=1
          enddo
c           
c           remove points on exclude list or not on include list
c           
          if(b3dxor(ifspecify.le.2, ifonlist.eq.1))then
            nzlist=nzlist-1
            do j=i,nzlist
              listz(j)=listz(j+1)
            enddo
          else
            i=i+1
          endif
        enddo
      endif
c       
c       Count the number of points in valid objects on selected views
      nprojpt=0
      nrealpt=0
      imodobj(1:nzlist) = 0
      do iobject=1,max_mod_obj
        ninobj=npt_in_obj(iobject)
        ibase=ibase_obj(iobject)
        if(ninobj.gt.1)then
c           
c           First determine if the object is usable at all: if it has at least 2 points
          inlist = 0
          do ipt=1,ninobj
            if (numberInList(nint((p_coord(3,object(ipt+ibase)) + zorig) / delta(3)),
     &          listz, nzlist, 0) .ne. 0) then
              inlist = inlist + 1
              if (inlist .gt. 1) exit
            endif
          enddo
c           
c           If so, then count the points on valid views, and keep track of those views
          if (inlist .gt. 1) then
            nRealPt = nRealPt + 1
            do ipt=1,ninobj
              iz=nint((p_coord(3,object(ipt+ibase)) + zorig) / delta(3))
              do i=1,nzlist
                if(iz.eq.listz(i))then
                  nProjPt = nProjPt + 1
                  imodobj(i) = imodobj(i) + 1
                  exit
                endif
              enddo
            enddo
          endif
        endif
      enddo
c       
c       Trim the Z list if that excluded any views
      inlist = 0
      do i = 1, nzlist
        if (imodobj(i) .gt. 0) then
          inlist = inlist + 1
          listz(inlist) = listz(i)
        endif
      enddo
      nzlist = inlist
      if (nzlist .lt. 2) call errorexit(
     &    'THE FIDUCIAL MODEL HAS USABLE POINTS ON ONLY ONE VIEW', 0)
      nview = nzlist
c       
c       Do some big allocations
      
      call allocateAlivar(nprojpt, nview, nrealpt, ierr)
      call memoryError(ierr, 'ARRAYS IN ALIVAR')
      call allocateFunctVars(ierr)
      call memoryError(ierr, 'ARRAYS FOR FUNCT')
c       
c       go through model finding objects with more than one point in the
c       proper z range, and convert to index coordinates, origin at center
c       
      nprojpt=0
      nrealpt=0
      do iobject=1,max_mod_obj                  !loop on objects
        ninobj=npt_in_obj(iobject)
        ibase=ibase_obj(iobject)
        if(ninobj.gt.1)then                     !non-empty object
          nprojtmp=nprojpt                      !save number at start of object
          do ipt=1,ninobj                       !loop on points
c             
c             find out if the z coordinate of this point is on the list
c             
            iz=nint((p_coord(3,object(ipt+ibase)) + zorig) / delta(3))
            inlist=0
            do i=1,nzlist
              if(iz.eq.listz(i))inlist=i
            enddo
            if(inlist.gt.0)then
c               
c               if so, tentatively add index coordinates to list but first
c               check for two points on same view
c               
              do i = nprojpt + 1, nprojtmp
                if (inlist .eq. isecview(i)) then
c                   
c                   Find the other point
                  do j = 1, ipt-1
                    if (nint((p_coord(3,object(j+ibase)) + zorig) / delta(3))
     &                  .eq. iz)inlist = j
                  enddo
                  call objtocont(iobject,obj_color,ibase,ninobj)
                  write(*,'(/,a,i5,a,i5,a,i5,a,i5,a,i4)')
     &                'ERROR: TILTALIGN - TWO POINTS (#',inlist,' AND',ipt,
     &                ') ON VIEW', iz + 1,' IN CONTOUR', ninobj,' OF OBJECT',
     &                ibase
                  call exit(1)
                endif
              enddo
              nprojtmp=nprojtmp+1
              if(nprojtmp.gt.maxprojpt)call errorexit(
     &            'TOO MANY PROJECTION POINTS FOR ARRAYS',0)
              xx(nprojtmp)=(p_coord(1,object(ipt+ibase))+xorig)/xdelt
     &            -xcen
              yy(nprojtmp)=(p_coord(2,object(ipt+ibase))+yorig)/ydelt
     &            -ycen
              isecview(nprojtmp)=inlist
            endif
          enddo
c           
c           if there are at least 2 points, take it as a real point
c           
          if(nprojtmp-nprojpt.ge.2)then
            nrealpt=nrealpt+1
            if(nrealpt.gt.maxreal)call errorexit(
     &          'TOO MANY FIDUCIAL POINTS FOR ARRAYS',0)
            irealstr(nrealpt)=nprojpt+1
            call objtocont(iobject,obj_color,imodobj(nrealpt),
     &          imodcont(nrealpt))
            nprojpt=nprojtmp
            npt_in_obj(iobject)=1
          else
            npt_in_obj(iobject)=0
          endif
        else
          npt_in_obj(iobject)=0
        endif
      enddo
      irealstr(nrealpt+1)=nprojpt+1             !Needed to get number in real sometimes
c       
c       shift listz to be a view list, and make the map of file to view
c       listz will be copied into mapviewtofile
      do i=1,nfileviews
        mapfiletoview(i)=0
      enddo
      do i=1,nzlist
        listz(i)=listz(i)-minzval+1
        mapfiletoview(listz(i))=i
      enddo
      return
      end



c       WRITE_XYZ_MODEL will write out a model containing only a single point
c       per object with the solved XYZ coordinates, to file MODELFILE, if
c       that string is non-blank.  IGROUP is an array with the group number
c       if the points have been assigned to surfaces.
c       
      subroutine write_xyz_model(modelfile,xyz,igroup,nrealpt)
c       
      implicit none
      real*4 xyz(3,*)
      integer*4 igroup(*),nrealpt
      character*(*) modelfile
      include 'smallmodel.inc'
      real*4 xyzmax,ximscale,yimscale,zimscale
      integer*4, allocatable :: imodObjSize(:), mapImodObj(:), numGroup1(:)
      integer getimodscales
      integer*4 imodobj,isize,ierr, newobj, maxImodObj, mapInd
c       
      integer*4 ireal,iobject,ipt,i
c       
      if(modelfile.eq.' ')return
c       
c       get a scattered point size (simplified 1/30/03)
c       
      xyzmax=0.
      do ireal = 1,nrealpt
        do i=1,3
          xyzmax=max(xyzmax,abs(xyz(i,ireal)))
        enddo
      enddo
      isize=max(3.,xyzmax/100.)
c       
c       Count the contours per object so that objects can be reused if all
c       their points are in group 2 and get moved elsewhere
      maxImodObj = 0
      do iobject = 1, max_mod_obj
        maxImodObj = max(maxImodObj, 256-obj_color(2,iobject))
      enddo
      allocate(imodObjSize(maxImodObj*2), mapImodObj(maxImodObj*2),
     &    numGroup1(maxImodObj), stat=ierr)
      if (ierr .ne. 0) call errorexit('ALLOCATING ARRAYS FOR WRITING MODEL', 0)
      imodObjSize = 0
      mapImodObj = 0
      numGroup1 = 0
      ireal = 0
      do iobject = 1, max_mod_obj
        if(npt_in_obj(iobject).gt.0) then
          imodobj = 256-obj_color(2,iobject)
          imodObjSize(imodobj) = imodObjSize(imodobj) + 1
          ireal = ireal + 1
          if (igroup(ireal) .eq. 1) numGroup1(imodobj) = numGroup1(imodobj) + 1
        endif
      enddo
c       
c       loop on model objects that are non-zero, stuff point coords into 
c       first point of object (now only point), and set color by group
c       
c       get scaling factors, might as well apply each one to each coordinate
c       12/11/08: No longer invert Z.  Model now fits exactly on tomogram
c       opened either with -Y or without, except for X-axis tilt
c       (invert Z so that model can be visualized on tomogram by shifting)
c       
      ierr=getimodscales(ximscale,yimscale,zimscale)
      ireal=0
      do iobject=1,max_mod_obj
        if(npt_in_obj(iobject).gt.0.and.ireal.lt.nrealpt)then
          ipt=object(ibase_obj(iobject)+1)
          ireal=ireal+1
          p_coord(1,ipt)=xyz(1,ireal)*ximscale
          p_coord(2,ipt)=xyz(2,ireal)*yimscale
          p_coord(3,ipt)=xyz(3,ireal)*zimscale
c           
c           DNM 5/15/02: only change color if there is a group
c           assignment, but then put it in the object this one maps to
c           If no map yet, find first free object
c           Also, define as scattered and put sizes out
c           
          imodobj=256-obj_color(2,iobject)
          if(igroup(ireal).ne.0)then
            imodObjSize(imodobj) = imodObjSize(imodobj) - 1
c
c             Multiply by 2 to get index to mapping for group 1 or 2 in object
c             If already mapped, fine
c             If in group 1, map to self and set to green
c             If in group 2, map to self if there are no group 1's in object,
c             otherwise map to first empty object, and set to magenta
            mapInd = 2 * (imodobj - 1) + igroup(ireal)
            if (mapImodObj(mapInd) .ne. 0) then
              imodobj = mapImodObj(mapInd)
            else if (igroup(ireal) .eq. 1) then
              mapImodObj(mapInd) = imodobj
              call putobjcolor(imodobj, 0, 255, 0)
            else
              if (numGroup1(imodobj) .gt. 0) then
                do newobj = 1, maxImodObj* 2 - 1
                  if (imodObjSize(newobj) .eq. 0) exit
                enddo
                imodobj = newobj
              endif
              mapImodObj(mapInd) = imodobj
              call putobjcolor(imodobj, 255, 0, 255)
            endif
            obj_color(2,iobject)=256-imodobj
            imodObjSize(imodobj) = imodObjSize(imodobj) + 1
          endif
          call putimodflag(imodobj,2)
          call putscatsize(imodobj,isize)
        endif
      enddo
      call putimodzscale(1.)
      call putimodrotation(0., 0., 0.)
c       
      n_object=ireal
      call write_wmod(modelfile)
      close(20)
      deallocate(imodObjSize, mapImodObj, numGroup1)
      return
      end
