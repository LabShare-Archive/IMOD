c***    INPUT_MODEL reads in the model data, reads the header info from an
c       image file, sorts out the model points that are to be included in
c       the analysis, and converts the coordinates to "index" coordinates
c       with the origin at the center of the section
c       
c       $Id$
c       Log at end
c       
      subroutine input_model(xx,yy,isecview,maxprojpt,maxreal,
     &    irealstr,ninreal, imodobj, imodcont,nview, nprojpt,nrealpt,
     &    iwhichout, xcen, ycen,xdelt,listz,mapfiletoview,nfileviews,
     &    modelfile,residualFile, pointfile, iuangle, iuxtilt,pipinput)
c       
      implicit none
      real*4 xx(*),yy(*),xcen,ycen
      integer*4 isecview(*),irealstr(*),ninreal(*),imodobj(*),imodcont(*)
      integer*4 mapfiletoview(*),maxprojpt,maxreal,nview,nprojpt,nrealpt
      integer*4 iwhichout, iuangle,iuxtilt,nfileviews
      character*(*) modelfile,pointfile,residualFile
c       
      character*160 solufile,anglefile
      logical stereopair,exist,readSmallMod,pipinput
      integer getimodhead,getimodscales,getimodmaxes,lnblnk
c       
      include 'smallmodel.inc'
c       
      integer*4 nxyz(3),mxyz(3),listz(*)
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
            iz=nint((p_coord(3,object(ipt+ibase_obj(iobject))) + 
     &          zorig) / delta(3))
            do i=1,nzlist
              if(iz.eq.listz(i))go to 20
            enddo
            nzlist=nzlist+1
            listz(nzlist)=iz
20        enddo
        endif
      enddo
      if (nzlist .eq. 0) call errorexit('THE FIDUCIAL MODEL IS EMPTY', 0)
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
        do i = 1, lnblnk(modelfile)-2
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
          call parselist(listString, isecview,ninoutlist)
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
            call rdlist(5,isecview,ninoutlist)
          else
            write(*,'(1x,a,$)')'Enter views to exclude (ranges ok): '
            call rdlist(5,isecview,ninoutlist)
          endif
        endif
      endif
      if (ifspecify .eq. 1) then
        ninoutlist=1+(ivend-ivstr)/ivinc
        do i=1,ninoutlist
          isecview(i)=ivstr+(i-1)*ivinc
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
            if(listz(i)+1.eq.isecview(j))ifonlist=1
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
          stereopair=.false.
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
                if (.not.stereopair .and. inlist .eq. isecview(i)) then
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
c               
c               if doing stereo pairs, set flag if it's not set, and if
c               flag is set, point must belong to second view
c               
              if(nzlist.eq.1)then
                if(stereopair)isecview(nprojtmp)=inlist+1
                stereopair=.true.
              endif
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
            ninreal(nrealpt)=nprojtmp-nprojpt
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
      irealstr(nrealpt+1)=nprojpt+1             !for convenient looping
      nview=max(2,nzlist)                       !nzlist maybe 1 for stereo pair
c       
c       shift listz to be a view list, and make the map of file to view
c       
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
      integer getimodscales
      integer*4 imodobj,isize,ierr
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
c           assignment, but then double the object numbers to keep them
c           separate.  Also, define as scattered and put sizes out
c           
          imodobj=256-obj_color(2,iobject)
          if(igroup(ireal).ne.0)then
            imodobj=2*(imodobj-1)+igroup(ireal)
            obj_color(2,iobject)=256-imodobj
            if (igroup(ireal).eq.1) call putobjcolor(imodobj, 0, 255, 0)
            if (igroup(ireal).eq.2) call putobjcolor(imodobj, 255, 0, 255)
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
      return
      end

c       
c       $Log$
c       Revision 3.19  2008/12/12 00:46:49  mast
c       Stop inverting points for output; add error if no points or points on
c       only one view
c
c       Revision 3.18  2007/11/18 04:57:10  mast
c       Redeclared concat at 320
c
c       Revision 3.17  2006/06/29 04:53:36  mast
c       Set up to use small model
c
c       Revision 3.16  2005/12/09 04:45:32  mast
c       gfortran: .xor., continuation, or byte fixes
c       
c       Revision 3.15  2005/11/13 23:06:26  mast
c       Fixed format
c       
c       Revision 3.14  2005/04/12 20:12:33  mast
c       Made it set object colors to standard colors when there are two groups
c       
c       Revision 3.13  2004/10/24 22:49:34  mast
c       fixed line length and lnblnk declaration problems
c       
c       Revision 3.12  2004/10/24 22:29:25  mast
c       Changes for pip input and residual file options
c       
c       Revision 3.11  2004/09/16 16:14:23  mast
c       Had it pass point file name out instead of opening it; made it give
c       error when a contour has two points on a view.
c       
c       Revision 3.10  2004/06/17 17:47:56  mast
c       Reset zscale and rotation of 3d fiducial model
c       
c       Revision 3.9  2004/06/10 05:39:35  mast
c       Return a pixel size to main program
c       
c       Revision 3.8  2004/05/05 05:44:49  mast
c       Output the 3D model with inverted Z to match tomogram
c       
c       Revision 3.7  2003/01/30 20:55:11  mast
c       Fixed xyz model output (again)
c       
c       Revision 3.6  2003/01/18 00:02:52  mast
c       Fixed bug in model output when there are many contours with one point
c       
c       Revision 3.5  2002/12/21 00:02:38  mast
c       Add option for getting both residual and 3D model output
c       
c       Revision 3.4  2002/07/28 22:54:03  mast
c       Applied scaling to 3-D model output
c       
c       Revision 3.3  2002/07/28 22:37:52  mast
c       Made it scale model coordinates correctly and get scaling/origin
c       information from model file if image file not given.  Also
c       standardized error output and had it exit rather than loop on
c       model file reading error.
c       
c       Revision 3.2  2002/05/20 15:55:04  mast
c       Fixed model output so that it works properly with points assigned to
c       two surfaces, and with multiple objects; also had it set object type
c       to scattered points and set point size appropriately.
c       
c       Revision 3.1  2002/05/07 02:07:12  mast
c       Changes to make things work well with a subset of views
c       
