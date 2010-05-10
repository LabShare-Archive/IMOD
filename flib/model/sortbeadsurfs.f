c       SORTBEADSURFS
c
c       This program is used to sort beads into two surfaces for a bead model
c       from findbeads3d, or to apply X axis tilt, shift coordinates, and 
c       combine objects for a model from tiltalign.  It is a preprocessor for
c       using such models in flattenwarp.
c
c       $Id$
c       Log at end of file
c       
      implicit none
      include 'smallmodel.inc'
      character*320 filin, filout
      integer*4 numInGroup(2), jred(2), jgreen(2), jblue(2)
      logical*4 exist, readSmallMod, already, majority, invertZ, rescale
      logical*4 oneSurface
      integer*4 ifflip, local, ierr, i, j, ix, iy,indy,indz,maxx,maxy,maxz
      real*4 xmax, xmin, ymin, ymax, deltaX, deltaY, xlo, xhi, ylo, yhi
      real*4 epsX, epsY, dx, dy, cosa, sina, xtilt, scalef
      integer*4 numAreaX, numAreaY, numPts, isize, minNUm, ired, igreen, iblue
      integer*4 imodobj, newNx, newNy,icolobj,icol, numColors, iobj,iystr,iyend
      integer*4 nxub, nyub, ixtrim0, ixTrim1, iyTrim0, iyTrim1, ibinRec
      integer*4 ibinPreali
      integer*4 getimodmaxes, getscatsize, deleteiobj, putImodMaxes,getObjColor
      real*4 sind, cosd

      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetFloat,PipGetTwoIntegers
      integer*4 PipGetInOutFile, PipGetLogical
      real*4 xyz(3,max_pt), xyzfit(3,max_pt), tiltdum
      integer*4 igroup(max_pt), igrfit(max_pt)

c       
c       fallbacks from ../../manpages/autodoc2man -2 2  sortbeadsurfs
c       
      integer numOptions
      parameter (numOptions = 15)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@output:OutputFile:FN:@flipyz:FlipYandZ:I:@'//
     &    'subarea:SubareaSize:I:@majority:MajorityObjectOnly:B:@'//
     &    'xtilt:XAxisTilt:F:@invert:InvertZAxis:B:@'//
     &    'already:AlreadySorted:B:@aligned:AlignedSizeXandY:IP:@'//
     &    'xtrim:XTrimStartAndEnd:IP:@ytrim:YTrimStartAndEnd:IP:@'//
     &    'prebin:PrealignedBinning:I:@recbin:ReconstructionBinning:I:@'//
     &    'rescale:RescaleByBinnings:B:@help:usage:B:'

      ifflip = -1
      local = 0
      newNx = 0
      newNy = 0
      xtilt = 0.
      ibinRec = 1
      ibinPreali = 1
      ixTrim0 = 0
      ixTrim1 = 0
      iyTrim0 = 0
      iyTrim1 = 0
      invertZ = .false.
      already = .false.
      majority = .false.
      rescale = .false.

      call PipReadOrParseOptions(options, numOptions, 'sortbeadsurfs',
     &    'ERROR: SORTBEADSURFS - ', .false., 1, 1, 1, numOptArg,
     &    numNonOptArg)
      
      if (PipGetInOutFile('InputFile', 1, ' ', filin)
     &    .ne. 0) call exitError('NO INPUT FILE SPECIFIED')
      if (PipGetInOutFile('OutputFile', 2, ' ', filout)
     &    .ne. 0) call exitError('NO OUTPUT FILE SPECIFIED')
      ierr = PipGetInteger('FlipYandZ', ifflip)
      ierr = PipGetInteger('SubareaSize', local)
      ierr = PipGetFloat('XAxisTilt', xtilt)
      ierr = PipGetLogical('AlreadySorted', already)
      ierr = PipGetLogical('OneSurface', oneSurface)
      ierr = PipGetLogical('MajorityObjectOnly', majority)
      ierr = PipGetTwoIntegers('AlignedSizeXandY', newNx, newNy)
      ierr = PipGetInteger('PrealignedBinning', ibinPreali)
      ierr = PipGetInteger('ReconstructionBinning', ibinRec)
      ierr = PipGetLogical('RescaleByBinnings', rescale)
      ierr = PipGetLogical('InvertZAxis', invertZ)
      ierr = PipGetTwoIntegers('XTrimStartAndEnd', ixTrim0, ixTrim1)
      ierr = PipGetTwoIntegers('YTrimStartAndEnd', iyTrim0, iyTrim1)
c       
c       Get the model and its size
      exist=readSmallMod(filin)
      if(.not.exist) call exitError('READING MODEL FILE')
      call scale_model(0)
      ierr = getimodmaxes(maxx, maxy, maxz)
c       
c       Flip coordinates if appropriate given ymax and zmax, or if user says to
      indy = 2
      indz = 3
      if ((ifflip .lt. 0 .and. maxz .gt. maxy) .or. ifflip .gt. 0) then
        indy = 3
        indz = 2
        ierr = maxy
        maxy = maxz
        maxz = ierr
        if (xtilt .ne. 0) call exitError(
     &      'X axis tilt cannot be applied to a flipped model')
        if (newNx .ne. 0 .or. newNy .ne. 0 .or. ibinPreali .gt. 1 .or.
     &      ibinRec .gt. 1 .or. rescale .or. invertZ .or. ixTrim0 .gt. 0 .or.
     &      ixTrim1 .gt. 0 .or. iyTrim0 .gt. 0 .or. iyTrim1 .gt. 0)
     &      call exitError('You cannot do size adjustments or Z '//
     &      'inversion with a flipped model')
      endif
c       
c       Find the X/Y range of the data and copy into a separate array
c       Rotate by the tilt angle not the negative of it.
      xmin = 1.e20
      xmax = -xmin
      ymin = xmin
      ymax = xmax
      cosa=cosd(xtilt)
      sina=sind(xtilt)
      do i = 1, n_point
        xyz(1,i) = p_coord(1,i)
        if (xtilt .ne. 0) then
          dy = p_coord(indy,i) - maxy / 2.
          xyz(2,i) = cosa * dy - sina * p_coord(indz,i) + maxy / 2.
          xyz(3,i) = sina * dy + cosa * p_coord(indz,i)
        else
          xyz(2,i) = p_coord(indy,i)
          xyz(3,i) = p_coord(indz,i)
        endif
        if (invertZ) xyz(3,i) = maxz - 1 - xyz(3,i)
        xmin = min(xmin, xyz(1,i))
        ymin = min(ymin, xyz(2,i))
        xmax = max(xmax, xyz(1,i))
        ymax = max(ymax, xyz(2,i))
        igroup(i) = -1
      enddo
c       
c       If points are already sorted, look at the object colors to deduce this
c       the sorting, unless the one surface option is given
      if (oneSurface) then
        do i = 1, n_point
          igroup(i) = 1
        enddo
        
      else if (already) then
        numColors = 0
        do iobj = 1, max_mod_obj
          if (npt_in_obj(iobj) .gt. 0) then
            imodobj = 256 - obj_color(2, iobj)
            ierr = getObjColor(imodobj, ired, igreen, iblue)
            icolobj = 0
            do icol = 1, numColors
              if (ired .eq. jred(icol) .and. igreen .eq. jgreen(icol) .and.
     &            iblue .eq. jblue(icol)) icolobj = icol
            enddo
            if (icolobj .eq. 0) then
              if (numColors .gt. 1) call exitError(
     &            'There are more than two colors among the various objects')
              numColors = numColors + 1
              icolobj = numColors
              jred(icolobj) = ired
              jgreen(icolobj) = igreen
              jblue(icolobj) = iblue
            endif
            do ix = 1, npt_in_obj(iobj)
              i = object(ibase_obj(iobj) + ix)
              igroup(i) = icolobj
            enddo
          endif
        enddo
      else
c       
c         To do sorting, first set up subareas
        numAreaX = 1
        numAreaY = 1
        if (local .gt. 0) then
          numAreaX = max(1, nint((xmax - xmin) / local))
          numAreaY = max(1, nint((ymax - ymin) / local))
        endif
        epsX = 0.0001 * (xmax - xmin)
        deltaX = (xmax - xmin) / numAreaX
        epsY = 0.0001 * (ymax - ymin)
        deltaY = (ymax - ymin) / numAreaY
        minNum = n_point + 1
c         
c         Loop on the areas
        do ix = 1, numAreaX
          xlo = xmin + (ix - 1.) * deltaX - epsX
          xhi = xmin + ix * deltaX + epsX
          do iy = 1, numAreaY
            ylo = ymin + (iy - 1.) * deltaY - epsY
            yhi = ymin + iy * deltaY + epsY
            numPts = 0
            do i = 1, n_point
c               
c               If a point is in area and hasn't been done yet, copy it over
c               and mark as in the fitting group
              if (igroup(i) .lt. 0 .and. xyz(1,i) .ge. xlo .and. xyz(1,i) .le.
     &            xhi .and. xyz(2,i) .ge. ylo .and. xyz(2,i) .le. yhi) then
                numPts = numPts + 1
                do j = 1, 3
                  xyzfit(j,numPts) = xyz(j,i)
                enddo
                igroup(i) = 0
              endif
            enddo
            if (local .gt. 0) write(*,'(a,2i3,a,i6,a)')'Subarea',ix,iy,' has',
     &          numPts,' points'
            minNum = min(minNum, numPts)
c             
c             Do the fits to find surfaces
            if (numPts .gt. 1) call find_surfaces(xyzfit, numPts, 2, 60., 5 ,
     &          tiltdum, igrfit, 0, 0., 0., 1)
c             
c             Copy the group numbers back
            j = 0
            do i = 1, n_point
              if (igroup(i) .eq. 0) then
                j = j + 1
                igroup(i) = igrfit(j)
              endif
            enddo
          enddo
        enddo
      endif
c
c       Count numbers in groups
      do iy = 1, 2
        numInGroup(iy) = 0
        do i = 1, n_point
          if (igroup(i) .eq. iy) numInGroup(iy) = numInGroup(iy) + 1
        enddo
        write(*,'(a,i2,a,i6,a)')'Group',iy,' has',numInGroup(iy),' points'
      enddo
c       
c       Set up for majority group if desired, or to avoid empty object
      iystr = 1
      iyend = 2
      if (majority .or. numInGroup(2) .eq. 0) then
        if (numInGroup(1) .ge. numInGroup(2)) iyend = 1
        if (numInGroup(1) .lt. numInGroup(2)) iystr = 2
      endif
c       
c       Set up to shift the data if size changing and set up scaling
      dx = 0.
      dy = 0.
      nxub = maxx * ibinPreali
      nyub = maxy * ibinPreali
      scalef = 1.
      if (newNx .gt. 0 .and. newNy .gt. 0) then
        dx = (newNx - nxub) / 2.
        dy = (newNy - nyub) / 2.
        nxub = newNx
        nyub = newNy
      endif
      if (ixTrim0 .gt. 0 .and. ixTrim1 .gt. 0) then
        dx = dx - (ixTrim0 - 1) * ibinRec
        nxub = (ixTrim1 + 1 - ixTrim0) * ibinRec
      endif
      if (iyTrim0 .gt. 0 .and. iyTrim1 .gt. 0) then
        dy = dy - (iyTrim0 - 1) * ibinRec
        nyub = (iyTrim1 + 1 - iyTrim0) * ibinRec
      endif
      dx = dx / ibinPreali
      dy = dy / ibinPreali
      if (rescale) then
        scalef = float(ibinPreali) / ibinRec
        nxub = nint(float(nxub) / ibinRec)
        nyub = nint(float(nyub) / ibinRec)
      else
        nxub = nint(float(nxub) / ibinPreali)
        nyub = nint(float(nyub) / ibinPreali)
      endif
      ierr = putImodMaxes(nxub, nyub, maxz)
c       
c       Set object properties
      isize = 3
      ierr = getscatsize(1, isize)
      isize = max(3, isize)
      ierr = deleteiobj()
      call putscatsize(1, isize)
      call putimodflag(1,2)
      call putobjcolor(1, 0, 255, 0)
      if (iystr .ne. iyend) then
        call putscatsize(2, isize)
        call putimodflag(2,2)
        call putobjcolor(2, 255, 0, 255)
      endif
c       
c       rebuild the model, make it one point per contour
      ix = 0
      do iy = iystr, iyend
        do i = 1, n_point
          if (igroup(i) .eq. iy) then
            ix = ix + 1
            ibase_obj(ix) = ix - 1
            npt_in_obj(ix) = 1
            obj_color(1, ix) = 1
            obj_color(2, ix) = 255 - (iy - iystr)
            p_coord(1,ix) = scalef * (xyz(1,i) + dx)
            p_coord(indy,ix) = scalef * (xyz(2,i) + dy)
            p_coord(indz,ix) = scalef * xyz(3,i)
          endif
        enddo
      enddo
      max_mod_obj = ix
      n_point = ix
      call scale_model(1)
      call write_wmod(filout)
      if (local .gt. 0 .and. minNum .le. 4) then
        print *,'Some areas have very few points - you should rerun this '//
     &      'with larger subareas'
      else if (local .gt. 0 .and. minNum .le. 10) then
        print *, 'If points are '//
     &      'sorted incorrectly, try rerunning with larger subareas'
      endif
      call exit(0)
      end
c
c       $Log$
c       Revision 3.3  2010/05/10 20:18:09  mast
c       Fixed initialization of all logicals
c
c       Revision 3.2  2010/04/02 03:21:39  mast
c       Added option for one surface and multiple objects
c
c       Revision 3.1  2009/12/09 01:33:33  mast
c       Added to package
c
c       Revision 3.23  2009/12/02 05:42:47  mast
c       *** empty log message ***
c
