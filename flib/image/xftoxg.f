c       XFTOXG takes a list of transformations (f) from each section to
c       the previous one, and computes a list of xforms (g) to apply to each
c       section to obtain a single consistent set of alignments.
c       
c       See man page for details.
c       
c       David Mastronarde 1988; hybrid method 7/26/01
c       
c       $Id$
c       Log at end of file
c       
      implicit none
      integer lmsc
      parameter (lmsc=100000)
      real*4 f(2,3,lmsc),g(2,3,lmsc),nat(2,3,lmsc) ,gav(2,3)
      real*4 natav(2,3),ginv(2,3),prod(2,3), slope(2,3,10),intcp(2,3),natpr(2,3)
      real*4 x(lmsc),y(lmsc),slop(10)
      integer*4 igroup(lmsc), nControl(lmsc)
      character*320 infil,outfil,errString
      real*4, allocatable :: dxGrid(:,:), dyGrid(:,:), dxCum(:,:,:), dyCum(:,:,:)
      real*4, allocatable :: dxProd(:,:), dyProd(:,:)
c       
      integer*4 nhybrid, ifshift, iorder, nlist, kl, i, ilist, kllo, klhi, nx, ny
      integer*4 j, ipow, nfit, ierr, lnblnk,numGroups, numInFirst,iordUse, ibin
      integer*4 irefSec, indWarpFile, nxGrid, nyGrid, nxGrTmp, nyGrTmp, iflags
      real*4 xStart, yStart, xInterval, yInterval, xStrTmp, yStrTmp, xIntTmp, yIntTmp
      logical*4 warping, control
      real*4 deltang, angdiff, bint, angleRange, pixelSize, xcen, ycen, xEnd, yEnd
      integer*4 readCheckWarpFile, getNumWarpPoints, getLinearTransform
      integer*4 getGridParameters, getWarpGrid, setWarpGrid, setGridSizeToMake
      integer*4 setLinearTransform, writeWarpFile, multiplyWarpings, newWarpFile
      integer*4 clearWarpFile, separateLinearTransform, expandAndExtrapGrid
      integer*4 gridSizeFromSpacing
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetFloat
      integer*4 PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  xftoxg
c       
      integer numOptions
      parameter (numOptions = 8)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@goutput:GOutputFile:FN:@nfit:NumberToFit:I:@'//
     &    'ref:ReferenceSection:I:@order:OrderOfPolynomialFit:I:@'//
     &    'mixed:HybridFits:I:@range:RangeOfAnglesInAverage:F:@help:usage:B:'
c       
c       defaults
c       
      ifshift = 7
      nhybrid = 0
      iorder = 1
      irefsec = 0
      angleRange = 999.
c       
c       initialize
c       
      call PipReadOrParseOptions(options, numOptions, 'xftoxg',
     &    'ERROR: XFTOXG - ', .true., 1, 1, 1, numOptArg, numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0

c       
c       get input parameters
c       
      if (pipinput) then
        if (PipGetInteger('ReferenceSection', irefSec) .eq. 0) then
          if (irefSec .lt. 1) call exitError('REFERENCE SECTION NUMBER MUST BE POSITIVE')
          ifshift = 0
        endif
        ierr = PipGetInteger('NumberToFit', ifshift)
        if (ifshift .lt. 0) call exitError('A negative value for nfit is not allowed')
        if (irefSec .gt. 0 .and. ifshift .gt. 0) call exitError(
     &      'A REFERENCE SECTION CAN ONLY BE USED WITH GLOBAL ALIGNMENT')

        ierr = PipGetInteger('OrderOfPolynomialFit', iorder)
        ierr = PipGetInteger('HybridFits', nhybrid)

        if (nhybrid .ne. 0) then
          if (ifshift .eq. 0) call exitError(
     &        'You cannot use hybrid and global alignment together')
          if (nhybrid .lt. 0) call exitError(
     &        'A negative value for hybrid alignment is not allowed')
          nhybrid=max(2,min(4,nhybrid))
        endif

        ierr = PipGetFloat('RangeOfAnglesInAverage', angleRange)
      else

        print *,'Enter 0 to align all sections to a single'
     &      //' average central alignment;'
        print *,'   or 1 to align to an average alignment that shifts based'
        print *,'         on a polynomial fit to the whole stack;'
        print *, '   or N to align each section to an average '//
     &      'alignment based'
        write(*,'(1x,a,/,a,$)')
     &      '         on a polynomial fit to the nearest N sections,',
     &      '   or -1 or -N for a hybrid of central and shifting '//
     &      'alignments: '
        read(*,*)ifshift
c         
        if(ifshift.lt.0)then
          write(*,'(1x,a,/,a,$)')'Enter # of parameters to do central'
     &        //' alignment on (2 for translation only,',
     &        ' 3 for translation/rotation; 4 for trans/rot/mag: '
          read(5,*)nhybrid
          ifshift=abs(ifshift)
          nhybrid=max(2,min(4,nhybrid))
        endif
c         
        if(ifshift.gt.0)then
          write(*,'(1x,a,$)')
     &        'Order of polynomial to fit (1 for linear): '
          read(*,*)iorder
        endif
      endif
      if(ifshift.gt.1)iorder=min(ifshift-1,iorder)
      iorder=max(1,min(10,iorder))
c       
      if (PipGetInOutFile('InputFile', 1, 'Input file of f transforms',
     &    infil) .ne. 0) call exitError('NO INPUT FILE SPECIFIED')
      if (PipGetInOutFile('GOutputFile', 2, 'Output file of g transforms',
     &    outfil) .ne. 0) then
        i = lnblnk(infil)
        if (infil(i-1:i) .ne. 'xf')call exitError
     &      ('NO OUTPUT FILE SPECIFIED AND INPUT FILENAME DOES NOT END IN XF')
        outfil = infil
        outfil(i:i) = 'g'
      endif
c       
c       Determine if there is warping
      indWarpFile = readCheckWarpFile(infil, 0, 1, nx, ny, nlist, ibin,
     &    pixelSize, iflags, errString)
      if (indWarpFile .lt. -1) call exitError(errString)
      warping = indWarpFile .ge. 0
      if (.not. warping) then
c       
c         Regular: open files, read the whole list of f's
c         
        call dopen(1,infil,'old','f')
        call dopen(2,outfil,'new','f')
        call xfrdall2(1,f,nlist,lmsc,ierr)
        if (ierr .eq. 2) call exitError('READING TRANSFORM FILE')
        if (ierr .eq. 1) call exitError('TOO MANY TRANSFORMS IN FILE FOR ARRAYS')
      endif
      if (nlist .gt. lmsc) call exitError('TOO MANY TRANSFORMS FOR ARRAYS')
      if (irefSec .gt. nlist) call exitError(
     &    'REFERENCE SECTION NUMBER TOO LARGE FOR NUMBER OF TRANSFORMS')
c       
      if (ifshift .gt. 0 .and. nlist .lt. 3) then
        write(*,'(/,a)')'WARNING: XFTOXG - COMPUTING A GLOBAL ALIGNMENT SINCE THERE '//
     &      'ARE FEWER THAN 3 TRANSFORMS'
        ifshift = 0
      endif
      if (warping) then
c         
c         warping
        write(*,'(a,a)')'Old warping file opened: ',trim(infil)
        control = mod(iflags / 2, 2) .ne. 0
        xcen = nx / 2.
        ycen = ny / 2.
c         
c         Get the linear transforms and determine smallest spacing needed
        xStart = nx
        yStart = ny
        xEnd = 0.
        yEnd = 0.
        xInterval = nx
        yInterval = ny
        nxGrid = 0
        nyGrid = 0
        do kl = 1, nlist
          nControl(kl) = 4
          if (control) then
            if (getNumWarpPoints(kl, nControl(kl)) .ne. 0)
     &          call exitError('GETTING NUMBER OF CONTROL POINTS')
          endif
c           IS THIS NEEDED?
          if (nControl(kl) .gt. 2) then
            if (separateLinearTransform(kl) .ne. 0) call exitError
     &          ('SEPARATING OUT THE LINEAR TRANSFORM FROM THE WARPING')
          endif
          if (getLinearTransform(kl, f(1,1,kl)) .ne. 0) call exitError
     &        ('GETTING LINEAR TRANSFORM FROM WARP FILE')
          if (nControl(kl) .ge. 4) then
            if (control) then
              if (gridSizeFromSpacing(kl, -1., -1., 1) .ne. 0) call exitError
     &            ('SETTING GRID SIZE FROM SPACING OF CONTROL POINTS')
            endif
            if (getGridParameters(kl, nxGrTmp, nyGrTmp, xStrTmp, yStrTmp, xIntTmp,
     &          yIntTmp) .ne. 0) call exitError('GETTING GRID PARAMETERS')
c            print *,nxGrTmp, nyGrTmp, xStrTmp, yStrTmp, xIntTmp,yIntTmp
            xStart = min(xStart, xStrTmp)
            xInterval = min(xInterval, xIntTmp)
            xEnd = max(xEnd, (nxGrTmp - 1) * xInterval)
            yStart = min(yStart, yStrTmp)
            yInterval = min(yInterval, yIntTmp)
            yEnd = max(yEnd, (nyGrTmp - 1) * yInterval)
          endif
        enddo
c        print *,xStart,xEnd,xInterval,yStart,yEnd,yInterval
        if (xStart .le. 0. .or. xEnd .ge. nx .or. yStart .le. 0. .or. yEnd .ge. ny)
     &      call exitError('CANNOT WORK WITH GRIDS THAT EXTEND OUTSIDE THE DEFINED '//
     &      'IMAGE AREA')
c         
c         Figure out the grid size and interval that fits in the range, but make it
c         fill the range
        xStart = min(xStart, xInterval / 2.)
        xEnd = max(xEnd, nx - xInterval / 2.)
        yStart = min(yStart, yInterval / 2.)
        yEnd = max(yEnd, ny - yInterval / 2.)
        nxGrid = max(2., (xEnd - xStart) / xInterval + 1.05)
        xInterval = (xEnd - xStart) / (nxGrid - 1)
        nyGrid = max(2., (yEnd - yStart) / yInterval + 1.05)
        yInterval = (yEnd - yStart) / (nyGrid - 1)
c        print *,xStart,xEnd,xInterval,yStart,yEnd,yInterval,nxGrid,nyGrid
c
        if (control) then
c           
c           Then set all the parameters for control point grids
          do kl = 1, nlist
            ierr = setGridSizeToMake(kl, nxGrid, nyGrid, xStart, yStart, xInterval,
     &          yInterval)
          enddo
        endif
c         
c         Allocate arrays
        allocate(dxGrid(nxGrid,nyGrid), dyGrid(nxGrid,nyGrid), dxCum(nxGrid,nyGrid,nlist),
     &      dyCum(nxGrid,nyGrid,nlist), dxProd(nxGrid,nyGrid), dyProd(nxGrid,nyGrid),
     &      stat = ierr)
        call memoryError(ierr, 'ARRAYS FOR WARPING GRIDS')
c         
c         Form cumulative product to align to the first section: initialize to unit
        call xfunit(g(1,1,1), 1.)
        dxCum = 0.                              !ARRAY OPERATIONS
        dyCum = 0.
        do kl = 2, nlist
          if (nControl(kl) .gt. 3) then
            if (getWarpGrid(kl, nxGrTmp, nyGrTmp, xStrTmp, yStrTmp, xIntTmp, yIntTmp,
     &          dxGrid, dyGrid, nxGrid) .ne. 0) call exitError('GETTING WARP GRID')
c            print *,'raw grid', nxGrTmp, nyGrTmp
c            write(*,'(f7.2,9f8.2)')((dxGrid(i,j),dyGrid(i,j),i=1,nxGrid),j=1,nyGrid)
            if (.not.control) then
              if (expandAndExtrapGrid(dxGrid, dyGrid, nxGrid, nyGrid, nxGrTmp, nyGrTmp,
     &            xStrTmp, yStrTmp, xIntTmp, yIntTmp, xStart, yStart, xEnd, yEnd, 0, nx,
     &            0, ny)
     &            .ne. 0) call exitError('EXPANDING A WARP GRID')
            endif
          else
            nxGrTmp = nxGrid
            nyGrTmp = nyGrid
            dxGrid = 0.
            dyGrid = 0.
          endif
          if (multiplyWarpings(dxGrid, dyGrid, nxGrid, nxGrTmp, nyGrTmp, xStrTmp,
     &        yStrTmp, xIntTmp, yIntTmp, f(1,1,kl), xcen, ycen,
     &        dxCum(1,1,kl-1), dyCum(1,1,kl-1), nxGrid, nxGrid, nyGrid,
     &        xStart, yStart, xInterval, yInterval, g(1,1,kl-1),
     &        dxCum(1,1,kl), dyCum(1,1,kl), g(1,1,kl), 0) .ne. 0)
     &        call exitError ('MULTIPLYING TWO WARPINGS TOGETHER FOR CUMULATIVE WARPING')
c          call xfwrite(6,g(1,1,kl))
c          print *,'cumulative',nxGrid,nyGrid
c          write(*,'(f7.2,9f8.2)')((dxCum(i,j,kl),dyCum(i,j,kl),i=1,nxGrid),j=1,nyGrid)
        enddo
c         
c         Find the mean grid and take its inverse, leave in dxGrid, dyGrid
        dxProd = 0.
        dyProd = 0.
        do kl = 1, nlist
          do j = 1, nyGrid
            do i = 1, nxGrid
              dxProd(i,j) = dxProd(i,j) + dxCum(i,j,kl) / nlist
              dyProd(i,j) = dyProd(i,j) + dyCum(i,j,kl) / nlist
            enddo
          enddo
        enddo
c        write(*,'(f7.2,9f8.2)')((dxProd(i,j),dyProd(i,j),i=1,nxGrid),j=1,nyGrid)
        
        call invertWarpGrid(dxProd, dyProd, nxGrid, nxGrid, nyGrid, xStart, yStart,
     &      xInterval, yInterval, g(1,1,1), xcen, ycen, dxGrid, dyGrid, prod)
c         
c         Start a new warp file
        ierr = clearWarpFile(indWarpFile)
        iflags = 1
        indWarpFile = newWarpFile(nx, ny, ibin, pixelSize, iflags)
        if (indWarpFile .lt. 0) call exitError('OPENING A NEW WARPING FILE')
      else
C         
c         Regular transforms: compute g's to align all sections to the first
C         It seems wrong to copy f(1) instead of starting with a unit...
        call xfcopy(f(1,1,1),g(1,1,1))
        do i=2,nlist
          call xfmult(f(1,1,i),g(1,1,i-1),g(1,1,i))
c          call xfwrite(6,g(1,1,i))
        enddo
      endif
c       
c       Usual treatment of regular transforms now that we have cumulative g in each case
c       Convert to "natural" transforms
c       
      deltang=0.
      do kl=1,nlist
        call amat_to_rotmag(g(1,1,kl),nat(1,1,kl),nat(1,2,kl) ,nat(2,1,kl),nat(2,2,kl))
        nat(1,3,kl)=g(1,3,kl)
        nat(2,3,kl)=g(2,3,kl)
c         
c         if rotation angle differs greatly from last one, adjust the
c         DELTANG value appropriately so that angles change in continuous
c         increments around + or - 180 degrees
c         
        if(kl.gt.1)then
          angdiff=nat(1,1,kl-1)-(nat(1,1,kl)+deltang)
          if(abs(angdiff).gt.180.)deltang=deltang+sign(360.,angdiff)
        endif
        nat(1,1,kl)=nat(1,1,kl)+deltang
c	  call xfwrite(6,nat(1,1,kl),*97)
      enddo
c       
c       average natural g's, convert back to xform, take inverse of average;
c       ginv is used if no linear fits, natav is used in hybrid cases
c       First get group with restricted rotation range
c       
      call groupRotations(nat, 1, nlist, angleRange, igroup, numGroups,
     &    numInFirst)
c	print *,numGroups,' groups, first has', numInFirst
c	print *,(igroup(kl),kl=1,nlist)
      call xfunit(natav,0.)
      do kl=1,nlist
        if (igroup(kl) .eq. 1)
     &      call xflincom(natav,1.,nat(1,1,kl),1./numInFirst,natav)
      enddo
      call rotmag_to_amat(natav(1,1),natav(1,2)
     &    ,natav(2,1),natav(2,2),gav)
      gav(1,3)=natav(1,3)
      gav(2,3)=natav(2,3)
      call xfinvert(gav,ginv)
c       
c       If doing reference section, just invert its transform
c
      if (irefSec .gt. 0) call xfinvert(g(1,1,irefSec), ginv)
c       
c       DNM 1/24/04: fixed bug in hybrid 3 or 4:
c       do not convert natav to be the inverse!
c       
c       loop over each section
c       
      do ilist=1,nlist
        if(ifshift.gt.0)then
c           if doing line fits, set up section limits for this fit
          if(ifshift.eq.1)then
            kllo=1				!ifshift=1: take all sections
            klhi=nlist
          else
            kllo=max(1,ilist-ifshift/2)         !>1: take N sections centered
            klhi=min(nlist,kllo+ifshift-1)	!on this one, or offset
            kllo=max(1,min(kllo,klhi+1-ifshift)) ! DNM 1/24/04 fixed ishift
          endif
          if(ifshift.gt.1.or.(ifshift.eq.1.and.ilist.eq.1))then
c             
c             do line fit to each component of the natural parameters
c             first time only if doing all sections, or each time for N
c             
            call groupRotations(nat, kllo, klhi, angleRange, igroup,
     &          numGroups, numInFirst)
            iordUse = max(1, min(iorder, numInFirst - 2))
            do i=1,2
              do j=1,3
                nfit=0
                do kl=kllo,klhi
                  if (igroup(kl) .eq. 1) then
                    nfit=nfit+1
                    x(nfit)=kl
                    y(nfit)=nat(i,j,kl)
                  endif
                enddo
c                 
                if (nfit .lt. 2) call exitError('Only 1 point in fit; '//
     &              'increase NumberToFit or RangeOfAngles')
                call polyfit(x,y,nfit,iordUse,slop,bint)
                do ipow=1,iordUse
                  slope(i,j,ipow)=slop(ipow)
                enddo
                intcp(i,j)=bint
              enddo
            enddo
          endif
          if(ifshift.eq.1.and.ilist.eq.1)then
            print *,'constants and coefficients of fit to',nfit ,' natural parameters'
            call xfwrite(6,intcp,*97)
            do ipow=1,iordUse
              call xfwrite(6,slope(1,1,ipow),*99)
99          enddo
          endif
c           
c           calculate the g transform at this position along the linear fit
c           and take its inverse
c           
          call xfcopy(intcp,natpr)
          do ipow=1,iordUse
            call xflincom(natpr,1.,slope(1,1,ipow),float(ilist)**ipow
     &          ,natpr)
          enddo
c           
c           for hybrid method, restore global average for translations,
c           restore rotation if nhybrid is 3 or 4; restore overall mag
c           if nhybrid is 4
c           
          if(nhybrid.gt.0)then
            natpr(1,3)=natav(1,3)
            natpr(2,3)=natav(2,3)
            if(nhybrid.gt.2)natpr(1,1)=natav(1,1)
            if(nhybrid.gt.3)natpr(2,1)=natav(2,1)
          endif
c           
          call rotmag_to_amat(natpr(1,1),natpr(1,2)
     &        ,natpr(2,1),natpr(2,2),prod)
          prod(1,3)=natpr(1,3)
          prod(2,3)=natpr(2,3)
          call xfinvert(prod,ginv)
        endif
c         
c         multiply this g by the inverse of the grand average or the locally
c         fitted average: generates a xform to the common central place or
c         to the locally fitted center.  This stuff about the linearly fitted
c         center position is pretty ad hoc and hokey, but it seems to give
c         good results in the final images.
c         
        if (warping) then
c           
c           If there are warpings, multiply cumulative warp by inverse warp based on
c           this transform and the inverse average warp
          if (multiplyWarpings(dxCum(1,1,ilist), dyCum(1,1,ilist), nxGrid, nxGrid, nyGrid,
     &        xStart, yStart, xInterval, yInterval, g(1,1,ilist), xcen, ycen, dxGrid,
     &        dyGrid, nxGrid, nxGrid, nyGrid, xStart, yStart, xInterval, yInterval,
     &        ginv, dxProd, dyProd, prod, 0) .ne. 0)
     &        call exitError ('MULTIPLYING TWO WARPINGS TOGETHER FOR FINAL WARPING')
          if (setLinearTransform(ilist, prod) .ne. 0 .or.
     &        setWarpGrid(ilist, nxGrid, nyGrid, xStart, yStart,
     &        xInterval, yInterval, dxProd, dyProd, nxGrid) .ne. 0)
     &        call exitError('STORING FINAL WARPING')
        else
          call xfmult(g(1,1,ilist),ginv,prod)
          call xfwrite(2,prod,*97)
        endif
      enddo
      if (warping) then
        if (writeWarpFile(outfil, 0) .ne. 0) call exitError('WRITING NEW WARP FILE')
        write(*,'(a,a)')'New warping file written: ',trim(outfil)
      else
        close(2)
      endif
      call exit(0)
97    call exitError('WRITING FILE')
      end

c       
c       groupRotations finds groups of sections whose rotation angles are
c       all within the range given by RANGE.  The angles are assumed to be
c       the first element of the trasnforms in F.  Only sections from KSTART
c       to KEND are considered.  IGROUP is returned with a group number for
c       each section; numGroups with the number of groups, and numInFirst
c       with the number of sections in the first (i.e., largest) group.
c       
      subroutine groupRotations(f, kStart, kEnd, range, igroup, numGroups,
     &    numInFirst)
      implicit none
      real*4 f(2,3,*), range
      integer*4 igroup(*), kStart, kEnd, numGroups, numInFirst
      real*4 diff, angMin, angMax, angleDiff
      integer*4 numFree, i, j, numInGroup, maxInGroup, iMax
c       
      numFree = kEnd + 1 - kStart
      numGroups = 0
      angMin = 1.e10
      angMax = -1.e10
      do i = kStart, kEnd
        igroup(i) = 0
        angMin = min(angMin, f(1,1,i))
        angMax = max(angMax, f(1,1,i))
      enddo
c       
c       If the angles all fit within the range, we are all set
c       
      if (angMax - angMin .le. range) then
        do i = kStart, kEnd
          igroup(i) = 1
        enddo
        numGroups = 1
        numInFirst = numFree
        return
      endif
c       
      do while (numFree .gt. 0)
c         
c         Find biggest group of angles that fit within range
c         
        i = kStart
        maxInGroup = 0
        do while (i .le. kEnd .and. maxInGroup .lt. numFree)
          if (igroup(i) .eq. 0) then
c             
c             For each ungrouped item, count up number of items that would
c             fit within a range starting at this item
c             
            numInGroup = 0
            do j = kStart, kEnd
              if (igroup(j) .eq. 0) then
                diff = angleDiff(f(1,1,i), f(1,1,j))
                if (diff .ge. 0. .and. diff .le. range)
     &              numInGroup = numInGroup + 1
              endif
            enddo
            if (numInGroup .gt. maxInGroup) then
              maxInGroup = numInGroup
              iMax = i
            endif
          endif
          i = i + 1
        enddo
c         
c         Now assign the angles within range to the group
c         
        numGroups = numGroups + 1
        do j = kStart, kEnd
          if (igroup(j) .eq. 0) then
            diff = angleDiff(f(1,1,iMax), f(1,1,j))
            if (diff .ge. 0. .and. diff .le. range) igroup(j) = numGroups
          endif
        enddo
        if (numGroups .eq. 1) numInFirst = maxInGroup
        numFree = numFree - maxInGroup
      enddo
      return
      end

      real*4 function angleDiff(ang1, ang2)
      real*4 ang1, ang2
      angleDiff = ang2 - ang1
      do while (angleDiff .gt. 180.)
        angleDiff = angleDiff - 360.
      enddo
      do while (angleDiff .le. -180.)
        angleDiff = angleDiff + 360.
      enddo
      return
      end

c       
c       $Log$
c       Revision 3.9  2011/06/23 14:57:05  mast
c       Reorder arguments to calls for consistency
c
c       Revision 3.8  2011/06/17 04:08:21  mast
c       Switched to new routines for warping
c
c       Revision 3.7  2011/06/10 04:10:11  mast
c       Added warping
c
c       Revision 3.6  2008/11/21 20:03:12  mast
c       Increased character size for filenames
c
c       Revision 3.5  2006/10/24 19:45:49  mast
c       Added ability to set reference section
c
c       Revision 3.4  2005/10/11 18:55:05  mast
c       Commented out debugging output
c	
c       Revision 3.3  2005/10/11 15:45:50  mast
c       Added option to based average center position on restricted range of
c       rotation angles
c	
c       Revision 3.2  2004/01/27 05:37:33  mast
c       Needed to split print line for SGI
c	
c       Revision 3.1  2004/01/27 03:33:12  mast
c       Converted to PIP input, and fixed hybrid option for eliminating
c       trends in rotation and magnification.
c	
