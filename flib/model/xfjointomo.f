c       XFJOINTOMO
c       
c       Computes transforms for aligning tomograms of serial sections from
c       modeled features.  The model can have two kinds of features: contours
c       following trajectories on either side of a boundary, and contours with 
c       one point on either side of a boundary.  The data from trajectories
c       allows the optimal spacing between the tomograms to be determined.
c       See man page for details.
c       
c       Written by David Mastronarde, 10/24/06
c
c       $Id$
c       
c       $Log$
c       Revision 3.6  2010/06/26 18:13:51  mast
c       Fixed lack of shortcircuit evaluation in while statements
c
c       Revision 3.5  2009/03/25 21:37:26  mast
c       Initialized reference section number
c
c       Revision 3.4  2008/07/14 23:45:15  mast
c       Made it skip points if a trajectory was coplanar on one side or other
c
c       Revision 3.3  2006/12/20 16:09:00  mast
c       Removed a ;
c
c       Revision 3.2  2006/12/19 22:19:37  mast
c       Reorganized output to be parseable
c
c       Revision 3.1  2006/10/26 19:08:20  mast
c       Added to package
c
c       
      implicit none
      integer limsec,limslc,idim
      include 'smallmodel.inc'
      include 'statsize.inc'
c
      parameter (limsec=500,limslc=10000,idim=400)
      real*4 f(2,3,limslc),gtmp(2,3),g(2,3,limslc),xnat(2,3,limslc),xnatav(2,3)
      real*4 ginv(2,3), gav(2,3)
      integer*4 nxyz(3),iobjUse(limsec),isecDo(limsec),izBound(limsec)
      integer*4 nzSizes(limsec),nx,ny,nz
      equivalence (nxyz(1),nx),(nxyz(2),ny),(nxyz(3),nz)
      real*4 xx(limsec),yy(limsec),zz(limsec)
      real*4 xr(msiz,idim)
      real*4 errmin(limsec),errmaxmin(limsec),gapmin(limsec)
      character*160 modelfile,xffile,xgfile
      character*1024 listString
      integer*4 imodobj,imodcont,ipntmax,izsec,npnts,ngaps,igap,indf,icol,idir
      real*4 xcen,ycen,devavg,gapinc,devmax,devsd
      logical exist,sliceOut,editOld, coplanar
      integer*4 limpnts,ifTrans,ifRoTrans,ifMagRot,joinBin,joinOffsetX,nfit
      integer*4 minfit,joinOffsetY,nobjUse,ierr2,ierr,numBoundaries,nzSecSum
      real*4 gapstr,gapend,delgap,gapz,extraz,slopex,bintx,ro,slopey,binty
      integer*4 nfWrite,nfRead,numSecDo,i,isec,iobject,ninobj,ifonlist
      integer*4 ibase,ipol,iAboveGap,lastInSec,iFirstInPrev,numAbove,numBelow
      integer*4 mfit,ipt,numSizes,ipnt,ifFullRpt,j,irefSec
      real*4 zmin, zmax
      integer*4 getImodMaxes
      logical readSmallMod
c       
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetBoolean,PipGetLogical
      integer*4 PipGetString, PipGetTwoIntegers, PipGetThreeIntegers
      integer*4 PipGetIntegerArray,PipGetThreeFloats
      integer*4 PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  xfjointomo
c       
      integer numOptions
      parameter (numOptions = 20)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@foutput:FOutputFile:FN:@'//
     &    'goutput:GOutputFile:FN:@edit:EditExistingFile:B:@'//
     &    'slice:SliceTransforms:B:@join:JoinFileOrSizeXYZ:FN:@'//
     &    'sizes:SizesOfSections:IA:@zvalues:ZValuesOfBoundaries:IA:@'//
     &    'offset:OffsetOfJoin:IP:@binning:BinningOfJoin:I:@'//
     &    'refsec:ReferenceSection:I:@transonly:TranslationOnly:B:@'//
     &    'rottrans:RotationTranslation:B:@magrot:MagRotTrans:B:@'//
     &    'boundaries:BoundariesToAnalyze:LI:@points:PointsToFit:IP:@'//
     &    'gap:GapStartEndInc:FT:@objects:ObjectsToInclude:LI:@'//
     &    'param:ParameterFile:PF:@help:usage:B:'
c       
c       defaults
c       
      limpnts = 4
      ifTrans=0
      ifRoTrans=0
      ifMagRot = 0
      joinBin = 1
      joinOffsetX = 0
      joinOffsetY = 0
      nfit = 5
      minfit = 2
      gapstr = 0.
      gapend = 0.
      delgap = 0.
      nobjUse = 0
      editOld = .false.
      sliceOut = .false.
      ifFullRpt = 0
      irefSec = 0
c
c       Pip startup: set error, parse options, check help
c       
      call PipReadOrParseOptions(options, numOptions, 'xfjointomo',
     &    'ERROR: XFJOINTOMO - ', .false., 2, 1, 2, numOptArg, numNonOptArg)

      ierr = PipGetLogical('SliceTransforms', sliceOut)
      if (PipGetInOutFile('InputFile', 1, ' ', modelfile)
     &    .ne. 0) call exitError('NO INPUT MODEL FILE SPECIFIED')
      if (PipGetInOutFile('FOutputFile', 2, ' ', xffile)
     &    .ne. 0) call exitError('NO OUTPUT FILE SPECIFIED FOR F TRANSFORMS')
      ierr = PipGetInOutFile('GOutputFile', 3, ' ', xgfile)
      if (ierr .ne. 0 .and. .not.sliceOut) call exitError(
     &    'NO OUTPUT FILE SPECIFIED FOR G TRANSFORMS TO BE USED IN FINISHJOIN')
      if (ierr .eq. 0 .and. sliceOut) call exitError('NO SECOND OUTPUT FILE'//
     &    ' ALLOWED WITH TRANSFORM OUTPUT FOR EVERY SLICE')

      exist=readSmallMod(modelfile)
      if(.not.exist)call exitError('READING MODEL FILE')
c       
c       Get boundaries or Z sizes; if the latter, fill boundary array and
c       compute a total Z size from section sizes
c
      numBoundaries = 0
      ierr = PipGetIntegerArray('ZValuesOfBoundaries', izBound, numBoundaries, 
     &    limsec)
      numSizes = 0
      ierr2 = PipGetIntegerArray('SizesOfSections', nzSizes, numSizes,  limsec)
      if (ierr + ierr2 .eq. 0) call exitError(
     &    'YOU CANNOT ENTER BOTH BOUNDARY Z VALUES AND SIZES OF SECTIONS')
      if (ierr + ierr2 .eq. 2) call exitError(
     &    'YOU MUST ENTER EITHER BOUNDARY Z VALUES OR SIZES OF SECTIONS')
      nzSecSum = 0
      if (ierr2 .eq. 0) then
        numBoundaries = numSizes - 1
        nzSecSum = nzSizes(1)
        do i = 1, numBoundaries
          izBound(i) = nzSecSum
          nzSecSum = nzSecSum + nzSizes(i + 1)
        enddo
      endif

c       
c       Get the image size from the file, or from the model
c       Z size must come from file if they want slice output, because we
c       cannot trust that model was loaded on whole image
c       Z size from file must match sum of section sizes if any
c
      nx = 0
      call get_nxyz(.true., 'JoinFileOrSizeXYZ', ' ', 1, nxyz)
      if (nx .eq. 0) then
        if (numSizes .eq. 0 .and. sliceOut) call exitError(
     &      'YOU MUST ENTER EITHER -join OR'//
     &      ' -sizes TO GET TRANSFORMS FOR EVERY SLICE')
        ierr = getImodMaxes(nx, ny, nz)
      else
        if (nzSecSum .gt. 0 .and. nzSecSum .ne. nz) call exitError(
     &      'SUM OF SECTION SIZES DOES NOT EQUAL Z SIZE OF JOIN FILE')
      endif
      xcen = nx / 2.
      ycen = ny / 2.
c       
c       Figure out number of transforms to output and initialize transforms
c       
      nfWrite = numBoundaries + 1
      if (sliceOut) nfWrite = nxyz(3)
      if (nfWrite .gt. limslc) call exitError('TOO MANY TRANSFORMS FOR ARRAYS')
      do i = 1, nfWrite
        call xfunit(f(1,1,i), 1.)
      enddo
      ierr = PipGetLogical('EditExistingFile', editOld)
      if (editOld) then
        open(1, file=xffile,status='old',form='formatted',err=21)
        call xfrdall(1, f, nfRead, *99)
        if (nfWrite .ne. nfRead) call exitError(
     &      'NUMBER OF TRANSFORMS IN FILE DOES NOT MATCH NUMBER TO BE WRITTEN')
        close(1)
      endif
c       
c       Jump to here if the old file doesn't exist this time
c       Get entries for adjusting transforms back to original data, and reject
c       them if asked for slice output.
c
21    ierr = PipGetInteger('ReferenceSection', irefSec)
      if (irefSec .gt. numBoundaries + 1) call exitError(
     &    'REFERENCE SECTION NUMBER TOO LARGE')
      ierr = PipGetInteger('BinningOfJoin', joinBin)
      ierr2 = PipGetTwoIntegers('OffsetOfJoin', joinOffsetX, joinOffsetY)
      if (sliceOut .and. (ierr + ierr2 .ne. 2 .or. irefSec .gt. 0))
     &    call exitError('IT MAKES NO SENSE TO ENTER -binning, -offset,'//
     &    ' OR -refsec WITH TRANSFORMS FOR EVERY SLICE')
c
      ierr = PipGetBoolean('TranslationOnly', ifTrans)
      ierr = PipGetBoolean('RotationTranslation', ifRoTrans)
      ierr = PipGetBoolean('MagRotTrans', ifMagRot)
      if (ifTrans + ifRoTrans + ifMagRot .gt. 1) call exitError(
     &    'ONLY ONE OF -transonly, -rottrans, AND -magrot CAN BE ENTERED')
      if (ifMagRot .ne. 0) ifRoTrans = 2
      if (ifRoTrans + ifTrans .gt. 0) limpnts = ifRoTrans + 1
c
      ierr = PipGetTwoIntegers('PointsToFit', nfit,minfit)
      if (nfit.lt.2 .or. minfit .lt. 2) call exitError(
     &    'NUMBER OF POINTS TO FIT MUST BE AT LEAST 2')
      ierr = PipGetThreeFloats('GapStartEndInc', gapstr,gapend,delgap)
      ngaps = 1
      if (gapstr .gt. gapend) call exitError(
     &    'STARTING GAP SIZE MUST NOT BE BIGGER THAN ENDING SIZE')
      if (delgap .gt. 0) ngaps = nint((gapend - gapstr) / delgap) + 1
c
      if (PipGetString('ObjectsToInclude', listString) .eq. 0) then
        call parselist(listString, iobjUse, nobjUse)
        if (nobjUse .gt. limsec) call exitError(
     &      'OBJECT LIST TOO LARGE FOR ARRAYS')
      endif
c       
      numSecDo = numBoundaries
      do i = 1, numSecDo
        iSecDo(i) = i
      enddo
      if (PipGetString('BoundariesToAnalyze', listString) .eq. 0) then
        call parselist(listString, iSecDo, numSecDo)
        if (numSecDo .gt. limsec) call exitError(
     &      'BOUNDARRY LIST TOO LARGE FOR ARRAYS')
      endif
      do i = 1, numSecDo
        izsec = iSecDo(i)
        if (izsec .lt. 1 .or. izsec .gt. numBoundaries)
     &      call exitError('SECTION NUMBER OUT OF RANGE')
      enddo
c       
      call dopen(1, xffile, 'new', 'f')
      call scale_model(0)
c       
c       Loop on the sections
c       
      write(*,122)
122   format(25x,'Deviations between transformed points extrapolated',/,
     &    26x,'from above boundary and points extrapolated from below',/,
     &    30x,'Mean     Max  @obj cont point     X-Y Position')
c       
      do isec = 1, numSecDo
        izsec = iSecDo(isec)
        gapz = izBound(izsec) - 0.5
        errmin(isec) = 1.e10
        if (ngaps .gt. 1) write(*,123) isecDo(isec)
123     format(/,'For boundary #',i4,':')
c
        do igap=1,ngaps
          gapinc=gapstr+(igap-1)*delgap
          npnts=0
          do iobject=1,max_mod_obj
            call objtocont(iobject,obj_color,imodobj,imodcont)
            ninobj=npt_in_obj(iobject)
            ibase=ibase_obj(iobject)
c             
c             See if contour is in included object
c
            ifonlist=0
            do i=1,nobjUse
              if(imodobj.eq.iobjUse(i))ifonlist=1
            enddo
            if (ninobj .gt. 1 .and. (ifonlist .eq. 1 .or. nobjUse .eq. 0)) then
c             
c               Set up indexes of limiting points in this section pair
c               
              ipol=1
              iAboveGap=1
              lastInSec = ninobj
              if(p_coord(3,object(ninobj+ibase)).lt.
     &            p_coord(3,object(1+ibase)))then
                ipol=-1
                iAboveGap=ninobj
                lastInSec = 1
              endif
              iFirstInPrev = iAboveGap
c               
c               find index of point above the gap, first point in previous
c               section and last point in current section
c               
              do while(iAboveGap .le. ninobj .and. iAboveGap .ge. 1)
                if (p_coord(3,object(iAboveGap+ibase)) .ge. gapz) exit
                iAboveGap = iAboveGap + ipol
              enddo
              do while (izsec .gt. 1 .and. iFirstInPrev .le. ninobj .and.
     &            iFirstInPrev .ge. 1)
                if (p_coord(3,object(iFirstInPrev+ibase)) .ge.
     &            izBound(izsec - 1) - 0.5) exit
                iFirstInPrev = iFirstInPrev + ipol
              enddo
              do while (izsec .lt. numBoundaries .and. lastInSec .gt. 1 .and.
     &            lastInSec .lt. ninobj - 1)
                if (p_coord(3,object(lastInSec + ibase + ipol)) .le.
     &            izBound(izsec + 1) - 0.5) exit
                lastInSec = lastInSec + ipol
              enddo
c
              numAbove = ipol * (lastInSec - iAboveGap) + 1
              numBelow = ipol * (iAboveGap - iFirstInPrev)
c               print *,imodobj,imodcont,ninobj,ipol
c               print *, iFirstInPrev,iAboveGap,lastInSec, numBelow, numAbove
              if (numAbove .ge. minfit .and. numBelow .ge. minfit) then
c                 
c                 If there are enough points, fit lines and extrapolate
c                 
                npnts=npnts+1
                xr(6,npnts)=iobject
                xr(7,npnts)=iAboveGap
                ipt=iAboveGap-ipol
                icol=4
                coplanar = .false.
                do idir=-1,1,2
                  mfit=0
                  zmin = 1.e30
                  zmax = -zmin
                  do while(mfit.lt.nfit.and. ipol * (ipt - iFirstInPrev) .ge. 0
     &                .and. ipol * (lastInSec - ipt) .ge. 0)
                    ipnt=abs(object(ipt+ibase))
                    ipt=ipt+idir*ipol
                    mfit=mfit+1
                    xx(mfit)=p_coord(1,ipnt)
                    yy(mfit)=p_coord(2,ipnt)
                    zz(mfit)=p_coord(3,ipnt)
                    zmin = min(zmin, zz(mfit))
                    zmax = max(zmax, zz(mfit))
                  enddo
c                   
c                   Do fits only if there is enough range in Z
                  if (zmax - zmin .gt. 0.1) then
                    call lsfit(zz,xx,mfit,slopex,bintx,ro)
                    call lsfit(zz,yy,mfit,slopey,binty,ro)
                    extraz=gapz-idir*gapinc/2.
                    xr(icol,npnts)=slopex*extraz+bintx-xcen
                    xr(icol+1,npnts)=slopey*extraz+binty-ycen
                  else
                    coplanar = .true.
                  endif
                  ipt=iAboveGap
                  icol=1
                enddo
c                 
c                 If either side was coplanar, drop the point-pair
                if (coplanar) npnts=npnts-1
c                write(*,'(7f8.2)')xr(1,npnts),xr(2,npnts),xr(4,npnts),
c     &              xr(5,npnts)
              else if (numAbove .eq. 1 .and. numBelow .eq. 1) then
c                 
c                 If one point on either side, use them alone
c
                npnts=npnts+1
                xr(6,npnts)=iobject
                xr(7,npnts)=iAboveGap
                ipnt=abs(object(iAboveGap+ibase))
                xr(1,npnts) = p_coord(1,ipnt) - xcen
                xr(2,npnts) = p_coord(2,ipnt) - ycen
                ipnt=abs(object(iAboveGap+ibase-ipol))
                xr(4,npnts) = p_coord(1,ipnt) - xcen
                xr(5,npnts) = p_coord(2,ipnt) - ycen
              endif
            endif
            if (npnts .ge. idim) call exitError(
     &          'TOO MANY POINTS BEING FIT FOR DATA MATRIX')
          enddo
c
          if (npnts.ge.limpnts) then
c           
c             now if there are at least limpnts points, do regressions
c             
            call findxf(xr,npnts,xcen,ycen,iftrans,ifrotrans,2,gtmp,
     &          devavg, devsd,devmax,ipntmax)
C             
c             save transform at minimum error, adjust for binning 
c
            if (devavg .lt. errmin(isec)) then
              errmin(isec) = devavg
              errmaxmin(isec) = devmax
              gapmin(isec) = gapinc
              indf = izsec + 1
              if (sliceOut) indf = izBound(izsec) + 1
              call xfcopy(gtmp, f(1,1,indf))
              f(1,3,indf) = joinBin * gtmp(1,3)
              f(2,3,indf) = joinBin * gtmp(2,3)
            endif
c             
            call objtocont(nint(xr(6,ipntmax)), obj_color, imodobj, imodcont)
            if (ngaps .gt. 1) then
              write(*,121)npnts,gapinc,devavg,devmax,
     &            imodobj,imodcont,nint(xr(7,ipntmax)),
     &            xr(8,ipntmax),xr(9,ipntmax)
121           format(i4, ' points, gap ',f6.1,2x,2f9.2,2i4,i6,2x,2f9.2)
            else
              write(*,125)izsec,npnts,devavg,devmax,
     &            imodobj,imodcont,nint(xr(7,ipntmax)),
     &            xr(8,ipntmax),xr(9,ipntmax)
125           format('boundary',i4, ',',i4,' points ',2f9.2,2i4,i6,2x,2f9.2)
            endif
            if(iffullrpt.ne.0) then
              write(*,124)
124             format(' Obj Cont Point        position        ',
     &              'deviation vector   angle   magnitude')
              do j = 1, npnts
                call objtocont(nint(xr(6,j)), obj_color, imodobj, imodcont)
                write(*,128)imodobj,imodcont,(xr(i,j),i=7,13)
128             format(2i4,f6.0,4f10.2,f9.0,f10.2)
              enddo
            endif
          else if (igap .eq. 1) then
            write(*,130)npnts,isec,limpnts
130         format('WARNING: There are only',i2,'points across boundary',i4,
     &          '; ',i2,' are needed to solve for the selected parameters')
          endif
        enddo
      enddo
      print *
      do isec = 1, numSecDo
        if (errmin(isec) .lt. 1.e9) write(*,126)iSecDo(isec),gapmin(isec),
     &      errmin(isec),errmaxmin(isec)
126     format('At boundary',i4,', best gap =',f6.1,' with error mean =',f9.2,
     &      ', max =',f9.2)
      enddo
c       
c       Write the F transforms
c       
      do i = 1, nfWrite
        call xfwrite(1,f(1,1,i),*94)
      enddo
      if (.not. sliceOut) then
c       
c         Now need to replicate xftoxg treatment of these transforms.  This 
c         a copy fo xftoxg with no angular majority vote analysis
c         Start by computing cumulative transform
c
        call dopen(2, xgfile, 'new', 'f')
        call xfcopy(f(1,1,1),g(1,1,1))
        do i=2,nfWrite
          call xfmult(f(1,1,i),g(1,1,i-1),g(1,1,i))
        enddo
        if (irefSec .gt. 0) then
c           
c           If doing reference section, just invert its transform
c           
          call xfinvert(g(1,1,irefSec), ginv)
        else
c           
c           Otherwise compute cumulative transform, then convert to natural
c           transforms
c           
          do i=1,nfWrite
            call amat_to_rotmag(g(1,1,i), xnat(1,1,i), xnat(1,2,i),
     &          xnat(2,1,i), xnat(2,2,i))
            xnat(1,3,i)=g(1,3,i)
            xnat(2,3,i)=g(2,3,i)
          enddo
c           
c           average natural g's, convert back to xform, take inverse of average
c           
          call xfunit(xnatav,0.)
          do i=1,nfWrite
            call xflincom(xnatav,1.,xnat(1,1,i),1./nfWrite,xnatav)
          enddo
          call rotmag_to_amat(xnatav(1,1), xnatav(1,2), xnatav(2,1),
     &        xnatav(2,2), gav)
          gav(1,3)=xnatav(1,3)
          gav(2,3)=xnatav(2,3)
          call xfinvert(gav,ginv)
        endif
c         
c         Compute the g transforms by multiplying by the inverse and adjust
c         them for the offset when the join was done
c         
        do i = 1, nfWrite
          call xfmult(g(1,1,i),ginv,gtmp)
          gtmp(1,3) = gtmp(1,3) + (1 - gtmp(1,1))* joinOffsetX -
     &        gtmp(1,2) * joinOffsetY
          gtmp(2,3) = gtmp(2,3) - gtmp(2,1) * joinOffsetX +
     &        (1 - gtmp(2,2)) * joinOffsetY
          call xfwrite(2,gtmp,*94)
        enddo
        close(2)
      endif
      close(1)
      call exit(0)
99    call exitError('READING EXISTING TRANSFORM FILE')
94    call exitError('WRITING OUT TRANSFORM FILE')
      end

