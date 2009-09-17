*       * * * * * * XYZPROJ * * * * * *
*       
c       This program will compute projections of a 3-dimensional block of an
c       image file at a series of tilts around either the X, the Y or the Z
c       axis.  The block may be any arbitrary subset of the image file.
c       
c       See man page for details.
c       
c       $Id$
c       Log at end of file
c	
      implicit none
      integer limpix,limstack,limproj,limray
c       
c       9/16/09: Increased stack size increases page faulting and CPU time
      parameter (limstack=40000000,limpix=40000,
     &    limproj=1440,limray=180*40000)
      real*4 array(limstack)
      character*320 filin,filout
      character*1 xyz
      integer*4 nxyzin(3),mxyzin(3),nxyzout(3),nxyzst(3)
      real*4 cell(6),title(20)
      data nxyzst/0,0,0/,cell/0.,0.,0.,90.,90.,90./
      integer*4 nxin,nyin,nzin,nxout,nyout,nzout
      common /nxyz/nxin,nyin,nzin,nxout,nyout,nzout
      equivalence (nxyzin(1),nxin),(nxyzout(1),nxout)
      integer*4 nrayinc(limray),nraymax(0:limproj)
      real*4 xraystr(limray),yraystr(limray),pixtmp(limpix)
      real*4 cosang(0:limproj),sinang(0:limproj),tiltAngles(limproj)
      integer*4 ixBoxLo(limproj), iyBoxLo(limproj), ixBoxHi(limproj)
      integer*4 ix0, ix1,iy0,iy1,iz0,iz1,modein,iyBoxHi(limproj)
      integer*4 nxblock,nyblock,nzblock,idirz,nxslice,nyslice,lenload
      integer*4 ixload0, ixload1, iyload0, iyload1, loadxlo, loadxhi
      integer*4 load0,loaddir,modeout,ifrayscale, loadylo, loadyhi
      real*4 dmin,dmax,dmean,tiltstr,tiltend,tiltinc,scaladd,scalfac
      real*4 fill,dmin2,dmax2,dmean2,dsum,angle,projMiddle
      integer*4 istackdel,limslices,nloads,ioutbase,iproj,ixout
      integer*4 iyoutstr,nslices,load1,invertAng,nview
      integer*4 izsec,indstack,iraybase,nraypts,iray,ipix
      real*4 xray,yray,dx,dy,v2,v4,v5,v6,v8,vmin,vmax,a,b,c,d
      real*4 sumtmp,rayfac,rayadd,tmin,tmax,tmean,tiltmax
      integer*4 ixr,iyr,ixy,ixy4,ixy6,ixy8,ixy2,indout,kti,i,ind,iload,ierr
      integer*4 ixdir, ixostr, ixoend
      logical*4 commonLine, fullImage
      real*4 sind,cosd
c      real*8 readtime,readstart,readdone,walltime,transtime,projtime
      common /bigarr/ array,xraystr,yraystr,nrayinc
c       
c       7/7/00 CER: remove the encode's; titlech is the temp space
c       
      character*80 titlech
      character dat*9,tim*8
c
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetTwoFloats,PipGetFloat,PipNumberOfEntries
      integer*4 PipGetString,PipGetTwoIntegers,PipGetThreeFloats
      integer*4 PipGetInOutFile, PipGetBoolean, PipGetLogical
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  xyzproj
c       
      integer numOptions
      parameter (numOptions = 18)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@output:OutputFile:FN:@'//
     &    'axis:AxisToTiltAround:CH:@xminmax:XMinAndMax:IP:@'//
     &    'yminmax:YMinAndMax:IP:@zminmax:ZMinAndMax:IP:@'//
     &    'angles:StartEndIncAngle:FT:@mode:ModeToOutput:I:@'//
     &    'width:WidthToOutput:I:@addmult:AddThenMultiply:FP:@'//
     &    'fill:FillValue:F:@constant:ConstantScaling:B:@'//
     &    'first:FirstTiltAngle:F:@increment:TiltIncrement:F:@'//
     &    'tiltfile:TiltFile:FN:@tangles:TiltAngles:FAM:@'//
     &    'param:ParameterFile:PF:@help:usage:B:'
c       
      tiltstr = 0.
      tiltend = 0.
      tiltinc = 1.
      scaladd=0.
      scalfac=1. 
      ifrayscale=1
      commonLine = .false.
c      transtime = 0.
c      readtime = 0.
c      projtime = 0.
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'xyzproj',
     &    'ERROR: XYZPROJ - ', .true., 3, 1, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0

      if (PipGetInOutFile('InputFile', 1, 'Name of input file', filin)
     &    .ne. 0) call exiterror('NO INPUT FILE SPECIFIED')
c       
      call imopen(1,filin,'ro')
      call irdhdr(1,nxyzin,mxyzin,modein,dmin,dmax,dmean)
c       
      if (PipGetInOutFile('OutputFile', 2, 'Name of output file', filout)
     &    .ne. 0) call exiterror('NO OUTPUT FILE SPECIFIED')
c       
      ix0=0
      ix1=nxin-1
      iy0=0
      iy1=nyin-1
      iz0=0
      iz1=nzin-1
      if (pipinput) then
        ierr = PipGetTwoIntegers('XMinAndMax', ix0, ix1)
        ierr = PipGetTwoIntegers('YMinAndMax', iy0, iy1)
        ierr = PipGetTwoIntegers('ZMinAndMax', iz0, iz1)
      else
        write(*,'(1x,a,/,a,$)')
     &      'Enter index coordinates of block: ix0,ix1,iy0,iy1,iz0,iz1'
     &      ,' (or / for whole volume): '
        read(*,*)ix0,ix1,iy0,iy1,iz0,iz1
      endif
      ix0=max(0,ix0)
      ix1=min(ix1,nxin-1)
      iy0=max(0,iy0)
      iy1=min(iy1,nyin-1)
      iz0=max(0,min(iz0,nzin-1))
      iz1=max(0,min(iz1,nzin-1))
      if(ix0.gt.ix1.or.iy0.gt.iy1)call exiterror('No volume specified')
c       
      if (pipinput) then
        ierr = PipNumberOfEntries('TiltAngles', ixload0)
        ierr = PipNumberOfEntries('TiltFile', ixload1)
        ierr = PipNumberOfEntries('FirstTiltAngle', iyload0)
        ierr = PipGetLogical('FullAreaAtTilt', fullImage)
        if (ixload0 + ixload1 + iyload0.gt. 0) then
          commonLine = .true.
          nview = nzin
          call get_tilt_angles(nview, 3, tiltAngles, limproj, 1)
          if (nview .ne. nzin) call exitError(
     &        'THERE MUST BE A TILT ANGLE FOR EACH VIEW')
          xyz = 'Z'
        endif
          
        if (PipGetString('AxisToTiltAround', xyz) .ne. 0 .and. .not.commonLine)
     &      call exiterror( 'YOU MUST ENTER AN AXIS TO TILT AROUND')
        if (commonLine .and. xyz .ne. 'Z' .and. xyz .ne. 'z') call exitError(
     &      'YOU CAN ENTER TILT ANGLES ONLY FOR PROJECTIONS AROUND THE Z AXIS')
        ierr = PipGetThreeFloats('StartEndIncAngle', tiltstr,tiltend,tiltinc)
      else
        write(*,'(1x,a,$)')'Axis to project around (enter X, Y or Z): '
        read(*,'(a)')xyz
c       
        write(*,'(1x,a,$)')'Starting, ending, increment tilt angles: '
        read(*,*)tiltstr,tiltend,tiltinc
      endif
      do while (tiltstr.gt.180.)
        tiltstr=tiltstr-360.
      enddo
      do while (tiltstr.le.-180.)
        tiltstr=tiltstr+360.
      enddo
      do while (tiltend.gt.180.)
        tiltend=tiltend-360.
      enddo
      do while (tiltend.le.-180.)
        tiltend=tiltend+360.
      enddo
      if(tiltinc.ge.0.and.tiltend.lt.tiltstr)tiltend=tiltend+360.
      if(tiltinc.lt.0.and.tiltend.gt.tiltstr)tiltend=tiltend-360.
      nzout=1
      if(tiltinc.ne.0.)nzout=(tiltend-tiltstr)/tiltinc + 1
      if(nzout.gt.limproj)call exiterror('TOO MANY PROJECTIONS FOR ARRAYS')
c       
      nxblock=ix1+1-ix0
      nyblock=iy1+1-iy0
      if(iz1.ge.iz0)then
        idirz=1
        nzblock=iz1+1-iz0
      else
        idirz=-1
        nzblock=iz0+1-iz1
      endif
c       
c       set up size of slices within which to project sets of lines,
c       total # of slices, and other parameters for the 3 cases
c       
      invertAng = 1
      if(xyz.eq.'x'.or.xyz.eq.'X')then          !tilt around X
        nxslice=nzblock
        nyslice=nyblock
        nxout = nyblock
        nyout=nxblock
        lenload=nyblock
        load0=ix0
        loaddir=1
      elseif(xyz.eq.'y'.or.xyz.eq.'Y')then      !tilt around Y
        nxslice=nxblock
        nyslice=nzblock
        nxout = nxblock
        nyout=nyblock
        lenload=nxblock
        load0=iy0
        loaddir=1
        invertAng = -1
      elseif(xyz.eq.'z'.or.xyz.eq.'Z')then      !tilt around Z
        nxslice=nxblock
        nyslice=nyblock
        nxout = nxblock
        nyout=nzblock
        lenload=0
        load0=iz0
        loaddir=idirz
      else
        call exiterror('YOU MUST ENTER ONE OF X, Y, Z, x, y, or z FOR AXIS')
      endif
c       
      modeout=1
      if(modein.eq.2)modeout=2
      fill=dmean
      ixload0 = ix0
      ixload1 = ix1
      iyload0 = iy0
      iyload1 = iy1
      if (commonLine) then
        nxout = 0
        ixload0 = nxin
        ixload1 = 0
        iyload0 = nyin
        iyload1 = 0
        tiltmax = 0.;
        do izsec = iz0, iz1
          tiltmax = max(tiltmax, abs(tiltAngles(izsec + 1)))
        enddo
        projMiddle = invertAng * (tiltstr+nzout*tiltinc/2.)
        do iproj=0,nzout-1
          angle=invertAng * (tiltstr+iproj*tiltinc)
          call commonLineBox(ix0, ix1, iy0, iy1, nxin, nyin, angle, tiltmax,
     &        projMiddle, ixBoxLo(iproj), iyBoxLo(iproj), ixBoxHi(iproj),
     &        iyBoxHi(iproj), loadxlo, loadxhi, loadylo, loadyhi, fullImage)
          nxout = max(nxout, ixBoxHi(iproj) + 1 - ixBoxLo(iproj))
          ixload0 = min(ixload0, loadxlo)
          ixload1 = max(ixload1, loadxhi)
          iyload0 = min(iyload0, loadylo)
          iyload1 = max(iyload1, loadyhi)
        enddo
        nxslice = ixload1 + 1 - ixload0
        nyslice = iyload1 + 1 - iyload0
      endif
c
      if (pipinput) then
        ierr = PipGetInteger('WidthToOutput', nxout)
        ierr = PipGetInteger('ModeToOutput', modeout)
        ierr = PipGetTwoFloats('AddThenMultiply', scaladd, scalfac)
        ierr = PipGetFloat('FillValue', fill)
        ixr = 0
        ierr = PipGetBoolean('ConstantScaling', ixr)
        ifrayscale = 1 - ixr
      else
        write(*,'(1x,a,i5,a,$)')'Width of output image [/ for', nxout,']: '
        read(*,*)nxout
c         
        write(*,'(1x,a,i2,a,$)')'Output data mode [/ ',modeout, ']: '
        read(*,*)modeout
c         
c         write(*,'(1x,a,$)')'0 to scale by 1/(vertical thickness),'//
c         &	    ' or 1 to scale by 1/(ray length): '
c         read(*,*)ifrayscale
c         
        write(*,'(1x,a,$)') 'Additional scaling factors to add '//
     &      'then multiply by [/ for 0,1]: '
        read(*,*)scaladd,scalfac
c         
        write(*,'(1x,a,/,a,f10.2,a,$)')
     &      'Value to fill parts of output not projected to',
     &    '   (before scaling, if any) [/ for mean=',dmean,']: '
        read(*,*)fill
      endif

      fill=(fill+scaladd)*scalfac
c       
c       set up output file and header
c       
c       
      call imopen(2,filout,'new')
      call itrhdr(2,1)
      call ialmod(2,modeout)
      call ialsiz(2,nxyzout,nxyzst)
      call ialsam(2,nxyzst)
      do i=1,3
        cell(i)=nxyzout(i)
      enddo
      call ialcel(2,cell)
      call time(tim)
      call date(dat)
c       
      write(titlech,301) ix0,ix1,iy0,iy1,iz0,iz1,xyz,dat,tim
      read(titlech,'(20a4)')(title(kti),kti=1,20)
301   format('XYZPROJ: x',2i5,', y',2i5,', z ',2i5,' about ',a1,t57,a9,2x,a8)
      dmax2=-1.e20
      dmin2=1.e20
      dsum=0.
c       
c       set up stack loading with slices
c       
      istackdel=nxslice*nyslice
      limslices=min(limpix,limstack/(istackdel + max(nxout,lenload)))
      if (commonLine) limslices = 1
      if(limslices.lt.1)call exiterror('IMAGES TOO LARGE FOR STACK')
      if(nxout*nzout.gt.limray)call exiterror(
     &    'TOO MANY PROJECTIONS FOR OUTPUT THIS WIDE')
      nloads=(nyout+limslices-1)/limslices
      ioutbase=1+limslices*istackdel
c       
c       analyse the number of points on each ray in each projection
c       
      do iproj=0,nzout-1
        angle=invertAng * (tiltstr+iproj*tiltinc)
        sinang(iproj)=sind(angle)
        cosang(iproj)=cosd(angle)
        iraybase=iproj*nxout
        if (.not.commonLine) call set_projection_rays(sinang(iproj),
     &      cosang(iproj), nxslice, nyslice, nxout, xraystr(irayBase+1),
     &      yraystr(irayBase+1), nrayinc(irayBase+1), nraymax(iproj))
      enddo
c       
c       loop on loads of several to many slices at once
c       
      iyoutstr=0				!starting y output line
      do iload=1,nloads
        nslices=min(limslices,nyout-iyoutstr)
        load1=load0+loaddir*(nslices-1)
c         
c         load the slices one of 3 different ways: for X, get vertical
c         rectangles of sections in input file, transpose into slice array
c         
        if(xyz.eq.'x'.or.xyz.eq.'X')then
          do izsec=iz0,iz1,idirz
            call imposn(1,izsec,0)
            call irdpas(1,array(ioutbase),nslices,nyblock,load0,
     &          load1,iy0,iy1,*99)
            call xtransp(array(ioutbase),idirz*(izsec-iz0)+1,
     &          array, nxslice,nyslice,nslices) 
          enddo
c           
c           for Y, get horizontal rectangles of sections in input file,
c           transpose differently into slice array
c           
        elseif(xyz.eq.'y'.or.xyz.eq.'Y')then
          do izsec=iz0,iz1,idirz
c            readstart = walltime()
            call imposn(1,izsec,0)
            call irdpas(1,array(ioutbase),nxblock,nslices,ix0,ix1,
     &          load0, load1,*99)
c            readdone = walltime()
c            readtime = readtime + readdone - readstart
            call ytransp(array(ioutbase),idirz*(izsec-iz0)+1,
     &          array, nxslice,nyslice,nslices) 
c            transtime =  transtime +  walltime() - readdone
          enddo
c           
c           for Z, just get slices directly from sections in file
c           
        else
          indstack=1
          do izsec=load0,load1,loaddir
            call imposn(1,izsec,0)
            call irdpas(1,array(indstack),nxslice,nyslice,ixload0,ixload1,
     &          iyload0,iyload1,*99)
            indstack=indstack+istackdel
          enddo
        endif
        load0=load0+loaddir*nslices
c         
c         loop on different projection views
c         
c        readdone = walltime()
        do iproj=0,nzout-1
          iraybase=iproj*nxout
          if (commonLine) then
            angle=invertAng * (tiltstr+iproj*tiltinc)
            call commonLineRays(ix0, ix1, iy0, iy1, nxin, nyin, ixload0,
     &          iyload0, nxout, angle, tiltAngles(load0+1), ixBoxLo(iproj),
     &          iyBoxLo(iproj), ixBoxHi(iproj), iyBoxHi(iproj),
     &          xraystr(iraybase+1), yraystr(iraybase+1),nrayinc(iraybase+1),
     &          nxslice, nyslice, fullImage)
            nraymax(iproj) = nrayinc(iraybase+1)
          endif
c           
c           Set x-independent part of scaling
          if(ifrayscale.eq.0)then
            rayfac=scalfac/nyslice
          else
            rayfac=scalfac/nraymax(iproj)
          endif
c           
c           set the output lines for this view to the fill value
          rayadd = fill
          if(ifrayscale.eq.0) rayadd = (fill * nraymax(iproj)) / nyslice
          do ind=ioutbase,ioutbase+nslices*nxout-1
            array(ind)=rayadd
          enddo
c           
c           For projections at an angle, process multiple slices at once
          if (sinang(iproj) .ne. 0.)then
c             
c             loop on pixels along line
            do ixout=1,nxout
              nraypts=nrayinc(ixout+iraybase)
c               print *,nraypts
              if(nraypts.gt.0)then
c                 
c                 if block along ray, clear temporary array for output pixels
                do ipix=1,nslices
                  pixtmp(ipix)=0.
                enddo
c               
c                 move along ray, computing at each point indexes and factors
c                 for quadratic interpolation
                do iray=0,nraypts-1
                  xray=xraystr(ixout+iraybase)+iray*sinang(iproj)
                  yray=yraystr(ixout+iraybase)+iray*cosang(iproj)
                  ixr=nint(xray)
                  iyr=nint(yray)
                  dx=xray-ixr
                  dy=yray-iyr
                  ixy=ixr+(iyr-1)*nxslice
                  ixy4=ixy-1
                  ixy6=ixy+1
                  ixy8=ixy+nxslice
                  ixy2=ixy-nxslice
c                   
c                   loop through pixels in different slices, do quadratic
c                   interpolation limited by values of surrounding pixels
                  do ipix=1,nslices
                    v2=array(ixy2)
                    v4=array(ixy4)
                    v5=array(ixy)
                    v6=array(ixy6)
                    v8=array(ixy8)
                    vmax=max(v2,v4,v5,v6,v8)
                    vmin=min(v2,v4,v5,v6,v8)
C                     
                    A = (V6 + V4)*.5 - V5
                    B = (V8 + V2)*.5 - V5
                    C = (V6 - V4)*.5
                    D = (V8 - V2)*.5
C                     
                    pixtmp(ipix) = pixtmp(ipix)+ max(vmin,min(vmax,
     &                  (A*DX*DX + B*DY*DY + C*DX + D*DY + V5)))
c                     
                    ixy=ixy+istackdel		!increment subscripts for
                    ixy2=ixy2+istackdel         !next slice
                    ixy4=ixy4+istackdel
                    ixy6=ixy6+istackdel
                    ixy8=ixy8+istackdel
                  enddo
                enddo
c               
c                 set up scaling and put pixels out in different lines
                rayadd=scaladd*scalfac + rayfac * (nraymax(iproj) - nraypts) *
     &              (fill / scalfac - scaladd)
c                   
                indout=ixout+ioutbase-1
                do ipix=1,nslices
                  array(indout)=rayfac*pixtmp(ipix)+rayadd
                  indout=indout+nxout
                enddo
              endif
            enddo

          else
c                 
c             simple case of straight projection
c             find limits of x to loop on
            ixostr = 0
            do ixout = 1, nxout
              if (ixostr .eq. 0 .and. nrayinc(ixout+iraybase) .gt. 0)
     &            ixostr = ixout
              if (nrayinc(ixout+iraybase) .gt. 0) ixoend = ixout
            enddo
c             
c             Get starting positions in X, Y, and directions
            ixdir = 1
            ixr=nint(xraystr(ixostr+iraybase))
            if (xraystr(ixoend+iraybase) .lt. ixr) ixdir = -1
            iyr=nint(yraystr(ixostr+iraybase))
            ixy8 = sign(1., cosang(iproj))
            nraypts = nrayinc(ixostr+iraybase)
            rayadd=scaladd*scalfac + rayfac * (nraymax(iproj) - nraypts) *
     &          (fill / scalfac - scaladd)
c             
c             loop on slices; clear the array segment for the slice
            do ipix=1,nslices
              indout = nxout * (ipix - 1) + ioutbase - 1
              do ixout = ixostr,ixoend
                array(indout + ixout) = 0.
              enddo
              
c               Loop on levels in Y (the ray points) and set starting index
              do iray=0,nraypts-1
                ixy=ixr+(iyr-1)*nxslice + iray*nxslice*ixy8 +
     &              (ipix-1)*istackdel
c                 
c                 Add line into array
                do ixout = ixostr,ixoend
                  array(indout + ixout) = array(indout + ixout) + array(ixy)
                  ixy = ixy + ixdir
                enddo
              enddo
c               
c                 Scale the data
              do ixout = ixostr,ixoend
                array(indout+ixout) = array(indout+ixout) * rayfac + rayadd
              enddo
            enddo
          endif
c           
c           get min, max, mean; output the lines to proper section
c           
          call iclden(array(ioutbase),nxout,nslices,1,nxout,1,
     &        nslices,tmin,tmax,tmean)
          dmin2=(min(dmin2,tmin))
          dmax2=(max(dmax2,tmax))
          dsum=dsum+tmean*nslices*nxout
          call imposn(2,iproj,iyoutstr)
          call iwrsecl(2,array(ioutbase),nslices)
        enddo
        iyoutstr=iyoutstr+nslices
c        projtime = projtime + walltime() - readdone
      enddo
c       
c       finish up file header
c       
      dmean2=dsum/(nxout*nyout*nzout)
      call iwrhdr(2,title,1,dmin2,dmax2,dmean2)
      call imclose(2)
      call imclose (1)
c      print *,'read time',readtime, '  transpose time',transtime,'
c       project time',projtime
      call exit(0)
99    call exiterror('READING FILE')
      end
c       
c       
c       Subroutines to transpose the pieces of sections read from input file
c       into the array of slices
c       
      subroutine xtransp(array,icolm,brray,nxsl,nysl,nsl)
      real*4 array(nsl,nysl),brray(nxsl,nysl,nsl)
      do iy=1,nysl
        do ix=1,nsl
          brray(icolm,iy,ix)=array(ix,iy)
        enddo
      enddo
      return
      end
      
      subroutine ytransp(array,irow,brray,nxsl,nysl,nsl)
      real*4 array(nxsl,nsl),brray(nxsl,nysl,nsl)
      do iy=1,nsl
        do ix=1,nxsl
          brray(ix,irow,iy)=array(ix,iy)
        enddo
      enddo
      return
      end

c       Compute the needed box for common line projections that fits within
c       the given area and within the image at the given projection angle
c       and when tilt-foreshortened at the maximum tilt angle.  Return the
c       coordinates defining the limits of the projection box in rotated
c       space, and the coordinates that need loading
c
      subroutine commonLineBox(ix0, ix1, iy0, iy1, nxin, nyin, projAng,
     &    tiltmax, projMiddle, ixlo, iylo, ixhi, iyhi, loadxlo, loadxhi,
     &    loadylo, loadyhi, fullImage)
      implicit none
      integer*4 ix0, ix1, iy0, iy1, nxin, nyin
      integer*4 ixlo, iylo, ixhi, iyhi, loadxlo, loadxhi, loadylo, loadyhi
      logical*4 fullImage
      real*4 projAng, tiltmax, projMiddle
c       
      real*4 cosrot, sinrot, xcen, ycen, x0, y0, x1, y1, xstep, ystep
      real*4 cosang, sinang
      integer*4 maxSteps, istep
      real*4 xll, xul, xlr, xur, yll, yul, ylr, yur, rotang,axisy,ylotfs,yhitfs
      real*4 xllr, xulr, xlrr, xurr, yllr, yulr, ylrr, yurr
      real*4 xllt, xult, xlrt, xurt, yllt, yult, ylrt, yurt
      real*4 cosd, sind
c       
c       Determine rotation of area so that it is within 45 deg, so that the
c       box that is projected is oriented as closely as possible to the
c       specified box at the middle angle
      rotang = projAng
      cosang = projMiddle
      do while (abs(cosang) .gt. 45.01)
        rotang = rotang - sign(90., cosang)
        cosang = cosang - sign(90., cosang)
      enddo
c      print *,'Proj angle', projAng,'  Reduced angle',rotang
c
c       Incoming and outgoing coordinates are all numbered from 0
      cosrot = cosd(rotang)
      sinrot = -sind(rotang)
      cosang = cosd(projAng)
      sinang = sind(projAng)
      xcen = (ix1 + ix0) / 2.
      ycen = (iy1 + iy0) / 2.
c       
c       Set up to loop on area, stepping size down symmetrically until rotated
c       box fits within the image
      x0 = ix0
      y0 = iy0
      x1 = ix1
      y1 = iy1
      maxSteps = max(ix1 + 1 - ix0, iy1 + 1 - iy0) / 2
      xstep = (ix1 + 1 - ix0) / maxSteps
      ystep = (iy1 + 1 - iy0) / maxSteps
      do istep = 1, maxsteps
c         
c         Rotate the box by negative of angle and see if it fits
        call rotatePoint(cosrot, sinrot, x0, y0, xll, yll)
        call rotatePoint(cosrot, sinrot, x0, y1, xul, yul)
        call rotatePoint(cosrot, sinrot, x1, y0, xlr, ylr)
        call rotatePoint(cosrot, sinrot, x1, y1, xur, yur)
c       
c       Back-rotate these boundaries by the original, unconstrained angle
c       and get the coordinates defining limits of projection box
        call rotatePoint(cosang, sinang, xll, yll, xllr, yllr)
        call rotatePoint(cosang, sinang, xul, yul, xulr, yulr)
        call rotatePoint(cosang, sinang, xlr, ylr, xlrr, ylrr)
        call rotatePoint(cosang, sinang, xur, yur, xurr, yurr)
        ixlo = ceiling(min(xllr, xulr, xlrr, xurr))
        ixhi = floor(max(xllr, xulr, xlrr, xurr))
        iylo = ceiling(min(yllr, yulr, ylrr, yurr))
        iyhi = floor(max(yllr, yulr, ylrr, yurr))
c       
c         tilt-foreshorten the y limits of projection box and recompute corners
        axisy = -(nxin / 2. - xcen) * sinang + (nyin / 2. - ycen) * cosang +
     &      ycen
        if (fullImage) then
          ylotfs = iylo
          yhitfs = iyhi
        else
          ylotfs = nint((iylo - axisy) * cosd(tiltmax) + axisy)
          yhitfs = nint((iyhi - axisy) * cosd(tiltmax) + axisy)
        endif
        call rotatePoint(cosang, -sinang, float(ixlo), ylotfs, xllt, yllt)
        call rotatePoint(cosang, -sinang, float(ixlo), yhitfs, xult, yult)
        call rotatePoint(cosang, -sinang, float(ixhi), ylotfs, xlrt, ylrt)
        call rotatePoint(cosang, -sinang, float(ixhi), yhitfs, xurt, yurt)
c        
c         Get limits of the box needing loading - allow for quadratic
c         interpolation with the margins here, no extra margin is needed
        loadxlo = floor(min(xll, xul, xlr, xur, xllt, xult, xlrt, xurt)) - 2
        loadxhi = ceiling(max(xll, xul, xlr, xur, xllt, xult, xlrt, xurt)) + 2
        loadylo = floor(min(yll, yul, ylr, yur, yllt, yult, ylrt, yurt)) - 2
        loadyhi = ceiling(max(yll, yul, ylr, yur, yllt, yult, ylrt, yurt)) + 2
c         
c         Test and see if this fits; if not reduce box
        if (loadxlo .ge. 0 .and. loadxhi .lt. nxin .and.
     &      loadylo .ge. 0 .and. loadyhi .lt. nyin) exit
        x0 = x0 + xstep
        x1 = x1 - xstep
        y0 = y0 + ystep
        y1 = y1 - ystep
        if (istep .gt. maxsteps - 10) call exitError(
     &      'CANNOT FIND ROTATED BOX THAT FITS WITHIN IMAGE')
      enddo
c      print *,istep,' steps, new box:',x0,x1,y0,y1       
c      print *,'Projection box limits:',ixlo,ixhi,iylo,iyhi
c      print *,'load limits:', loadxlo, loadxhi, loadylo, loadyhi
      return 

      contains
      subroutine rotatePoint(cosa, sina, xin, yin, xout, yout)
      implicit none
      real*4 xin, yin, xout, yout, cosa, sina
      xout = (xin - xcen) * cosa - (yin - ycen) * sina + xcen
      yout = (xin - xcen) * sina + (yin - ycen) * cosa + ycen
      end subroutine rotatePoint
      
      end subroutine commonLineBox


c       Compute the ray parameters for a common line projection given the
c       original coordinate limits, the start of loaded data, and coordinates
c       defining the projection box computed before
c
      subroutine commonLineRays(ix0, ix1, iy0, iy1, nxin, nyin, loadXst,
     &    loadYst, nxout, projAng, tilt, inIxlo, iylo, inIxhi, iyhi, xraystr,
     &    yraystr, nrayinc, nxslice, nyslice, fullImage)
      implicit none
      integer*4 ix0, ix1, iy0, iy1, nxin, nyin, loadXst, loadYst, nxout
      integer*4 inIxlo, iylo, inIxhi, iyhi, nrayinc(*)
      real*4 projAng, tilt, xraystr(*), yraystr(*)
      integer*4 nxslice, nyslice
      logical*4 fullImage
c       
      integer*4 iylotfs, iyhitfs, ix, i, ixlo, ixhi
      real*4 cosrot, sinrot, xcen, ycen, axisy,xend, yend
      real*4 cosd, sind
c
      cosrot = cosd(projAng)
      sinrot = -sind(projAng)
      xcen = (ix1 + ix0) / 2.
      ycen = (iy1 + iy0) / 2.
c       
c       Trim the X limits if they are bigger than nxout
      ixlo = inIxlo
      ixhi = inIxhi
      if (ixhi + 1 - ixlo .gt. nxout) then
        ix = ixhi + 1 - ixlo - nxout
        ixlo = ixlo + ix / 2
        ixhi = ixhi - (ix - ix / 2)
      endif
c       
c       Rotate the center of the image to determine the Y value of tilt axis
c       then tilt-foreshorten the lower and upper Y limits
      axisy = (nxin / 2. - xcen) * sinrot + (nyin / 2. - ycen) * cosrot + ycen
      if (fullImage) then
        iylotfs = iylo
        iyhitfs = iyhi
      else
        iylotfs = nint((iylo - axisy) * cosd(tilt) + axisy)
        iyhitfs = nint((iyhi - axisy) * cosd(tilt) + axisy)
      endif
c       
c       Rotate the bottom line of this box to get ray starts, adjust by load
c       Also, incoming coordinates are numbered from 0 but we need array index
c       coordinates numbered from 1, so add 1 at this stage
      do ix = ixlo, ixhi
        i = ix + 1 - ixlo
        xraystr(i) = (ix - xcen) * cosrot - (iylotfs - ycen) * sinrot + xcen +
     &      1. - loadXst
        yraystr(i) = (ix - xcen) * sinrot + (iylotfs - ycen) * cosrot + ycen +
     &      1. - loadYst
        nrayinc(i) = iyhitfs + 1 - iylotfs
c$$$        xend = xraystr(i) - (nrayinc(i)-1)*sinrot
c$$$        yend = yraystr(i) + (nrayinc(i)-1)*cosrot
c$$$        if (xraystr(i) .lt. 1.5 .or. yraystr(i) .lt. 1.5 .or. xraystr(i) .gt.
c$$$     &      nxslice - 0.5 .or.yraystr(i) .gt. nyslice - 0.5) print *,'OOPS',
c$$$     &      projang, tilt, axisy, ix, xraystr(i),yraystr(i),iylo,iylotfs,
c$$$     &      iyhi,iyhitfs
c$$$        if (xend .lt. 1.5 .or. yend .lt. 1.5 .or. xend .gt.
c$$$     &      nxslice - 0.5 .or.yend .gt. nyslice - 0.5) print *,'OOPSend',
c$$$     &      projang, tilt, axisy, ix, xend,yend,iylo,iylotfs,iyhi,iyhitfs
      enddo
      do i = ixhi + 1 - ixlo, nxout
        nrayinc(i) = 0
      enddo
      return
      end

c       
c       $Log$
c       Revision 3.7  2007/07/15 20:57:03  mast
c       Fixed edge fill with the constant scaling option
c
c       Revision 3.6  2007/04/26 19:16:01  mast
c       Added option to give constant scaling
c
c       Revision 3.5  2006/06/20 04:52:09  mast
c       Converted to PIP, changed for changes in set_projection_rays
c
c       Revision 3.4  2006/05/17 00:09:15  mast
c       Fixed scaling to avoid excessive amplification near edges; it now
c       scales as if the block is padded with fill data on the sides.
c       Added proper error exits.
c
c       Revision 3.3  2005/12/09 04:45:32  mast
c       gfortran: .xor., continuation, or byte fixes
c	
c       Revision 3.2  2002/02/26 22:34:47  mast
c       Had to decrease limray size to avoid exceeding stack limit on SGI; it
c       only needs to be big enough to allow lots of images or very big images
c	
c       Revision 3.1  2002/01/28 16:14:22  mast
c       Increased limit on number of projections and added check on limit.
c       Added declarations for implicit none.
