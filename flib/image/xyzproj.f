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
      parameter (limstack=20000000,limpix=40000,
     &    limproj=1440,limray=180*40000)
      real*4 array(limstack)
      character*160 filin,filout
      character*1 xyz
      integer*4 nxyzin(3),mxyzin(3),nxyzout(3),nxyzst(3)
      real*4 cell(6),title(20)
      data nxyzst/0,0,0/,cell/0.,0.,0.,90.,90.,90./
      integer*4 nxin,nyin,nzin,nxout,nyout,nzout
      common /nxyz/nxin,nyin,nzin,nxout,nyout,nzout
      equivalence (nxyzin(1),nxin),(nxyzout(1),nxout)
      integer*4 nrayinc(limray),nraymax(0:limproj)
      real*4 xraystr(limray),yraystr(limray),pixtmp(limpix)
      real*4 cosang(0:limproj),sinang(0:limproj)
      integer*4 ix0, ix1,iy0,iy1,iz0,iz1,modein
      integer*4 nxblock,nyblock,nzblock,idirz,nxslice,nyslice,lenload
      integer*4 load0,loaddir,modeout,ifrayscale
      real*4 dmin,dmax,dmean,tiltstr,tiltend,tiltinc,scaladd,scalfac
      real*4 fill,dmin2,dmax2,dmean2,dsum,angle
      integer*4 istackdel,limslices,nloads,ioutbase,iproj,ixout
      integer*4 iyoutstr,nslices,load1,invertAng
      integer*4 izsec,indstack,iraybase,nraypts,iray,ipix
      real*4 xray,yray,dx,dy,v2,v4,v5,v6,v8,vmin,vmax,a,b,c,d
      real*4 sumtmp,rayfac,rayadd,tmin,tmax,tmean
      integer*4 ixr,iyr,ixy,ixy4,ixy6,ixy8,ixy2,indout,kti,i,ind,iload,ierr
      real*4 sind,cosd
      common /bigarr/ array,xraystr,yraystr,nrayinc
c       
c       7/7/00 CER: remove the encode's; titlech is the temp space
c       
      character*80 titlech
      character dat*9,tim*8
c
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetTwoFloats,PipGetFloat
      integer*4 PipGetString,PipGetTwoIntegers,PipGetThreeFloats
      integer*4 PipGetInOutFile, PipGetBoolean
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  xyzproj
c       
      integer numOptions
      parameter (numOptions = 14)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@output:OutputFile:FN:@'//
     &    'axis:AxisToTiltAround:CH:@xminmax:XMinAndMax:IP:@'//
     &    'yminmax:YMinAndMax:IP:@zminmax:ZMinAndMax:IP:@'//
     &    'angles:StartEndIncAngle:FT:@mode:ModeToOutput:I:@'//
     &    'width:WidthToOutput:I:@addmult:AddThenMultiply:FP:@'//
     &    'fill:FillValue:F:@constant:ConstantScaling:B:@'//
     &    'param:ParameterFile:PF:@help:usage:B:'
c       
      tiltstr = 0.
      tiltend = 0.
      tiltinc = 1.
      scaladd=0.
      scalfac=1. 
      ifrayscale=1
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
        if (PipGetString('AxisToTiltAround', xyz) .ne. 0) call exiterror(
     &      'YOU MUST ENTER AN AXIS TO TILT AROUND')
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
        write(*,'(1x,a,i5,a,$)')'Width of output image [/ for',
     &      nxout,']: '
        read(*,*)nxout
c         
        write(*,'(1x,a,i2,a,$)')'Output data mode [/ ',modeout,
     &      ']: '
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
c       7/7/00 CER: remove the encodes
c       
C       encode(80,301,title)ix0,ix1,iy0,iy1,iz0,iz1,xyz
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
        call set_projection_rays(sinang(iproj), cosang(iproj), nxslice,
     &      nyslice, nxout, xraystr(irayBase+1), yraystr(irayBase+1),
     &      nrayinc(irayBase+1), nraymax(iproj))
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
            call imposn(1,izsec,0)
            call irdpas(1,array(ioutbase),nxblock,nslices,ix0,ix1,
     &          load0, load1,*99)
            call ytransp(array(ioutbase),idirz*(izsec-iz0)+1,
     &          array, nxslice,nyslice,nslices) 
          enddo
c           
c           for Z, just get slices directly from sections in file
c           
        else
          indstack=1
          do izsec=load0,load1,loaddir
            call imposn(1,izsec,0)
            call irdpas(1,array(indstack),nxslice,nyslice,ix0,ix1,
     &          iy0,iy1,*99)
            indstack=indstack+istackdel
          enddo
        endif
        load0=load0+loaddir*nslices
c         
c         loop on different projection views
c         
        do iproj=0,nzout-1
          iraybase=iproj*nxout
c           
c           set the output lines for this view to the fill value
c           
          rayadd = fill
          if(ifrayscale.eq.0) rayadd = (fill * nraymax(iproj)) / nyslice
          do ind=ioutbase,ioutbase+nslices*nxout-1
            array(ind)=rayadd
          enddo
c           
c           loop on pixels along line
c           
          do ixout=1,nxout
            nraypts=nrayinc(ixout+iraybase)
c	      print *,nraypts
            if(nraypts.gt.0)then
c               
c               if block along ray, clear temporary array for output pixels
c               
              do ipix=1,nslices
                pixtmp(ipix)=0.
              enddo
c               
c               move along ray, computing at each point indexes and factors
c               for quadratic interpolation
c               
              if (sinang(iproj) .ne. 0.)then
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
c                   
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
              else
c                 
c                 simple case of straight projection
c                 
                ixr=nint(xraystr(ixout+iraybase))
                iyr=nint(yraystr(ixout+iraybase))
                ixy8 = sign(1., cosang(iproj))
                do ipix=1,nslices
                  ixy=ixr+(iyr-1)*nxslice
                  sumtmp=0.
                  do iray=0,nraypts-1
                    sumtmp=sumtmp+array(ixy)
                    ixy=ixy+nxslice*ixy8
                  enddo
                  ixr=ixr+istackdel
                  pixtmp(ipix)=sumtmp
                enddo
              endif
c               
c               set up scaling and put pixels out in different lines
c               
              if(ifrayscale.eq.0)then
                rayfac=scalfac/nyslice
              else
                rayfac=scalfac/nraymax(iproj)
              endif
              rayadd=scaladd*scalfac + rayfac * (nraymax(iproj) - nraypts) *
     &            (fill / scalfac - scaladd)
c               
              indout=ixout+ioutbase-1
              do ipix=1,nslices
                array(indout)=rayfac*pixtmp(ipix)+rayadd
                indout=indout+nxout
              enddo
            endif
          enddo
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
      enddo
c       
c       finish up file header
c       
      dmean2=dsum/(nxout*nyout*nzout)
      call iwrhdr(2,title,1,dmin2,dmax2,dmean2)
      call imclose(2)
      call imclose (1)
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

c       
c       $Log$
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
