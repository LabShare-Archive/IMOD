* * * * * * * XYZPROJ * * * * * *
*	  
c	  This program will compute projections of a 3-dimensional block of an
c	  image file at a series of tilts around either the X, the Y or the Z
c	  axis.  The block may be any arbitrary subset of the image file.
c	  
c	  A projection along a ray line is simply the average of the pixels in
c	  the block along that line.  However, rather than taking the values of
c	  the pixels that lie near the ray, interpolation is used to sample
c	  density at points evenly spaced at one pixel intervals along the ray.
c	  
c	  Inputs to the program are:
c
c	  Input image file
c	  
c	  Output image file
c	  
c	  Index coordinates (with numbering starting from 0 in X, Y and Y) of
c	  the starting and ending X, Y then Z coordinates of the block.  The
c	  default is the whole image file.  The Z coordinates may be entered
c	  in inverted order (e.g. 56,34) and should be so entered if the input
c	  stack is a tomogram that was built in inverted order.
c
c	  Axis to tilt around for projections.  Enter X, Y or Z.  The Z axis
c	  passes perpendicular to the sections in the file.
c	  
c	  Starting tilt angle, ending tilt angle, and increment to apply
c	  between these limits.  All angles are allowed.
c	  
c	  Width (x dimension) of the output images.  An appropriate default may
c	  be selected with /
c	  
c	  Data mode for output file.
c	  
c	  Scaling factors to apply to the average pixel values: a factor to be
c	  added to the values, then a factor to multiply by after the addition.
c	  The default is 0,1, for no scaling.
c	  
c	  Value to fill parts of the output image that have no points in the
c	  block projecting to them  The scaling factors are applied to this
c	  value.  The default is the mean of the input file.
c	  
c	  
c	  The user must determine the proper scaling in order to output data
c	  most efficiently (mode 0).  The user must also set the header
c	  information properly to make get the coordinate system to correspond
c	  to that of the input image file.  This program does not set up the
c	  header to indicate that the output file is a tilt series.

c	  David Mastronarde  10/13/89
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.1  2002/01/28 16:14:22  mast
c	  Increased limit on number of projections and added check on limit.
c	  Added declarations for implicit none.
c	
	implicit none
	integer limpix,limstack,limproj,limray
	parameter (limstack=5000000,limpix=10000,
     &	    limproj=720,limray=180*10000)
	real*4 array(limstack)
	character*80 filin,filout
	character*1 xyz
	integer*4 nxyzin(3),mxyzin(3),nxyzout(3),nxyzst(3)
	real*4 cell(6),title(20)
	data nxyzst/0,0,0/,cell/0.,0.,0.,90.,90.,90./
	integer*4 nxin,nyin,nzin,nxout,nyout,nzout
	common /nxyz/nxin,nyin,nzin,nxout,nyout,nzout
	equivalence (nxyzin(1),nxin),(nxyzout(1),nxout)
	integer*2 nrayinc(limray)
	real*4 xraystr(limray),yraystr(limray),pixtmp(limpix)
	real*4 cosang(0:limproj),sinang(0:limproj),xgood(4),ygood(4)
	integer*4 ix0, ix1,iy0,iy1,iz0,iz1,modein
	integer*4 nxblock,nyblock,nzblock,idirz,nxslice,nyslice,lenload
	integer*4 load0,loaddir,modeout,ifrayscale
	real*4 dmin,dmax,dmean,tiltstr,tiltend,tiltinc,scaladd,scalfac
	real*4 fill,dmin2,dmax2,dmean2,dsum,angle,xraytmp,yraytmp
	integer*4 istackdel,limslices,nloads,ioutbase,iproj,ixout
	integer*4 nraytmp,ngoodinter,indgood,iyoutstr,nslices,load1
	integer*4 izsec,indstack,iraybase,nraypts,iray,ipix
	real*4 tanang,rayintcp,xleft,xright,ybot,ytop,yleft,yright,xtop
	real*4 xbot,xray,yray,dx,dy,v2,v4,v5,v6,v8,vmin,vmax,a,b,c,d
	real*4 sumtmp,rayfac,rayadd,tmin,tmax,tmean
	integer*4 ixr,iyr,ixy,ixy4,ixy6,ixy8,ixy2,indout,kti,i,ind,iload
	real*4 sind,cosd
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech
c
	write(*,'(1x,a,$)')'Input image file: '
	read(*,'(a)')filin
c
	call imopen(1,filin,'ro')
	call irdhdr(1,nxyzin,mxyzin,modein,dmin,dmax,dmean)
c
	write(*,'(1x,a,$)')'Output image file: '
	read(*,'(a)')filout
c	  
	ix0=0
	ix1=nxin-1
	iy0=0
	iy1=nyin-1
	iz0=0
	iz1=nzin-1
	write(*,'(1x,a,/,a,$)')
     &	    'Enter index coordinates of block: ix0,ix1,iy0,iy1,iz0,iz1'
     &	    ,' (or / for whole volume): '
	read(*,*)ix0,ix1,iy0,iy1,iz0,iz1
	ix0=max(0,ix0)
	ix1=min(ix1,nxin-1)
	iy0=max(0,iy0)
	iy1=min(iy1,nyin-1)
	iz0=max(0,min(iz0,nzin-1))
	iz1=max(0,min(iz1,nzin-1))
	if(ix0.gt.ix1.or.iy0.gt.iy1)then
	  print *,'XYZPROJ: No volume selected'
	  call exit(1)
	endif
c	  
	write(*,'(1x,a,$)')'Axis to project around (enter X, Y or Z): '
	read(*,'(a)')xyz
c	  
	write(*,'(1x,a,$)')'Starting, ending, increment tilt angles: '
	read(*,*)tiltstr,tiltend,tiltinc
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
	if(nzout.gt.limproj)then
	  print *,'XYZPROJ: TOO MANY PROJECTIONS FOR ARRAYS'
	  call exit(1)
	endif
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
c	  set up size of slices within which to project sets of lines,
c	  total # of slices, and other parameters for the 3 cases
c
	if(xyz.eq.'x'.or.xyz.eq.'X')then			!tilt around X
	  nxslice=nzblock
	  nyslice=nyblock
	  nyout=nxblock
	  lenload=nyblock
	  load0=ix0
	  loaddir=1
	elseif(xyz.eq.'y'.or.xyz.eq.'Y')then			!tilt around Y
	  nxslice=nxblock
	  nyslice=nzblock
	  nyout=nyblock
	  lenload=nxblock
	  load0=iy0
	  loaddir=1
	else					!tilt around Z
	  nxslice=nxblock
	  nyslice=nyblock
	  nyout=nzblock
	  lenload=0
	  load0=iz0
	  loaddir=idirz
	endif
c	    
	nxout=nxslice
	write(*,'(1x,a,i5,a,$)')'Width of output image [/ for',
     &	    nxout,']: '
	read(*,*)nxout
c	  
	modeout=1
	if(modein.eq.2)modeout=2
	write(*,'(1x,a,i2,a,$)')'Output data mode [/ ',modeout,
     &	    ']: '
	read(*,*)modeout
c	  
	ifrayscale=1
c	write(*,'(1x,a,$)')'0 to scale by 1/(vertical thickness),'//
c    &	    ' or 1 to scale by 1/(ray length): '
c	read(*,*)ifrayscale
c	  
	scaladd=0.
	scalfac=1. 
	write(*,'(1x,a,$)') 'Additional scaling factors to add '//
     &	    'then multiply by [/ for 0,1]: '
	read(*,*)scaladd,scalfac
c	  
	fill=dmean
	write(*,'(1x,a,/,a,f10.2,a,$)')
     &	    'Value to fill parts of output not projected to',
     &	    '   (before scaling, if any) [/ for mean=',dmean,']: '
	read(*,*)fill
	fill=(fill+scaladd)*scalfac
c	  
c	  set up output file and header
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
c
c 7/7/00 CER: remove the encodes
c
C       encode(80,301,title)ix0,ix1,iy0,iy1,iz0,iz1,xyz
        write(titlech,301) ix0,ix1,iy0,iy1,iz0,iz1,xyz
        read(titlech,'(20a4)')(title(kti),kti=1,20)
301	format('Projection of block x:',2i4,'; y:'
     &	    ,2i4,'; z: ',2i4,' tilted around ',a1)
	dmax2=-1.e20
	dmin2=1.e20
	dsum=0.
c
c	  set up stack loading with slices
c
	istackdel=nxslice*nyslice
	limslices=min(limpix,limstack/(istackdel + max(nxout,lenload)))
	if(limslices.lt.1)then
	  print *, 'XYZPROG: IMAGES TOO LARGE FOR STACK'
	  call exit(1)
	endif
	if(nxout*nzout.gt.limray)then
	  print *, 'XYZPROJ: TOO MANY PROJECTIONS FOR OUTPUT THIS WIDE'
	  call exit(1)
	endif
	nloads=(nyout+limslices-1)/limslices
	ioutbase=1+limslices*istackdel
c
c	  analyse the number of points on each ray in each projection
c	  
	do iproj=0,nzout-1
	  angle=tiltstr+iproj*tiltinc
	  sinang(iproj)=sind(angle)
	  cosang(iproj)=cosd(angle)
	  iraybase=iproj*nxout
	  do ixout=1,nxout
	    nraytmp=0
c	      
c	      if a near-vertical projection, set up to be exactly vertical
c
	    if(abs(sinang(iproj)).lt.0.01)then
	      sinang(iproj)=0.
	      cosang(iproj)=sign(1.,cosang(iproj))
	      if(cosang(iproj).gt.0.)then
		yraytmp=2.
		xraytmp=nxslice/2 + ixout - nxout/2
	      else
		yraytmp=nyslice-1.
		xraytmp=nxslice/2 + 2 + nxout/2 - ixout 
	      endif		
	      if(xraytmp.gt.1 .and. xraytmp.lt.nxslice)
     &		  nraytmp=nyslice-2
c	      
c	      if a near-horizontal projection, set up to be exactly horizontal
c
	    elseif(abs(cosang(iproj)).lt.0.01)then
	      sinang(iproj)=sign(1.,sinang(iproj))
	      cosang(iproj)=0.
	      if(sinang(iproj).lt.0.)then
		xraytmp=2.
		yraytmp=nyslice/2 + 2 + nxout/2 - ixout
	      else
		xraytmp=nxslice-1.
		yraytmp=nyslice/2 + ixout - nxout/2
	      endif
	      if(yraytmp.gt.1 .and. yraytmp.lt.nyslice)
     &		  nraytmp=nxslice-2
c		
c		otherwise need to look at intersections with slice box
c
	    else
	      ngoodinter=0
	      tanang=sinang(iproj)/cosang(iproj)
	      rayintcp=(ixout-1-nxout/2)/sinang(iproj)
c		
c		coordinates of edges of slice box
c
	      xleft=0.51-(nxslice/2)
	      xright=nxslice - 1.51 - (nxslice/2)
	      ybot=0.51-(nyslice/2)
	      ytop=nyslice - 1.51 - (nyslice/2)
c		
c		corresponding intersections of the ray with extended edges
c
	      yleft=-xleft/tanang + rayintcp
	      yright=-xright/tanang + rayintcp
	      xbot=-(ybot-rayintcp)*tanang
	      xtop=-(ytop-rayintcp)*tanang
c		
c		make list of intersections that are actually within slice box
c
	      if(yleft.ge.ybot.and.yleft.le.ytop)then
		ngoodinter=ngoodinter+1
		xgood(ngoodinter)=xleft
		ygood(ngoodinter)=yleft
	      endif
	      if(yright.ge.ybot.and.yright.le.ytop)then
		ngoodinter=ngoodinter+1
		xgood(ngoodinter)=xright
		ygood(ngoodinter)=yright
	      endif
	      if(xbot.ge.xleft.and.xbot.le.xright)then
		ngoodinter=ngoodinter+1
		xgood(ngoodinter)=xbot
		ygood(ngoodinter)=ybot
	      endif
	      if(xtop.ge.xleft.and.xtop.le.xright)then
		ngoodinter=ngoodinter+1
		xgood(ngoodinter)=xtop
		ygood(ngoodinter)=ytop
	      endif
c		
c		if there are real intersections, use them to set up ray start
c
 	      if(ngoodinter.gt.0)then
		indgood=1
		if(ygood(2).lt.ygood(1).xor.cosang(iproj).lt.0)indgood=2
		xraytmp=xgood(indgood)+nxslice/2+1
		yraytmp=ygood(indgood)+nyslice/2+1
		nraytmp=1+
     &		    sqrt((xgood(1)-xgood(2))**2+(ygood(1)-ygood(2))**2)
		if(nraytmp.lt.3)nraytmp=0
	      endif
	    endif
c	      
c	      store final ray information
c
	    xraystr(ixout+iraybase)=xraytmp
	    yraystr(ixout+iraybase)=yraytmp
	    nrayinc(ixout+iraybase)=nraytmp
	  enddo
	enddo

c
c	  loop on loads of several to many slices at once
c	  
	iyoutstr=0				!starting y output line
	do iload=1,nloads
	  nslices=min(limslices,nyout-iyoutstr)
	  load1=load0+loaddir*(nslices-1)
c	    
c	    load the slices one of 3 different ways: for X, get vertical
c	    rectangles of sections in input file, transpose into slice array
c
	  if(xyz.eq.'x'.or.xyz.eq.'X')then
	    do izsec=iz0,iz1,idirz
	      call imposn(1,izsec,0)
	      call irdpas(1,array(ioutbase),nslices,nyblock,load0,
     &		  load1,iy0,iy1,*99)
	      call xtransp(array(ioutbase),idirz*(izsec-iz0)+1,
     &		  array, nxslice,nyslice,nslices) 
	    enddo
c	      
c	      for Y, get horizontal rectangles of sections in input file,
c	      transpose differently into slice array
c
	  elseif(xyz.eq.'y'.or.xyz.eq.'Y')then
	    do izsec=iz0,iz1,idirz
	      call imposn(1,izsec,0)
	      call irdpas(1,array(ioutbase),nxblock,nslices,ix0,ix1,
     &		  load0, load1,*99)
	      call ytransp(array(ioutbase),idirz*(izsec-iz0)+1,
     &		  array, nxslice,nyslice,nslices) 
	    enddo
c	      
c	      for Z, just get slices directly from sections in file
c
	  else
	    indstack=1
	    do izsec=load0,load1,loaddir
	      call imposn(1,izsec,0)
	      call irdpas(1,array(indstack),nxslice,nyslice,ix0,ix1,
     &		  iy0,iy1,*99)
	      indstack=indstack+istackdel
	    enddo
	  endif
	  load0=load0+loaddir*nslices
c	    
c	    loop on different projection views
c	    
	  do iproj=0,nzout-1
	    iraybase=iproj*nxout
c	      
c	      set the output lines for this view to the fill value
c	      
	    do ind=ioutbase,ioutbase+nslices*nxout-1
	      array(ind)=fill
	    enddo
c	      
c	      loop on pixels along line
c	      
	    do ixout=1,nxout
	      nraypts=nrayinc(ixout+iraybase)
c	      print *,nraypts
	      if(nraypts.gt.0)then
c		  
c		  if block along ray, clear temporary array for output pixels
c
		do ipix=1,nslices
		  pixtmp(ipix)=0.
		enddo
c		  
c		  move along ray, computing at each point indexes and factors
c		  for quadratic interpolation
c
		if (sinang(iproj).ne.0..or.cosang(iproj).ne.1.)then
		  do iray=0,nraypts-1
		    xray=xraystr(ixout+iraybase)-iray*sinang(iproj)
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
c		      loop through pixels in different slices, do quadratic
c		      interpolation limited by values of surrounding pixels
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
     &			  (A*DX*DX + B*DY*DY + C*DX + D*DY + V5)))
c			
		      ixy=ixy+istackdel		!increment subscripts for
		      ixy2=ixy2+istackdel	!next slice
		      ixy4=ixy4+istackdel
		      ixy6=ixy6+istackdel
		      ixy8=ixy8+istackdel
		    enddo
		  enddo
		else
c		    
c		    simple case of straight projection
c		    
		  ixr=nint(xraystr(ixout+iraybase))
		  iyr=nint(yraystr(ixout+iraybase))
		  do ipix=1,nslices
		    ixy=ixr+(iyr-1)*nxslice
		    sumtmp=0.
		    do iray=0,nraypts-1
		      sumtmp=sumtmp+array(ixy)
		      ixy=ixy+nxslice
		    enddo
		    ixr=ixr+istackdel
		    pixtmp(ipix)=sumtmp
		  enddo
		endif
c
c		  set up scaling and put pixels out in different lines
c
		if(ifrayscale.eq.0)then
		  rayfac=scalfac/nyslice
		else
		  rayfac=scalfac/nraypts
		endif
		rayadd=scaladd*scalfac
c		    
		indout=ixout+ioutbase-1
		do ipix=1,nslices
		  array(indout)=rayfac*pixtmp(ipix)+rayadd
		  indout=indout+nxout
		enddo
	      endif
	    enddo
c		  
c		get min, max, mean; output the lines to proper section
c		
	    call iclden(array(ioutbase),nxout,nslices,1,nxout,1,
     &		nslices,tmin,tmax,tmean)
	    dmin2=(min(dmin2,tmin))
	    dmax2=(max(dmax2,tmax))
	    dsum=dsum+tmean*nslices*nxout
	    call imposn(2,iproj,iyoutstr)
	    call iwrsecl(2,array(ioutbase),nslices)
	  enddo
	  iyoutstr=iyoutstr+nslices
	enddo
c	    
c	    finish up file header
c
	dmean2=dsum/(nxout*nyout*nzout)
	call iwrhdr(2,title,1,dmin2,dmax2,dmean2)
	call imclose(2)
	call imclose (1)
	call exit(0)
99	print *,'XYZPROJ: READ ERROR'
	call exit(1)
	end
c
c
c	  Subroutines to transpose the pieces of sections read from input file
c	  into the array of slices
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
