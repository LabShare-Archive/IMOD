* * * * * MTLENGTHS * * * * *
c	  
c	  MTLENGTHS computes the lengths, in microns, of contours in
c	  IMOD models, and produces a list giving the object number and contour
c	  length for each contour.  This list may be used to generate a
c	  histogram of lengths with a program such as GENHSTPLT.  One may
c	  optionally add a certain amount of length to each contour at its
c	  high Z end, so that the length calculation will include the
c	  thickness of every section in which a contour appears.
c	  
c	  If the sections were significantly tilted during microscopy, the
c	  program can adjust for these tilts given the proper information.
c	  Prepare a file in which the first line shows the Z value and the
c	  tilt of the first tilted section (or of the first section, if that
c	  one was tilted), and each successive line shows the Z value and tilt
c	  for each section on which tilt was changed.  Z values should occur in
c	  ascending order.
c
c	  Entries to the program are:
c	  
c	  Name of model file
c	  
c	  Name of file with tilt information, or Return if none
c	  
c	  IF the model has no scaling information in its header, make the
c	  following three entries:
c
c	  .  Magnification of negatives (without any commas)
c	  
c	  .  Scale, in microns per pixel, at which negatives were digitized
c	  
c	  .  Section thickness in nanometers
c	  
c	  Amount to add to the high Z end of each object in order to account
c	  .  for section thickness.  For example, enter 1 to allow a section's
c	  .  worth of length for each section that an object appears in.
c
c	  Name of output file in which to place list of lengths, or Return for
c	  .  output to the terminal
c	  
c	  List of IMOD objects to exclude from the output, or Return to output
c	  .  all objects.  If the data are from a WIMP model, the IMOD object
c	  .  number is 256 minus the WIMP color (1 for 255, etc.), or the
c	  .  negative of this value for WIMP objects that are turned off.
c	  
c	  David Mastronarde  1/27/90; modified for IMOD 4/4/97
c
	include 'nimp_source:model.inc'
	character*80 modelfile,newmodel,tiltfile
	logical exist,readw_or_imod
	real*4 tiltzstrt(1000),tilt(1000),costilt(1000),remapz(1000)
	integer*4 iobjexcl(255)
	integer getimodhead
c
91	write(*,'(1x,a,$)')'Name of input model file: '
	read(*,'(a)')modelfile
c
c	  read in the model
c
	exist=readw_or_imod(modelfile)
	if(.not.exist)go to 91
c	  
	write(*,'(1x,a,$)')
     &	    'Name of file of tilt info (Return if none): '
	read(*,'(a)')tiltfile
	ntilts=0
	if(tiltfile.ne.' ')then
	  call dopen(3,tiltfile,'ro','f')
3	  i=ntilts+1
	  read(3,*,end=5)tiltzstrt(i),tilt(i)
	  ntilts=i
	  costilt(ntilts)=cosd(tilt(ntilts))
	  go to 3
5	  remapz(1)=tiltzstrt(1)
	  do i=1,ntilts-1
	    remapz(i+1)=remapz(i)+(tiltzstrt(i+1)-tiltzstrt(i))
     &		/costilt(i)
	  enddo
	endif
c	  
	defscal=1.e6
	ierr=getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip)
	if(ierr.eq.0.and.abs(xyscal-defscal)/defscal.gt.1.e-5)then
	  write(*,'(a,f10.6,a)')' Scale set from model header at',
     &	      xyscal,' microns/pixel'
	  zscal=xyscal*zscale
	else
	  ifflip=0
	  write(*,'(1x,a,$)')'Magnification of negatives: '
	  read(*,*)xmag
	  write(*,'(1x,a,$)')'Scale at which negatives were digitized'
     &	      //' (microns per pixel from VIDS): '
	  read(*,*)umperpix
	  write(*,'(1x,a,$)')'Nominal section thickness (nm): '
	  read(*,*)secthick
c
	  xyscal=umperpix/xmag
	  zscal=secthick/1000.
	endif
	if(ifflip.eq.0)then
	  indy=2
	  indz=3
	else
	  indz=2
	  indy=3
	endif
c
	write(*,'(1x,a,/,a,$)')'Enter amount to add to maximum Z in '//
     &	    'each object to account for','   section thickness (i.e.'//
     &	    ', 1 to add one section worth of length): '
	read(5,*)zadd
c
	write(*,'(1x,a,$)')'Name of output file (Return for terminal): '
	read(*,'(a)')newmodel
	if(newmodel.eq.' ')then
	  iout=6
	else
	  iout=7
	  call dopen(7,newmodel,'new','f')
	endif
c
c	write(*,'(1x,a,$)')'0 to output all objects, 1 to exclude'//
c     &	    ' any that are turned off: '
c	read(*,*)ifexclude
	nobjexcl=0
	print *,'Enter list of IMOD object #s to exclude, or ',
     &	    'Return to include all objects'
	call rdlist(5,iobjexcl,nobjexcl)
c	  
	zminall=1.e10
	zmaxall=-1.e10
	nout=0
	do iobj=1,max_mod_obj
	  zminobj=1.e10
	  zmaxobj=-1.e10
	  if(npt_in_obj(iobj).gt.0)then
	    imodobj=256-obj_color(2,iobj)
	    if(obj_color(1,iobj).eq.0)imodobj=-imodobj
	    ibase=ibase_obj(iobj)
	    do ipt=1,npt_in_obj(iobj)
	      ip1=abs(object(ipt+ibase))
	      p_coord(1,ip1)=p_coord(1,ip1)*xyscal
	      p_coord(indy,ip1)=p_coord(indy,ip1)*xyscal
	      zraw=p_coord(indz,ip1)
	      zminobj=min(zraw,zminobj)
	      zmaxobj=max(zraw,zmaxobj)
	      p_coord(indz,ip1)=scalez(zraw,zscal,tiltzstrt,remapz,
     &		  costilt, ntilts)
	    enddo
	    zminall=min(zminall,zminobj)
	    zmaxall=max(zmaxall,zmaxobj)
	    ifexclude=0
	    do ie=1,nobjexcl
	      if(imodobj.eq.iobjexcl(ie))ifexclude=1
	    enddo
	    if(ifexclude.eq.0)then
	      sum=0.
	      do ipt=1,npt_in_obj(iobj)-1
		ip1=abs(object(ipt+ibase))
		ip2=object(ipt+1+ibase)
		if(ip2.gt.0) then
		  sum=sum+sqrt((p_coord(1,ip1)-p_coord(1,ip2))**2+
     &		      (p_coord(2,ip1)-p_coord(2,ip2))**2+ 	       
     &		      (p_coord(3,ip1)-p_coord(3,ip2))**2)
		endif
	      enddo
	      if(zadd.gt.0.) sum=sum + scalez(zmaxobj+zadd,zscal,
     &		  tiltzstrt,remapz,costilt,ntilts) - scalez(zmaxobj,
     &		  zscal,tiltzstrt,remapz,costilt,ntilts)
	      write(iout,'(i4,f12.4)')256-obj_color(2,iobj),sum
	      nout=nout+1
	    endif
	  endif
	enddo
	zminscl=scalez(zminall,zscal,tiltzstrt,remapz,costilt,ntilts)
	zmaxscl=scalez(zmaxall+zadd,zscal,tiltzstrt,remapz,costilt,
     &	    ntilts)
	write(*,103)zminall,zmaxall,zminscl,zmaxscl
103	format(' Original minimum and maximum Z in the model were ',f8.2
     &	    ,' and',f8.2,/, ' These values were scaled to',f9.3,' and',
     &	    f9.3,' microns')
	if(zadd.ne.0)write(*,104)zadd
104	format(' This includes the effects of adding',f5.1,
     &	    ' section at high Z')
	close(7)
	print *,nout,' objects output'
	stop
	end


c	  SCALEZ will scale the Z index coordinate ZZ by first remapping
c	  the Z values via the tilt remappings, then by multiplying by ZSCAL
c
	function scalez(zz,zscal,tiltzstrt,remapz,costilt,ntilts)
	real*4 tiltzstrt(*),remapz(*),costilt(*)
	scalez=zz
	if(ntilts.gt.0)then
	  if(zz.ge.tiltzstrt(1))then
	    itilt=ntilts
	    do while(zz.lt.tiltzstrt(itilt))
	      itilt=itilt-1
	    enddo
	    scalez=remapz(itilt)+(zz-tiltzstrt(itilt))/costilt(itilt)
	  endif
	endif
	scalez=scalez*zscal
	return
	end
