* * * * * * SOLVEMATCH * * * * * *
c
c	  SOLVEMATCH will solve for a 3-dimensional linear transformation
c	  relating the tomogram volumes resulting from tilt series around two
c	  different axes.  It uses information from the 3-D coordinates of
c	  fiducials found by TILTALIGN.  See man page for further information.
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.9  2004/08/22 14:58:37  mast
c	  Used line_is_filename as workaround to Windows problem
c	
c	  Revision 3.8  2004/06/10 05:29:30  mast
c	  Added ability to deal with absolute coordinates
c	
c	  Revision 3.7  2004/01/29 03:12:11  mast
c	  Fixed bug in getting output file for Pip input
c	
c	  Revision 3.6  2003/12/24 19:03:20  mast
c	  Incorporated new method for handling fiducials on one surface and
c	  converted to PIP input.
c	
c	  Revision 3.5  2003/05/20 23:43:45  mast
c	  Add space before wrlist output
c	
c	  Revision 3.4  2002/11/11 22:26:37  mast
c	  Added argument to calls to do3multr and solve_wo_outlier for
c	  fixed column
c	
c	  Revision 3.3  2002/07/28 00:24:47  mast
c	  Added a second level of indexing so that point numbers in the
c	  fiducial coordinate file are read and used to refer to points.
c	  Made the matching model points be referred to be a negative
c	  number.
c	
c	  Revision 3.2  2002/07/21 19:26:30  mast
c	  *** empty log message ***
c	
c	  Revision 3.1  2002/07/21 19:24:27  mast
c	  Resurrected the ability to use matching models, added scaling of
c	  coordinates based on information from model header, and added
c	  ability to solve for transformation using model files alone.  Also
c	  standardized error output. 
c
c	  David Mastronarde, 1995; modified for zero shifts, 7/4/97;
c	  Added outlier elimination and error exit, 6/5/99
c	  Added ability to start with small initial set of matches, 3/20/00
c
	implicit none
        include 'statsize.inc'
        include 'model.inc'
	integer idim
        parameter (idim=1000)
        real*4 xr(msiz,idim)
        real*4 pnta(3,idim),pntb(3,idim),a(3,4),dxyz(3),devxyz(3)
	real*4 devxyzmax(3),orig(3,2),ptrot(3),cenloc(3)
	integer*4 idrop(idim),iorig(idim),mapped(idim)
	integer*4 listcorra(idim),listcorrb(idim),icont2ptb(idim)
        integer*4 mapab(idim),nxyz(3,2),jxyz(3)/1,3,2/
	integer*4 iconta(idim),icontb(idim),icont2pta(idim)
        character*80 filename
	integer*4 nstartmin,itry,npnta,npntb,nlist,nlista,nlistb,ndat
	integer*4 ia,nsurf,model,iftransp,nmodpt,ipt,ip,iobj,i,ndata
	integer*4 mod,j,ifadded,ipntmax,ipta,iptb,iptamin,iptbmin
	integer*4 maxdrop,ndrop,idum,iofs,ixyz, ierrFactor
	real*4 addratio,cosa,cosb,tmpy,addcrit,distmin,devavg,devsd
	real*4 devmax,dx,dist,crit,elimmin,critabs,stoplim,xtilta,xtiltb
	real*4 sina,sinb,dy,dz,aScale, bScale, absFidCrit
	integer*4 ierr,ifflip,ncolfit,maxconta,maxcontb,icolfix,k
	real*4 xyscal,zscale,xofs,yofs,zofs,ximscale, yimscale, zimscale
	real*4 loMaxAvgRatio, hiMaxAvgRatio, loMaxLimRatio, hiMaxLimRatio
	real*4 aDelta, bDelta, aPixelSize, bPixelSize, xcen, ycen
	integer*4 nxFidA, nyFidA, nxFidB, nyFidB
	logical relativeFids

        logical readw_or_imod
	real*4 sind,cosd
	integer*4 getimodhead,getimodscales
c
	logical pipinput
	integer*4 numOptArg, numNonOptArg
	integer*4 PipGetInteger
	integer*4 PipGetString,PipGetTwoFloats,PipGetFloat
	integer*4 PipGetInOutFile
	character*6 modelOption(2)/'AMatch','BMatch'/
	character*9 tomoOption(2)/'ATomogram','BTomogram'/
	character*10240 listString
c	  
c	  fallbacks from ../../manpages/autodoc2man -2 2  solvematch
c	  
	integer numOptions
	parameter (numOptions = 15)
	character*(40 * numOptions) options(1)
	options(1) =
     &      'output:OutputFile:FN:@afiducials:AFiducialFile:FN:@'//
     &      'bfiducials:BFiducialFile:FN:@'//
     &      'alist:ACorrespondenceList:LI:@'//
     &      'blist:BCorrespondenceList:LI:@scales:ScaleFactors:FP:@'//
     &      'xtilts:XAxisTilts:FP:@surfaces:SurfacesOrUseModels:I:@'//
     &      'maxresid:MaximumResidual:F:@amatch:AMatchingModel:FN:@'//
     &      'bmatch:BMatchingModel:FN:@'//
     &      'atomogram:ATomogramOrSizeXYZ:CH:@'//
     &      'btomogram:BTomogramOrSizeXYZ:CH:@'//
     &      'param:ParameterFile:PF:@help:usage:B:'
c	  
c	  don't add a point if it's this much higher than the limit for
c	  quitting
c
	addratio=1.
c	  
c	  require at least this many points to start
c
	nstartmin=4
c	  
c	  initialize these items here in case no fiducials are entered
c
	npnta=0
	npntb=0
	nlista = 0
	nlistb = 0
        ndat=0
	nsurf = 0
	ncolfit = 3
	icolfix = 0
	filename = ' '
	xtilta = 0.
	xtiltb = 0.
	stoplim = 8.
	aScale = 1.
	bScale = 1.
	aDelta = 0.
	bDelta = 0.
	aPixelSize = 0.
	bPixelSize = 0.
	relativeFids = .true.
c	  
c	  Pip startup: set error, parse options, check help, set flag if used
c
	call PipReadOrParseOptions(options, numOptions, 'solvematch',
     &	    'ERROR: SOLVEMATCH - ', .true., 3, 0, 1, numOptArg,
     &	    numNonOptArg)
	pipinput = numOptArg + numNonOptArg .gt. 0
c
c	  Get first fid filename if any
c	  Also get the surfaces option; if it's -2, then force models only
c	  even if there are fids
c
	if (pipinput) then
	  ierr = PipGetString('AFiducialFile', filename)
	  nsurf = 2
	  ierr = PipGetInteger('SurfacesOrUseModels', nsurf)
	  if (nsurf .eq. -2) then
	    nsurf = 0
	    filename = ' '
	  endif
	else
	  write(*,'(1x,a,/,a$)') 'Name of file with 3-D fiducial'//
     &	      ' coordinates for first tilt series,',
     &	      ' or Return to align with matching model files only: '
	  read(*,'(a)')filename
	endif
c	  
c	  If no fiducials are to be used, skip down to maximum residual entry
c
	if (filename .eq. ' ')go to 40
c
	ncolfit = 4
	call getFiducials(filename, iconta, pnta, npnta, idim,
     &	    'first tomogram', aPixelSize,  nxFidA, nyFidA)
c
	if (pipinput) then
	  if (PipGetString('BFiducialFile', filename) .gt. 0) call errorexit
     &	      ('NO FILE SPECIFIED FOR FIDUCIALS IN SECOND TILT SERIES')
	else
	  write(*,'(1x,a,$)') 'Name of file with 3-D fiducial'//
     &	      ' coordinates for second tilt series: '
	  read(*,'(a)')filename
	endif
	call getFiducials(filename, icontb, pntb, npntb, idim,
     &	    'second tomogram', bPixelSize, nxFidB, nyFidB)
c	  
c	  fill listcorr with actual contour numbers, and find maximum contours
c
	maxconta = 0
	maxcontb = 0
	do i=1,npnta
	  listcorra(i)=iconta(i)
	  mapab(i)=0
	  maxconta=max(maxconta,iconta(i))
	enddo
	do i=1,npntb
	  listcorrb(i)=icontb(i)
	  mapped(i)=0
	  maxcontb=max(maxcontb,icontb(i))
	enddo
c	  
c	  build index from contour numbers to points in array
c
	if (maxconta.gt.idim.or.maxcontb.gt.idim)call errorexit(
     &	    'CONTOUR NUMBERS TOO HIGH FOR ARRAYS')
	do i=1,max(maxconta,maxcontb)
	  icont2pta(i)=0
	  icont2ptb(i)=0
	enddo
	do i=1,npnta
	  icont2pta(iconta(i))=i
	enddo
	do i=1,npntb
	  icont2ptb(icontb(i))=i
	enddo
c
	nlist=min(npnta,npntb)
	nlista=nlist
	if (pipinput) then
	  if (PipGetString('ACorrespondenceList', listString) .eq. 0)
     &	      call parselist(listString, listcorra,nlista)
	else
	  write (*,113)nlist
113	  format('Enter a list of points in the first series for which',
     &	      ' you are sure of the',/,' corresponding point in the',
     &	      ' second series (Ranges are OK;',/' enter / if the first',
     &	      i3, ' points are in one-to-one correspondence between',
     &	      ' the series')
	  call rdlist(5,listcorra,nlista)
	endif
	if (nlista .gt. idim) call errorexit('TOO MANY POINTS FOR ARRAYS')
	if(nlista.lt.nstartmin)then
	  print *
	  print *,'ERROR: SOLVEMATCH - NEED AT LEAST',nstartmin,
     &	      ' POINTS TO GET STARTED'
	  call exit(1)
	endif
	if(nlista.gt.nlist)call errorexit(
     &	    'YOU HAVE ENTERED MORE NUMBERS THAN THE'//
     &	      ' MINIMUM NUMBER OF POINTS IN A AND B')
c	
	nlistb=nlista
	if (pipinput) then
	  if (PipGetString('BCorrespondenceList', listString) .eq. 0)
     &	      call parselist(listString, listcorrb,nlistb)
	else
	  write (*,114)
114	  format('Enter a list of the corresponding points in the',
     &	      ' second series ',/,' - enter / for ', $)
	  call wrlist(listcorrb, nlist)
	  call rdlist(5,listcorrb,nlistb)
	endif
c
	if(nlistb.ne.nlista)then
	  print *
 	  print *,'ERROR: SOLVEMATCH - YOU MUST HAVE THE SAME NUMBER '
     &	      //' OF ENTRIES IN EACH LIST','YOU MADE',nlista,' AND',
     &	      nlistb, 'ENTRIES FOR LISTS A AND B'
	  call exit(1)
	endif
c	  
c	  check legality and build map lists
c	  
	do i=1,nlista
	  if(listcorra(i).le.0.or.listcorrb(i).le.0)call errorexit(
     &	      'YOU ENTERED A POINT NUMBER LESS '//
     &		'THAN OR EQUAL TO ZERO')
	  if(listcorra(i) .gt. maxconta)call errorexit(
     &	      ' YOU ENTERED A POINT NUMBER HIGHER'//
     &		' THAN THE NUMBER OF POINTS IN A')
	  ipta = icont2pta(listcorra(i))
	  if(ipta .eq. 0)call errorexit(
     &	      ' YOU ENTERED A POINT NUMBER THAT IS NOT INCLUDED'//
     &		' IN THE POINTS FROM A')
	  if(listcorrb(i).gt. maxcontb)call errorexit(
     &	      ' YOU ENTERED A POINT NUMBER HIGHER'//
     &		' THAN THE NUMBER OF POINTS IN B')
	  iptb = icont2ptb(listcorrb(i))
	  if(iptb .eq. 0)call errorexit(
     &	      ' YOU ENTERED A POINT NUMBER THAT IS NOT INCLUDED'//
     &		' IN THE POINTS FROM B')
	  if(mapab(ipta).ne.0)then
	    print *
	    print *,'ERROR: SOLVEMATCH - POINT #',listcorra(i),
     &		' IN A REFERRED TO TWICE'
	    call exit(1)
	  elseif(mapped(iptb).ne.0)then
	    print *
	    print *,'ERROR: SOLVEMATCH - POINT #',listcorrb(i),
     &		' IN B REFERRED TO TWICE'
	    call exit(1)
	  endif
	  mapab(ipta)=iptb
	  mapped(iptb)=ipta
	enddo
c	  
c	  Get tilt axis angle, plus try to get tomogram pixel size (delta)
c	  and compute scaling factors, overriden by an entry
c	  
	if (pipinput) then
	  ierr = PipGetTwoFloats('XAxisTilts', xtilta, xtiltb)
	  call getDelta('ATomogramOrSizeXYZ', aDelta, nxyz(1,1)) 
	  call getDelta('BTomogramOrSizeXYZ', bDelta, nxyz(1,2))
	  if (aDelta * aPixelSize .gt. 0.) aScale = aPixelSize / aDelta
	  if (bDelta * bPixelSize .gt. 0.) bScale = bPixelSize / bDelta
	  ierrFactor = PipGetTwoFloats('ScaleFactors', aScale, bScale)
c	  print *,aDelta,bDelta,aPixelSize,bPixelSize,ierrFactor
c	    
c	    Conditions for absolute fiducials are that the pixel sizes be
c	    available (meaning new absolute coordinates) and that either
c	    the tomogram pixel sizes were available too or scale factors
c	    were entered and tomogram sizes were provided by getDelta
c
	  if (nsurf .ne. 0 .and. aPixelSize .gt. 0. .and. bPixelSize .gt. 0.
     &	      .and. (aDelta .gt. 0. .or. (ierrFactor .eq. 0 .and.
     &	      aDelta .eq. 0.)) .and. (bDelta .gt. 0 .or.
     &	      (ierrFactor .eq. 0 .and. bDelta .eq. 0.))) then
c
c	      Shift the original X and Y to the center of the volume
c	      have to divide size by scale because points aren't scaled up yet
c	      Use the actual fiducial size if it exists
c
	    xcen = 0.5 * nxyz(1, 1) / aScale
	    ycen = 0.5 * nxyz(jxyz(2), 1) / aScale
	    if (nxFidA .gt. 0 .and. nyFidA .gt. 0) then
	      xcen = 0.5 * nxFidA
	      ycen = 0.5 * nyFidA
	    endif
	    do i = 1, npnta
	      pnta(1, i) = pnta(1, i) - xcen
	      pnta(2, i) = pnta(2, i) - ycen
	    enddo
c	      
	    xcen = 0.5 * nxyz(1, 2) / bScale
	    ycen = 0.5 * nxyz(jxyz(2), 2) / bScale
	    if (nxFidB .gt. 0 .and. nyFidB .gt. 0) then
	      xcen = 0.5 * nxFidB
	      ycen = 0.5 * nyFidB
	    endif
	    do i = 1, npntb
	      pntb(1, i) = pntb(1, i) - xcen
	      pntb(2, i) = pntb(2, i) - ycen
	    enddo
	    relativeFids = .false.
	  endif
	else
	  write(*,'(1x,a,$)')'Tilts around the X-axis applied in '//
     &	      'generating tomograms A and B: '
	  read(5,*)xtilta,xtiltb
	endif
c	  
c	  use the negative of the angle to account for the inversion of the
c	  tomogram; rotate the 3-d points about the X axis
c	  This is a good place to impose the scaling too
c
	cosa=cosd(-xtilta) * aScale
	sina=sind(-xtilta) * aScale
	do i=1,npnta
	  tmpy=cosa*pnta(2,i)-sina*pnta(3,i)
	  pnta(3,i)=sina*pnta(2,i)+cosa*pnta(3,i)
	  pnta(2,i)=tmpy
	  pnta(1,i) = pnta(1,i) *aScale
	enddo
	cosb=cosd(-xtiltb) * bScale
	sinb=sind(-xtiltb) * bScale
	do i=1,npntb
	  tmpy=cosb*pntb(2,i)-sinb*pntb(3,i)
	  pntb(3,i)=sinb*pntb(2,i)+cosb*pntb(3,i)
	  pntb(2,i)=tmpy
	  pntb(1,i) = pntb(1,i) *bScale
	enddo
c	  
40	if (pipinput) then
	  ierr = PipGetFloat('MaximumResidual', stoplim)
	else
	  write(*,'(1x,a,/,a,$)')'Maximum residual value above which '//
     &	      'this program should',' exit with an error: '
	  read(5,*)stoplim
	endif
c	  
c	  if no fiducials, now skip to model entry section
c
	if (npnta.eq.0) go to 50
c       
c       fill array for regression
c
        do ia=1,npnta
          if(mapab(ia).ne.0)then
            ndat=ndat+1
            do j=1,3
              xr(j,ndat)=pntb(jxyz(j),mapab(ia))
              xr(j+5,ndat)=pnta(jxyz(j),ia)
            enddo
            xr(4,ndat)=1.
	    iorig(ndat)=iconta(ia)
c	    print *,(xr(j,ndat),j=1,3),(xr(j,ndat),j=6,8)
	  endif
        enddo
c
	print *,ndat,' pairs of fiducial points'
	if (.not. pipinput) then
	  nsurf = 2
	  write(*,'(1x,a,/,a,/,a,/,a,$)')'Enter 0 to solve for '//
     &	      'displacements using matching model files, or -1, 1 or 2'
     &	      ,'  to solve only for 3x3 matrix (2 if fiducials are on 2'
     &	      //' surfaces, 1 if they are',
     &	      '  on one surface and tomograms are NOT inverted,'//
     &	      ' or -1 if fiducials ARE on one','  surface'
     &	      //' and tomograms are inverted relative to each other): '
	  read(5,*)nsurf
	endif
c	  
c	  Enter matching models if nsurf is 0
c
50	if(nsurf.eq.0)then
	  iofs=ncolfit+1
	  do model=1,2
c	      
c	      DNM 7/20/02: Changed to just get the nx,ny,nz and not the
c	      origin from the image file; to use model header information
c	      to scale back to image index coordinates; and to not use or mess
c	      up the y-z transposition variable
c
	    if (.not.pipinput) write(*,'(1x,a,i2,a)')'Enter NX, NY, NZ of '//
     &		'tomogram', model, ', or name of tomogram file'
	    call get_nxyz(pipinput, tomoOption(model), 'SOLVEMATCH', 1,
     &		nxyz(1,model))
c	    print *,(nxyz(i,model),i=1,3)
	    if (pipinput) then
	      if (PipGetString(modelOption(model), filename) .gt. 0) then
		print *
		print *,'ERROR: SOLVEMATCH - NO MATCHING MODEL FOR TOMOGRAM',
     &		    model
		call exit(1)
	      endif
	    else
	      write(*,'(1x,a,i2,a,$)')
     &		  'Name of model file from tomogram',model,': '
	      read(*,'(a)')filename
	    endif
	    if(.not.readw_or_imod(filename))call errorexit(
     &		'READING MODEL FILE')

	    ierr=getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip)
	    ierr = getimodscales(ximscale, yimscale, zimscale)
	    
	    nmodpt=0
	    do iobj=1,max_mod_obj
	      do ip=1,npt_in_obj(iobj)
		ipt=abs(object(ibase_obj(iobj)+ip))
		nmodpt=nmodpt+1
		xr(1+iofs,ndat+nmodpt)=(p_coord(1,ipt)-xofs)/ximscale
     &		    - 0.5*nxyz(1,model)
		xr(2+iofs,ndat+nmodpt)=(p_coord(2,ipt)-yofs)/yimscale
     &		    - 0.5*nxyz(2,model)
		xr(3+iofs,ndat+nmodpt)=(p_coord(3,ipt)-zofs)/zimscale
     &		    - 0.5*nxyz(3,model)
		xr(4,ndat+nmodpt)=0.
		iorig(ndat+nmodpt)=-nmodpt
		if (npnta .eq. 0)iorig(ndat+nmodpt)=nmodpt
	      enddo
	    enddo
	    if(model.eq.1)ndata=nmodpt
	    iofs=0
	  enddo
	  if(nmodpt.ne.ndata)call errorexit(
     &		'# OF POINTS DOES NOT MATCH BETWEEN MATCHING MODELS')
	  print *,nmodpt,' point pairs from models'
	  iofs = ncolfit + 1
	else
c
c	    If no model points
c	    get rid of dummy column: fit 3 columns, pack dependent vars to
c	    the left, and set the offset for adding more dependent var data
c
	  nmodpt=0
	  ncolfit = 3
	  do i = 1, ndat
	    do j = 5,7
	      xr(j, i) = xr(j + 1, i)
	    enddo
	  enddo
	  iofs = 4
c
c	    if only one surface, "fix" column 2, encode sign in icolfix
c
	  if(abs(nsurf).eq.1) icolfix = sign(2, nsurf)
	endif
	ndat=ndat+nmodpt
c	  
c	  loop to add points that weren't initially indicated - as long as
c	  there are any left to add and the last minimum distance was still
c	  low enough
c
	ifadded=0
	addcrit=addratio*stoplim
	distmin=0.
c
	do while(ndat-nmodpt.lt.min(npnta,npntb).and.distmin.lt.addcrit)
	  call do3multr(xr,ndat,ncolfit,ndat,icolfix,a,dxyz,cenloc,
     &	      devavg,devsd, devmax,ipntmax, devxyzmax)
	  distmin=1.e10
c	    
c	    apply to each point in B that is not mapped to
c
	  do iptb=1,npntb
	    if(mapped(iptb).eq.0)then
	      do ixyz=1,3
		ptrot(ixyz)=dxyz(ixyz)
		if (ncolfit .eq. 4) ptrot(ixyz)=dxyz(ixyz) + a(ixyz,4)
		do j=1,3
		  ptrot(ixyz)=ptrot(ixyz)+
     &		      a(ixyz,j)*pntb(jxyz(j),iptb)
		enddo
	      enddo
c		
c		search through points in A that don't have map
c		
	      do ipta=1,npnta
		if(mapab(ipta).eq.0)then
		  dx=pnta(jxyz(1),ipta)-ptrot(1)
		  if(abs(dx).lt.distmin)then
		    dy=pnta(jxyz(2),ipta)-ptrot(2)
		    if(abs(dy).lt.distmin)then
		      dz=pnta(jxyz(3),ipta)-ptrot(3)
		      if(abs(dz).lt.distmin)then
			dist=sqrt(dx**2+dy**2+dz**2)
			if(dist.lt.distmin)then
			  iptamin=ipta
			  iptbmin=iptb
			  distmin=dist
			endif
		      endif
		    endif
		  endif
		endif
	      enddo
	    endif
	  enddo
c	    
c	    add the closest fitting point-pair
c
c	  print *,iptbmin,iptamin,distmin,devavg,devmax
	  if(distmin.lt.addcrit)then
	    ifadded=1
	    ndat=ndat+1
	    mapab(iptamin)=iptbmin
	    mapped(iptbmin)=iptamin
	    do j=1,3
              xr(j,ndat)=pntb(jxyz(j),iptbmin)
              xr(j+iofs,ndat)=pnta(jxyz(j),iptamin)
            enddo
            xr(4,ndat)=1.
	    iorig(ndat)=iconta(iptamin)
	  endif
	enddo
	if(ifadded.ne.0)then
c	    
c	    rebuild lists of actual contour numbers
c
	  print *,'In the final list of correspondences used for',
     &	      ' fits, points from A are:'
	  nlista=0
	  do i=1,npnta
	    if (mapab(i) .ne. 0)then
	      nlista=nlista+1
	      listcorra(nlista) = iconta(i)
	    endif
	  enddo
	  call wrlist(listcorra,nlista)
	  print *,'Points from B are:'
	  nlistb=0
	  do i=1,npntb
	    if (mapped(i) .ne. 0)then
	      nlistb=nlistb+1
	      listcorrb(nlistb) = icontb(i)
	    endif
	  enddo
	  call wrlist(listcorrb,nlistb)
	endif

c	write(*,105)((xr(i,j),i=1,4),(xr(i,j),i=1+iofs,3+iofs),j=1,ndat)
105	format(7f9.2)
	maxdrop=nint(0.1*ndat)
	crit=0.01
	elimmin=3.
	critabs=0.002
	call solve_wo_outliers(xr,ndat,ncolfit,icolfix,maxdrop,crit,critabs,
     &	    elimmin, idrop,ndrop, a,dxyz,cenloc, devavg,devsd,devmax,
     &	    ipntmax, devxyzmax)
c
	if(ndrop.ne.0)then
	  write(*,104)ndrop,devavg,devsd,(iorig(idrop(i)),i=1,ndrop)
	  write(*,115)(xr(ncolfit+1,i),i=ndat+1-ndrop,ndat)
104	  format(/,i3,' points dropped by outlier elimination; ',
     &	      'residual mean =',f7.2,', SD =',f7.2,/,
     &	      ' point # in A:',(11i6))
115	  format(' deviations  :',(11f6.1))
	endif
c
        write(*,101)devavg,devmax,iorig(ipntmax),(devxyzmax(i),i=1,3)
101     format(//,' Mean residual',f8.3,',  maximum',f9.3,
     &      ' at point #',i4,' (in A)'/,'  Deviations:',3f9.3)
c
	if (ncolfit .gt. 3) then
c	    
c	    fit to both: report the dummy variable offset, make sure that
c	    the dxyz are non-zero for matchshifts scanning
c
	  write(*,103)(a(i,4),i=1,3)
103	  format(/,' X, Y, Z offsets for fiducial dummy variable:',3f10.3)
	  if (abs(dxyz(1)) .lt. 0.001 .and. abs(dxyz(2)) .lt. 0.001 .and.
     &	      abs(dxyz(3)) .lt. 0.001) dxyz(1) = 0.0013
c
	else if (nmodpt .eq. 0 .and. relativeFids) then
c	    
c	    No matching models and relative fiducials: set dxyz to zero
c	    and report the offset from the fit
c
	  write(*,107)(dxyz(i),i=1,3)
107	  format(/,' X, Y, Z offsets from fiducial fit:',3f10.3)
	  dxyz(1) = 0.
	  dxyz(2) = 0.
	  dxyz(3) = 0.
	else
c	    
c	    Absolute fiducials: just make sure they are not exactly zero
c
	  if (abs(dxyz(1)) .lt. 0.001 .and. abs(dxyz(2)) .lt. 0.001 .and.
     &	      abs(dxyz(3)) .lt. 0.001) dxyz(1) = 0.0013
	endif
c
	print *,'Transformation matrix for matchvol:'
        write(*,102)((a(i,j),j=1,3),dxyz(i),i=1,3)
102     format(3f10.6,f10.3)

	filename = ' '
	if (pipinput) then
	  if (PipGetInOutFile('OutputFile', 1, ' ', filename)
     &	    .ne. 0) call errorexit('NO OUTPUT FILE SPECIFIED')
	else
	  print *,'Enter name of file to place transformation in, or ',
     &	      'Return for none'
	  read(5,'(a)')filename
	endif
	if(filename.ne.' ')then
	  call dopen(1,filename,'new','f')
	  write(1,102)((a(i,j),j=1,3),dxyz(i),i=1,3)
	  close(1)
	endif
c
	if (devmax.gt.stoplim) then
c
c	    Give some guidance based upon the ratios between max and mean
c	    deviation and max deviation and stopping limit
c	    
	  print *
	  write(*,106)devmax
106	  format(/, 'The maximum residual is',f8.2,', too high to proceed')
	  loMaxAvgRatio = 4.
	  hiMaxAvgRatio = 12.
	  loMaxLimRatio = 2.
	  hiMaxLimRatio = 3.
	  if (devmax .lt. loMaxAvgRatio * devavg .and.
     &	      devmax .lt. loMaxLimRatio * stoplim) then
	    write(*,111)loMaxAvgRatio, devavg, loMaxLimRatio
111	    format('Since the maximum residual is less than',f6.1,
     &		' times the mean residual (', f8.2,')',/,' and less than',
     &		f6.1, ' times the specified residual limit,',/,
     &		' this is probably due to distortion between the volumes,',/,
     &		' and you should probably just raise the residual limit')
	  elseif (devmax .gt. hiMaxAvgRatio * devavg .or.
     &		devmax .gt. hiMaxLimRatio * stoplim) then
	    if (devmax .gt. hiMaxAvgRatio * devavg)
     &		write(*,108)hiMaxAvgRatio, devavg
108	    format('The maximum residual is more than', f6.1,
     &		' times the mean residual (', f8.2,')')
	    if (devmax .gt. hiMaxLimRatio * stoplim)
     &		write(*,109)hiMaxLimRatio
109	    format('The maximum residual is more than', f6.1,
     &		' times the specified residual limit')
	    print *,'This is probably due to a bad correspondence list.'
	    write (*,110)iorig(ipntmax)
110	    format('Check the points (especially',i4,
     &		') or start with a subset of the list')
	  else
	    print *,'The situation is ambiguous but could be due to a',
     &		' bad correspondence list.'
	    write (*,110)iorig(ipntmax)
	  endif
	  call errorexit('MAXIMUM RESIDUAL IS TOO HIGH TO PROCEED')
	endif
	call exit(0)
        end


c	  GETDELTA returns the "delta" or pixel size from a tomogram as well
c	  as the nxyz
c	  OPTION is the option that specifies the tomogram or nx,ny,nz
c	  DELTA is returned with -1 if the option was not entered at all,
c	  0 if sizes were entered, or the actual delta value from the file
c
	subroutine getDelta(option, delta, nxyz)
	implicit none
	integer*4 nxyz(3),mxyz(3),mode,i, PipGetString
	character*(*) option
	character*80 line
	real*4 dmin,dmax,dmean,deltmp(3),delta
	logical line_is_filename
c
	delta = -1.
	if (PipGetString(option, line) .gt. 0)  return
	delta = 0.
	if (line_is_filename(line)) go to 10
	read(line,*,err=10,end=10)(nxyz(i),i=1,3)
	return
c
10	call ialprt(.false.)
	call imopen(5,line,'ro')
	call irdhdr(5,nxyz,mxyz,mode,dmin,dmax,dmean)
	call irtdel(5, deltmp)
	call imclose(5)
	call ialprt(.true.)
	delta = deltmp(1)
	return
	end


c	  getFiducials reads the fiducials from FILENAME, storing the
c	  sequential point number in ICONTA, the X,Y,Z coordinates in PNTA,
c	  and the number of points in NPNTA
c	  IDIM specifies the dimensions of the arrays
c	  AXIS specifies the axis for a message
c	  If a pixel size is found on the first line, it is returned in
c	  PIXELSIZE, otherwise this is set to 0
c
	subroutine getFiducials(filename, iconta, pnta, npnta, idim, axis,
     &	    pixelSize, nxFid, nyFid)
	implicit none
	character*(*) filename
	character*(*) axis
	character*100 line
	integer*4 iconta(*),idim,npnta,len,lnblnk,i,j,nxFid, nyFid
	real*4 pnta(3, idim), pixelSize
c
	npnta = 0
	pixelSize = 0.
	nxFid = 0
	nyFid = 0
	call dopen(1,filename,'old','f')
c	  
c	  get the first line and search for PixelSize: (first version)
c	  or Pix: and Dim: (second version)
c
	read(1,'(a)',end=15)line
	len = lnblnk(line)
	i = 1
	do while (i .lt. len - 11)
	  if (line(i:i+10) .eq. 'Pixel size:')
     &	      read(line(i+11:len),*, err=8)pixelSize
	  if (line(i:i+3) .eq. 'Pix:')
     &	      read(line(i+4:len),*, err=8)pixelSize
	  if (line(i:i+3) .eq. 'Dim:')
     &	      read(line(i+4:len),*, err=8)nxFid,nyFid
	  i = i + 1
	enddo
c	  
c	  process first line then loop until end
c
8	read(line, *)iconta(1),(pnta(j,1),j=1,3)
	go to 12
c
10	read(1,*,end=15)iconta(npnta+1),(pnta(j,npnta+1),j=1,3)
12	npnta=npnta+1
c
c	  invert the Z coordinates because the tomogram built by TILT is
c	  inverted relative to the solution produced by TILTALIGN
c
	pnta(3,npnta) = -pnta(3,npnta)
	if (npnta.ge.idim)call errorexit('TOO MANY POINTS FOR ARRAYS')
	go to 10
15	print *,npnta,' points from ', axis
	close(1)
	return
	end


	subroutine errorexit(message)
	character*(*) message
	print *
	print *,'ERROR: SOLVEMATCH - ',message
	call exit(1)
	end
