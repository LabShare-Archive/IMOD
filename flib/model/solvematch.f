* * * * * * SOLVEMATCH * * * * * *
c
c	  SOLVEMATCH will solve for a 3-dimensional linear transformation
c	  relating the tomogram volumes resulting from tilt series around two
c	  different axes.  It uses information from the 3-D coordinates of
c	  fiducials found by TILTALIGN.  With this information alone, it can
c	  solve for the 3 by 3 transformation but not for the shifts in X, Y
c	  and Z.  There are two ways to get these shifts: by cross-correlation
c	  of one transformed volume with the other, and by giving SOLVEMATCH
C	  models describing corresponding points in the two tomograms.  The
c	  shell script that does correlations, MATCHSHIFTS, should be able to
c	  handle large displacements between the two volumes, so it should
c	  rarely be necessary to use models.  
c	  
c	  To use correlation, simply indicate to SOLVEMATCH whether the gold
c	  fiducials are on one or two surfaces.  If there is gold on only one
c	  surface, you also need to determine beforehand whether the two
c	  tomograms are inverted in Y (Z in flipped orientation); i.e. whether
c	  the gold is on the "top" of the section in one tomogram and the
c	  "bottom" in the other.  The program needs to know this because it
c	  introduces two dummy fiducials to fix the scaling in Y at either 1.0
c	  or -1.0.  After SOLVEMATCH is run, the shell script "matchshifts"
c	  will perform the necessary operations to find the shifts and
c	  incorporate them into the transformation.
c
c	  If you do need to create models of corresponding points, pick a set
c	  of points that can be localized well in both tomograms and enter them
c	  in the same order into a model for each tomogram.  The points can be
c	  in the same or in different objects or contours, as long as they
c	  occur in the same order in each model.  If the fiducials from
c	  TILTALIGN are not confined to one surface, then they provide nearly
c	  all of the information needed to determine the transformation, and
c	  the modeled points are needed only to determine the X, Y, and Z
c	  shift between the tomograms.  In this case, three modeled points are
c	  sufficient.  However, if fiducials are essentially on one surface,
c	  then the modeled points are needed to find the scaling in Y (Z in
c	  flipped orientation) between the two tomograms.  In this case, be
c	  sure to distribute the points in depth (rather than all on one
c	  surface) so as to provide information on this scaling.  Here, 5 or 6
c	  points are recommended.
c	  
c	  The program needs to know how the fiducial points correspond between
c	  the two tilt series.  If it is given a small list of points which do
c	  correspond, it can find the rest of the correspondences and ignore
c	  any points from either set that do not have a mate.  There are two
c	  restrictions in using this capability.  First, one must identify at
c	  least 4 initial correspondences; in fact, having at least 5 is
c	  recommended.  Second, if there are fiducials on both surfaces of the
c	  section, this initial list must include at least one from each
c	  surface.
c
c	  The program will automatically remove "outliers", pairs of points
c	  that are likely to be incorrect because their residual errors are
c	  so extreme relative to the other points.  Up to 10% of points may
c	  be removed in this way.  If, even after removing such outliers, the
c	  maximum residual in the linear fit is still higher than a value that
c	  you specify, than the program will exit with an error.
c
c	  Inputs to the program:
c	  
c	  Name of file with coordinate of fiducials from the first tilt series
c
c	  Name of file with coordinate of fiducials from the second series
c	  
c	  A list of the points in series 1 for which a corresponding point in
c	  .  series 2 is known with confidence.  Ranges may be entered
c	  .  (e.g. 1-5,7,6,-12).  Enter / if all points correspond between the
c	  .  two series.
c	  
c	  A list of the corresponding points in the second series.  The same
c	  .  number of values must be entered in this list as in the preceding
c	  .  list.  Again,  enter / if all points correspond between the
c	  .  two series.
c
c	  The values of X axis tilt that were used in generating the first and
c	  .  second tomograms.  These angles are needed to make the fiducial
c	  .  coordinates from TILTALIGN correspond to the actual positions of
c	  .  the particles in the tomograms.
c
c	  The limiting value for the maximum residual, so that the program will
c	  .  exit with an error if this value is exceeded.
c
c	  Enter either 0 to determine shifts from model files of corresponding
c	  .  points, or -1, 1, or 2 to determine just the 3 by 3 transformation
c	  .  and not the shifts.  Enter 2 if there are fiducials on two
c	  .  surfaces, 1 if fiducials are on one surface and the tomograms are
c	  .  NOT inverted relative to each other, or -1 if the tomograms are
c	  .  inverted and there are fiducials on only one surface.
c
c	  IF you entered 0, make the following four entries:
c
c	  .  Either the file name of the first tomogram, or the X, Y and Z
c	  .    dimensions and the X, Y and Z origin of that tomogram.
c	  .    (NOTE: the standard origin values put out by TILT are NX/2, 
c	  .    NY/2, and 0.)
c
c	  .  Name of model of points from the first tomogram
c	  
c	  .  Either the file name of the second tomogram, or the X, Y and Z
c	  .    dimensions and X, Y and Z origin of that tomogram.
c
c	  .  Name of model of points from the second tomogram
c	  
c	  Finally, enter name of file for output of the transformation, or
c	  .  Return for none
c
c	  The program determines the transformation and computes the deviation
c	  between each actual point in the first tomogram and the
c	  transformation of the corresponding point in the second tomogram.  It
c	  reports the mean and S.D. of these deviations, the number of the
c	  point with the highest deviation, and the magnitude of its deviation.
c	  (The modeled points are placed in the array after the fiducial
c	  points, so the highest numbers refer to these points.)
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
	integer*4 listcorra(idim),listcorrb(idim)
        integer*4 mapab(idim),nxyz(3,2),jxyz(3)/1,3,2/
        character*80 filename
        logical readw_or_imod
	integer*4 nstartmin,itry,npnta,npntb,nlist,nlista,nlistb,ndat
	integer*4 ia,nsurf,model,iftransp,nmodpt,ipt,ip,iobj,i,ndata
	integer*4 mod,j,ifadded,ipntmax,ipta,iptb,iptamin,iptbmin
	integer*4 maxdrop,ndrop,idum,iofs,ixyz
	real*4 addratio,cosa,cosb,tmpy,addcrit,distmin,devavg,devsd
	real*4 devmax,dx,dist,crit,elimmin,critabs,stoplim,xtilta,xtiltb
	real*4 sina,sinb,dy,dz
	real*4 sind,cosd
c	  
c	  don't add a point if it's this much higher than the limit for
c	  quitting
c
	addratio=1.
c	  
c	  require at least this many points to start
c
	nstartmin=4
	write(*,'(1x,a,$)') 'Name of file with 3-D fiducial'//
     &	    ' coordinates for first tilt series: '
	read(*,'(a)')filename
	call dopen(1,filename,'old','f')
c
c	  invert the Z coordinates because the tomogram built by TILT is
c	  inverted relative to the solution produced by TILTALIGN
c
	itry=0
	npnta=0
10	read(1,*,end=15)idum,(pnta(j,npnta+1),j=1,3)
	npnta=npnta+1
	pnta(3,npnta)=-pnta(3,npnta)
	go to 10
15	print *,npnta,' points from A'
	close(1)
	write(*,'(1x,a,$)') 'Name of file with 3-D fiducial'//
     &	    ' coordinates for second tilt series: '
	read(*,'(a)')filename
	call dopen(1,filename,'old','f')
	npntb=0
20	read(1,*,end=25)idum,(pntb(j,npntb+1),j=1,3)
	npntb=npntb+1
	pntb(3,npntb)=-pntb(3,npntb)
	go to 20
25	print *,npntb,' points from B'
	close(1)
c	  
	do i=1,npnta
	  listcorra(i)=i
	  mapab(i)=0
	enddo
	do i=1,npntb
	  listcorrb(i)=i
	  mapped(i)=0
	enddo
	nlist=min(npnta,npntb)
	write (*,113)nlist
113	format('Enter a list of points in the first series for which',
     &	    ' you are sure of the',/,' corresponding point in the',
     &	    ' second series (Ranges are OK;',/' enter / if points 1-',
     &	    i3, ' are in one-to-one correspondence between the series')
	nlista=nlist
	call rdlist(5,listcorra,nlista)
	if(nlista.lt.nstartmin)then
	  print *,'SOLVEMATCH NEEDS AT LEAST',nstartmin,
     &	      ' POINTS TO GET STARTED'
	  call exit(1)
	endif
	if(nlista.gt.nlist)then
	  print *,'SOLVEMATCH: YOU HAVE ENTERED MORE NUMBERS THAN THE',
     &	      ' MINIMUM NUMBER OF POINTS IN A AND B'
	  call exit(1)
	endif
	write (*,114)nlista
114	format('Enter a list of the corresponding points in the',
     &	    ' second series ',/,' (enter / for 1-', i3, ')')
	nlistb=nlista
	call rdlist(5,listcorrb,nlistb)
	if(nlistb.ne.nlista)then
	  print *,'SOLVEMATCH: YOU MUST HAVE THE SAME NUMBER OF '//
     &	      'ENTRIES IN EACH LIST','YOU MADE',nlista,' AND',nlistb,
     &	      'ENTRIES FOR LISTS A AND B'
	  call exit(1)
	endif
c	  
c	  check legality and build map lists
c	  
	do i=1,nlista
	  if(listcorra(i).le.0.or.listcorrb(i).le.0)then
	    print *,'SOLVEMATCH: YOU ENTERED AN POINT NUMBER LESS '//
     &		'THAN OR EQUAL TO ZERO'
	    call exit(1)
	  elseif(listcorra(i).gt.npnta)then
	    print *,'SOLVEMATCH: YOU ENTERED A POINT NUMBER HIGHER'//
     &		' THAN THE NUMBER OF POINTS IN A'
	    call exit(1)
	  elseif(listcorrb(i).gt.npntb)then
	    print *,'SOLVEMATCH: YOU ENTERED A POINT NUMBER HIGHER'//
     &		' THAN THE NUMBER OF POINTS IN B'
	    call exit(1)
	  elseif(mapab(listcorra(i)).ne.0)then
	    print *,'SOLVEMATCH: POINT #',listcorra(i),
     &		' IN A REFERRED TO TWICE'
	    call exit(1)
	  elseif(mapped(listcorrb(i)).ne.0)then
	    print *,'SOLVEMATCH: POINT #',listcorrb(i),
     &		' IN B REFERRED TO TWICE'
	    call exit(1)
	  endif
	  mapab(listcorra(i))=listcorrb(i)
	  mapped(listcorrb(i))=listcorra(i)
	enddo
c	  
	write(*,'(1x,a,$)')'Tilts around the X-axis applied in '//
     &	    'generating tomograms A and B: '
	read(5,*)xtilta,xtiltb
c	  
c	  use the negative of the angle to account for the inversion of the
c	  tomogram; rotate the 3-d points about the X axis
c
	cosa=cosd(-xtilta)
	sina=sind(-xtilta)
	do i=1,npnta
	  tmpy=cosa*pnta(2,i)-sina*pnta(3,i)
	  pnta(3,i)=sina*pnta(2,i)+cosa*pnta(3,i)
	  pnta(2,i)=tmpy
	enddo
	cosb=cosd(-xtiltb)
	sinb=sind(-xtiltb)
	do i=1,npntb
	  tmpy=cosb*pntb(2,i)-sinb*pntb(3,i)
	  pntb(3,i)=sinb*pntb(2,i)+cosb*pntb(3,i)
	  pntb(2,i)=tmpy
	enddo
c	  
	write(*,'(1x,a,/,a,$)')'Maximum residual value above which '//
     &	    'this program should',' exit with an error: '
	read(5,*)stoplim
c       
c       fill array for regression
c
        ndat=0
        do ia=1,npnta
          if(mapab(ia).ne.0)then
            ndat=ndat+1
            do j=1,3
              xr(j,ndat)=pntb(jxyz(j),mapab(ia))
              xr(j+5,ndat)=pnta(jxyz(j),ia)
            enddo
            xr(4,ndat)=1.
	    iorig(ndat)=ia
	  endif
        enddo
c
        print *,ndat,' pairs of fiducial points'
	write(*,'(1x,a,/,a,/,a,/,a,$)')'Enter 0 to solve for '//
     &	    'displacements using matching model files, or -1, 1 or 2'
     &	    ,'  to solve only for 3x3 matrix (2 if fiducials are on 2'//
     &	    ' surfaces, 1 if they are',
     &	    '  on one surface and tomograms are NOT inverted,'//
     &	    ' or -1 if fiducials ARE on one','  surface'
     &	    //' and tomograms are inverted relative to each other): '
	read(5,*)nsurf
	if(nsurf.eq.0)then
	  do model=1,2
	    write(*,'(1x,a,i2,a)')'Enter NX, NY, NZ and X, Y, Z origin of '//
     &		'tomogram', model, ', or name of tomogram file'
	    call get_nxyz_org(1,nxyz(1,model),orig(1,model))
	    print *,(nxyz(i,model),i=1,3)
81	    write(*,'(1x,a,i2,a,$)')
     &		'Name of model file from tomogram',model,': '
	    read(*,'(a)')filename
	    if(.not.readw_or_imod(filename))go to 81
c	      write(*,'(1x,a,$)') '1 if Y and Z were flipped when '//
c	      &	      'building that model (loaded -Y), or 0 if not: '
c	      read(5,*)iftransp
	    iftransp=0
	    if(iftransp.eq.0)then
	      jxyz(2)=2
	      jxyz(3)=3
	    else
	      jxyz(2)=3
	      jxyz(3)=2
	    endif
	    nmodpt=0
	    iofs=10-5*model
	    do iobj=1,max_mod_obj
	      do ip=1,npt_in_obj(iobj)
		ipt=abs(object(ibase_obj(iobj)+ip))
		nmodpt=nmodpt+1
		do i=1,3
		  xr(i+iofs,ndat+nmodpt)=p_coord(jxyz(i),ipt)+
     &		      orig(i,model)- 0.5*nxyz(i,model)
		enddo
		xr(4,ndat+nmodpt)=0.
		iorig(ndat+nmodpt)=npnta+nmodpt
	      enddo
	    enddo
	    if(model.eq.1)ndata=nmodpt
	  enddo
	  if(nmodpt.ne.ndata)then
	    print *, 'SOLVEMATCH: '//
     &		'# OF POINTS DOES NOT MATCH BETWEEN MATCHING MODELS'
	    call exit(1)
	  endif
	  print *,nmodpt,' point pairs from models'
	else
	  nmodpt=1
	  if(abs(nsurf).eq.1)nmodpt=2
	  do mod=1,nmodpt
	    do j=1,8
	      xr(j,ndat+mod)=0.
	    enddo
	    iorig(ndat+mod)=npnta+mod
	  enddo
	  if(abs(nsurf).eq.1)then
	    xr(2,ndat+1)=-100.
	    xr(7,ndat+1)=-sign(100,nsurf)
	    xr(2,ndat+2)=100.
	    xr(7,ndat+2)=sign(100,nsurf)
	  endif
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
	do while(ndat-nmodpt.lt.min(npnta,npntb).and.distmin.lt.addcrit)
	  call do3multr(xr,ndat,4,ndat,a,dxyz,cenloc, devavg,devsd,
     &	      devmax,ipntmax, devxyzmax)
	  distmin=1.e10
c	    
c	    apply to each point in B that is not mapped to
c
	  do iptb=1,npntb
	    if(mapped(iptb).eq.0)then
	      do ixyz=1,3
		ptrot(ixyz)=dxyz(ixyz)+a(ixyz,4)
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
              xr(j+5,ndat)=pnta(jxyz(j),iptamin)
            enddo
            xr(4,ndat)=1.
	    iorig(ndat)=iptamin
	  endif
	enddo
	if(ifadded.ne.0)then
	  print *,'The final list of correspondences used for fits is:'
	  call wrlist(mapab,npnta)
	endif
c	write(*,105)((xr(i,j),i=1,4),(xr(i,j),i=6,8),j=1,ndat)
105	format(7f9.2)
	maxdrop=nint(0.1*ndat)
	crit=0.01
	elimmin=3.
	critabs=0.002
	call solve_wo_outliers(xr,ndat,4,maxdrop,crit,critabs,elimmin,
     &	    idrop,ndrop, a,dxyz,cenloc, devavg,devsd,devmax,ipntmax,
     &	    devxyzmax)
c
	if(ndrop.ne.0)then
	  write(*,104)ndrop,devavg,devsd,(iorig(idrop(i)),i=1,ndrop)
	  write(*,115)(xr(5,i),i=ndat+1-ndrop,ndat)
104	  format(i3,' points dropped by outlier elimination; ',
     &	      'residual mean =',f7.2,', SD =',f7.2,/,
     &	      ' point-pairs:',(11i6))
115	  format(' deviations :',(11f6.1))
	endif
c
        write(*,101)devavg,devmax,iorig(ipntmax),(devxyzmax(i),i=1,3)
101     format(/,' Mean residual',f8.3,',  maximum',f8.3,
     &      ' at point-pair',i4,/,'  Deviations:',3f8.3)
c
	write(*,103)(a(i,4),i=1,3)
103	format(/,' X, Y, Z offsets for fiducial dummy variable:',3f10.3)
	print *,'Transformation matrix for matchvol:'
        write(*,102)((a(i,j),j=1,3),dxyz(i),i=1,3)
102     format(3f10.6,f10.3)
	print *,'Enter name of file to place transformation in, or ',
     &	    'Return for none'
	read(5,'(a)')filename
	if(filename.ne.' ')then
	  call dopen(1,filename,'new','f')
	  write(1,102)((a(i,j),j=1,3),dxyz(i),i=1,3)
	  close(1)
	endif
	if (devmax.gt.stoplim) then
	  print *, 'MAXIMUM RESIDUAL IS TOO HIGH TO PROCEED'
	  call exit(1)
	endif
	call exit(0)
        end

c	  GET_NXYZ_ORG accepts a line of input and attempts to read it as 3
c	  integers and 3 reals.  If this succeeds, the values are returned as
c	  NXYZ and ORIG. If this generates an error, it then uses the line as
c	  a filename and tries to open an MR file on unit IUNIT.  NXYZ and
c	  ORIG are then fetched from the header.
c
	subroutine get_nxyz_org(iunit,nxyz,orig)
	implicit none
	integer*4 nxyz(3),mxyz(3),iunit,mode,i
	real*4 orig(3),dmin,dmax,dmean
	character*80 line
	read(5,'(a)')line
	if(line(1:1).eq.'/')go to 10
	read(line,*,err=10)(nxyz(i),i=1,3),(orig(i),i=1,3)
	return
10	call ialprt(.false.)
	call imopen(iunit,line,'ro')
	call irdhdr(iunit,nxyz,mxyz,mode,dmin,dmax,dmean)
	call irtorg(iunit,orig(1),orig(2),orig(3))
	call imclose(iunit)
	call ialprt(.true.)
	return
	end
