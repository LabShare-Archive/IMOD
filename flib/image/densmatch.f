* * * * * * DENSMATCH * * * * * *
c
c	  DENSMATCH scales the density values in one volume so that its mean
c	  and standard deviation match that of another volume.  To determine
c	  the mean and S.D. for each volume, it samples up to 100000 pixels in
c	  the central eighth of each volume (central half in X, Y, and Z).  It
c	  can write the scaled values either to a new file or back into the
c	  file of the volume being scaled.  It can also simply report the
c	  scaling factors without scaling the data.
c
c	  See man page for details.
c
c	  David Mastronarde, November 1995
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  
	implicit none
	integer idim, maxsamp, idimb
	parameter (idim=100000,maxsamp=100000,idimb=5000*5000)
	integer*4 nx,ny,nz
	COMMON //NX,NY,NZ
C   
	integer*4 NXYZ(3),MXYZ(3)
	real*4 ARRAY(idim),BRRAY(idimb)
	common /bigarr/brray
C   
	CHARACTER*120 FILIN,FILOUT
C   
	EQUIVALENCE (NX,NXYZ)
	real*4 avg(2),sd(2)
C   
	character dat*9,tim*8
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech
	logical report
	integer*4 nout,ierr,iun,mode,idsamp,nsx,ixs,nsy,iys,nsz,izs,ndat,jz,iz
	integer*4 jy,iy,jx,ix,i
	real*4 dmin,dmax,dmean,sem,scl,fadd,tsum,tmin,tmax,tmean
c
	logical pipinput
	integer*4 numOptArg, numNonOptArg
	integer*4 PipGetString,PipGetInteger,PipGetFloat, PipGetLogical
	integer*4 PipGetInOutFile
c	  
c	  fallbacks from ../../manpages/autodoc2man -2 2  densmatch
c
	integer numOptions
	parameter (numOptions = 5)
	character*(40 * numOptions) options(1)
	options(1) =
     &      'reference:ReferenceFile:FN:@scaled:ScaledFile:FN:@'//
     &      'output:OutputFile:FN:@report:ReportOnly:B:@help:usage:B:'
c
	filout = ' '
	report = .false.
c	  
c	  Pip startup: set error, parse options, check help, set flag if used
c
	call PipReadOrParseOptions(options, numOptions, 'densmatch',
     &	    'ERROR: DENSMATCH - ', .true., 1, 2, 1, numOptArg,
     &	    numNonOptArg)
	pipinput = numOptArg + numNonOptArg .gt. 0
c	  
c	  Get input and output files
c
	if (PipGetInOutFile('ReferenceFile', 1,
     &	    'Name of reference volume', filin) .ne. 0) call errorexit(
     &	    'NO REFERENCE FILE WAS SPECIFIED')
	call imopen(1,filin,'ro')
c
	if (PipGetInOutFile('ScaledFile', 2, 'Name volume to be scaled',
     &	    filin) .ne. 0) call errorexit(
     &	    'NO FILE WAS SPECIFIED TO BE SCALED')
	call imopen(2,filin,'old')
c
	ierr = PipGetInOutFile('OutputFile', 3, 'Name of output file,'//
     &	    ' or Return to rewrite file to be scaled', filout)
c
	nout=2
	if(filout.ne.' ')then
	  call imopen(3,filout,'new')
	  nout=3
	endif
	ierr = PipGetLogical('ReportOnly', report)
c	  
c	  sample each volume to find mean and SD
c
	do iun=1,2
	  call irdhdr(iun,nxyz,mxyz,mode,dmin,dmax,dmean)
	  if(nx*ny.ge.idimb)call errorexit('IMAGE TOO LARGE FOR ARRAYS')
	  idsamp=((float(nx*ny)*nz)/(8.*maxsamp))**.3333 + 1.
	  nsx=(nx/2)/idsamp+1
	  ixs=nx/2-0.5*nsx*idsamp
	  ixs=max(0,ixs)
	  if(nsx.eq.1)ixs=nx/2
	  nsy=(ny/2)/idsamp+1
	  iys=ny/2-0.5*nsy*idsamp
	  iys=max(0,iys)
	  if(nsy.eq.1)iys=ny/2
	  nsz=(nz/2)/idsamp+1
	  izs=nz/2-0.5*nsz*idsamp
	  izs=max(0,izs)
	  if(nsz.eq.1)izs=nz/2
	  ndat=0
	  do jz=1,nsz
	    iz=izs+(jz-1)*idsamp
	    do jy=1,nsy
	      iy=iys+(jy-1)*idsamp
	      call imposn(iun,iz,iy)
	      call irdlin(iun,array,*99)
	      do jx=1,nsx
		ix=1+ixs+(jx-1)*idsamp
		ndat=ndat+1
		brray(ndat)=array(ix)
	      enddo
	    enddo
	  enddo
	  call avgsd(brray,ndat,avg(iun),sd(iun),sem)
	  write(6,103)iun,avg(iun),sd(iun)
103	  format(' Volume',i2,': mean =',f12.4,',  SD =',f12.4)
	enddo
c	  
c	  scale second volume to match first
c
	scl=sd(1)/sd(2)
	fadd=avg(1)-avg(2)*scl
c
	if (report) then
	  write(*,102)scl, fadd
102	  format('Scale factors to multiply by then add:', 2g14.6)
	  call exit(0)
	endif
c
	if(nout.eq.3)call itrhdr(3,2)
	tsum=0.
	tmin=1.e30
	tmax=-1.e30
	do iz=1,nz
	  call imposn(2,iz-1,0)
	  call irdsec(2,brray,*99)
	  if(mode.ne.0)then
	    do i=1,nx*ny
	      brray(i)=scl*brray(i)+fadd
	    enddo
	  else
	    do i=1,nx*ny
	      brray(i)=min(255.,max(0.,scl*brray(i)+fadd))
	    enddo
	  endif
	  call iclden(brray,nx,ny,1,nx,1,ny,dmin,dmax,dmean)
	  tmin=min(tmin,dmin)
	  tmax=max(tmax,dmax)
	  tsum=tsum+dmean
	  call imposn(nout,iz-1,0)
	  call iwrsec(nout,brray)
	enddo
	tmean=tsum/nz
	call date(dat)
	call time(tim)
c
c 7/7/00 CER: remove the encodes
c
C       encode ( 80, 3000, title ) dat, tim
	write(titlech,3000) dat,tim
c        read(titlech,'(20a4)')(title(kti),kti=1,20)
	call iwrhdrc(nout,titlech,1,tmin,tmax,tmean)
	call imclose(nout)
	call exit(0)
3000	format ( 'DENSMATCH: Scaled volume to match another',t57,a9,2x,
     &	    a8)
99	call errorexit('READING FILE')
	end

	subroutine errorexit(message)
	character*(*) message
	print *
	print *,'ERROR: DENSMATCH - ',message
	call exit(1)
	end
