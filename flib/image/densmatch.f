* * * * * * DENSMATCH * * * * * *
c
c	  DENSMATCH scales the density values in one volume so that its mean
c	  and standard deviation match that of another volume.  To determine
c	  the mean and S.D. for each volume, it samples up to 100000 pixels in
c	  the central eighth of each volume (central half in X, Y, and Z).  It
c	  can write the scaled values either to a new file or back into the
c	  file of the volume being scaled.  THE LATTER WILL DESTROY THE
c	  ORIGINAL VALUES IN THAT FILE.
c	  
c	  Inputs to the program:
c	  
c	  Name of the first volume, the one whose densities are being matched
c	  
c	  Name of the second volume, the one being scaled
c	  
c	  Name of an output file, or a blank line to have the scaled values
c	  written back into the second file.
c
c
c	  David Mastronarde, November 1995
c	  
	parameter (idim=10000,maxsamp=100000,idimb=100*maxsamp)
	COMMON //NX,NY,NZ
C   
	DIMENSION NXYZ(3),MXYZ(3),NXYZST(3),
     &      ARRAY(idim),BRRAY(idimb),TITLE(20)
C   
	CHARACTER*80 FILIN,FILOUT
C   
	EQUIVALENCE (NX,NXYZ)
	real*4 avg(2),sd(2)
C   
	character dat*9,tim*8
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech
c
	write(*,'(1x,a,$)')'Name of reference volume: '
	read(5,'(a)')filin
	call imopen(1,filin,'ro')
	write(*,'(1x,a,$)')'Name of volume to be scaled: '
	read(5,'(a)')filin
	call imopen(2,filin,'old')
	print *,'Enter name of output file, or Return to place',
     &	    ' output back into second file'
	read(5,'(a)')filout
	nout=2
	if(filout.ne.' ')then
	  call imopen(3,filout,'new')
	  nout=3
	endif
c	  
c	  sample each volume to find mean and SD
c
	do iun=1,2
	  call irdhdr(iun,nxyz,mxyz,mode,dmin,dmax,dmean)
	  if(nx*ny.ge.idimb)stop 'IMAGE TOO LARGE FOR DENSMATCH'
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
	if(nout.eq.3)call itrhdr(3,2)
	scl=sd(1)/sd(2)
	fadd=avg(1)-avg(2)*scl
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
        read(titlech,'(20a4)')(title(kti),kti=1,20)
	call iwrhdr(nout,title,1,tmin,tmax,tmean)
	call imclose(nout)
	call exit(0)
3000	format ( 'DENSMATCH: Scaled volume to match another',t57,a9,2x,
     &	    a8)
99	stop 'DENSMATCH:  read error'
	end
