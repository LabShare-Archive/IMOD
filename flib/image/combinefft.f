* * * * * * COMBINEFFT * * * * * *
c
c	  COMBINEFFT combines the FFTs from the two tomograms of a double-axis
c	  tilt series, taking into account the tilt range of each tilt series
c	  and the transformation used to match one tomogram to the other.  For
c	  a location in Fourier space where there is data from one tilt series
c	  but not the other, it takes the Fourier value from just the one
c	  appropriate FFT; everywhere else it averages the Fourier values from
c	  the two FFT files.
c	  This program is a successor to Jim Kremer's ADDTOMFFT.
c	  
c	  Inputs to the program:
c	  
c	  File name of FFT of first tomogram (the one matched TO)
c	  
c	  File name of FFT of second tomogram (the one produced by MATCHVOL)
c	  
c	  Name of output file for resulting FFT, or Return to write into the
c	  .   file of the second FFT, overwriting that FFT.
c	  
c	  Name of file with inverse of transformation used to match the two
c	  .   tomograms (output by MATCHVOL)
c	  
c	  For the first tomogram file, either the starting and ending tilt
c	  .  angles, or the name of a file with tilt angles in it.  In the
c	  .  latter case, the first number on the first line will be taken as
c	  .  the starting tilt angle; the first number on the last line will
c	  .  be taken as the ending tilt angle.
c	  
c	  For the second tomogram file, either the starting and ending tilt
c	  .  angles, or the name of a file with tilt angles in it.
c
c
c	  David Mastronarde, November 1995
c
	parameter (idim=5000*400)
	parameter (numlook=1600)
	COMMON //NX,NY,NZ
C   
	DIMENSION NXYZ(3),MXYZ(3),NXYZST(3),TITLE(20)
	complex ARRAY(idim),BRRAY(idim),crray(idim)
C   
	real*4 minv(3,3)
	CHARACTER*80 FILIN,FILOUT
C   
	EQUIVALENCE (NX,NXYZ)
	character dat*9,tim*8
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech

	logical ina,inb
c	  
	write(*,'(1x,a,$)')'Name of first tomogram FFT file: '
	read(5,'(a)')filin
	call imopen(1,filin,'ro')
	write(*,'(1x,a,$)')'Name of second tomogram FFT file: '
	read(5,'(a)')filin
	call imopen(2,filin,'old')
	write(*,'(1x,a,$)')
     &	    'Name of output file, or Return to put in 2nd file: '
	read(5,'(a)')filout
	call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
	call irdhdr(2,nxyz,mxyz,mode,dmin,dmax,dmean)
	if(nx*ny.ge.idim)stop 'IMAGE TOO LARGE FOR COMBINEFFT'
	iunout=2
	if(filout.ne.' ')then
	  call imopen(3,filout,'new')
	  iunout=3
	  call itrhdr(3,1)
	endif
	write(*,'(1x,a,$)')
     &	    'File with inverse of matching transformation: '
	read(5,'(a)')filin
	call dopen(1,filin,'ro','f')
	do i=1,3
	  read(1,*)(minv(i,j),j=1,3)
	enddo
	close(1)
c
	print *,'For first tomogram file:'
	call gettilts(acritlo,acrithi)
	print *,'For second tomogram file:'
	call gettilts(bcritlo,bcrithi)
c
	tsum=0.
	tmin=1.e30
	tmax=-1.e30
	delx=0.5/(nx-1.)
	dely=1./ny
	delz=1./nz
c
	do iz=1,nz
	  call irdsec(1,array,*99)
	  call imposn(2,iz-1,0)
	  call irdsec(2,brray,*99)
	  ind=1
c	    
c	    this assumes Z has been reordered to be sequential in 3-D FFT
c
	  za=delz*(iz-1.)-0.5
	  do iy=1,ny
c	    
c	    this assumes Y has been reordered to be sequential in 2-D FFT
c
	    ya=dely*(iy-1.)-0.5
	    yasq=ya**2
	    do ix=1,nx
	      xa=delx*(ix-1.)
	      xasq=xa**2
c		
c		back transform this position to get vector in fft b
c		
	      xp=minv(1,1)*xa+minv(1,2)*ya+minv(1,3)*za
	      yp=minv(2,1)*xa+minv(2,2)*ya+minv(2,3)*za
	      if(xp.lt.0.)then
		xp=-xp
		yp=-yp
	      endif
c		
	      rata=ya/max(xa,1.e-6)
	      ratb=yp/max(xp,1.e-6)
	      ina=rata.ge.acritlo.and.rata.le.acrithi
	      inb=ratb.ge.bcritlo.and.ratb.le.bcrithi
c	      array(ind)=(0.,0.)
	      if(.not.ina.and..not.inb)then
c		  
c		  if in neither, take simple mean
c
		array(ind)=0.5*(array(ind)+brray(ind))
c		array(ind)=(1.,0.)
	      elseif(.not.ina.and.inb)then
c		  
c		  if in B alone, take b's value
c		  
		array(ind)=brray(ind)
c		array(ind)=(2.,0.)
	      elseif(ina.and.inb)then
c		  
c		  in both: need to mix in selected way
c		  
		wa=0.5
		wb=0.5
c		array(ind)=(3.,0.)
		array(ind)=wa*array(ind)+wb*brray(ind)
c		  
c		    else if in A and not in B, leave A value as is
c
	      endif
	      ind=ind+1
	    enddo
	  enddo
	  call iclcdn(array,nx,ny,1,nx,1,ny,dmin,dmax,dmean)
	  tmin=min(tmin,dmin)
	  tmax=max(tmax,dmax)
	  tsum=tsum+dmean
	  call imposn(iunout,iz-1,0)
	  call iwrsec(iunout,array)
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
	call iwrhdr(iunout,title,1,tmin,tmax,tmean)
	call imclose(iunout)
	call exit(0)
3000	format ( 'COMBINEFFT: Combined FFT from two tomograms',t57,a9,
     &	    2x,a8)
99	stop 'read error'
	end


	subroutine gettilts(critlo,crithi)
c	  
	character*60 line
c
	print *,'Enter either the starting and ending tilt angles,',
     &	    ' or the name of a file with tilt angles in it'
	read(5,'(a)')line
	tilthi=-9999.
	read(line,*,err=10)tiltlo,tilthi
	go to 20
10	call dopen(1,line,'ro','f')
	read(1,*)tiltlo
15	read(1,*,end=20)tiltin
	tilthi=tiltin
	go to 15
20	if(tilthi.eq.-9999.)stop 'bad tilt file'
c	  
c	  INVERT ANGLES BECAUSE TILT PROGRAM IS WEIRD
c
	critlo=tand(min(-tiltlo,-tilthi))
	crithi=tand(max(-tiltlo,-tilthi))
	close(1)
	end
