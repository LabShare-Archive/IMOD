* * * * * SUBIMSTAT * * * * *
c	  
c	  SUBIMSTAT subtracts one average image from another and uses
c	  standard deviation or variance images to find the statistical
c	  significance of the difference at each pixel, as evaluated by a t-
c	  statistic.  It then sets to zero all differences that are less
c	  significant than the specified level of significance.  It can also
c	  output pixel values reflecting the level of significance rather than
c	  the difference.
c	  
c	  The average and standard deviation/variance images can be ones
c	  produced by by IMAVGSTAT or by other means.  When one starts the
c	  program, one designates a pair of A files (with average and S.D./
c	  variance images) and a pair of B files.  One can then subtract any
c	  section in B from any section in A; A and B may be the same pair of
c	  files.
c	  
c	  The user is responsible for keeping track of how many samples were
c	  used in making each average, and informing this program of those
c	  numbers.  The program needs these numbers to compute the t-statistic
c	  and evaluate its significance.
c	  
c	  Entries to the program:
c	  
c	  Average image file A
c	  Standard deviation or variance image file A
c	  Average image file B, or Return if same as file for A
c	  Standard deviation or variance image file B, or Return if same as
c	  .  file for A
c	  Output image file to store differences in
c	  
c	  0 if the files have standard deviations in them, or 1 if the files
c	  .  have variances
c
c	  Number of differences to compute
c	  
c	  For each difference, enter:
c
c	  .  Section # in file A, section # in file B
c
c	  .  Number of samples making up those averages in A and B
c
c	  .  Significance level for setting differences to zero.  Enter a
c	  .     probability value such as 0.05, 0.01, etc.  Differences less
c	  .     significant than this value will be set to zero.  Enter a
c	  .     negative value to have significant pixels values set to 
c	  .     the negative of the log of the probability, or to the positive
c	  .     log for negative differences.  For example, positive and
c	  .     negative differences with a P of 0.01 would be output as
c	  .     2 and -2, respectively.
c
c	  David Mastronarde  1/27/90
c	  4/23/90 - have program look up criterion t value.
c	  4/12/95 - use local subroutines for t-value
c	  Must be linked with plib:imlib/l,hvemlib/l
c
	parameter (maxarr=1024*1024)

	real	array(maxarr), brray(maxarr),asdray(maxarr),
     &	    bsdray(maxarr), crray(maxarr), title(20), cell(6)
	integer	nxyz(3), mxyz(3), nxyzst(3), asec, bsec
	equivalence (nxyz,nx)
	common//nx,ny,nz
	character*80 afile, bfile,asdfile,bsdfile, cfile
	character dat*9, tim*8
c
c 7/7/00 CER: remove the encode's; titlech is the temp space
c
        character*80 titlech
c
	data nxyzst/0,0,0/
	data cell /0.,0.,0.,90.,90.,90./
	real*8 g01caf
	tprob(ndf,t)=1.-betai(0.5*ndf,0.5,ndf/(ndf+t**2))/2.

c	---------------------------------
c	--- initialize and input info ---

	print *,'This program will subtract a section of file B',
     &	    ' from a section of file A,',' retaining only',
     &	    ' differences that are statistically significant'
	write (*,'(1x,a,$)')'Name of average file A : '
	read  (*, '( a )' )	afile
	write (*,'(1x,a,$)')
     &	    'Name of standard deviation or variance file A : '
	read  (*, '( a )' )	asdfile
	write (*,'(1x,a,$)')
     &	    'Name of average file B (Return if same as A): '
	read  ( *, '( a )' )	bfile
	write (*,'(1x,a,$)')
     &	    'Name of SD or var file B (Return if same as A): '
	read  ( *, '( a )' )	bsdfile


	write ( *,'(1x,a,$)')'Name of file to place differences in: '
	read  ( *, '( a )' )	cfile

	call imopen(1,afile,'ro')
	call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
	if (nx*ny .gt. maxarr)stop '- IMAGES TOO BIG FOR ARRAYS'
	call imopen(2,asdfile,'ro')
	call irdhdr(2,nxyz,mxyz,mode,dmin,dmax,dmean)
c
	if(bfile.ne.' ')then
	  call imopen(3,bfile,'ro')
	  imbavg=3
	  call irdhdr(3,nxyz,mxyz,mode,dmin,dmax,dmean)
	else
	  imbavg=1
	endif
c
	if(bsdfile.ne.' ')then
	  call imopen(4,bsdfile,'ro')
	  imbsd=4
	  call irdhdr(4,nxyz,mxyz,mode,dmin,dmax,dmean)
	else
	  imbsd=2
	endif
c	  
	call imopen(5,cfile,'new')
	call itrhdr(5,1)
c	  
	write(*,'(1x,a,$)')
     &	    '0 if files have SDs, 1 if they have variances: '
	read(5,*)ifvarnc
c
	write(*,'(1x,a,$)')'Number of differences to compute: '
	read(*,*)nz
	dmin=1.e10
	dmax=-1.e10
	dsum=0.
c	  
	do idiff=1,nz
	  write(*,'(1x,a,$)')
     &	      'Section #''s for A and B, to compute (A-B): '
	  read(*,*)asec,bsec
c	    
	  write(*,'(1x,a,$)')
     &	      'Number of samples in averages for A and B: '
	  read(*,*)nsa,nsb
c
	  ntdf=nsa+nsb-2
	  write(*,'(1x,a,$)')'Significance level (e.g. 0.05) for '//
     &	      'setting differences to zero: '
	  read(*,*)psignif
	  ifsigout=0
	  if(psignif.lt.0)then
	    ifsigout=1
	    psignif=-psignif
	  endif
	  ifail=0
c	  tcrit=g01caf(dble(1.-psignif),ntdf,ifail)
	  tcrit=tvalue(1.-psignif,ntdf)  
	  write(*,'(a,f8.3,a,i5,a)')' T criterion is',tcrit,' with',
     &	      ntdf,' degrees of freedom'
c
	  call imposn(1,asec,0)
	  call irdsec(1,array,*99)
	  call imposn(2,asec,0)
	  call irdsec(2,asdray,*99)
	  call imposn(imbavg,bsec,0)
	  call irdsec(imbavg,brray,*99)
	  call imposn(imbsd,bsec,0)
	  call irdsec(imbsd,bsdray,*99)
	  if(ifvarnc.eq.0)then
	    print *,'squaring arrays'
	    do i=1,nx*ny
	      asdray(i)=asdray(i)**2
	      bsdray(i)=bsdray(i)**2
	    enddo
	  endif
c	    
c	    ooh- this is pretty convoluted now.  The test was whether
c	    abs of diff / sqrt (varfac * (nsam1*sd1**2+nsam2*sd2**2) was
c	    less than tcrit.  So instead, see if diff**2 is .lt. tcrit**2
c	    times the denominator (the argument of the sqrt)  Quicker, doesn't
c	    blow up.
c	    
	  varfac=(1./nsa+1./nsb)/ntdf
	  tcritfac=tcrit**2*varfac
	  nsam1=nsa-1
	  nsbm1=nsb-1
	  do i=1,nx*ny
	    diff=array(i)-brray(i)
	    denomcrit=tcritfac*(nsam1*asdray(i)+nsbm1*bsdray(i))
	    if(diff**2.lt.denomcrit)then
	      diff=0.
	    elseif(ifsigout.eq.1)then
	      denom=sqrt(max(0.,
     &		  varfac*(nsam1*asdray(i)+nsbm1*bsdray(i))))
	      absdiff=abs(diff)
	      tt=absdiff/(max(0.05*absdiff,denom))
	      pval=max(0.000001,1.-tprob(ntdf,tt))
	      diff=sign(alog10(pval),diff)
	    endif
	    crray(i)=diff
	  enddo
c
	  call iclden(crray,nx,ny,1,nx,1,ny,tmin,tmax,tmean)
	  dmin=min(dmin,tmin)
	  dmax=max(dmax,tmax)
	  dsum=dsum+tmean
c	    
	  call iwrsec(5,crray)
	enddo
c
	cell(1) = float(nx)
	cell(2) = float(ny)
	cell(3) = float(nz)
	call ialsiz(5,nxyz,nxyzst)
	call ialsam(5,nxyz)
	call ialcel(5,cell)
	call date(dat)
	call time(tim)
c
c 7/7/00 CER: remove the encodes
c
c       encode ( 80, 3000, title ) dat, tim
        write(titlech,3000) dat,tim
        read(titlech,'(20a4)')(title(kti),kti=1,20)
	dmean=dsum/nz
	call iwrhdr(5,title,1,dmin,dmax,dmean)

	call imclose(5)

	call exit(0)
99	stop 'error reading file'
3000	format ( 'SUBIMSTAT: Subtract section B from section A.',t57,
     &	    a9, 2x, a8 )

	end
