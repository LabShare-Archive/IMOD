c	  XFTOXG takes a list of transformations (f) from each section to
c	  the previous one, and computes a list of xforms (g) to apply to each
c	  section to obtain a single consistent set of alignments.
c	  
c	  See man page for details.
c
c	  David Mastronarde 1988; hybrid method 7/26/01
c	  
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.1  2004/01/27 03:33:12  mast
c	  Converted to PIP input, and fixed hybrid option for eliminating
c	  trends in rotation and magnification.
c	
c
	implicit none
	integer lmsc
	parameter (lmsc=100000)
C	structure /xform/
C	  real*4 a(2,2),dx,dy
C	end structure
	real*4 f(2,3,lmsc),g(2,3,lmsc),nat(2,3,lmsc) ,gav(2,3)
	real*4 natav(2,3),ginv(2,3),prod(2,3),
     &	    slope(2,3,10),intcp(2,3),natpr(2,3)
	real*4 x(lmsc),y(lmsc),slop(10)
	character*120 infil,outfil
c	  
	integer*4 nhybrid, ifshift, iorder, nlist, kl, i, ilist, kllo, klhi
	integer*4 j, ipow, nfit, ierr, lnblnk
	real*4 deltang, angdiff, bint
c
	logical pipinput
	integer*4 numOptArg, numNonOptArg
	integer*4 PipGetInteger
	integer*4 PipGetInOutFile
c	  
c         fallbacks from ../../manpages/autodoc2man -2 2  xftoxg
c	  
	integer numOptions
	parameter (numOptions = 6)
	character*(40 * numOptions) options(1)
	options(1) =
     &      'input:InputFile:FN:@goutput:GOutputFile:FN:@'//
     &      'nfit:NumberToFit:I:@order:OrderOfPolynomialFit:I:@'//
     &      'mixed:HybridFits:I:@help:usage:B:'
c	  
c	  defaults
c	  
	ifshift = 7
	nhybrid = 0
	iorder = 1
c	  
c	  initialize
c
	call PipReadOrParseOptions(options, numOptions, 'xftoxg',
     &	    'ERROR: XFTOXG - ', .true., 1, 1, 1, numOptArg,
     &	    numNonOptArg)
	pipinput = numOptArg + numNonOptArg .gt. 0

c	  
c	  get input parameters
c
	if (pipinput) then
	  ierr = PipGetInteger('NumberToFit', ifshift)
	  if (ifshift .lt. 0) call errorexit(
     &	      'A negative value for nfit is not allowed')
	  ierr = PipGetInteger('OrderOfPolynomialFit', iorder)
	  ierr = PipGetInteger('HybridFits', nhybrid)

	  if (nhybrid .ne. 0) then
	    if (ifshift .eq. 0) call errorexit(
     &		'You cannot use hybrid and global alignment together')
	    if (nhybrid .lt. 0) call errorexit(
     &		'A negative value for hybrid alignment is not allowed')
	    nhybrid=max(2,min(4,nhybrid))
	  endif
	else

	  print *,'Enter 0 to align all sections to a single'
     &	      //' average central alignment;'
	  print *,'   or 1 to align to an average alignment that shifts based'
	  print *,'         on a polynomial fit to the whole stack;'
	  print *, '   or N to align each section to an average '//
     &	      'alignment based'
	  write(*,'(1x,a,/,a,$)')
     &	      '         on a polynomial fit to the nearest N sections,',
     &	      '   or -1 or -N for a hybrid of central and shifting '//
     &	      'alignments: '
	  read(*,*)ifshift
c
	  if(ifshift.lt.0)then
	    write(*,'(1x,a,/,a,$)')'Enter # of parameters to do central'
     &		//' alignment on (2 for translation only,',
     &		' 3 for translation/rotation; 4 for trans/rot/mag: '
	    read(5,*)nhybrid
	    ifshift=abs(ifshift)
	    nhybrid=max(2,min(4,nhybrid))
	  endif
c
	  if(ifshift.gt.0)then
	    write(*,'(1x,a,$)')
     &		'Order of polynomial to fit (1 for linear): '
	    read(*,*)iorder
	  endif
	endif
	if(ifshift.gt.1)iorder=min(ifshift-1,iorder)
	iorder=max(1,min(10,iorder))
c
	if (PipGetInOutFile('InputFile', 1, 'Input file of f transforms',
     &	    infil) .ne. 0) call errorexit('NO INPUT FILE SPECIFIED')
	if (PipGetInOutFile('GOutputFile', 2, 'Output file of g transforms',
     &	    outfil) .ne. 0) then
	  i = lnblnk(infil)
	  if (infil(i-1:i) .ne. 'xf')call errorexit
     &	      ('No output file specified and input filename '//
     &	      'does not end in xf')
	  outfil = infil
	  outfil(i:i) = 'g'
	endif

c
c	  open files, read the whole list of f's
c
	call dopen(1,infil,'old','f')
	call dopen(2,outfil,'new','f')
	call xfrdall(1,f,nlist,*96)
	if (nlist .gt. lmsc) call errorexit('TOO MANY TRANSFORMS FOR ARRAYS')
C
c	  compute g's to align all sections to the first
C	
	call xfcopy(f(1,1,1),g(1,1,1))
	do i=2,nlist
	  call xfmult(f(1,1,i),g(1,1,i-1),g(1,1,i))
	enddo
c	  
c	  convert to "natural" transforms
c	  
	deltang=0.
	do kl=1,nlist
	  call amat_to_rotmag(g(1,1,kl),nat(1,1,kl),nat(1,2,kl)
     &	      ,nat(2,1,kl),nat(2,2,kl))
	  nat(1,3,kl)=g(1,3,kl)
	  nat(2,3,kl)=g(2,3,kl)
c	    
c	    if rotation angle differs greatly from last one, adjust the
c	    DELTANG value appropriately so that angles change in continuous
c	    increments around + or - 180 degrees
c
	  if(kl.gt.1)then
	    angdiff=nat(1,1,kl-1)-(nat(1,1,kl)+deltang)
	    if(abs(angdiff).gt.180.)deltang=deltang+sign(360.,angdiff)
	  endif
	  nat(1,1,kl)=nat(1,1,kl)+deltang
c	  call xfwrite(6,nat(1,1,kl),*97)
	enddo
c
c	  average natural g's, convert back to xform, take inverse of average;
c	  ginv is used if no linear fits
c
	call xfunit(natav,0.)
	do kl=1,nlist
	  call xflincom(natav,1.,nat(1,1,kl),1./nlist,natav)
	enddo
	call rotmag_to_amat(natav(1,1),natav(1,2)
     &	      ,natav(2,1),natav(2,2),gav)
	gav(1,3)=natav(1,3)
	gav(2,3)=natav(2,3)
	call xfinvert(gav,ginv)
c	  
c	  DNM 1/24/04: fixed bug in hybrid 3 or 4:
c	  do not convert natav to be the inverse!
c	  
c	  loop over each section
c
	do ilist=1,nlist
	  if(ifshift.gt.0)then
c	      if doing line fits, set up section limits for this fit
	    if(ifshift.eq.1)then
	      kllo=1				!ifshift=1: take all sections
	      klhi=nlist
	    else
	      kllo=max(1,ilist-ifshift/2)	!>1: take N sections centered
	      klhi=min(nlist,kllo+ifshift-1)	!on this one, or offset
	      kllo=max(1,min(kllo,klhi+1-ifshift))  ! DNM 1/24/04 fixed ishift
	    endif
	    if(ifshift.gt.1.or.(ifshift.eq.1.and.ilist.eq.1))then
c
c		do line fit to each component of the natural parameters
c		first time only if doing all sections, or each time for N
c
	      do i=1,2
		do j=1,3
		  nfit=0
		  do kl=kllo,klhi
		    nfit=nfit+1
		    x(nfit)=kl
		    y(nfit)=nat(i,j,kl)
		  enddo
		  call polyfit(x,y,nfit,iorder,slop,bint)
		  do ipow=1,iorder
		    slope(i,j,ipow)=slop(ipow)
		  enddo
		  intcp(i,j)=bint
		enddo
	      enddo
	    endif
	    if(ifshift.eq.1.and.ilist.eq.1)then
	      print *,'constants and coefficients of fit to',nfit
     &		  ,' natural parameters'
	      call xfwrite(6,intcp,*97)
	      do ipow=1,iorder
		call xfwrite(6,slope(1,1,ipow),*99)
99	      enddo
	    endif
c	      calculate the g transform at this position along the linear fit
c	      and take its inverse
	    call xfcopy(intcp,natpr)
	    do ipow=1,iorder
	      call xflincom(natpr,1.,slope(1,1,ipow),float(ilist)**ipow
     &		  ,natpr)
	    enddo
c	      
c	      for hybrid method, restore global average for translations,
c	      restore rotation if nhybrid is 3 or 4; restore overall mag
c	      if nhybrid is 4
c
	    if(nhybrid.gt.0)then
	      natpr(1,3)=natav(1,3)
	      natpr(2,3)=natav(2,3)
	      if(nhybrid.gt.2)natpr(1,1)=natav(1,1)
	      if(nhybrid.gt.3)natpr(2,1)=natav(2,1)
	    endif
c	      
	    call rotmag_to_amat(natpr(1,1),natpr(1,2)
     &		,natpr(2,1),natpr(2,2),prod)
	    prod(1,3)=natpr(1,3)
	    prod(2,3)=natpr(2,3)
	    call xfinvert(prod,ginv)
	  endif
c
c	    multiply this g by the inverse of the grand average or the locally
c	    fitted average: generates a xform to the common central place or
c	    to the locally fitted center.  This stuff about the linearly fitted
c	    center position is pretty ad hoc and hokey, but it seems to give
c	    good results in the final images.
c
	  call xfmult(g(1,1,ilist),ginv,prod)
	  call xfwrite(2,prod,*97)
	enddo
	close(2)
	call exit(0)
96	call errorexit('reading file')
97	call errorexit('writing file')
	end

	subroutine errorexit(message)
	character*(*) message
	print *
	print *,'ERROR: XFTOXG - ',message
	call exit(1)
	end
