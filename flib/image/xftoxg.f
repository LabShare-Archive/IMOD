c   XFTOXG takes a list of transformations (f) from each section to
c   the previous one, and computes a list of xforms (g) to apply to each
c   section to obtain a single consistent set of alignments.

c   Three different modes of operation can be selected.	 In the first, all
c   sections will be transformed to a single position in the "center" of
c   the series.	 Here "position" refers to the entire set of parameters
c   describing the transformation, i.e. not just the x,y coordinates but
c   the rotation and stretch as well.  This mode will remove any consistent
c   trends in the data stack, such a progressive rotation, compression or
c   shift.

c   In the second mode, the "center" position is assumed to change
c   progressively over the whole series, based on a polynomial fit to the
c   series, and a transformation will be computed for each section that will
c   align it to a fitted shifting center position.  If a polynomial order of 1
c   is selected, the center position will shift linearly from one end to the
c   other of the data stack.  This mode will retain trends in the data stack
c   to the extent that they can be fit by the order of polynomial chosen.  For
c   example, if order 1 is chosen, the fit is a linear one, and the resulting
c   transforms will retain trends that occur at a constant rate from one end
c   to the other.

c   In the third mode, one defines a distance (number of sections) over which
c   to fit a polynomial to a shifting center position.  For each section, the
c   program fits polynomials to the transformation parameters over the
c   selected distance (centered on the given section, if possible), and uses
c   these polynomial fits to define the position to which that section should
c   be transformed.  This mode will retain trends in the data stack that occur
c   over the range of the specified distance.
c	  
c   It is also possible to do a hybrid alignment in which trends are retained
c   for some parameters while they are eliminated for others.  Trends will
c   be eliminated for X and Y translations, and can optionally be eliminated
c   for rotations and overall size changes.  The most appropriate choice would
c   be to eliminate trends for translations and rotations, unless the rotation
c   trends in the data are meaningful and should be preserved.  This hybrid
c   method can be used with either local or global polynomial fits (the second
c   or third mode of operation).
c
c   To do the averaging and fitting, the program converts the A matrix of the
c   g transforms to the "semi-natural" parameters: global rotation and
c   magnification, and differences between Y and X axis rotation and stretch.
c   This conversion allows the hybrid alignments to be done.  It also means
c   that if input transformations involve only a restricted set of these 
c   parameters (e.g., rotation only), this restriction will be retained in the
c   final g transforms.
c
c   The ability to use both the local fits and fits of order higher than 1 is
c   somewhat redundant.  If local fitting is used, start with order 1 and go
c   to higher orders only if necessary.
c	  
c   Inputs to the program:
c   
c   0 to align all sections to a single average central alignment, 1 to align
c   to an average alignment fit to a single polynomial, N > 1 to align each
c   section to an average alignment fit to the nearest N sections, or -1 or
c   -N to do a hybrid or central and shifting alignments with fits to a single
c   polynomial or to locally fitted polynomials
c	  
c   IF you entered a negative number, next enter 2 for central alignment of
c   translations only, 3 for central alignment of translations and rotations,
c   or 4 for central alignment of translations, rotations and size changes
c	  
c   IF you entered a nonzero number to the first query, next enter the order
c   of the polynomials to fit (1 for linear fits)
c	  
c   Name of input file of F transforms
c	  
c   Name of output file of G transforms
c
c   David Mastronarde 1988; hybrid method 7/26/01
c
	parameter (lmsc=1000)
C	structure /xform/
C	  real*4 a(2,2),dx,dy
C	end structure
	real*4 f(2,3,lmsc),g(2,3,lmsc),nat(2,3,lmsc) ,gav(2,3)
	real*4 natav(2,3),ginv(2,3),prod(2,3),
     &	    slope(2,3,10),intcp(2,3),natpr(2,3)
	real*4 x(lmsc),y(lmsc),slop(10)
	character*80 infil,outfil
c	  
c	  get input parameters
c
	print *,'Enter 0 to align all sections to a single'
     &	    //' average central alignment;'
	print *,'   or 1 to align to an average alignment that shifts based'
	print *,'         on a polynomial fit to the whole stack;'
	print *,
     &	    '   or N to align each section to an average alignment based'
	write(*,'(1x,a,/,a,$)')
     &	    '         on a polynomial fit to the nearest N sections,',
     &	    '   or -1 or -N for a hybrid of central and shifting '//
     &	    'alignments: '
	read(*,*)ifshift
c
	nhybrid=0
	if(ifshift.lt.0)then
	  write(*,'(1x,a,/,a,$)')'Enter # of parameters to do central'
     &	      //' alignment on (2 for translation only,',
     &	      ' 3 for translation/rotation; 4 for trans/rot/mag: '
	  read(5,*)nhybrid
	  ifshift=abs(ifshift)
	  nhybrid=max(2,min(4,nhybrid))
	endif
c
	if(ifshift.gt.0)then
	  write(*,'(1x,a,$)')
     &	      'Order of polynomial to fit (1 for linear): '
	  read(*,*)iorder
	  if(ifshift.gt.1)iorder=min(ifshift-1,iorder)
	  iorder=max(1,min(10,iorder))
	endif
c
	write(*,'(1x,a,$)')'Input file of f transforms: '
	read(*,'(a)')infil
	write(*,'(1x,a,$)')'Output file of g transforms: '
	read(*,'(a)')outfil
c
c	  open files, read the whole list of f's
c
	call dopen(1,infil,'old','f')
	call dopen(2,outfil,'new','f')
	call xfrdall(1,f,nlist,*96)
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
	call amat_to_rotmag(ginv,natav(1,1),natav(1,2)
     &	      ,natav(2,1),natav(2,2))
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
	      kllo=max(1,min(kllo,klhi+1-ishift))
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
96	print *,'error reading file'
	call exit(0)
97	print *,'error writing file'
	call exit(0)
	end

