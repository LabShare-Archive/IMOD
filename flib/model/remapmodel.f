*	  * * * * * REMAPMODEL * * * * *
*	  
c	  This program allows one to remap coordinates in a model,
c	  in two ways:
c	  1) The set of Z values may be mapped, one-to-one, to any arbitrary
c	  new set of Z values; and
c	  2) The X, Y or Z coordinates may be shifted by a constant.
c	  
c	  The mapping of Z values is done by first converting converting each Z
c	  value to the nearest integer, looking up the new integral Z value
c	  that you wish to map that integer to, then shifting the actual Z
c	  value by the difference betweenthe new and the original integral
c	  value.
c	  
c	  ENTRIES:
c	  
c	  Name of input model file
c	  
c	  Name of output model file
c	  
c	  New list of Z values.  Using ranges (e.g. 0-3,5-7), specify a list 
c	  .  of integers with the same number of Z values as in the input list.
c	  .  The Z values in the input list will be mapped one-to-one onto the
c	  .  new list.  Enter "/" to take the input list without modification.
c	  .  Enter numbers between -999 and -990 to delete points with a
c	  .  particular Z value; e.g. if the input model has Z values from 0
c	  .  to 9, entering 0-4,-999,5-8 will delete all points with Z between
c	  .  4.5 and 5.5, and shift the Z of points with Z greater then 5.5
c	  .  down by 1.  If the input model has Z from 0-19, entering
c	  .  0-9,-999--990 will remove all points with Z from 10 to 19.
c	  .  Enter -1 to replace each Z value by its negative.
c	  
c	  Amounts to add to all X, Y, and Z coordinates.  These values will be
c	  .  added after the remapping of Z values, if any.  Values should be
c	  .  in units of pixels (image index coordinates).
c	  
c	  IF your new list of Z values is not in monotonic order, enter 1 to
c	  .  have the program reorder the points within each object so that
c	  .  they occur with monotonically changing Z.  This option would be
c	  .  used if you wanted to switch two sections within a stack.
c
c	  David Mastronarde  5/8/89
c	  DNM 7/20/89  changes for new model format
c	  DNM 2/20/90  changes to negate Z and reorder by Z
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.2  2002/05/22 23:44:10  mast
c	  *** empty log message ***
c	
c	  Revision 3.1  2002/05/20 15:51:09  mast
c	  Made sure the elimination of Z values did not confuse it into 
c	  thinking the new Z values were out of order.
c	
c
	parameter (limsec=1000)
	integer*4 listz(limsec),newlist(limsec),indxzlst(50*limsec)
	include 'model.inc'
	integer*4 izlist(max_pt)
c
	character*80 modelfile,newmodel
	logical exist,readw_or_imod
	integer*4 getimodhead,getimodscales
c	  
	integer*4 ninzlis,iobj,indobj,ipnt,nlistz,i,nnew
	integer*4 ifreorder,ninobj,ibase,izval,newzval,indmov,itmp
	integer*4 lastkeep,idir,idirfirst,inorder
	real*4 xadd,yadd,zadd
c
91	write(*,'(1x,a,$)')'Name of input model file: '
	read(*,'(a)')modelfile
c
c	  read in the model
c
75	exist=readw_or_imod(modelfile)
	if(.not.exist)go to 91
c
	write(*,'(1x,a,$)')'Name of output model file: '
	read(*,'(a)')newmodel
c	  
c	  scale to index coordinates
c
	ierr = getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip)
	ierr = getimodscales(ximscale, yimscale, zimscale)
	do i=1,n_point
	  p_coord(1,i)=(p_coord(1,i)-xofs) / ximscale
	  p_coord(2,i)=(p_coord(2,i)-yofs) / yimscale
	  p_coord(3,i)=(p_coord(3,i)-zofs) / zimscale
	enddo
c
c	  make list of all z values in the model
c
	ninzlis=0
	do iobj=1,max_mod_obj
	  do indobj=1,npt_in_obj(iobj)
	    ipnt=object(indobj+ibase_obj(iobj))
	    if(ipnt.gt.0.and.ipnt.le.n_point)then
	      ninzlis=ninzlis+1
	      izlist(ninzlis)=nint(p_coord(3,ipnt))
	    endif
	  enddo
	enddo
	call fill_listz(izlist,ninzlis,listz,nlistz)
c
	print *,'The current list of Z values (nearest integers)'//
     &	    ' in model is:'
	call wrlist(listz,nlistz)
c	  
c	  get new list of z values for remapping
c
20	write(*,'(/,a,i4,a,/,a,/,a,/,a)')' Enter new list of',nlistz,
     &	    ' Z values to remap these values to.',
     &	    ' Enter ranges just as above, or / to leave list alone,',
     &	    '   or -1 to replace each Z value with its negative.',
     &	    ' Use numbers from -999 to -990 to remove points with a'//
     &	    ' particular Z value.'
	do i=1,nlistz
	  newlist(i)=listz(i)
	enddo
	nnew=nlistz
	call rdlist(5,newlist,nnew)
c
	if(nnew.eq.1.and.newlist(1).eq.-1)then
	  do i=1,nlistz
	    newlist(i)=-listz(i)
	  enddo
	elseif(nnew.ne.nlistz)then
	  print *,'Number of Z values does not correspond'
	  go to 20
	endif
	do i=1,nlistz
	  indxzlst(listz(i)+1-listz(1))=i
	enddo
c
	write(*,'(1x,a,$)')
     &	    'Amounts to ADD to ALL X, Y and Z coordinates: '
	read(*,*)xadd,yadd,zadd
c
c	  see if there are any Z's out of order: find direction between
c	  first retained Z's and make sure all other intervals match
c	  
	inorder=1
	idirfirst=0
	lastkeep=0
	do i=1,nlistz
	  if(newlist(i).lt.-999.or.newlist(i).gt.-990)then
c	      
c	      if this is a retained Z and there is a previous one
c	      get the sign of the interval
c
	    if(lastkeep.gt.0)then
	      idir=sign(1,newlist(i)-newlist(lastkeep))
c		
c		store the sign the first time, compare after that
c
	      if(idirfirst.eq.0)then
		idirfirst=idir
	      elseif(idir.ne.idirfirst)then
		inorder=0
	      endif
	    endif
	    lastkeep=i
	  endif
	enddo
c
	ifreorder=0
	if(inorder.eq.0)then
	  write(*,'(1x,a,/,a,/,a,$)')'Your new Z''s are not in'//
     &	      ' monotonically increasing order.',
     &	      ' Enter 1 or -1 to reorder points in each object'//
     &	      ' by increasing or decreasing','   new Z value,'//
     &	      ' or 0 not to: '
	  read(*,*)ifreorder
	endif
c
c	  loop through objects, recompute # of objects
c
	n_object=0
	do iobj=1,max_mod_obj
	  ninobj=npt_in_obj(iobj)
	  indobj=1
	  ibase=ibase_obj(iobj)
	  do while(indobj.le.ninobj)
	    ipnt=object(indobj+ibase)
	    if(ipnt.gt.0.and.ipnt.le.n_point)then
c		
c		for valid point, get new z value
c
	      izval=nint(p_coord(3,ipnt))
	      newzval=newlist(indxzlst(izval+1-listz(1)))
c		
c		just shift values if not -999
c
	      if(newzval.lt.-999.or.newzval.gt.-990)then
		p_coord(1,ipnt)=p_coord(1,ipnt)+xadd
		p_coord(2,ipnt)=p_coord(2,ipnt)+yadd
		p_coord(3,ipnt)=p_coord(3,ipnt)+zadd+newzval-izval
		indobj=indobj+1
	      else
c		  
c		  or delete point by moving rest of pointers in object down
c
		ninobj=ninobj-1
		do indmov=indobj,ninobj
		  object(indmov+ibase)=object(indmov+1+ibase)
		enddo
	      endif
	    endif
	  enddo
	  npt_in_obj(iobj)=ninobj
	  if(ninobj.gt.0)n_object=n_object+1
c	    
c	    if reordering, sort object array by Z value
c	    
	  if(ifreorder.ne.0)then
	    do i=1,ninobj-1
	      do j=i,ninobj
		if(ifreorder*(p_coord(3,abs(object(j+ibase)))-
     &		    p_coord(3,abs(object(i+ibase)))).lt.0.)then
		  itmp=object(i+ibase)
		  object(i+ibase)=object(j+ibase)
		  object(j+ibase)=itmp
		endif
	      enddo
	    enddo
	  endif
c
	enddo
c	  
c	  scale data back
c
	do i=1,n_point
	  p_coord(1,i)=ximscale*p_coord(1,i)+xofs
	  p_coord(2,i)=yimscale*p_coord(2,i)+yofs
	  p_coord(3,i)=zimscale*p_coord(3,i)+zofs
	enddo
	call write_wmod(newmodel)
	call exit(0)
	end
