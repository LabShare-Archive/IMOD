*	  * * * * * EDPIECEPOINT * * * * *
*	  
c	  This program allows one to "edit" a list of piece coordinates, or a
c	  list of point coordinates, in two ways:
c	  1) The set of Z values may be mapped, one-to-one, to any arbitrary
c	  new set of Z values; and
c	  2) The X, Y or Z coordinates of all pieces or points may be shifted
c	  by a constant.
c	  
c	  The program allows one to digitize a set of serial sections in
c	  inverse order, if that is more convenient that doing it in the proper
c	  order.  Simply enter "-1" at the entry of the new list of Z values,
c	  then enter "0,0,ZMAX" as the values to add to the X, Y and Z
c	  coordinates, where ZMAX is the desired maximum Z value.  (This
c	  assumes that the Z values in the original list start at 0; otherwise
c	  a different ZMAX would be needed.)
c	  
c	  ENTRIES:
c	  
c	  Name of input piece or point list file, or Return if none
c	  
c	  IF you enter "Return", then the program will create a dummy piece
c	  .  list.  Enter the number (N) of piece coordinates to set up.  The
c	  .  piece coordinates will all be (0,0), with Z running from 0 to N-1.
c	  
c	  Name of output piece or point list file
c	  
c	  New list of Z values.  Using ranges (e.g. 0-3,5-7), specify a list 
c	  .  with the same number of Z values as in the input list.  The
c	  .  Z values in the input list will be mapped one-to-one onto the new
c	  .  list.  Enter "/" to take the input list without modification
c	  .  Enter numbers between -999 and -990 to delete points with a
c	  .  particular Z value; e.g. if the input list has Z values from 0
c	  .  to 9, entering 0-4,-999,5-8 will delete all points with Z between
c	  .  4.5 and 5.5, and shift the Z of points with Z greater then 5.5
c	  .  down by 1.  If the input list has Z from 0-19, entering
c	  .  0-9,-999--990 will remove all pooints with Z from 10 to 19.
c	  .  Enter -1 to replace each Z value with its negative.
c	  
c	  Amounts to add to all X, Y, and Z coordinates.  These values will be
c	  .  added after the remapping of Z values, if any.
c	  
c	  David Mastronarde  5/8/89
c
	parameter (limpcl=100000,limsec=100000)
	character*80 pclfil
	integer*4 ixpclist(limpcl),iypclist(limpcl),izpclist(limpcl)
	integer*4 listz(limsec),newlist(limsec),indxzlst(limsec)
c
	write(*,'(1x,a)')'Enter name of input piece or point'//
     &	    ' list file (Return if none)'
	read(*,'(a)')pclfil
	call read_piece_list(pclfil,ixpclist,iypclist,izpclist,npclist)
c
	if(npclist.eq.0)then
	  write(*,'(1x,a,$)')'# of sections to make dummy list for: '
	  read(*,*)npclist
	  do i=1,npclist
	    ixpclist(i)=0
	    iypclist(i)=0
	    izpclist(i)=i-1
	  enddo
	endif
c
	write(*,'(1x,a,$)')
     &	    'Name of output piece or point list file: '
	read(*,'(a)')pclfil
	call fill_listz(izpclist,npclist,listz,nlistz)
c	  
c	  get list of ranges
c
	print *,'The current list of Z values in input list is:'
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
	read(*,*)ixadd,iyadd,izadd
c
	ipc=1
	do while(ipc.le.npclist)
	  newzval=newlist(indxzlst(izpclist(ipc)+1-listz(1)))
	  if(newzval.lt.-999.or.newzval.gt.-990)then
	    izpclist(ipc)=newzval+izadd
	    ixpclist(ipc)=ixpclist(ipc)+ixadd
	    iypclist(ipc)=iypclist(ipc)+iyadd
	    ipc=ipc+1
	  else
	    npclist=npclist-1
	    do indmov=ipc,npclist
	      ixpclist(indmov)=ixpclist(indmov+1)
	      iypclist(indmov)=iypclist(indmov+1)
	      izpclist(indmov)=izpclist(indmov+1)
	    enddo
	  endif
	enddo
	call dopen(3,pclfil,'new','f')
	write(3,'(3i6)')(ixpclist(i),iypclist(i),izpclist(i)
     &	    ,i=1,npclist)
	close(3)
	call exit(0)
	end
