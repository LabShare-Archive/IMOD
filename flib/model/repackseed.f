c	  REPACKSEED is a companion program for the shell script Transferfid.
c	  It is used to give the user a list of how points correspond between
c	  the original fiducial model and the new seed model, and to repack
c	  the seed model to eliminate empty contours.  If it is given a file
c	  listing object and contour numbers for fiducials whose positions
c	  have been solved for in the first set, the 
c
c	  Its inputs are:
c
c	  File name of original fiducial model for the first axis
c	  
c	  Name of file with X/Y/Z coordinates, as produced by Tiltalign, or
c	  Return if none available
c
c	  Name of new seed model for the second axis, produced by Beadtrack and
c	  stripped of points from the first axis
c	  
c	  Output file for final repacked seed model
c	  
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.2  2002/07/28 20:28:30  mast
c	  Added better analysis of mapping based on fiducial coordinate file
c	  from Tiltalign
c	
c	  Revision 3.1  2002/05/23 04:42:15  mast
c	  Creation of program
c	
c
	implicit none
	integer idim
	parameter (idim=10000)
	include 'model.inc'
	character*120 fidname,seedname,outfile,xyzname
	integer*4 ifid_in_a(idim),maplist(idim)
	integer*4 ixyzcont(idim),ixyzpnt(idim),ixyzobj(idim)
	integer*4 iobj,nmap,noriginal,i,imodobj,imodcont,nxyz,nmaplen
	real*4 dum
	logical*4 readw_or_imod
c	  
	write(*,'(1x,a,$)')'Name of fiducial file from first axis: '
	read(5,101)fidname
101	format(a)
	write(*,'(1x,a)')'Enter name of file with X/Y/Z coordinates,'
     &	    //' or Return if none available'
	read(5,101)xyzname
	write(*,'(1x,a,$)')'Name of input file with new seed model: '
	read(5,101)seedname
	write(*,'(1x,a,$)')'Name of output file for packed model: '
	read(5,101)outfile
c	  
c	  read original file
c
	if (.not.readw_or_imod(fidname))then
	  print *,'ERROR: REPACKSEED - reading original fiducial file',
     &	      fidname
	  call exit(1)
	endif
c	  
c	  read xyz file if any
c	  
	nxyz = 0
	if (xyzname .ne. ' ') then
	  call dopen(1, xyzname, 'ro', 'f')
10	  read(1,*,end=20)ixyzpnt(nxyz+1), dum, dum, dum,
     &	      ixyzobj(nxyz+1), ixyzcont(nxyz+1)
	  nxyz = nxyz + 1
	  go to 10
20	  continue
	endif
c	  
c	  Keep track of original object numbers
c	  
	noriginal=0
	do iobj=1,max_mod_obj
	  if (nxyz .eq. 0) then
c	      
c	      if no xyz file available, have to assume that every contour with
c	      at least one point will be a fiducial
c
	    if(npt_in_obj(iobj).gt.0)then
	      noriginal=noriginal+1
	      ifid_in_a(iobj)=noriginal
	    else
	      ifid_in_a(iobj)=0
	    endif
	  else
c	      
c	      look for object in xyz list, take number if found
c	      
	    call objtocont(iobj,obj_color,imodobj,imodcont)
	    ifid_in_a(iobj)=0
	    do i = 1, nxyz
	      if (ixyzobj(i).eq.imodobj .and. ixyzcont(i) .eq. imodcont)
     &		  ifid_in_a(iobj) = ixyzpnt(i)
	    enddo
	  endif
	enddo
c	  
c	  read new seed model
c	  
	if (.not.readw_or_imod(seedname))then
	  print *,'ERROR: REPACKSEED - reading new seed file',
     &	      seedname
	  call exit(1)
	endif
c	  
c	  pack objects down and accumulate map list
c	  
	nmap=0
	do iobj=1,max_mod_obj
	  if (npt_in_obj(iobj).gt.0)then
	    nmap=nmap+1
	    maplist(nmap)=ifid_in_a(iobj)
	    npt_in_obj(nmap)=npt_in_obj(iobj)
	    ibase_obj(nmap)=ibase_obj(iobj)
	    obj_color(1,nmap)=obj_color(1,iobj)
	    obj_color(2,nmap)=obj_color(2,iobj)
	  endif
	enddo

	do iobj=nmap+1,max_mod_obj
	  npt_in_obj(iobj)=0
	enddo
	max_mod_obj=nmap
c	  
c	  output the model and the map list
c	  
	call write_wmod(outfile)
	write(*,102)
102	format(//,' Make the following entries to Setupcombine or ',
     &	    'Solvematch to describe how ',/,
     &	    ' fiducials correspond between the first and second axes',/,
     &	    ' Points in A: ',$)
	call wrlist(maplist,nmap)
	call int_iwrite(outfile, nmap, nmaplen)
	write(*,'(a,a)')' Points in B: 1-',outfile(1:nmaplen)
	if (nxyz.eq.0)then
	  write(*,103)
103	  format(/,' These lists may be wrong if they include a contour',
     &	     ' in A that will not',/,' correspond to a fiducial ',
     &	      'point (e.g., if it has only one point)')
	else
	  write(*,'(///)')
	endif
	write(*,105)
105	format(' These lists may be thrown off if you delete a contour',
     &	    ' in either set',/,' and will be thrown off if one of the',
     &	    ' seed points fails to track')
	call exit(0)
	end
