c	  REPACKSEED is a companion program for the shell script Transferfid.
c	  It is used to give the user a list of how points correspond between
c	  the original fiducial model and the new seed model, and to repack
c	  the seed model to eliminate empty contours.
c
c	  Its inputs are:
c
c	  File name of original fiducial model for the first axis
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
c
	implicit none
	include 'model.inc'
	character*120 fidname,seedname,outfile
	integer*4 ifid_in_a(10000),maplist(10000)
	integer*4 iobj,nmap,noriginal
	logical*4 readw_or_imod
c	  
	write(*,'(1x,a,$)')'Name of fiducial file from first axis: '
	read(5,101)fidname
101	format(a)
	write(*,'(1x,a,$)')'Name of input file with new seed model: '
	read(5,101)seedname
	write(*,'(1x,a,$)')'Name of output file for packed model: '
	read(5,101)outfile
c	  
c	  read original file
c
	if (.not.readw_or_imod(fidname))then
	  print *,'REPACKSEED: Error reading original fiducial file',
     &	      fidname
	  call exit(1)
	endif
c	  
c	  Keep track of 
c	  
	noriginal=0
	do iobj=1,max_mod_obj
	  if(npt_in_obj(iobj).gt.0)then
	    noriginal=noriginal+1
	    ifid_in_a(iobj)=noriginal
	  else
	    ifid_in_a(iobj)=0
	  endif
	enddo
c	  
c	  read new seed model
c	  
	if (.not.readw_or_imod(seedname))then
	  print *,'REPACKSEED: Error reading original fiducial file',
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
102	format(/,' Make the following entries to Setupcombine or ',
     &	    'Solvematch to describe how ',/,
     &	    ' fiducials correspond between the first and second axes',/,
     &	    ' Points in A:',$)
	call wrlist(maplist,nmap)
	write(*,'(a,i4)')' Points in B: 1-',nmap
	call exit(0)
	end
