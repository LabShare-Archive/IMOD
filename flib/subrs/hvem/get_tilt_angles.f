c	  GET_TILT_ANGLES will read in tilt angles by the user's method of
c	  choice.  NVIEW should be the number of views, if known, or 0 if it
c	  not known, in which case the routine will return the number in this
c	  variable.  NUNIT should contain the number of a free logical unit.
c	  Tilt angles are returned in TILT.
c
	subroutine get_tilt_angles(nview,nunit,tilt)
	real*4 tilt(*)
	character*80 filename
c	  
	if(nview.eq.0)then
	  write(*,'(1x,a,/,a,/,a,$)')'Enter the # of tilt angles to '//
     &	      'specify them by starting and increment angle,',
     &	      '     - the # of angles to specify each individual value,'
     &	      ,'    or 0 to read angles from a file: '
	  read(5,*)nview
	  nvin=nview
	else
	  write(*,'(1x,a,/,a,/,a,$)')'Enter 1 to '//
     &	      'specify starting and increment angle,',
     &	      '     -1 to specify each individual value,'
     &	      ,'    or 0 to read angles from a file: '
	  read(5,*)nvin
	endif
	if(nvin.gt.0)then
	  write(*,'(1x,a,$)')'Starting and increment angle: '
	  read(5,*)tiltstr,tiltinc
	  do i=1,nview
	    tilt(i)=tiltstr+(i-1)*tiltinc
	  enddo
	elseif(nvin.lt.0)then
	  print *,'Enter all tilt angles'
	  read(5,*)(tilt(i),i=1,nview)
	else
	  write(*,'(1x,a,$)')'Name of file with tilt angles: '
	  read(5,'(a)')filename
	  call dopen(nunit,filename,'ro','f')
	  if(nview.eq.0)then
10	    read(nunit,*,end=20)tilt(nview+1)
	    nview=nview+1
	    go to 10
	  else
	    read(nunit,*)(tilt(i),i=1,nview)
	  endif
20	  close(nunit)
	endif
	return
	end

