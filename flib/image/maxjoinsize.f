c	  MAXJOINSIZE computes the maximum size and offsets needed to hold
c	  transformed data when joining serial sections
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
	integer maxfiles
	parameter (maxfiles = 999)
	real*4 f(2,3,maxfiles), dmin, dmax, dmean
	integer*4 nx(maxfiles), ny(maxfiles), nxyz(3), mxyz(3), mode
	integer*4 numF, numFiles, i, lineSkip, maxx, maxy, idirx, idiry
	real*4 xcen, ycen, xhalf, yhalf, xmin, xmax, ymin, ymax, xcorn, ycorn
	integer*4 ixofs, iyofs, newx, newy
	character*120 rootname,filename
	integer*4 iargc, lnblnk
c	
	if (iargc() .eq. 0) then
	  print *,'Usage: maxjoinsize number_of_sections ',
     &	      'lines_to_skip root_name'
	  print *,' Computes size and offsets needed to contain all ',
     &	      'data from joined tomograms'
	  print *,' It use transforms in root_name.tomoxg and gets ',
     &	      'image filenames from'
	  print *, ' root_name.info, skipping the given number of lines to',
     &	      ' get to the filenames'
	  call exit(0)
	endif
	if (iargc().ne.3) call errorexit('THERE MUST BE THREE ARGUMENTS')
c	  
c	  get number of files and lines to skip
c
	call getarg(1, rootname)
	read(rootname, *, err=91, end = 91)numFiles
	call getarg(2, rootname)
	read(rootname, *, err=91, end = 91)lineSkip
c	  
c	  Get the rootname, open tomoxg and read transforms
c
	call getarg(3, rootname)
	filename = rootname(1:lnblnk(rootname))//'.tomoxg'
	open(3,file=filename,form='formatted',status='old' ,err=92)

	call xfrdall(3, f, numF, *93)
	if (numF .ne. numFiles) call errorexit(
     &	    'WRONG NUMBER OF TRANSFORMS IN FILE')
	close(3)
c	  
c	  open the info file and skip lines
c

	filename = rootname(1:lnblnk(rootname))//'.info'
	open(3,file=filename,form='formatted',status='old' ,err=94)
	do i = 1, lineSkip
	  read(3, '(a)', err=94, end=94)filename
	enddo
	call ialprt(.false.)
c	  
c	  open image files, get sizes and max size
c
	maxx = 0
	maxy = 0
	do i = 1, numFiles
	  read(3, '(a)', err=95, end=95)filename
	  call imopen(1, filename, 'ro')
	  call irdhdr(1, nxyz, mxyz, mode, dmin, dmax, dmean)
	  call imclose(1)
	  nx(i) = nxyz(1)
	  ny(i) = nxyz(2)
	  maxx = max(maxx, nx(i))
	  maxy = max(maxy, ny(i))
	enddo
	close(3)
c	  
c	  transform the 4 corners in the coordinate system of the maximum size
c	  
	xcen = maxx / 2.
	ycen = maxy / 2.
	xmin = xcen
	ymin = xcen
	xmax = xcen
	ymax = xcen
	do i = 1, numFiles
	  xhalf = nx(i) / 2.
	  yhalf = ny(i) / 2.
	  do idirx = -1, 1, 2
	    do idiry = -1, 1, 2
	      call xfapply(f(1,1,i), xcen, ycen, xcen + idirx * xhalf,
     &		  ycen +idiry * yhalf, xcorn, ycorn)
	      xmin = min(xmin, xcorn)
	      ymin = min(ymin, ycorn)
	      xmax = max(xmax, xcorn)
	      ymax = max(ymax, ycorn)
	    enddo
	  enddo
	enddo
c	  
c	  Compute and return numbers
c
	ixofs = nint(0.5*(xmax+xmin) - xcen)
	iyofs = nint(0.5*(ymax+ymin) - ycen)
	newx = 2 * nint(0.5*(xmax - xmin))
	newy = 2 * nint(0.5*(ymax - ymin))
	write(*,101)newx,newy,ixofs,iyofs
101	format('Maximum size required:',2i6,/,'Offset needed to center:',
     &	    2i5)
	call exit(0)
c
91	call errorexit('READING NUMBER OF FILES OR LINES TO SKIP')
92	call errorexit('OPENING .tomoxg FILE')
93	call errorexit('READING TRANSFORMS FROM .tomoxg FILE')
94	call errorexit('OPENING .info FILE')
95	call errorexit('READING FILENAME FROM .info FILE')
	end

	subroutine errorexit(message)
	character*(*) message
	print *
	print *,'ERROR: MAXJOINSIZE - ',message
	call exit(1)
	end

	
