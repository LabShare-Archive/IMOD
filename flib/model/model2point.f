* * * * * * MODEL2POINT * * * * *
c	  
c	  MODEL2POINT will convert an imod or WIMP ascii model file to a
c	  list of points with integer coordinates.  Each point will be written
c	  on a separate line, and any information about which object or contour
c	  the point came from will be omitted.  The input file may be either
c	  an imod model or a WIMP ascii model file; the output file will
c	  contain the list of points.
c	  
C	  The program will accept file names either from the command line or as
C	  entries to the program after it is started.  If there are two names
C	  on the command line, they will be taken as the input and output file
C	  names; if there is one name, it will be taken as the input file name
C	  and the program will ask for the output file name; if there are no
C	  command line arguments, the program will ask for both input and
C	  output file names.
C
	character*80 modelfile,pointfile
	logical exist,readw_or_imod
c
	include 'model.inc'
c
c	  read model in; leave it closed
c
	call getinout(2,modelfile,pointfile)
75	exist=readw_or_imod(modelfile)
	close(20)
	if(.not.exist)then
	  print *,'bad model file'
	  call exit(0)
	endif
c	  
	call dopen(1,pointfile,'new','f')
c
c	  scan through all objects to get points
c	  
	npnts=0
	do iobject=1,max_mod_obj
	  ninobj=npt_in_obj(iobject)
	  if(ninobj.gt.0)then
	    do ipt=1,ninobj
	      ipnt=object(ipt+ibase_obj(iobject))
	      if(ipnt.gt.0)then
		write(1,102) nint(p_coord(1,ipnt)),
     &		    nint(p_coord(2,ipnt)), nint(p_coord(3,ipnt))
		npnts=npnts+1
	      endif
	    enddo
	  endif
	enddo
102	format(3i6)
	print *,npnts,' points output to file'
	call exit(0)
	end
