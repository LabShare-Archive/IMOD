*************HEADER.FOR**********************************************
*
*	A JIFFY TO READ THE HEADER ON AN IMAGE FILE
*
************************************************************************
c       Version for unix can take the file name either on the command line
c       or as an entry to the program.  If here is no file name on the command
c       line, the program asks for the file name.
*
	
	DIMENSION NXYZ(3),MXYZ(3)
C
	CHARACTER*60 FILIN
	INTEGER*2 EXTRA(32)
C
	EQUIVALENCE (NX,NXYZ)
C
	call getinout(1,filin,filin)
c
	CALL IMOPEN(1,FILIN,'RO')
	CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C
	CALL IMCLOSE(1)
c
	STOP
	END


