C*IMUNIT
C
C	Returns the lower-level compatible unit number (Function call)
C
	FUNCTION IMUNIT(ISTREAM)
	include 'imsubs.inc'
	IMUNIT = LSTREAM(ISTREAM)
	RETURN
	END
