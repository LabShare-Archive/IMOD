c$$ icalc_angles -- return angles from matrix supplied
c   call icalc_angles(angles,matrix)
c
c	parameters:
c	angles.f3	rotation angles
c			1 - alpha (about X)
c			2 - beta  (about Y)
c			3 - gamma (about Z)
c	matrix.f(3,3)	rotation matrix
c
c	conventions are the same as for icalc_matrix
c	nb  returns same angles as input for -180<alpha<180
c					      -90<beta<90
c					     -180<gamma<180
c	if beta is outside this range a different set of angles is
c	returned (although geometrically equivalent)
c	if beta=90.0 then the solution is degenerate since alpha
c	and gamma are then rotations about the same axis. Alpha is
c	set to zero.	
c
c	version 1.00	peter shaw	16-apr-87

	subroutine icalc_angles(angles,matrix)

	real angles(3), matrix(3,3)
	data small/0.0000001/, crit/0.01/
	data cnv/0.0174532921/

	r11 = matrix(1,1)
	r12 = matrix(1,2)
	r13 = matrix(1,3)
	r21 = matrix(2,1)
	r22 = matrix(2,2)
	r23 = matrix(2,3)
	r31 = matrix(3,1)
	r32 = matrix(3,2)
	r33 = matrix(3,3)
c
c	first check matrix
c
	det = r11*r22*r33 - r11*r23*r32 + r12*r23*r31 - r12*r21*r33
     &	    + r13*r21*r32 - r13*r22*r31
	if (abs(det-1.0).gt.crit) then		! determinant not 1.0
	   write (6,*) 'icalc_angles - matrix',matrix
	   write (6,*) 'determinant - ',det
	   stop 'icalc_angles - not a pure rotation matrix'
	end if					! can't be a rotation

	if (abs(r13-1.0).le.small
     &	    .or. abs(r13+1.0).le.small) then	! sinb = 1 or -1
	   beta = asin(r13)			! beta = 90 or 270
	   gamma = atan2 (r21,r22)		! can only solve for
	   alpha = 0.0				! gamma+alpha - set alpha
						! to zero
	else if (abs(r13).le.small) then	! sinb = 0
						! beta = 0,180 - set to
						! zero
	   beta = 0.0                           ! and solve for gamma
	   gamma = atan2 (-r12,r11)             ! and alpha
	   alpha = atan2 (-r23,r33)

	else

	   alpha = atan2 (-r23,r33)
	   gamma = atan2 (-r12,r11)
	   cosg = cos(gamma)
	   sing = sin(gamma)
	   if (abs(cosg).gt.crit) then
	      cosb = r11/cosg
	   else
	      cosb = -r12/sing
	   end if
	   beta = atan2 (r13,cosb)
    	end if
	angles(1) = alpha/cnv
	angles(2) = beta/cnv
	angles(3) = gamma/cnv
	return
	end
