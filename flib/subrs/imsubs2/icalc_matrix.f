c$$ icalc_matrix -- returns 3_D rotation matrix from 3 rotation angles
c  call icalc_matrix(angles,matrix)
c	parameters:
c	angles.f3	array of 3 rotation angles in degrees
c			1 -- alpha - rotation about x axis
c			2 -- beta  - rotation about y axis
c			3 -- gamma - rotation about z axis
c	matrix.f(3,3)	array returned with rotation matrix
c
c	conventions:
c	[R] premultiplies column vector of coordinates:
c
c	|xnew|       | r11  r12  r13 | |xold|
c	|ynew|   =   | r21  r22  r23 | |yold|
c	|znew|       | r31  r32  r33 | |zold|
c
c	rotations applied in the order Z first, X last
c	ie  [R] = [X][Y][Z]
c
c	rotations are right-handed - ie +ve rotation moves object
c	anti-clockwise
c
c	[X] = |   1    0     0   |
c	      |   0   cosa -sina |
c	      |   0   sina  cosa |
c
c	[Y] = |  cosb  0   sinb  |
c	      |   0    1     0   |
c	      | -sinb  0   cosb  |
c
c	[Z] = |  cosg -sinb  0   |
c	      |  sing  cosg  0   |
c	      |   0    0     1   |
c
c  version 1.00		peter shaw		16-apr-87

	subroutine icalc_matrix(angles,matrix)

	real angles(3), matrix(3,3)
	data cnv/0.0174532921/

	alpha = angles(1)*cnv	! convert angles
	beta  = angles(2)*cnv
	gamma = angles(3)*cnv
	ca = cos(alpha)
	cb = cos(beta)
	cg = cos(gamma)
	sa = sin(alpha)
	sb = sin(beta)
	sg = sin(gamma)

	matrix(1,1) = cb*cg		! set up matrix
	matrix(1,2) = -cb*sg
	matrix(1,3) = sb
	matrix(2,1) = sa*sb*cg + ca*sg
	matrix(2,2) = -sa*sb*sg + ca*cg
	matrix(2,3) = -sa*cb
	matrix(3,1) = -ca*sb*cg + sa*sg
	matrix(3,2) = ca*sb*sg + sa*cg
	matrix(3,3) = ca*cb

	return
	end
