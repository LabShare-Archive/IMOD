c	  ROTMAGSTR_TO_AMAT obtains a 2 by 2 transformation matrix AMAT from
c	  four "natural" parameters of image transformation: THETA is overall
c	  rotation, SMAG is overall magnification, STR is a unidirectional
c	  stretch, and PHI is the angle of the stretch axis.

	subroutine rotmagstr_to_amat(theta,smag,str,phi,amat)
	real*4 amat(2,2),theta,smag,str,phi
	costh=cosd(theta)
	sinth=sind(theta)
	cosphi=cosd(phi)
	sinphi=sind(phi)
	cosphisq=cosphi**2
	sinphisq=sinphi**2
	f1=smag*(str*cosphisq+sinphisq)
	f2=smag*(str-1.)*cosphi*sinphi
	f3=smag*(str*sinphisq+cosphisq)
	amat(1,1)=f1*costh-f2*sinth
	amat(1,2)=f2*costh-f3*sinth
	amat(2,1)=f1*sinth+f2*costh
	amat(2,2)=f2*sinth+f3*costh
	return
	end
