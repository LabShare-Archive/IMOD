c	  AMAT_TO_ROTMAGSTR converts a 2 by 2 transformation matrix AMAT into
c	  four "natural" parameters of image transformation: THETA is overall
c	  rotation, SMAG is overall magnification, STR is a unidirectional
c	  stretch, and PHI is the angle of the stretch axis.
c	  Two equivalent solutions are possible, with the stretch axis in the
c	  first or fourth quadrant.  The program returns the solution that
c	  makes the magnification SMAG nearer to 1.0.
c
c	  To solve for the variables, the first step is to solve for THETA by
c	  taking the arctangent of a function of the AMAT values.  It is then
c	  possible to compute F1, F2 and F3, intermediate factors whose
c	  equations are listed in ROTMAGSTR_TO_AMAT below. The equations for
c	  F1, F2 and F3 can then be solved explicitly for the square of the
c	  cosine of PHI.  There are two solutions, which are both valid, but
c	  in different quadrants.  The sign of F2, and whether one of the
c	  formulas for STR would yield a value > or < 1, then determines which
c	  solution is valid for the first quadrant.  STR is computed from
c	  one of two different formulas, depending on whether PHI is near 45
c	  degrees or not, then SMAG is computed.
c	  
c	  David Mastronarde 12/29/88, vastly improved 2/5/92
c
	subroutine amat_to_rotmagstr(amat,theta,smag,str,phi)
	implicit real*4 (a-h, o-z)		!can set to *8 easily
	real*4 amat(2,2),theta,smag,str,phi
	
	a11=amat(1,1)				!convenience variables
	a12=amat(1,2)
	a21=amat(2,1)
	a22=amat(2,2)
c	
c	  first find the rotation angle theta that gives the same solution for
c	  f2 when derived from a11 and a21 as when derived from a12 and a22
c
	theta=0.
	if(a21.ne.a12.or.a22.ne.-a11)theta=atan2d(a21-a12,a22+a11)
	costh=cosd(theta)
	sinth=sind(theta)
	f1=a11*costh+a21*sinth
	f2=a21*costh-a11*sinth
	f3=a22*costh-a12*sinth
c	
c	  Next solve for phi
c
	if(abs(f2).lt.1.e-10)then
c
c	    if f2=0, pick phi=0., set cos phi to 1.
c
	  cosphisq=1.
	else
c
c	    otherwise, solve quadratic equation, pick the solution that is
c	    right for the first quadrant
c
	  afac=(f3-f1)**2
	  bfac=4.*f2**2
	  cosphisq=0.5*(1.+sqrt(1.-bfac/(bfac+afac)))
	  sinphisq=1.-cosphisq
	  fnum=abs(f1*cosphisq-f3*sinphisq)
	  fden=abs(f3*cosphisq-f1*sinphisq)
	  if(f2.gt.0.and.fnum.lt.fden.or.
     &	      f2.lt.0.and.fnum.gt.fden)cosphisq=1.-cosphisq
	endif
	phi=acosd(sqrt(cosphisq))
	sinphisq=1.-cosphisq
c	    
c	    solve for str.
c
	if(abs(cosphisq-0.5).gt.0.25)then
c
c	    for angles far from 45 deg, use an equation that is good at 0 or
c	    90 deg but blows up at 45 deg.
c
	  str=(f1*cosphisq-f3*sinphisq)/(f3*cosphisq-f1*sinphisq)
c	    write(*,'(a,f12.7)')' outer',str
	else
c
c	    for angles near 45 deg, use an equation that is good there but
c	    blows up at 0.
c
	  factmp=(f1+f3)*sqrt(cosphisq*sinphisq)
	  str=(factmp+f2)/(factmp-f2)
c	    write(*,'(a,f12.7)')' inner',str
	endif
c	    
c	  solve for smag from the equation for f1, or f2 if that would fail
c	  (which it does with stretch -1 along 45 degree line)
c	  
	dentmp=str*cosphisq+sinphisq
	if(abs(dentmp).gt.1.e-5)then
	  smag=f1/dentmp
	else
	  smag=1./((str-1.)*sqrt(cosphisq*sinphisq))
	endif
c	  
c	  if it will make smag closer to 1.0, flip stretch axis 90 deg
c
	if(abs(smag-1.).gt.abs(str*smag-1.))then
	  smag=smag*str
	  str=1./str
	  phi=phi-90.
	endif
c
c	write(*,'(4f12.7)')str,phi,smag,theta
	return
	end
