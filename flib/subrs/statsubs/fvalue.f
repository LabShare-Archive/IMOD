c	  FVALUE finds tha value of F that corresponds to a particular
c	  significance level SIGNIF, with NDF1 and NDF2 degrees of freedom
c	  It replicates the NAG function G01CBF
c
	function fvalue(signif,ndf1,ndf2)
	fprob(ndf1,ndf2,f)=1-betai(0.5*ndf2,0.5*ndf1,ndf2/(ndf2+ndf1*f))
c	  
c	  Start at zero and find first value higher than SIGNIF
c
	x2=0.
	y2=fprob(ndf1,ndf2,x2)
	do while(y2.lt.signif)
	  x1=x2
	  y1=y2
	  x2=x1+1
	  y2=fprob(ndf1,ndf2,x2)
	enddo
	if(y2.eq.signif)then
	  fvalue=x2
	  return
	endif
c
c	  here are some limits that apply in the Newton method:
c
	errlim=1.e-7				!on error
	isteplim=100				!on # of steps in Newton
	dxlim=1.e-7				!on step size in X
	err=1.
	istep=0
	dx=1.
c
c	  Newton's method loop
c
	do while(abs(err).gt.errlim.and.istep.le.isteplim.and.
     &	    dx.gt.dxlim)
	  xnew=x1+(signif-y1)*(x2-x1)/(y2-y1)
	  ynew=fprob(ndf1,ndf2,xnew)
	  err=ynew-signif
	  if(err.gt.0)then
	    dx=x2-xnew
	    x2=xnew
	    y2=ynew
	  else
	    dx=xnew-x1
	    x1=xnew
	    y1=ynew
	  endif
	  istep=istep+1
	enddo
	fvalue=xnew
	return
	end



