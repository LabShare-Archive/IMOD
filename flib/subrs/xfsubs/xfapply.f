c	  apply transform f to (x,y) to obtain (xp,yp); xc,yc specifies the
c	  center of the image
	subroutine xfapply(f,xc,yc,x,y,xp,yp)
	real*4 f(2,3)
	xadj=x-xc
	yadj=y-yc
	xp=f(1,1)*xadj+f(1,2)*yadj+f(1,3)+xc
	yp=f(2,1)*xadj+f(2,2)*yadj+f(2,3)+yc
	return
	end
