	subroutine imdash(xscal,xlo,xad,yscal,ylo,yad)
c  subroutine will draw one line after requesting parameters for line
c  slope and intercept and starting and ending position, line thickness,
c  and distances to dash on and off.
c  call parameters define transformation between plotting area and user
c  units: x (in inches) = xscal*(x in user units - xlo)+xad
	write(*,'('' slope, intercept, start and end x (user units),
     1 thickness (- to switch x&y),'')')
	write(*,'('' dash on and off in inches
     1 (0,0 no dash): '',$)')
	read(5,*)b,a,xstr,xend,linthk,dshon,dshoff
        call dodash(xscal,xlo,xad,yscal,ylo,yad,b,a,xstr,xend,linthk
     1,dshon,dshoff)
        return
        end

        subroutine dodash(xscal,xlo,xad,yscal,ylo,yad,b,a,xstr,xend
     1,linthk,dshon,dshoff)
	dconv=1./(xscal*sqrt(1.+(b*yscal/xscal)**2))
	if(linthk.lt.0)dconv=1./(yscal*sqrt(1.+(b*xscal/yscal)**2))
	dshn=dconv*dshon
	dshff=dconv*dshoff
	call imset(iabs(linthk),c1,c2,c3,0)
	xfrom=xstr
80	xto=xend
	if(dshn.gt.0)xto=amin1(xend,xfrom+dshn)
	yfrom=a+b*xfrom
	yto=a+b*xto
	xplfrm=xfrom
	xplto=xto
	if(linthk.lt.0)then
	  xplfrm=yfrom
	  xplto=yto
	  yfrom=xfrom
	  yto=xto
	endif
	call imma(xscal*(xplfrm-xlo)+xad,yscal*(yfrom-ylo)+yad)
	call imva(xscal*(xplto-xlo)+xad,yscal*(yto-ylo)+yad)
	xfrom=xto+dshff
	if(xfrom.lt.xend)go to 80
90	continue
	return
	end
