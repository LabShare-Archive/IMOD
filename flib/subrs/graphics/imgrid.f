c	  IMGRID and IMLGRD use the IMPAK interface to NCAR graphics to draw
c	  grid lines with equally spaced or logarithmically spaced ticks.
c
	subroutine imlgrd(xs,ys,xran,yran,xtick,ntick,tiksiz)
	dimension xtick(*),x(310),y(310)
	nt=iabs(ntick)
	do 2 i=1,nt
	alt=alog10(xtick(i)/xtick(1))
	x(i)=xs+xran*alt
2	y(i)=ys+yran*alt
	go to 104
	entry imgrid(xs,ys,xran,yran,ntick,tiksiz)
	if(ntick.eq.0)return
	nt=iabs(ntick)+1
	do 4 i=1,nt
	x(i)=xs+(i-1)*xran/(nt-1)
4	y(i)=ys+(i-1)*yran/(nt-1)
104	ifhalf=0
	if(ntick)5,35,10
5	ifhalf=1
10	dx=tiksiz
	dy=0.
	if(xran.eq.0.)go to 15
	dy=tiksiz
	dx=0
15	do 30 it=1,nt
	xx=x(it)
	yy=y(it)
	if(it.eq.1)call imma(xx,yy)
	call imva(xx,yy)
	call imma(xx+dx,yy+dy)
	if(ifhalf.ne.0)call imva(xx,yy)
	if(ifhalf.eq.0)call imva(xx-dx,yy-dy)
30	call imma(xx,yy)
35	return
	end
