* * * * * BSPLT * * * * *
c	  
c	  Subroutine BSPLT allows one to plot one variable against another,
c	  with graphical display on either a graphics device, if available, or
c	  on the terminal (ANSI standard/VT100 class), and hard copy output via
c	  NCAR graphics calls and obsolete X/Y plotter calls.
c	  
c	  The first entry controls where the graph is displayed:
c	  .  0 for plot on the graphics device (which must have been enabled by
c	  .      selecting the appropriate option in the calling program)
c	  .  1 for plot on the terminal
c	  
c	  The next entry allows you to type or file the data or allows you to
c	  specify some aspects of a hard-copy plot:
c	  .  0 for none of these options
c	  .  1 to type the data on the terminal
c	  .  -1 to store the data in the file
c	  .  n+1 to draw n lines on hard-copy plot; e.g. 4 to draw 3 lines
c	  .  -n-1 to connect points with lines of thickness n in the hard-copy
c	  .      plot; e.g. -2 to connect with lines of thickness 1.  For
c	  .      standard usage of the routine, subtract 100 to connect only
c	  .      points within the same (e.g. -102).  When the routine is
c	  .      called specifically to generate error bars, it will connect
c	  .      only within groups with -n-1, and subtracting 100 will make
c	  .      it connect all points.  Sorry about this.
c	  
c	  IF you elect to write the data to a file, next enter a file name.
c	  .  If the file already exists, the data will be appended to the file.
c
c	  The next entry can select a hard-copy plot:
c	  .  0 for no plot - return to calling program
c	  .  1-5 for plots on obsolete X/Y plotter - ignore
c	  .  6, 7, 8, 9 for laser printer plots in the upper left, lower left,
c	  .      upper right, or lower right quadrants of a page, or 10 to
c	  .      specify the size and location of the graph on the page.  The
c	  .      range of X and Y units plotted will be the same as in the
c	  .      plot on the screen.
c	  .  -6 to -10 for plots in the fixed or specified location, if you
c	  .      wish to change the range of X and Y units that will be plotted
c	  .      or control the number of ticks, and the grid and symbol size
c	  .      and thickness
c	  
c	  IF you selected a negative plot number, next make these entries:
c	  
c	  .  Lower and upper limits of the range of X units to be plotted,
c	  .      lower and upper limits for Y, and the number of ticks for the
c	  .      X and for the Y axis.  If one or both of these units are
c	  .      logarithmic, you will be informed of the range of the actual
c	  .      values (not of their logarithms), and you can then enter the
c	  .      lower and upper limits of the actual values, not of the
c	  .      logarithms.  For a linear axis, the "number of ticks" is
c	  .      actually the number of divisions along the axis; but for a
c	  .      logarithmic axis, it is the number of tick marks, which is
c	  .      the number of divisions plus 1.  Sorry again.  Enter negative
c	  .      the number of ticks to get unidirectional ticks.
c	  
c	  .  Tick and symbol size (in inches), grid and symbol thickness (small
c	  .      integers), 1 for a complete box, i.e. axes on all 4 sides.
c	  .      If you selected unidirectional ticks, positive and negative
c	  .      tick lengths give ticks toward the inside and outside of the
c	  .      box, respectively.
c	  
c	  .  If you are plotting error bars, next enter the total length of
c	  .      ticks at the ends of the bars, in inches.
c	  
c	  IF you selected graph 10 (or -10), make these entries:
c	  .      X and Y size of graph, and lower left X and Y coordinates, in
c	  .      inches.  Enter the negative of the left X coordinate to offset
c	  .      the vertical grid lines from the edge of the graphing area,
c	  .      and/or the negative of the lower Y coordinate to offset the
c	  .      horizontal grid lines.  (Sorry again)
c	  
c	  .  IF you entered a negative left X coordinate, enter the amount to
c	  .      offset the vertical grid lines in the X direction
c	  
c	  .  IF you entered a negative lower Y coordinate, enter the amount to
c	  .      offset the horizontal grid lines in the Y direction
c	  
c	  .  IF you are plotting fraction boxes, enter the box width, height
c	  .      and tick size
c	  
c	  IF you selected a graph and are plotting Tukey box plots, enter:
c	  .      Width of box, size of tick at 10/90% points, gap between
c	  .      ticks and outlying points (points closer than this amount to
c	  .      a tick will be omitted), and line thickness.  The first three
c	  .      values are all in inches.
c
c	  IF you selected a graph, enter 1 for a new page, or 0 to plot on the
c	  .  same page as any previous graphs
c	  
c	  IF you selected a negative graph number and the X-axis is
c	  logarithmic, enter the values at which X ticks should be drawn
c	  
c	  IF you selected a negative graph number and the Y-axis is
c	  logarithmic, enter the values at which Y ticks should be drawn
c	  
c	  IF you said that you wanted to draw some lines, next enter the
c	  .  specification for each line.  The format is:
c	  .  Slope (Y/X), Y-intercept, starting and ending X coordinate (all in
c	  .  your units, not inches), line thickness (small integer), and
c	  .  0,0 for a solid line or length of dash and length between dashes
c	  .  for a dashed line.  If you enter the negative of line thickness,
c	  .  then X and Y are inverted, so the entries are the X/Y slope, the
c	  .  X-intercept, and the starting and ending Y coordinates.
c
c	  IF you selected a negative graph number, then you can next make many
c	  .  entries to label the axes and add other symbols and lines to the
c	  .  graph.  The FIRST time that you encounter these options (via
c	  .  BSPLT or BSHST), you will be asked if you want regular characters
c	  .  or the simpler, sans serif characters (enter 1 for the latter).
c
c	  First, for the X axis, enter:
c
c	  .  # of ticks to label with numeric labels, and number of lines of
c	  .      text labels.
c	  
c	  .  IF you specified a nonzero number of ticks to label, next enter:
c
c	  .    If the ticks are to be labeled at regular intervals, enter the
c	  .        number of the first tick to label (first tick is #1) and
c	  .        the interval between labeled ticks (e.g. 2 for every other
c	  .        tick or enter 0,0 to specify a list of ticks to label.
c
c	  .        IF you entered 0,0, next enter the #'s of the ticks to label
c	  
c	  .    Labels for the ticks, in one line, separated by commas or spaces
c	  
c	  .    Numeric label size and separation from axis, in inches
c	  
c	  .  IF you specified a nonzero number of text labels, next enter for
c	  .      each label in turn:
c
c	  .     Text label size, separation from axis, and offset along axis
c	  .          between center of axis and center of text.
c	  
c	  .     Text label
c	  
c	  Next enter these parameters in the same order for the Y axis
c	  
c	  Finally, the program calls the IMMISC subroutine.  Here, coordinates
c	  .  may be specified in one of three ways independently for each of
c	  .  the entries to the program.  The coordinates may be in "user"
c	  .  units (the units of the numbers being graphed), in absolute units
c	  .  of inches on the plotter page, or in units relative to the frame
c	  .  of the graph (e.g. 0.1,0.9 for a position in the upper left
c	  .  corner, or 0.5,1.1 for a position centered above the graph frame).
c
c	  .  Enter the number of text labels, # of letters in circles,
c	  .      # of symbols in boxes, and # of dashed or solid lines to draw
c	  
c	  .  IF you entered a non-zero # of text labels, then for each, enter:
c	  
c	  .      X and Y position, and 0 for user or 1 for absolute or -1 for
c	  .          relative units 
c	  
c	  .      0 to center, -1 to left justify, or 1 to right justify the
c	  .          character string on this position
c	  
c	  .      Size of characters in inches, and orientation angle in
c	  .          degrees (usually 0 or 90)
c	  
c	  .      Text, on one line
c	  
c	  .  IF you entered a non-zero # of circled letters, then for each:
c	  
c	  .      X and Y position, and 0 for user or 1 for absolute or -1 for
c	  .          relative units 
c	  
c	  .      Diameter of circle in inches, line thickness
c
c	  .      Size of character in inches
c
c	  .      Letter (upper case)
c	  
c	  .  IF you entered a non-zero # of boxed symbols, then for each:
c	  
c	  .      X and Y position, and 0 for user or 1 for absolute or -1 for
c	  .          relative units 
c	  
c	  .      Symbol type (or 0 for no symbol), symbol size in inches, 
c	  .          symbol thickness (a small integer), box size in inches
c	  .          (or 0 for no box), box line thickness
c	  
c	  .  IF you entered a non-zero # of lines, then for each, enter:
c	  
c	  .      0 for user, 1 for absolute, or -1 for relative units
c	  
c	  .      A line specification as described above, in the units just
c	  .          indicated
c
c	  NOTE FOR SIMPLEST USE: If you just want some standard graphs, with
c	  the same scaling as appears on the screen, then there are only 4
c	  entries:
c	  .  0 for graphics, 1 for terminal plot
c	  .  0, or -2 if you need connected points
c	  .  6, 7, 8, or 9 (in that sequence)
c	  .  1 for new page
c	  Be sure to note down the range of X and Y values plotted.
c
	subroutine bsplt(namx,xx,yy,ngx,nx,nsymb,ngrps,irecx,irecy
     1,colx,coly,iflogx,iflogy)
	common /pltp/ifgpl
	dimension irecx(*),irecy(*),colx(*),coly(*),namx(*)
	dimension xx(*),ngx(*),nsymb(*),yy(*),xtick(310),ytick(310)
	dimension xls(4),yls(4)
	character*80 namfl
	character*1 hisymb(19)
	logical bygrup,docnct
	data hisymb/'-','|','<','>','/','\\','X','+','O','@','v','^'
     1,'3','4','5','6','7','8','9'/
	data xls,yls/0.,0.,5.2,5.2,5.2,0.,5.2,0./
	data ifterm/0/, iftyp/0/, ifplt/0/
	save ifterm,iftyp,ifplt
c
	call minmax(xx,nx,xmin,xmax)
	call minmax(yy,nx,ymin,ymax)
	if(ifgpl.lt.0)then
	  ifnonzer=0
	  do 5 i=1,nx
	    ymin=min(ymin,yy(i)-colx(i))
	    ymax=max(ymax,yy(i)+colx(i))
	    if(colx(i).ne.0.)ifnonzer=1
5	  continue
	endif
	ifterm=0
	write(*,'('' 0 for graphics plot, 1 for plot on terminal: '',$)')
	read(5,*)ifterm
	if(ifterm.gt.0)then
	  call chrout(27)
	  call chrout(ichar('['))
	  call chrout(ichar('2'))
	  call chrout(ichar('J'))
	  call setpos(21,0)
	endif
	call dsaxes(xmin,xmax,ymin,ymax,xsc,xad,ysc,yad,dx,xlo,dy,ylo)
	if(ifterm.eq.0)then
	  do 16 i=1,nx
	    ng=ngx(i)
	    ix=xsc*xx(i)+xad
	    iy=ysc*yy(i)+yad
	    if(ifgpl.lt.0.and.i.gt.1.and.ngx(i).eq.ngx(max(1,i-1)))
     &		call va(ix,iy)
	    isymbt=nsymb(ng)
	    if(isymbt.lt.0)isymbt=-i
	    call scpnt(ix,iy,isymbt)
	    if(ifgpl.lt.0)then
	      idy=ysc*colx(i)
	      call ma(ix,iy+idy)
	      call va(ix,iy-idy)
	      call ma(ix,iy)
	    endif
16	  continue
	  call updat(1)
	elseif(ifterm.gt.0)then
	  do 20 i=1,nx
	    jcol=7.9999*(xx(i)-xlo)/dx
	    if(ifgpl.lt.0)then
	      irow=2.09995*(10.-(yy(i)+colx(i)-ylo)/dy)
	      call setpos(irow,jcol)
	      call chrout(ichar('-'))
	      irow=2.09995*(10.-(yy(i)-colx(i)-ylo)/dy)
	      call setpos(irow,jcol)
	      call chrout(ichar('-'))
	    endif
	    irow=2.09995*(10.-(yy(i)-ylo)/dy)
	    call setpos(irow,jcol)
	    call chrout(ichar(hisymb(max(1,nsymb(ngx(i))))))
20	  continue
	  call setpos(22,65)
	  do 201 i=1,12
	    call chrout(ichar(hisymb(i)))
201	  continue
	endif
	write(*,'('' draw n lines (n+1), type (1), to file (-1), connect
     1 (-thick-1), no (0): '',$)')
	read(5,*)iftyp
	nlines=0
	ifcnct=0
	if(iftyp.gt.1) nlines=iftyp-1
	if(iftyp.lt.-1) ifcnct=-1-iftyp
c	  if not doing errors, default is to connect all, and addition of
c	  100 indicates connect by group; but if doing errors, default is
c	  to do by group only, and addition of 100 is needed to specify
c	  connect all
	bygrup=(ifcnct.gt.100.and.ifgpl.ge.0).or.
     &	    (ifcnct.gt.0.and.ifcnct.lt.100.and.ifgpl.lt.0)
	ifcnct=mod(ifcnct,100)
	if(iabs(iftyp).gt.1)iftyp=0
	iout=6
	if(iftyp)24,26,26
24	iout=8
	write(*,*) 'enter output file name'
	call flnam(namfl,0,'0')
	open(8,file=namfl,err=24,status='unknown')
25	read(8,'(a4)',end=26)namfl
	go to 25
26	call xyset(1)
	write(*,'('' plot # (6-10), - # to set new 
     1 x/y range, or no plot (0): '',$)')
	read(5,*)ifplt
	if(ifplt.eq.0.and.iftyp.eq.0)go to 60
	iaplt=iabs(ifplt)
	ifimg=iaplt-5
	defscl=1.
	if(ifimg.gt.0)then
	  call imset(1,wthinch,c2,c3,0)
	  defscl=0.74*wthinch/7.5
	  iaplt=iaplt-5
	endif
	ntx=10
	nty=10
	xgrofs=0.
	ygrofs=0.
	xhi=xlo+10.*dx
	yhi=ylo+10.*dy
	xran=4.7*defscl
	yran=4.7*defscl
	symwid=.1
	errlen=0.8*symwid
	tiksiz=0.05
	ithsym=1
	ithgrd=1
	ifbox=0
	if(abs(ifplt).le.5)go to 60
	if(ifplt.lt.0)then
	  write(*,101)xmin,xmax,ymin,ymax
101	  format(' min and max values of x:',2f10.3,',  y:',2f10.3)
	  if(iflogx.ne.0)then
	    xlnmn=10.**xmin
	    xlnmx=10.**xmax
	    write(*,235)xlnmn,xlnmx
235	    format(' linear limits of x:',2f10.3)
	  endif
	  if(iflogy.ne.0)then
	    ylnmn=10.**ymin
	    ylnmx=10.**ymax
	    write(*,236)ylnmn,ylnmx
236	    format(' linear limits of y:',2f10.3)
	  endif
	  write(*,'('' lower and upper limits of x, of y, # ticks x and y
     1: '',$)')
	  read(5,*)xlo,xhi,ylo,yhi,ntx,nty
	  if(ifimg.gt.0)then
	    write(*,'('' tick and symbol size, grid and symbol thickness,
     1 1 for box: '',$)')
	    read(5,*)tiksiz,symwid,ithgrd,ithsym,ifbox
	    if(ifgpl.lt.0.and.ifnonzer.ne.0)then
	      write(*,'(1x,a,$)')
     &		  'Length of ticks at ends of error bars (inches): '
	      read(5,*)errlen
	    endif
	  endif
	endif
	if(iaplt.ge.5)then
	  write(*,'('' X and Y size, lower left X and Y
     1 (- to offset grids): '',$)')
	  read(5,*)xran,yran,xll,yll
	  if(xll.lt.0.)then
	    write(*,'('' grid offset in x: '',$)')
	    read(5,*)xgrofs
	    xll=-xll
	  endif
	  if(yll.lt.0.)then
	    write(*,'('' grid offset in y: '',$)')
	    read(5,*)ygrofs
	    yll=-yll
	  endif
	  if(nsymb(1).eq.-2)then
	    frcwid=0.
	    frchit=0.
	    frctic=0.
	    write(*,'(1x,a,$)')'fraction box width, height, tick size: '
	    read(5,*)frcwid,frchit,frctic
	    call fbxset(frcwid,frchit,frctic)
	  endif
	else
	  xll=xls(iaplt)*defscl
	  yll=yls(iaplt)*defscl
	endif
	if(ifgpl.eq.2)then
	  tukwid=0.4
	  tuktic=0.1
	  tukgap=0.
	  ithtuk=1
	  write(*,'(1x,a,$)')'Tukey box width, tick size,'//
     &	      ' tick-symbol gap, line thickness: '
	  read(5,*)tukwid,tuktic,tukgap,ithtuk
	endif
	ixlo=10+102.*xll
	iylo=10+102.*yll
	xad=ixlo
	yad=iylo
	iabntx=iabs(ntx)
	iabnty=iabs(nty)
	ixran=102.*xran
	if(ifimg.gt.0)then
	  call symsiz(symwid)
	  call imset(ithgrd,c1,c2,c3,0)
	  write(*,'('' new page (0 or 1)?: '',$)')
	  read(5,*)ifnewp
	  if(ifnewp.ne.0)call frame()
	  xad=xll+0.1
	  yad=yll+0.1
	endif
	if(iflogx.eq.0.or.ifplt.gt.0)then
	  if(ifimg.gt.0)then
	    xscal=xran/(xhi-xlo)
	    call imgrid(xad,yad-ygrofs,xran,0.,ntx,tiksiz)
	    if(ifbox.ne.0)
     &		call imgrid(xad,yad+yran+ygrofs,xran,0.,ntx,-tiksiz)
	  else
	    idx=ixran/ntx
	    ixran=idx*ntx
	    xscal=ixran/(xhi-xlo)
	    call xygrd(ixlo,iylo,idx,0,ntx)
	  endif
	else
	  xlo=alog10(xlo)
	  xhi=alog10(xhi)
	  write(*,'(i3,'' x ticks: '',$)')iabntx
	  read(5,*)(xtick(i),i=1,iabntx)
	  if(ifimg.gt.0)then
	    xscal=xran/(xhi-xlo)
	    call imlgrd(xad,yad-ygrofs,xscal,0.,xtick,ntx,tiksiz)
	    if(ifbox.ne.0) call imlgrd(xad,yad+yran+ygrofs,xscal,0.,
     &		xtick,ntx,-tiksiz)
	  else
	    xscal=ixran/(xhi-xlo)
	    call logax(ixlo,iylo,xscal,0.,xtick,ntx)
	  endif
	endif
	iyran=102.*yran	
	if(iflogy.eq.0.or.ifplt.ge.0)then
	  if(ifimg.gt.0)then
	    yscal=yran/(yhi-ylo)
	    call imgrid(xad-xgrofs,yad,0.,yran,nty,tiksiz)
	    if(ifbox.ne.0) call imgrid(xad+xran+xgrofs,yad,0.,
     &		yran,nty,-tiksiz)
	  else
	    idy=iyran/nty
	    iyran=idy*nty
	    yscal=iyran/(yhi-ylo)
	    call xygrd(ixlo,iylo,0,idy,nty)
	  endif
	else
	  ylo=alog10(ylo)
	  yhi=alog10(yhi)
	  write(*,'(i3,'' y ticks: '',$)')iabnty
	  read(5,*)(ytick(i),i=1,iabnty)
	  if(ifimg.gt.0)then
	    yscal=yran/(yhi-ylo)
	    call imlgrd(xad-xgrofs,yad,0.,yscal,ytick,nty,tiksiz)
	    if(ifbox.ne.0) call imlgrd(xad+xran+xgrofs,yad,0.,yscal,
     &		ytick,nty,-tiksiz)
	  else
	    yscal=iyran/(yhi-ylo)
	    call logax(ixlo,iylo,0.,yscal,ytick,nty)
	  endif
	endif
60	if(ifimg.gt.0)call imset(ithsym,c1,upi,c3,0)
	do 72 ng=1,ngrps
	  sx=0.
	  sy=0.
	  sxsq=0.
	  sysq=0.
	  sxy=0.
	  nn=0
	  do 70 i=1,nx
	    if(ngx(i).ne.ng)go to 70
	    if(iftyp.ne.0)then
	      if(ifgpl.eq.0)then
		write(iout,103)ng,namx(i),irecx(i),xx(i),colx(i)
     1		    ,irecy(i),yy(i),coly(i)
	      elseif(ifgpl.gt.0)then
		write(iout,203)ng,xx(i),yy(i)
	      else
		write(iout,203)ng,xx(i),yy(i),colx(i)
	      endif
	    endif
103	    format(i3,2i7,2f10.3,i10,2f10.3)
203	    format(i3,3f10.3)
	    sx=sx+xx(i)
	    sy=sy+yy(i)
	    sxy=sxy+xx(i)*yy(i)
	    sxsq=sxsq+xx(i)**2
	    sysq=sysq+yy(i)**2
	    nn=nn+1
	    if(ifgpl.eq.2)colx(nn)=yy(i)	      
70	  continue
	  if(nn.le.1)go to 72
	  rnum=nn*sxy-sx*sy
	  den=nn*sxsq-sx**2
	  radic=den*(nn*sysq-sy**2)
	  if(radic.le.0.)go to 71
	  rr=rnum/sqrt(radic)
	  aa=(sy*sxsq-sx*sxy)/den
	  bb=rnum/den
	  se=0.
	  term=(sysq-aa*sy-bb*sxy)
	  if(nn.gt.2.and.term.ge.0.)se=sqrt(term/(nn-2))
	  sa=se*sqrt(1./nn+(sx**2/nn)/den)
	  sb=se/sqrt(den/nn)
	  write(*,105)ng,nn,rr,aa,bb,sa,sb
105	  format(' grp',i3,', n=',i4,', r=',f6.3,', a=',f10.3,', b='
     1	      ,f10.3,', sa=',f9.3,', sb=',f9.3)
71	  if(ifgpl.eq.2.and.nn.ge.2.and.ifimg.gt.0)then
c	      
c	      order values in colx
c	      
	    do 74 i=1,nn-1
	      do 73 j=i+1,nn
		if(colx(j).lt.colx(i))then
		  xtmp=colx(i)
		  colx(i)=colx(j)
		  colx(j)=xtmp
		endif
73	      continue
74	    continue
c	      
c	      get percentiles and draw box
c	      
	    call imset(ithtuk,c1,c2,c3,0)
	    tukadj=(ithtuk-1)/upi
	    p10=pctile(colx,nn,0.10)
	    p25=pctile(colx,nn,0.25)
	    p50=pctile(colx,nn,0.50)
	    p75=pctile(colx,nn,0.75)
	    p90=pctile(colx,nn,0.90)
	    write(*,106)ng,nn,p10,p25,p50,p75,p90
106	    format(' grp',i3,', n=',i4,',  10,25,50,75,90%s:',5f9.3)
	    y10=yscal*(p10-ylo)+yad-tukadj
	    y25=yscal*(p25-ylo)+yad-tukadj
	    y50=yscal*(p50-ylo)+yad-tukadj
	    y75=yscal*(p75-ylo)+yad-tukadj
	    y90=yscal*(p90-ylo)+yad-tukadj
	    rx=xscal*(sx/nn-xlo)+xad
	    xcen=rx-tukadj
	    xlft=xcen-tukwid/2.
	    xrt=xcen+tukwid/2.
	    ticlft=xcen-tuktic/2.
	    ticrt=xcen+tuktic/2.
	    call imma(xlft,y50)
	    call imva(xrt,y50)
	    call imma(xlft,y25)
	    call imva(xrt,y25)
	    call imva(xrt,y75)
	    call imva(xlft,y75)
	    call imva(xlft,y25)
	    call imma(xcen,y25)
	    call imva(xcen,y10)
	    call imma(xcen,y75)
	    call imva(xcen,y90)
	    call imma(ticlft,y10)
	    call imva(ticrt,y10)
	    call imma(ticlft,y90)
	    call imva(ticrt,y90)
	    call imset(ithsym,c1,c2,c3,0)
c	      
c	      plot points outside 10/90 plus gap
c	    
	    do 76 i=1,nn
	      yt=max(ylo,min(yhi,colx(i)))
	      ry=yscal*(yt-ylo)+yad
	      if(ry.lt.y10-tukgap.or.ry.gt.y90+tukgap)
     &		  call imsymb(rx,ry,nsymb(ng))
76	    continue
	  endif
72	continue
	if(abs(ifplt).le.5)return
	if(ifgpl.eq.2)go to 82
	conadj=(ifcnct-1)/upi
	nglas=-1
	do 80 i=1,nx
	  xt=max(xlo,min(xhi,xx(i)))
	  yt=max(ylo,min(yhi,yy(i)))
	  docnct=(ifcnct.gt.0).and.(i.gt.1).and.(.not.bygrup.or.ngx(i)
     1	      .eq.nglas)
	  nglas=ngx(i)
	  rxlas=rxcon
	  rylas=rycon
	  rx=xscal*(xt-xlo)+xad
	  ry=yscal*(yt-ylo)+yad
	  rxcon=rx-conadj
	  rycon=ry-conadj
	  if(docnct)then
	    call imset(ifcnct,c1,c2,c3,0)
	    cutdist=1.1*symwid
	    fracut=0.
	    distot=sqrt((rxcon-rxlas)**2+(rycon-rylas)**2)
	    if(isymbt.ne.0.and.distot.gt.1.e-4)fracut=cutdist/distot
	    if(fracut.le.0.45)then
	      xcut=fracut*(rxcon-rxlas)
	      ycut=fracut*(rycon-rylas)
	      call imma(rxlas+xcut,rylas+ycut)
	      call imva(rxcon-xcut,rycon-ycut)
	    endif
	    call imset(ithsym,c1,c2,c3,0)
	  endif
	  isymbt=nsymb(ngx(i))
	  if(isymbt.eq.-2.and.ifimg.gt.0)then
	    call fracbx(rx,ry,colx(i))
	  else
	    if(isymbt.lt.0)isymbt=-i
	    if(ifimg.le.0)call symbl(int(rx),int(ry),isymbt)
	    if(ifimg.gt.0)call imsymb(rx,ry,isymbt)
	  endif
	  if(ifimg.gt.0.and.ifgpl.lt.0.and.ifnonzer.ne.0)then
	    ypos=yscal*(max(ylo,min(yhi,yy(i)+colx(i)))-ylo)+yad-conadj
	    yneg=yscal*(max(ylo,min(yhi,yy(i)-colx(i)))-ylo)+yad-conadj
	    if(ifcnct.gt.0)call imset(ifcnct,c1,c2,c3,0)
	    call imma(rxcon-errlen/2.,yneg)
	    call imva(rxcon+errlen/2.,yneg)
	    call imma(rxcon-errlen/2.,ypos)
	    call imva(rxcon+errlen/2.,ypos)
	    call imma(rxcon,yneg)
	    call imva(rxcon,ypos)
	    if(docnct)call imset(ithsym,c1,c2,c3,0)
	  endif
80	continue
82	call penup(1)
	do 90 il=1,nlines
90	call imdash(xscal,xlo,xad,yscal,ylo,yad)
	if(ifplt.lt.0.and.ifimg.gt.0) then
	  call label_axis
     &	    (xad,yad-ygrofs,xran,xscal,abs(ntx),xtick,iflogx,0)
	  call label_axis
     &	    (xad-xgrofs,yad,yran,yscal,abs(nty),ytick,iflogy,1)
	  call immisc(xscal,xlo,xad,xran,yscal,ylo,yad,yran)
	endif
c	if(ifimg.gt.0)call flushb
	return
	end

	subroutine gnplt(xx,yy,ngx,nx,nsymb,ngrps,iflogx,iflogy)
	dimension xx(*),ngx(*),nsymb(*),yy(*)
	dimension irecx(1),irecy(1),colx(1),coly(1),namx(1)
	common /pltp/ifgpl
	data ifgpl/0/
	ifgpl=1
	call bsplt(namx,xx,yy,ngx,nx,nsymb,ngrps,irecx,irecy
     1,colx,coly,iflogx,iflogy)
	return
	end

	subroutine errplt(xx,yy,ngx,nx,nsymb,ngrps,colx,iflogx,iflogy)
	dimension xx(*),ngx(*),nsymb(*),yy(*),colx(*)
	dimension irecx(1),irecy(1),coly(1),namx(1)
	common /pltp/ifgpl
	ifgpl=-1
	call bsplt(namx,xx,yy,ngx,nx,nsymb,ngrps,irecx,irecy
     1,colx,coly,iflogx,iflogy)
	return
	end

	subroutine boxplt(xx,yy,ngx,nx,nsymb,ngrps,colx,iflogx,iflogy)
	dimension xx(*),ngx(*),nsymb(*),yy(*),colx(*)
	dimension irecx(1),irecy(1),coly(1),namx(1)
	common /pltp/ifgpl
	ifgpl=2
	call bsplt(namx,xx,yy,ngx,nx,nsymb,ngrps,irecx,irecy
     1,colx,coly,iflogx,iflogy)
	return
	end

c	block data
c	common /pltp/ ifgpl
c	data ifgpl/0/
c	end


	subroutine setpos(irow,jcol)
	call chrout(27)
	call chrout(ichar('['))
	idig1=(irow+1)/10
	if(idig1.gt.0)call chrout(idig1+48)
	call chrout(48+(irow+1-10*idig1))
	call chrout(ichar(';'))
	idig2=(jcol+1)/100
	idig1=(jcol+1-100*idig2)/10
	if(idig2.gt.0)call chrout(idig2+48)
	if(idig1.gt.0.or.idig2.gt.0)call chrout(idig1+48)
	call chrout(48+(jcol+1-10*idig1-100*idig2))
	call chrout(ichar('H'))
	return
	end	



	function pctile(x,n,p)
	real*4 x(*)
	v=n*p+0.5
	v=max(1.,min(float(n),v))
	iv=min(v,n-1.)
	f=v-iv
	pctile=(1.-f)*x(iv)+f*x(iv+1)
c	write(*,'(f6.0,f9.3)')100.*p,pctile
	return
	end
