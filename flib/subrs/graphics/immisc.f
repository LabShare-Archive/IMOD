c	  IMMISC draws miscellaneous items for publication-quality graphs:
c	  text lables, letters in circles, symbols in boxes, and solid or
c	  dashed lines.
c	  The parameters define a transformation between plotting area and user
c	  units: x (in inches) = XSCAL*(x in user units - XLO)+XAD
c	  XRAN, YRAN are total ranges of graph frame in inches
c
	subroutine immisc(xscal,xlo,xad,xran,yscal,ylo,yad,yran)
	character*160 line
	character*320 pwrstr
	character*1 letter
	save ntext,ncirc,nsymb,nlines
	data ntext/0/,ncirc/0/,nsymb/0/,nlines/0/
c	  
c	  modified for PS - Postscript
	data pwxupi/107./
c
	call setpwrmode
c	  
	write(*,'(1x,a,$)')'Number of text strings, letters in '//
     &	    'circles, symbols in boxes, lines: '
	read(5,*)ntext,ncirc,nsymb,nlines
	do 10 itext=1,ntext
	  write(*,'(a,i2)')' Enter parameters for text string # ',itext
	  call cnvrtpos(xscal,xlo,xad,xran,yscal,ylo,yad,yran,xpos,ypos)
	  write(*,'(1x,a,$)')'0 to center, 1 to right justify, '//
     &	      'or -1 to left justify to that position: '
	  read(5,*)just
	  write(*,'(1x,a,$)')'Character size, orientation in degrees: '
	  read(5,*)size,jor
	  jsize=pwxupi*size
	  write(*,*)'Enter text string'
	  read(5,'(a)')line
	  call pwritx_parse(line,pwrstr,nchar)
	  call pwritx(xpos,ypos,pwrstr,nchar,jsize,jor,just)
10	continue
c
	do 20 icirc=1,ncirc
	  write(*,'(a,i2)')' Enter parameters for circled letter # ',
     &	      icirc
	  call cnvrtpos(xscal,xlo,xad,xran,yscal,ylo,yad,yran,xpos,ypos)
	  write(*,'(1x,a,$)')'Circle diameter and thickness: '
	  read(5,*)circsize,ithick
	  write(*,'(1x,a,$)')'Character size: '
	  read(5,*)size
	  jsize=pwxupi*size
	  write(*,'(1x,a,$)')'Letter: '
	  read(5,'(a)')letter
c	  
c	    modified for PS - Postscript
	  call imset(ithick,c1,c2,c3,0)
	  thkofs=0.5*(ithick-1)/c2
	  call pwritx(xpos+thkofs,ypos+thkofs-0.007,letter,1,jsize,0,0)
	  call imma(xpos+circsize/2.,ypos)
	  do 15 i=0,32
	    theta=i*3.14159/16.
	    xx=xpos+0.5*circsize*cos(theta)
	    yy=ypos+0.5*circsize*sin(theta)
	    call imva(xx,yy)
15	  continue 
20	continue
c	  
	do 30 isymb=1,nsymb
	  write(*,'(a,i2)')' Enter parameters for boxed symbol # ',isymb
	  call cnvrtpos(xscal,xlo,xad,xran,yscal,ylo,yad,yran,xpos,ypos)
	  write(*,'(1x,a,/,a,$)')'Enter symbol type (0 for none), size,'
     &	      //' and thickness,',
     &	      '    and box size (0 for none) and thickness: '
	  read(5,*)itype,size,isymthk,boxsiz,iboxthk
	  if(itype.ne.0)then
	    call symsiz(size)
	    call imset(isymthk,c1,upi,c3,0)
	    call imsymb(xpos,ypos,itype)
	  endif
	  if(boxsiz.ne.0.)then
	    call imset(iboxthk,c1,upi,c3,0)
	    xyadj=0.5*(iboxthk-1)/upi
	    call imma(xpos-xyadj-boxsiz/2.,ypos-xyadj-boxsiz/2.)
	    call imvi(boxsiz,0.)
	    call imvi(0.,boxsiz)
	    call imvi(-boxsiz,0.)
	    call imvi(0.,-boxsiz)
	  endif
30	continue
c	  
	do 50 iline=1,nlines
	  write(*,'(a,i2)')' Enter parameters for line # ',iline
	  write(*,'(1x,a,$)')'0 for user units, 1 for absolute'//
     &	      ' inches, or -1 for units relative to frame: '
	  read(5,*)iscltyp
	  write(*,'('' slope, intercept, start & end x (in those units),
     1 thickness (- to switch x&y),'')')
	  write(*,'('' dash on and off in inches
     1 (0,0 no dash): '',$)')
	  read(5,*)b,a,xstr,xend,linthk,dshon,dshoff
	  if(iscltyp.gt.0)then
	    call dodash(1.,0.,0.,1.,0.,0.,b,a,xstr,xend,linthk,dshon,
     &		dshoff)
	  elseif(iscltyp.lt.0)then
	    call dodash(xran,0.,xad,yran,0.,yad,b,a,xstr,xend, linthk,
     &		dshon,dshoff)
	  else
	    call dodash(xscal,xlo,xad,yscal,ylo,yad,b,a,xstr,xend,
     &		linthk, dshon,dshoff)
	  endif
50	continue
	return
	end


	subroutine cnvrtpos(xscal,xlo,xad,xran,yscal,ylo,yad,yran,xpos,
     &	      ypos)
	write(*,'(1x,a,/,a,$)')'Enter X and Y coordinates, and 0 if'//
     &	    ' user units','    or 1 if absolute inches or -1 if'//
     &	    ' relative to graph frame: '
	read(5,*)xpos,ypos,iscltyp
	if(iscltyp.lt.0)then
	  xpos=xad+xran*xpos
	  ypos=yad+yran*ypos
	elseif(iscltyp.eq.0)then
	  xpos=xscal*(xpos-xlo)+xad
	  ypos=yscal*(ypos-ylo)+yad
	endif
	return
	end

