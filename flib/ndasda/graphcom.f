

	subroutine graphcom(ifanyplot,maxgraph,nextragrf,listextra)
	logical checkgrf
	integer*4 listextra(*),igrplt(100)
	data xgutter/0.2/,ygutter/0.2/
	integer*4 in5
	common /nmsinput/ in5
c	  
	print *,'Enter list of graphs to plot (ranges ok)'
	call rdlist(in5,igrplt,ngrplt)
	if(ngrplt.le.0)return
	do i=1,ngrplt
	  if(.not.checkgrf(igrplt(i),maxgraph,nextragrf,listextra))then
	    print *,'illegal graph #'
	    return
	  endif
	enddo
	write(*,'(1x,a,$)')'# of columns and rows: '
	read(in5,*)ncol,nrow
	if(nrow.le.0.or.ncol.le.0)return
	write(*,'(1x,a,$)')
     &	    '0 to fill row by row, 1 to fill column by column: '
	read(in5,*)ifbycol
	write(*,'(1x,a,$)')'# of ticks along X and Y: '
	read(in5,*)nxtick,nytick
	write(*,'(1x,a,2f5.2,a,$)')'X and Y gutter size (/ for',xgutter
     &	    ,ygutter,'): '
	read(in5,*)xgutter,ygutter
	write(*,'(1x,a,$)')
     &	    'Value to scale Y to, or 0 for no rescale of Y: '
	read(in5,*)comymax
c	  
	close(5)
c 7/14/00 CER remove carriagecontrol for  g77
	open(5,file='ndamtk.tmp',status='new')
	ifanytmp=ifanyplot
	irow=1
	icol=1
	xsize=(7.29-(ncol-1)*xgutter)/ncol
	ysize=(7.29-(nrow-1)*ygutter)/nrow
	do igrf=1,ngrplt
	  write(5,5)5
5	  format(5i4)
	  write(5,5)igrplt(igrf)
	  if(comymax.gt.0.)then
	    write(5,5)6
	    write(5,5)1,1
	    write(5,6)comymax
6	    format(f10.3)
	  endif
	  write(5,5)8
	  write(5,5)1,0
	  if(ifanytmp.ne.0)then
	    if(irow.eq.1.and.icol.eq.1)then
	      write(5,5)1
	    else
	      write(5,5)0
	    endif
	  endif
	  ifanytmp=ifanytmp+1
	  xpos=(icol-1)*(xsize+xgutter)
	  ypos=(nrow-irow)*(ysize+ygutter)
	  write(5,7)xsize,ysize,xpos,ypos
7	  format(4f6.3)
	  write(5,8)-nxtick,-nytick,-.05,1,1
8	  format(2i5,f6.2,2i5)
	  write(5,5)0,0
	  write(5,5)0,0
	  write(5,5)0,0,0,0
	  if(ifbycol.ne.0)then
	    irow=irow+1
	    if(irow.gt.nrow)then
	      irow=1
	      icol=icol+1
	      if(icol.gt.ncol)icol=1
	    endif
	  else
	    icol=icol+1
	    if(icol.gt.ncol)then
	      icol=1
	      irow=irow+1
	      if(irow.gt.nrow)irow=1
	    endif
	  endif
	enddo
	write(5,5)24
	write(5,5)
	rewind 5
	return
	end
