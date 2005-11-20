c	  An interface between DNMs old graphics calls (for a Megatek board in
c	  a NOVA 1220) and the Parallax graphics device
c
c	  12/27/90: added ability to change color
c	  3/18/91: made it return and not turn on flag if p_start fails,
c	  made grfopn call erase if parallax is not on yet, to turn it on

c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.3  2005/11/19 17:01:13  mast
c	  Provide routines for calling getarg/iargc since these can be intrinsic
c	
c	  Revision 3.2  2003/08/29 17:01:05  mast
c	  Added an actual erase call to plax routines so they will clear stack
c	

c
	subroutine erase(ix)
	common /smplgr/ifplxon,ixcur,iycur,icolor
	integer*4 p_start
	if(ifplxon.lt.0)return
	if(ifplxon.eq.0)then
	  if( p_start().eq.-1)return
	  ifplxon=1
	  icolor=241
	endif
c	call p_zoom(1,1)
c	call p_pan(0,1023)
	call plax_erase()
	call p_clt8(0,0,0,0)
	call p_clt8(241,255,255,255)
	call p_clt8(250,255,0,0)
	call p_clt8(251,0,255,0)
	call p_clt8(252,0,0,255)
	call p_clt8(253,255,255,0)
	call p_clt8(254,255,0,255)
	call p_clt8(255,0,255,255)
	call p_box(0,0,0,1279,1023)
	ixcur=0
	iycur=0
	call p_b_flush
	return
c
	entry plxoff
	if(ifplxon.le.0)return
	call p_box(0,0,0,1279,1023)
	call p_end
	ifplxon=0
	return
c	  
	entry updat(ix)
	if(ifplxon.le.0)return
	call p_b_flush
	return
c	  
	entry chgcol(ix)
	icolor=ix
	return
	end
c	  
	subroutine grfopn(iffil)
	common /smplgr/ifplxon,ixcur,iycur,icolor
c	  if iffil is 0, want it on, so bump from -1 to 0 or leave as is
	if(iffil.eq.0)then
	  ifplxon=max(0,ifplxon)
	  if(ifplxon.eq.0)call erase(1)
	else
c	    otherwise, want it off; turn it off, mark flag as -1
	  call plxoff
	  ifplxon=-1
	endif
	return
	end

	subroutine ma(ix,iy)
	common /smplgr/ifplxon,ixcur,iycur,icolor
	ixcur=ix
	iycur=iy
	return
c	  
	entry mi(ix,iy)
	ixcur=ixcur+ix
	iycur=iycur+iy
	return
c	  
	entry va(ix,iy)
	if(ifplxon.le.0)return
	call p_vect(icolor,ixcur,iycur,ix,iy)
	ixcur=ix
	iycur=iy
	return
c
	entry vi(ix,iy)
	if(ifplxon.le.0)return
	call p_vect(icolor,ixcur,iycur,ixcur+ix,iycur+iy)
	ixcur=ixcur+ix
	iycur=iycur+iy
	return
c	  
	entry pa(ix,iy)
	if(ifplxon.le.0)return
	ixcur=ix
	iycur=iy
	call p_circ(icolor,1,ixcur,iycur)
	return
c
	entry pi(ix,iy)
	if(ifplxon.le.0)return
	ixcur=ixcur+ix
	iycur=iycur+iy
	call p_circ(icolor,1,ixcur,iycur)
	return
	end
c	  
	subroutine dspnts(jx,jy,np)
	integer*4 jx(*),jy(*)
	do i=1,np
	  call pa(jx(i),jy(i))
	enddo
	call updat(1)
	return
	end
c	  
	subroutine dslins(jx,jy,np)
	integer*4 jx(*),jy(*)
	do i=1,np
	  call va(jx(i),jy(i))
	enddo
	call updat(1)
	return
	end
c	  
	subroutine scpnt(ix,iy,itype)
	common /smplgr/ifplxon,ixcur,iycur,icolor
	integer*2 ivec(6)
	character*4 dummy
	if(ifplxon.le.0)return
c	  size was 5 for Parallax - set to 8 for X windows
	isiz=8
	isizm=isiz-1
	if(itype.lt.0)then
	  iscal=nint(2*(isiz+3)/2.5)
	  write(dummy,'(i4)')-itype
	  nchar=alog10(float(-itype))+1.0001
	  iyofs=nint(2.5*iscal/2.)
	  ixofs=nint((7.5-nchar)*iscal)
	  call p_sctext(1,iscal,iscal,icolor,ix-ixofs,iy-iyofs,dummy)
	  return
	endif
	if(itype.gt.0)itype=mod(itype-1,8)+1
	go to(1,2,3,4,5,5,7,8)itype
	return
1	call p_circ(icolor,isiz,ix,iy)
	return
2	call p_circo(icolor,isiz,ix,iy)
	call p_circo(icolor,isizm,ix,iy)
	return
3	call p_box(icolor,ix-isiz,iy-isiz,ix+isiz,iy+isiz)
	return
4	call p_boxo(icolor,ix-isiz,iy-isiz,ix+isiz,iy+isiz)
	call p_boxo(icolor,ix-isizm,iy-isizm,ix+isizm,iy+isizm)
	return
5	ivec(1)=ix-isiz
	ivec(3)=ix+isiz
	ivec(5)=ix
	leny=nint(1.732*isiz)
	iybot=iy-leny/3
	ivec(2)=iybot
	ivec(4)=iybot
	ivec(6)=leny+iybot
	if(itype.eq.5)then
	  call p_poly(icolor,3,ivec)
	else
	  call p_polyo(icolor,3,ivec)
	endif
	return
7	call p_vect(icolor,ix-isizm,iy-isizm,ix+isizm,iy+isizm)
	call p_vect(icolor,ix+isizm,iy-isizm,ix-isizm,iy+isizm)
	return
8	call p_vect(icolor,ix,iy-isiz,ix,iy+isiz)
	call p_vect(icolor,ix-isiz,iy,ix+isiz,iy)
	return
	end

	subroutine dsgrd(ix0,iy0,idx,idy,ninterv)
	common /smplgr/ifplxon,ixcur,iycur,icolor
	if(ifplxon.le.0)return
	tiksiz=5.
	axang=atan2(float(idy),float(idx))
	ixtick=-tiksiz*sin(axang)
	iytick=tiksiz*cos(axang)
	do i=0,ninterv
	  ixcen=ix0+i*idx
	  iycen=iy0+i*idy
	  call p_vect(icolor,ixcen-ixtick,iycen-iytick,ixcen+ixtick,
     &	      iycen+iytick)
	enddo
	call p_vect(icolor,ix0,iy0,ixcen,iycen)
	call updat(1)
	return
	end

	subroutine label(numb,nchar)
	common /smplgr/ifplxon,ixcur,iycur,icolor
	character*8 dummy,dum2
	if(ifplxon.le.0)return
	dum2=' '
	write(dummy,'(i8)')numb
	dum2(1:nchar)=dummy(9-nchar:8)
	call p_sctext(1,8,8,icolor,ixcur,iycur,dum2)
c	call updat(1)
	return
	end

	subroutine xyset(ix)
	entry setno(ix)
	entry penup(ix)
	entry pendn(ix)
	entry grfwat(ix)
	entry xyma(ix,iy)
	entry xyva(ix,iy)
	entry xypa(ix,iy)
	entry xymi(ix,iy)
	entry xyvi(ix,iy)
	entry xypi(ix,iy)
	entry xypnt(ix)
	entry xyspd(ix,iy)
	entry symbl(ix,iy,iz)
	entry xygrd(ix,iy,iz,ia,ib)
	entry logax(ix,iy,rx,ry,rz,iz)
	return
	end

	subroutine fortgetarg(i, string)
	character*(*) string
	call getarg(i, string)
	return
	end

	integer*4 function fortiargc()
	fortiargc = iargc()
	return
	end

