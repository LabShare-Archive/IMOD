C*CUBINTERP.FOR***************************************************
C
C	This subroutine will perform coordinate transformations
C	(rotations,translations, etc.) by cubic interpolation.
c       It eliminates all range tests from the inner loop, but falls back
c	to quadratic interpolation (which range tests) around the edges of
c	the input image area.
C
C	BRAY = T[ ARRAY ]*scale
C
C	ARRAY	- The input image array
C	BRAY	- The output image array
C	NXA,NYA	- The dimensions of ARRAY
C	NXB,NYB	- The dimensions of BRAY
C	AMAT	- A 2x2 matrix to specify rotation,scaling,skewing
C	XC,YC	- The cooridinates of the Center of ARRAY
C	XT,YT	- The translation to add to the final image. The
C		  center of the output array is normally taken as
C		  NXB/2, NYB/2
C	SCALE	- A multiplicative scale actor for the intensities
C       DMEAN   - Mean intensity of image- dnm added to use this instead of 0
C	LINEAR  - Set non-zero to do linear interpolation
C	
C	Xo = a11(Xi - Xc) + a12(Yi - Yc) + NXB/2. + XT
C	Yo = a21(Xi - Xc) + a22(Yi - Yc) + NYB/2. + YT
C	  
c	  written by DNM, April 2000.
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.1  2002/07/31 01:09:22  mast
c	  Changed margins for computing xlft and xrt from 2.0 and 1.001 to
c	  2.01 and 1.01 because of crash on SGI, probably due to rounding
c	  error
c	
c
	SUBROUTINE CUBINTERP(ARRAY,BRAY,NXA,NYA,NXB,NYB,AMAT,
     &	    XC,YC,XT,YT,SCALE,DMEAN, linear)
	implicit none
	integer*4 NXA,NYA,NXB,NYB,linear
	real*4 ARRAY(NXA,NYA),BRAY(NXB,NYB),AMAT(2,2)
	real*4 XC,YC,XT,YT,SCALE,dmean
	real*4 xcen,ycen,xco,yco,denom,a11,a12,a22,a21,dyo,xbase,ybase
	real*4 xst,xnd,xlft,xrt,xp,yp,dennew,dx,dy,v2,v4,v6,v8,v5,a,b,c,d
	real*4 dxm1,dxdxm1,fx1,fx2,fx3,fx4,dym1,dydym1,v1,v3,vmin,vmax
	integer*4 iy,ix,ixp,ixpp1,iyp,iypp1,iypp2,ixpp2,ixpm1,iypm1,linefb
	integer*4 ixnd,ixst,ixfbst,ixfbnd,iqst,iqnd,ifall
C
C   Calc inverse transformation
C
	XCEN = NXB/2. + XT + 1
	YCEN = NYB/2. + YT + 1
	XCO = XC + 1
	YCO = YC + 1
	DENOM = AMAT(1,1)*AMAT(2,2) - AMAT(1,2)*AMAT(2,1)
	A11 =  AMAT(2,2)/DENOM
	A12 = -AMAT(1,2)/DENOM
	A21 = -AMAT(2,1)/DENOM
	A22 =  AMAT(1,1)/DENOM
C
C Loop over output image
C
	DO IY = 1,NYB
	  DYO = IY - YCEN
	  xbase = a12*dyo +xco - a11*xcen
	  ybase = a22*dyo + yco - a21*xcen
	  xst=1
	  xnd=nxb
	  linefb=0
	  if(abs(a11).gt.1.e-10)then
	    xlft=(2.01-xbase)/a11
	    xrt=(nxa-1.01-xbase)/a11
	    xst=max(xst,min(xlft,xrt))
	    xnd=min(xnd,max(xlft,xrt))
	  elseif(xbase.lt.2..or.xbase.ge.nxa-1.)then
	    xst=nxb
	    xnd=1
	    if(xbase.ge.0.5.or.xbase.le.nxa+0.5)linefb=1
	  endif
	  if(abs(a21).gt.1.e-10)then
	    xlft=(2.01-ybase)/a21
	    xrt=(nya-1.01-ybase)/a21
	    xst=max(xst,min(xlft,xrt))
	    xnd=min(xnd,max(xlft,xrt))
	  elseif(ybase.lt.2..or.ybase.ge.nya-1.)then
	    xst=nxb
	    xnd=1
	    if(ybase.ge.0.5.or.ybase.le.nya+0.5)linefb=1
	  endif
c	    
c	    truncate the ending value down and the starting value up
c
	  ixnd=xnd
	  ixst=nxb+1-int(nxb+1-xst)
c	    
c	    if they're crossed, set them up so fill will do whole line
c
	  if(ixst.gt.ixnd)then
	    ixst=nxb/2
	    ixnd=ixst-1
	  endif
c	    
c	    set up fallback region limits depending on whether doing 2 pixels
c	    or whole line
c
	  if(linefb.eq.0)then
	    ixfbst=max(1,ixst-2)
	    ixfbnd=min(nxb,ixnd+2)
	  else
	    ixfbst=1
	    ixfbnd=nxb
	  endif
c	    
c	    do fill outside of fallback
c
	  do ix=1,ixfbst-1
	    bray(ix,iy)=dmean
	  enddo
	  do ix=ixfbnd+1,nxb
	    bray(ix,iy)=dmean
	  enddo
c	    
c	    do fallback to quadratic with tests
c	    
	  iqst=ixfbst
	  iqnd=ixst-1
	  do ifall=1,2
	    if (linear .eq. 0) then
C		    
C		    Do quadratic interpolation
C		    
	      do ix=iqst,iqnd
		xp = a11*ix + xbase
		yp = a21*ix + ybase
		IXP = NINT(XP)
		IYP = NINT(YP)
		dennew = DMEAN
		IF (IXP .ge. 1 .and. IXP .le. NXA .and. IYP .ge. 1 .and.
     &		    IYP .le. NYA) then
		  DX = XP - IXP
		  DY = YP - IYP
		  IXPP1 = IXP + 1
		  IXPM1 = IXP - 1
		  IYPP1 = IYP + 1
		  IYPM1 = IYP - 1
		  IF (IXPM1 .LT. 1) IXPM1 = 1
		  IF (IYPM1 .LT. 1) IYPM1 = 1
		  IF (IXPP1 .GT. NXA) IXPP1 = NXA
		  IF (IYPP1 .GT. NYA) IYPP1 = NYA
C		    
C		    Set up terms for quadratic interpolation
C		    
		  V2 = ARRAY(IXP, IYPM1)
		  V4 = ARRAY(IXPM1, IYP)
		  V5 = ARRAY(IXP, IYP)
		  V6 = ARRAY(IXPP1, IYP)
		  V8 = ARRAY(IXP, IYPP1)
		  vmax=max(v2,v4,v5,v6,v8)
		  vmin=min(v2,v4,v5,v6,v8)
C		    
		  A = (V6 + V4)*.5 - V5
		  B = (V8 + V2)*.5 - V5
		  C = (V6 - V4)*.5
		  D = (V8 - V2)*.5
C		    
		  dennew = SCALE*(A*DX*DX + B*DY*DY + C*DX + D*DY + V5)
		  if(dennew.gt.vmax)dennew=vmax
		  if(dennew.lt.vmin)dennew=vmin
		endif
		bray(ix,iy)=dennew
C		  
	      enddo

	    else
c		
c		fallback to linear
c
	      do ix=iqst,iqnd
		xp = a11*ix + xbase
		yp = a21*ix + ybase
		IXP = XP
		IYP = YP
		bray(ix,iy)=dmean
		IF (IXP .ge. 1 .and. IXP .lt. NXA .and. IYP .ge. 1 .and.
     &		    IYP .lt. NYA) then
		  DX = XP - IXP
		  DY = YP - IYP
		  IXPP1 = IXP + 1
		  IYPP1 = IYP + 1
		  bray(ix, iy) = (1. - dy) * ((1. - dx) * array(ixp, iyp) +
     &		      dx * array(ixpp1, iyp)) +
     &		      dy * ((1. - dx) * array(ixp, iypp1) +
     &		      dx * array(ixpp1, iypp1))
		  
		endif
	      enddo
	    endif
	    iqst=ixnd+1
	    iqnd=ixfbnd
	  enddo

	  if (linear .eq. 0) then
C
C   Do cubic interpolation on the central region
C
	    DO IX = ixst,ixnd
	      xp = a11*ix + xbase
	      yp = a21*ix + ybase
	      IXP = XP
	      IYP = YP
	      DX = XP - IXP
	      DY = YP - IYP
	      IXPP1 = IXP + 1
	      IXPM1 = IXP - 1
	      IYPP1 = IYP + 1
	      IYPM1 = IYP - 1
	      ixpp2 = ixp + 2
	      iypp2 = iyp + 2
	      
	      dxm1 = dx-1.
	      dxdxm1=dx*dxm1
	      fx1=-dxm1*dxdxm1
	      fx4=dx*dxdxm1
	      fx2=1+dx**2*(dx-2.)
	      fx3=dx*(1.-dxdxm1)
	      
	      dym1 = dy-1.
	      dydym1=dy*dym1
	      
	      v1=fx1*array(ixpm1,iypm1)+fx2*array(ixp,iypm1)+
     &		  fx3*array(ixpp1,iypm1)+fx4*array(ixpp2,iypm1)
	      v2=fx1*array(ixpm1,iyp)+fx2*array(ixp,iyp)+
     &		  fx3*array(ixpp1,iyp)+fx4*array(ixpp2,iyp)
	      v3=fx1*array(ixpm1,iypp1)+fx2*array(ixp,iypp1)+
     &		  fx3*array(ixpp1,iypp1)+fx4*array(ixpp2,iypp1)
	      v4=fx1*array(ixpm1,iypp2)+fx2*array(ixp,iypp2)+
     &		  fx3*array(ixpp1,iypp2)+fx4*array(ixpp2,iypp2)
	      bray(ix,iy)=-dym1*dydym1*v1+(1.+dy**2*(dy-2.))*v2+
     &		  dy*(1.-dydym1)*v3 +dy*dydym1*v4
c		
	    enddo

	  else
c	      
c	      do linear interpolation
c	      
	    DO IX = ixst,ixnd
	      xp = a11*ix + xbase
	      yp = a21*ix + ybase
	      IXP = XP
	      IYP = YP
	      DX = XP - IXP
	      DY = YP - IYP
	      IXPP1 = IXP + 1
	      IYPP1 = IYP + 1
	      bray(ix, iy) = (1. - dy) * ((1. - dx) * array(ixp, iyp) +
     &		  dx * array(ixpp1, iyp)) +
     &		  dy * ((1. - dx) * array(ixp, iypp1) +
     &		  dx * array(ixpp1, iypp1))
	    enddo
	  endif
	enddo
C
	RETURN
	END

