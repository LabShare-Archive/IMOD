c	  interpolateGrid returns a value from one position in a grid
c	  
c	  X, Y is the position in the grid
c	  dxGrid, dyGrid are the grid arrays, first dimension ixgDim
c	  nxGrid, nyGrid are the number of grid points in X & Y
c	  ixGridStrt, iyGridStrt are coordinates as which grid starts
c	  xGridIntrv, yGridIntrv are the spacing between grid points in X & Y
c	  dx and dy are the interpolated values at the given position
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c
	subroutine interpolateGrid(x, y, dxGrid, dyGrid, ixgDim,
     &	    nxGrid, nyGrid, ixGridStrt, xGridIntrv, iyGridStrt,
     &	    yGridIntrv, dx, dy)
	implicit none
	integer*4 ixgDim, nxGrid, nyGrid, ixGridStrt, iyGridStrt
	real*4 dxGrid(ixgDim, *), dyGrid(ixgDim, *)
	real*4 xGridIntrv, yGridIntrv, x, y, dx, dy
	real*4 xgrid,ygrid,fx1,fx,fy1,fy,c00,c10,c01,c11
	integer*4 ixg,iyg,ixg1,iyg1

	xgrid = 1. + (x - ixGridStrt) / xGridIntrv
	ixg=xgrid
	ixg=max(1,min(nxGrid-1,ixg))
	fx1=max(0.,min(1.,xgrid-ixg))		!NO EXTRAPOLATIONS ALLOWED
	fx=1.-fx1
	ixg1=ixg+1
	ygrid = 1. + (y - iyGridStrt) / yGridIntrv
	iyg=ygrid
	iyg=max(1,min(nyGrid-1,iyg))
	fy1=max(0.,min(1.,ygrid-iyg))
	fy=1.-fy1
	iyg1=iyg+1
	c00=fx*fy
	c10=fx1*fy
	c01=fx*fy1
	c11=fx1*fy1
c	  
c	  interpolate
c
	dx = c00*dxGrid(ixg,iyg) + c10*dxGrid(ixg1,iyg)
     &	    + c01*dxGrid(ixg,iyg1) + c11*dxGrid(ixg1,iyg1)
	dy = c00*dyGrid(ixg,iyg) + c10*dyGrid(ixg1,iyg)
     &	    + c01*dyGrid(ixg,iyg1) + c11*dyGrid(ixg1,iyg1)
	return
	end
