c	  rotmatwarp.inc  - definitions of array sizes and commons for
c	  Rotatevol, Matchvol, and Warpvol
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c
c	  keep limdim less than 2/3 of inpdim**2
        integer inpdim, limdim, lmcube, limout
	parameter (inpdim=256,limdim=16200,lmcube=limdim/inpdim)
	parameter (limout=(inpdim*3)/2)
c
	real*4 array(inpdim,inpdim,inpdim),brray(limout,limout)
	common /bigarr/array,brray
c
	integer*4 nxyzin(3),nxyzout(3)
        integer*4 nxin,nyin,nzin,nxout,nyout,nzout
	real*4 cxyzin(3),cxyzout(3)
	real*4 cxin,cyin,czin,cxout,cyout,czout
	common /xyz/nxin,nyin,nzin,nxout,nyout,nzout,cxin,cyin,czin
     &	    ,cxout,cyout,czout
	equivalence (nxyzin(1),nxin),(nxyzout(1),nxout)
	equivalence (cxyzin(1),cxin),(cxyzout(1),cxout)
c
        real*4 minv(3,3),title(20),dmeanin
	integer*4 ncubes(3),mode
	integer*4 nxyzcube(3,lmcube),ixyzcube(3,lmcube)
	integer*2 ifile(lmcube,lmcube),izinfile(lmcube,lmcube,limout)
        common /cubes/minv,ncubes,nxyzcube,ixyzcube, mode,title,dmeanin,
     &	    ifile,izinfile
