c       rotmatwarp.inc  - definitions of array sizes and commons for
c       Rotatevol, Matchvol, and Warpvol
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 3.6  2008/08/29 21:34:47  mast
c       Swapped array and brray to dodge bug in gfortran 4.3
c
c       Revision 3.5  2007/08/30 20:13:38  mast
c       Change izinfile from i*2 3d to i*4 1D array
c
c       Revision 3.4  2006/06/03 00:03:00  mast
c       How about nominal 500 MB instead
c
c       Revision 3.3  2006/06/01 14:17:48  mast
c       Increased dimensions to nominal 256 MB
c
c       Revision 3.2  2005/04/14 00:35:58  mast
c       *** empty log message ***
c	
c       Revision 3.1  2003/10/11 00:22:07  mast
c       Creation of file
c	
c       
c       keep limdim less than 2/3 of inpdim**2
c       with this assumptiom, memory in MB is 16/3 * inpdim**3 + 10 * inpdim**2
c       
      integer inpdim, limdim, lmcube, limout, limizfile
      parameter (inpdim=530,limdim=180000,lmcube=limdim/inpdim)
      parameter (limout=(inpdim*3)/2, limizfile=4*limout*lmcube)
c       
      real*4 array(inpdim,inpdim,inpdim),brray(limout,limout)
      common /bigarr/brray,array
c       
      integer*4 nxyzin(3),nxyzout(3)
      integer*4 nxin,nyin,nzin,nxout,nyout,nzout
      real*4 cxyzin(3),cxyzout(3)
      real*4 cxin,cyin,czin,cxout,cyout,czout
      common /xyz/nxin,nyin,nzin,nxout,nyout,nzout,cxin,cyin,czin
     &    ,cxout,cyout,czout
      equivalence (nxyzin(1),nxin),(nxyzout(1),nxout)
      equivalence (cxyzin(1),cxin),(cxyzout(1),cxout)
c       
      real*4 minv(3,3),title(20),dmeanin
      integer*4 ncubes(3),mode
      integer*4 nxyzcube(3,lmcube),ixyzcube(3,lmcube),izinfile(limizfile)
      integer*2 ifile(lmcube,lmcube)
      common /cubes/minv,ncubes,nxyzcube,ixyzcube, mode,title,dmeanin,
     &    ifile,izinfile
