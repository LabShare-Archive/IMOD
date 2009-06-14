c       rotmatwarp.f  - contains a common module for Rotatevol, Matchvol, and
c       Warpvol
c       
c       $Id$
c       Log at end
c       
c       
      module rotmatwarp
      implicit none
      integer lmcube
      parameter (lmcube=2500)
      integer*4 inpDim(3), idimOut(3)
      logical*4 needScratch(4)
      integer*4 memoryLim/768/
      real*8 arraySize8
      integer*4 iVerbose/0/
      integer*4 maxZout/64/
c       
      integer*4 nxyzin(3),nxyzout(3)
      integer*4 nxin,nyin,nzin,nxout,nyout,nzout
      real*4 cxyzin(3),cxyzout(3)
      real*4 cxin,cyin,czin,cxout,cyout,czout
      equivalence (nxyzin(1),nxin),(nxyzin(2),nyin),(nxyzin(3),nzin)
      equivalence (nxyzout(1),nxout),(nxyzout(2),nyout),(nxyzout(3),nzout)
      equivalence (cxyzin(1),cxin),(cxyzin(2),cyin),(cxyzin(3),czin)
      equivalence (cxyzout(1),cxout),(cxyzout(2),cyout),(cxyzout(3),czout)
c       
      real*4 minv(3,3),title(20),dmeanin
      integer*4 ncubes(3),mode,ioutXaxis,ioutYaxis,ioutZaxis,limInner, limOuter
      integer*4 idirXaxis,nxyzcube(3,lmcube),ixyzcube(3,lmcube)
      integer*4, allocatable :: izinfile(:,:,:)
      integer*2, allocatable :: ifile(:,:)
      real*4, allocatable :: array(:,:,:),brray(:)
      end module rotmatwarp

c       
c       $Log$
