c         $Id$
c       
c       Module for variables created when new mapping into local subsets added(?)
      module tltcntrl
c
      use alivar
c       allocated to limview or maxInArea
      real*4, allocatable :: tiltorig(:),gmagorig(:),rotorig(:),dxysav(:,:),tltall(:)
      integer*4, allocatable :: iobjali(:), ivsepIn(:,:)
      integer*4 nmapSpecMag(maxgrp),ivSpecStrMag(maxgrp),ivSpecEndMag(maxgrp)
      integer*4 nmapSpecRot(maxgrp),ivSpecStrRot(maxgrp),ivSpecEndRot(maxgrp)
      integer*4 nmapSpecTilt(maxgrp),ivSpecStrTilt(maxgrp)
      integer*4 ivSpecEndTilt(maxgrp)
      integer*4 nsepingrpIn(maxgrp)
      integer*4 nvuall,imintilt,mininview,minvtiltali,initxyzdone
      real*4 randoaxis,randotilt, scalexy,xcen,ycen,xorig,yorig
      real*4 xdelt,ydelt,facm,eps,cgx,cgy
      integer*4 ncycle,nvlocal,nobjdo,maxh
      integer*4 nmapMag,nmapRot,nmapTilt,nRanSpecMag,nRanSpecRot,nRanSpecTilt
c       Allocated to maxAllReal, and (3, maxAllReal)
      integer*4, allocatable :: iobjseq(:)
      real*4, allocatable :: xyzsav(:,:)
      real*4, allocatable :: h(:)
      end module tltcntrl
