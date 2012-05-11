c         $Id$
c       
c       Module for variables created when new mapping into local subsets added(?)
      module tltcntrl
c
c       alivar is needed just for definition of maxview, but this allows users to just
c       use this module
      use alivar
      integer maxAllReal
      parameter (maxAllReal=50000)
      real*4 tiltorig(maxview),gmagorig(maxview),rotorig(maxview)
      integer*4 iobjali(maxreal),iobjseq(maxAllReal)
      real*4 dxysav(2,maxview),xyzsav(3,maxAllReal),tltall(maxview)
      integer*4 nmapSpecMag(maxgrp),ivSpecStrMag(maxgrp),ivSpecEndMag(maxgrp)
      integer*4 nmapSpecRot(maxgrp),ivSpecStrRot(maxgrp),ivSpecEndRot(maxgrp)
      integer*4 nmapSpecTilt(maxgrp),ivSpecStrTilt(maxgrp)
      integer*4 ivSpecEndTilt(maxgrp)
      integer*4 ivsepIn(maxview,maxgrp),nsepingrpIn(maxgrp)
      integer*4 nvuall,imintilt,mininview,minvtiltali,initxyzdone
      real*4 randoaxis,randotilt, scalexy,xcen,ycen,xorig,yorig
      real*4 xdelt,ydelt,facm,eps,cgx,cgy
      integer*4 nsolve,ncycle,nvlocal,nobjdo
      integer*4 nmapMag,nmapRot,nmapTilt,nRanSpecMag,nRanSpecRot,nRanSpecTilt
      end module tltcntrl
