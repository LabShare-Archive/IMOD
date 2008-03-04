c         $Id$
c
c	  $Log$
c	  Revision 1.1  2005/04/07 04:01:22  mast
c	  Created when new mapping implemented
c	
c
        integer maxgrp,maxAllReal
        parameter (maxgrp=20,maxAllReal=50000)
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
	common /tltcntrl/nvuall,imintilt,mininview,minvtiltali,
     &	    randoaxis,randotilt, scalexy,xcen,ycen,cgx,cgy,xorig,
     &	    yorig,xdelt,ydelt, initxyzdone,nsolve,facm, ncycle,eps,
     &	    tiltorig,gmagorig,rotorig, tltall,nmapSpecMag,ivSpecStrMag,
     &	    ivSpecEndMag,nmapSpecRot,ivSpecStrRot,ivSpecEndRot,
     &	    nmapSpecTilt,ivSpecStrTilt,ivSpecEndTilt,ivsepIn,nsepingrpIn,
     &	    iobjali,dxysav,xyzsav,nvlocal,iobjseq,nobjdo,
     &	    nmapMag,nmapRot,nmapTilt,nRanSpecMag,nRanSpecRot,nRanSpecTilt
