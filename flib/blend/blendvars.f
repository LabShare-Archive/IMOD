c       $Id$
c
      module blendvars
      implicit none
      integer ifastsiz,maxbin,liminit,memMinimum,memPreferred
      integer memlim,limedgbf,lmField,maxFields,memMaximum
      integer maxPcNear, maxDistNear,maxUseEdge,maxInPc
      integer limXcorrPeaks
c       
c       Needed size for idimc now determined by maximum unpadded size after
c       binning, 1K x 1K
c       
      parameter (ifastsiz=32,maxbin=8,liminit = 2500000)
      parameter (maxDistNear = 5, maxInPc = 100)
      parameter (maxPcNear = (2*maxDistNear + 1) * (2*maxDistNear + 1))
      parameter (maxUseEdge = 100, limXcorrPeaks = 30)
      real*4, allocatable :: array(:), brray(:)
      complex*8, allocatable :: xcray(:), xdray(:)
c       
      integer*4 nxyzin(3),nxyzout(3),nxin,nyin,nzin,nxout,nyout,nzout,idimc
      integer*4 nxyzbin(3),nxbin,nybin,nzbin, limnpc, limsect, limedge,maxsiz
      integer*4 noverlap(2),nedge(2),nxoverlap,nyoverlap,maxlinelength,maxbsiz
      integer*4, allocatable :: ixpclist(:),iypclist(:) !piece coords in x,y
      integer*4, allocatable :: izpclist(:),neglist(:) !section #, negative #
      integer*4, allocatable :: limDataLo(:,:,:),limDataHi(:,:,:)
      integer*4, allocatable :: iedgelower(:,:),iedgeupper(:,:), limDataInd(:)
      integer*4, allocatable :: ipiecelower(:,:),ipieceupper(:,:),ibufedge(:,:)
      integer*4, allocatable :: ifskipEdge(:,:)
      integer*4 indent(2)			!minimum indent short & long
      integer*4 intgrid(2)			!grid interval short & long
      integer*4 iboxsiz(2)			!box size short & long
      integer*4 nxgrid(2),nygrid(2)		!# of grid points for x&y edges
      real*4 edgelonear(2),edgehinear(2)	!limits for values near edges
      equivalence (nxin,nxyzin(1)),(nyin,nxyzin(2)),(nzin,nxyzin(3))
      equivalence (nxbin,nxyzbin(1)),(nybin,nxyzbin(2)),(nzbin,nxyzbin(3))
      equivalence (nxout,nxyzout(1)),(nyout,nxyzout(2)),(nzout,nxyzout(3))
      equivalence (noverlap(1),nxoverlap),(noverlap(2),nyoverlap)
      integer*4 npclist,minxpiece,minypiece,nxpieces,nypieces,interpOrder
      real*4 dmean
      integer*4 izUseDefLow, izUseDefHigh, numUseEdge, ixyUseEdge(maxUseEdge)
      integer*4 ixFrmUseEdge(maxUseEdge), iyFrmUseEdge(maxUseEdge)
      integer*4 izLowUse(maxUseEdge), izHighUse(maxUseEdge), lastWritten(2)
c       
      integer*4 numpieces                       !# of pieces point is in
      integer*4 inpiece(0:maxInPc)                !piece # of pieces point in
      real*4 xinpiece(maxInPc),yinpiece(maxInPc) !x,y coordinates within piece
c       frame number of each piece in X and Y, and min and max frames
      integer*4 inpxframe(maxInPc), inpyframe(maxInPc)
      integer*4 maxxframe,maxyframe, minxframe,minyframe
      integer*4 numedges(2)			!number of edges in x and y
      integer*4 inedge(maxInPc,2)               !edge # of edges point is in
c       list index of pieces on either side of edge
      integer*4 inedlower(maxInPc,2),inedupper(maxInPc,2)
      integer*4 numPcNear, idxPcNear(maxPcNear), idyPcNear(maxPcNear)
c
      integer*4, allocatable :: mappiece(:,:)	!map of pieces in this section
      logical, allocatable :: anyDisjoint(:,:)   !If any corner is disjoint
      integer*4, allocatable :: mapDisjoint(:,:) !Type of disjoint edge (X/Y)
      logical dogxforms,multng                  !if doing g's, if negs in sect
      logical limitData                         !if limiting data in X/Y
      real*4 hxcen,hycen			!coord of center of input frame
      real*4 gxcen,gycen			!coord of output image center
      real*4 ginv(2,3)                          !inverse of g and h xforms
      real*4, allocatable :: hinv(:,:,:), htmp(:,:,:)
c       
      parameter (memlim=256, memMaximum = 2000000000)
      parameter (memMinimum=32000000, memPreferred=340000000)
      integer*4 izmemlist(memlim),lastused(memlim)
      integer*4, allocatable :: memIndex(:)
      integer*4 maxload,jusecount,ilistz
      integer(kind = 8) npixin
c       
      parameter (limedgbf=20)
      integer*4 iedgbflist(limedgbf),ixybflist(limedgbf),lasedguse(limedgbf)
      integer*4 iunedge(2),ixgdim,iygdim
      integer*4 nxgrbf(limedgbf),nygrbf(limedgbf),ixgrdstbf(limedgbf)
      integer*4 iygrdstbf(limedgbf) ,ixofsbf(limedgbf),iyofsbf(limedgbf)
      integer*4 intgrcopy(2),intxgrbf(limedgbf),intygrbf(limedgbf)
      real*4, allocatable :: dxgrbf(:,:,:), dygrbf(:,:,:), ddengrbf(:,:,:)
      real*4, allocatable :: dxgrid(:,:), dygrid(:,:),ddengrid(:,:),sdgrid(:,:)
      integer*4 jusedgct,needbyteswap,izUnsmoothedPatch, izSmoothedPatch
c       
      parameter (lmField = 200, maxFields = 16)
      real*4 distDx(lmField,lmField),distDy(lmField,lmField)
      real*4 fieldDx(lmField,lmField,maxFields)
      real*4 fieldDy(lmField,lmField,maxFields)
      logical doFields,undistort,doMagGrad,focusAdjusted, doingEdgeFunc,debug
      real*4 pixelMagGrad, axisRot
      real*4, allocatable :: tiltAngles(:), dmagPerUm(:), rotPerUm(:)
      integer*4 ixFieldStrt, iyFieldStrt, nxField, nyField, numMagGrad
      integer*4 numAngles
      real*4 xFieldIntrv, yFieldIntrv

      integer*4 ifDumpXY(2),nzOutXY(2),nxOutXY(2),nyOutXY(2),ipcBelowEdge
      integer*4 ifillTreatment, numXcorrPeaks,nbinXcorr,ixdebug,iydebug
      real*4 padFrac, aspectMax, extraWidth
      real*4 radius1, radius2, sigma1, sigma2
c       
c       Variables for finding shifts and for finding gradients
      integer*4 limvar
      real*4, allocatable :: rowTmp(:), dxyvar(:,:), bb(:,:), fpsWork(:)
      integer*4, allocatable :: ivarpc(:), indvar(:)
      integer*4, allocatable :: iallVarPc(:), ivarGroup(:), listCheck(:)
      real*4, allocatable :: gradXcenLo(:), gradXcenHi(:), gradYcenLo(:)
      real*4, allocatable :: gradYcenHi(:), overXcenLo(:), overXcenHi(:)
      real*4, allocatable :: overYcenLo(:), overYcenHi(:)
      real*4, allocatable :: dxedge(:,:), dyedge(:,:), dxadj(:,:), dyadj(:,:)
c       
c       Variables added when internal sub didn't work
      integer*4 iblend(2)			!blending width in x and y
      integer*4 indedge4(3,2)
      real*4 edgefrac4(3,2)
      integer*4 indp1234(8)
      equivalence (indp1234(1),indp1),(indp1234(2),indp2),(indp1234(3),indp3),
     &    (indp1234(4),indp4)
      integer*4 indp1,indp2,indp3,indp4,inde12,inde13,inde34,inde24,nactivep
      integer*4 lastxyDisjoint,lastp1, lastp2, lastp3, lastp4
      real*4 wll,wlr,wul,wur,startSkew, endSkew,ex,ey
c       
      end module blendvars
