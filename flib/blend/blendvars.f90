! $Id$
!
module blendvars
  implicit none
  integer ifastSiz, maxBin, limInit, memMinimum, memPreferred
  integer limEdgBf, memMaximum
  integer maxPcNear, maxDistNear, maxUseEdge, maxInPc
  integer limXcorrPeaks
  !
  ! Needed size for idimc now determined by maximum unpadded size after
  ! binning, 1K x 1K
  !
  parameter (ifastSiz = 32, maxBin = 8, limInit = 2500000)
  parameter (maxDistNear = 5, maxInPc = 100)
  parameter (maxPcNear = (2 * maxDistNear + 1) * (2 * maxDistNear + 1))
  parameter (maxUseEdge = 100, limXcorrPeaks = 30)
  real*4, allocatable :: array(:), brray(:)
  complex * 8, allocatable :: xcray(:), xdray(:), xeray(:)
  !
  integer*4 nxyzIn(3), nxyzOut(3), nxin, nyin, nzin, nxOut, nyOut, nzOut, idimc
  integer*4 nxyzBin(3), nxBin, nyBin, nzBin, limNpc, limSect, limEdge, maxSiz
  integer*4 nOverlap(2), nedge(2), nXoverlap, nyOverlap, maxLineLength, maxBsiz
  integer*4, allocatable :: ixPcList(:), iyPcList(:) !piece coords in x, y
  integer*4, allocatable :: izPcList(:), negList(:) !section #, negative #
  integer*4, allocatable :: limDataLo(:,:,:), limDataHi(:,:,:)
  integer*4, allocatable :: iedgeLower(:,:), iedgeUpper(:,:), limDataInd(:)
  integer*4, allocatable :: ipieceLower(:,:), ipieceUpper(:,:), ibufEdge(:,:)
  integer*4, allocatable :: ifSkipEdge(:,:)
  integer*4 indent(2)                        !minimum indent short & long
  integer*4 intGrid(2)                       !grid interval short & long
  integer*4 iboxSiz(2)                       !box size short & long
  integer*4 nxGrid(2), nyGrid(2)             !# of grid points for x&y edges
  real*4 edgeLoNear(2), edgeHiNear(2)        !limits for values near edges
  equivalence (nxin, nxyzIn(1)), (nyin, nxyzIn(2)), (nzin, nxyzIn(3))
  equivalence (nxBin, nxyzBin(1)), (nyBin, nxyzBin(2)), (nzBin, nxyzBin(3))
  equivalence (nxOut, nxyzOut(1)), (nyOut, nxyzOut(2)), (nzOut, nxyzOut(3))
  equivalence (nOverlap(1), nXoverlap), (nOverlap(2), nyOverlap)
  integer*4 npcList, minXpiece, minYpiece, nxPieces, nyPieces, interpOrder
  real*4 dmean, dfill
  integer*4 izUseDefLow, izUseDefHigh, numUseEdge, ixyUseEdge(maxUseEdge)
  integer*4 ixFrmUseEdge(maxUseEdge), iyFrmUseEdge(maxUseEdge)
  integer*4 izLowUse(maxUseEdge), izHighUse(maxUseEdge), lastWritten(2)
  !
  integer*4 numPieces                         !# of pieces point is in
  integer*4 inPiece(0:maxInPc)                !piece # of pieces point in
  real*4 xInPiece(maxInPc), yInPiece(maxInPc) !x, y coordinates within piece
  ! frame number of each piece in X and Y, and min and max frames
  integer*4 inpXframe(maxInPc), inpYframe(maxInPc)
  integer*4 maxXframe, maxYframe, minXframe, minYframe
  integer*4 numEdges(2)                       !number of edges in x and y
  integer*4 inEdge(maxInPc,2)                 !edge # of edges point is in
  ! list index of pieces on either side of edge
  integer*4 inEdLower(maxInPc,2), inEdUpper(maxInPc,2)
  integer*4 numPcNear, idxPcNear(maxPcNear), idyPcNear(maxPcNear)
  !
  integer*4, allocatable :: mapPiece(:,:)     !map of pieces in this section
  logical, allocatable :: anyDisjoint(:,:)    !If any corner is disjoint
  integer*4, allocatable :: mapDisjoint(:,:)  !Type of disjoint edge (X/Y)
  logical doGxforms, multng                   !if doing g's, if negs in sect
  logical limitData                           !if limiting data in X / Y
  real*4 hxCen, hyCen                         !coord of center of input frame
  real*4 gxCen, gyCen                         !coord of output image center
  real*4 ginv(2,3)                            !inverse of g and h xforms
  real*4, allocatable :: hinv(:,:,:), htmp(:,:,:)
  !
  parameter (memMaximum = 2000000000)
  parameter (memMinimum = 32000000, memPreferred = 340000000)
  integer*4, allocatable :: memIndex(:), izMemList(:), lastUsed(:)
  integer*4 maxLoad, juseCount, ilistz, memLim
  integer(kind = 8) npixIn
  !
  parameter (limEdgBf = 20)
  integer*4 iedgBfList(limEdgBf), ixyBfList(limEdgBf), lasEdgUse(limEdgBf)
  integer*4 iunEdge(2), ixgDim, iygDim
  integer*4 nxGrBf(limEdgBf), nyGrBf(limEdgBf), ixGrdStBf(limEdgBf)
  integer*4 iyGrdStBf(limEdgBf) , ixOfsBf(limEdgBf), iyOfsBf(limEdgBf)
  integer*4 intGrCopy(2), intXgrBf(limEdgBf), intYgrBf(limEdgBf)
  real*4, allocatable :: dxGrBf(:,:,:), dyGrBf(:,:,:), ddenGrBf(:,:,:)
  real*4, allocatable :: dxGrid(:,:), dyGrid(:,:), ddenGrid(:,:), sdGrid(:,:)
  integer*4 jusEdgCt, needByteSwap, izUnsmoothedPatch, izSmoothedPatch
  !
  real*4, allocatable :: distDx(:,:), distDy(:,:), fieldDx(:,:,:), fieldDy(:,:,:)
  real*4, allocatable :: warpDx(:,:), warpDy(:,:)
  logical doFields, undistort, doMagGrad, focusAdjusted, doingEdgeFunc, debug, secHasWarp
  real*4 pixelMagGrad, axisRot, xFieldStrt, yFieldStrt, xFieldIntrv, yFieldIntrv
  real*4, allocatable :: tiltAngles(:), dmagPerUm(:), rotPerUm(:)
  integer*4 nxField, nyField, numMagGrad, lmField, maxFields, nxWarp, nyWarp
  integer*4 numAngles, lmWarpX, lmWarpY
  real*4 xWarpStrt, yWarpStrt, xWarpIntrv, yWarpIntrv

  integer*4 ifDumpXY(2), nzOutXY(2), nxOutXY(2), nyOutXY(2), ipcBelowEdge
  integer*4 ifillTreatment, numXcorrPeaks, nbinXcorr, ixDebug, iyDebug
  real*4 padFrac, aspectMax, extraWidth
  real*4 radius1, radius2, sigma1, sigma2, robustCrit
  !
  ! Variables for finding shifts and for finding gradients
  integer*4 limVar
  real*4, allocatable :: rowTmp(:), dxyVar(:,:), bb(:,:), fpsWork(:)
  integer*4, allocatable :: ivarPc(:), indVar(:)
  integer*4, allocatable :: iallVarPc(:), ivarGroup(:), listCheck(:)
  real*4, allocatable :: gradXcenLo(:), gradXcenHi(:), gradYcenLo(:)
  real*4, allocatable :: gradYcenHi(:), overXcenLo(:), overXcenHi(:)
  real*4, allocatable :: overYcenLo(:), overYcenHi(:)
  real*4, allocatable :: dxEdge(:,:), dyEdge(:,:), dxAdj(:,:), dyAdj(:,:)
  !
  ! Variables added when internal sub didn't work
  integer*4 iblend(2)                       !blending width in x and y
  integer*4 indEdge4(3,2)
  real*4 edgeFrac4(3,2)
  integer*4 indp1234(8)
  equivalence (indp1234(1), indp1), (indp1234(2), indp2), (indp1234(3), indp3), &
      (indp1234(4), indp4)
  integer*4 indp1, indp2, indp3, indp4, inde12, inde13, inde34, inde24, nActiveP
  integer*4 lastxyDisjoint, lastp1, lastp2, lastp3, lastp4
  real*4 wll, wlr, wul, wur, startSkew, endSkew, ex, ey
  !
end module blendvars
