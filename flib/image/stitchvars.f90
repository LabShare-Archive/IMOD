! stitchvars.f: module file for stitchalign
!
! $Id$
!
!
module stitchvars
  implicit none
  integer MAXVOLS, MAXVECS, MAXPATCH, MAXBAND
  parameter (MAXVOLS = 100, MAXPATCH = 2000, MAXVECS = 2 * MAXVOLS * MAXPATCH)
  parameter (MAXBAND = 20)
  !
  ! Alignment variables
  real*4 dxyz(MAXVOLS, 3), gmag(MAXVOLS), comp(MAXVOLS), dmag(MAXVOLS)
  real*4 skew(MAXVOLS), alpha(MAXVOLS), beta(MAXVOLS), gamma(MAXVOLS)
  real*4 volGeomVars(MAXVOLS,7)
  equivalence (volGeomVars(1, 1), gmag(1)), (volGeomVars(1, 2), comp(1))
  equivalence (volGeomVars(1, 3), dmag(1)), (volGeomVars(1, 4), skew(1))
  equivalence (volGeomVars(1, 5), alpha(1)), (volGeomVars(1, 6), beta(1))
  equivalence (volGeomVars(1, 7), gamma(1))
  !
  ! Indices to where variables start in var, or 0 if no variable; flag for
  ! whether stitchfunc should unload variables
  integer*4 mapGmag, mapComp, mapDmag, mapSkew, mapAlpha, mapBeta, mapGamma
  integer*4 mapArray(7), ifUnload
  equivalence (mapArray(1), mapGmag), (mapArray(2), mapComp)
  equivalence (mapArray(3), mapDmag), (mapArray(4), mapSkew)
  equivalence (mapArray(5), mapAlpha), (mapArray(6), mapBeta)
  equivalence (mapArray(7), mapGamma)

  !
  ! The corresponding positions in lower and upper volumes
  real*4 posLower(3, MAXVECS), posUpper(3, MAXVECS)
  !
  ! Index to starting value and number of vectors in each overlap
  integer*4 istrVector(2*MAXVOLS+1), numVectors(2*MAXVOLS)
  !
  ! Original and rotated vectors
  real*4 center(3,MAXVECS), vector(3,MAXVECS), cenRot(3,MAXVECS)
  real*4 vecRot(3,MAXVECS)
  !
  ! Size of input volumes and of output volume
  integer*4 nxyzin(3, MAXVOLS), nxyzout(3), nxout, nyout, nzout
  equivalence (nxout, nxyzout(1)), (nyout, nxyzout(2)), (nzout, nxyzout(3))
  !
  ! Original piece numbers
  ! Index from volume to upper one in each direction,
  ! index to overlap vector set in each direction; number of volumes loaded
  integer*4 ixPiece(MAXVOLS), iyPiece(MAXVOLS)
  integer*4 iVolUpper(2, MAXVOLS), indVector(2, MAXVOLS)
  integer*4 iVolLower(2,MAXVOLS), numVols
  !
  ! intervals between volumes; indexes for getting between full set of
  ! volumes and one in minimization
  integer*4  intervals(2,3), intervalX, intervalY
  equivalence (intervals(1, 1), intervalX), (intervals(2, 2), intervalY)
  integer*4 iVolSolve(0:MAXVOLS), numSolve, iVolFull(0:MAXVOLS)
  !
  ! matrices in the fit or based on the final resolution of variables;
  ! scale factor and scaling intervals, residuals from the fit
  real*4 fitMat(3,3,MAXVOLS), spacings(2,3), scalexyz, resid(3,MAXVECS)
  real*4 aa(3,3,MAXVOLS)
  equivalence (aa, fitMat)
  !
  ! matrices used to call gaussj (used to be doubles)
  real*4 daa(MAXVOLS,MAXVOLS), dbb(7,MAXVOLS)
  !
  ! Inverse of final full matrices, and final dxyz of the volumes
  real*4 dtor, fInvMat(3,3,MAXVOLS), fInvDxyz(3,MAXVOLS), fullDxyz(3,MAXVOLS)
  !
  ! Variables describing the original vector grid: number in each dimension
  ! index to vectors, starting location in index array and free position
  ! starting and delta coordinates of grid in each dimension
  integer*4 numVecGrid(3,2*MAXVOLS), indGridToVec(MAXVECS)
  integer*4 istrVecGrid(2*MAXVOLS), indG2Vbase
  real*4 vecGridStart(3,2*MAXVOLS), vecGridDelta(3,2*MAXVOLS)
  !
  ! Parameters set by countedges and used in findwarpvector
  integer*4 numEdges(2), numPieces, inPiece(4), inEdge(2,2)
  integer*4 inEdLower(2,2), inEdUpper(2,2), nIniExtend(2,2)
  real*4 xInPiece(4), yInPiece(4), edgefrac4(2,2)
  !
  ! Overall min and max of edges, and min and max within bands along the
  ! long dimension, for finding edge fractions
  real*4 edgeMin(3,2*MAXVOLS), edgeMax(3,2*MAXVOLS), bandDel(2*MAXVOLS)
  real*4 extendedEdMin(3,2*MAXVOLS), extendedEdMax(3,2*MAXVOLS)
  real*4 bandMin(MAXBAND,2*MAXVOLS), bandMax(MAXBAND,2*MAXVOLS)
  integer*4 numBands(2*MAXVOLS)
  !
  ! Mean width of edges, mean spacing of vectors in each dimension
  ! Effective limits of each volume in each corner in XY, used for finding
  ! end fractions
  real*4 edgeWidthMean, delMean(3), volLimTR(2,MAXVOLS)
  real*4 volLimBL(2,MAXVOLS), volLimTL(2,MAXVOLS), volLimBR(2,MAXVOLS)
  !
end module stitchvars
