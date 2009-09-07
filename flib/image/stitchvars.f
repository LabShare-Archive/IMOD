c       stitchvars.f: module file for stitchalign
c       
c       $Id$
c       $Log$
c       Revision 3.1  2007/04/08 16:13:55  mast
c       Added to package
c
c       
      module stitchvars
      implicit none
      integer maxvols, maxvecs, maxpatch, maxband
      parameter (maxvols=100, maxpatch=2000, maxvecs=2*maxvols*maxpatch)
      parameter (maxband=20)
c
c       Alignment variables 
      real*4 dxyz(maxvols, 3), gmag(maxvols), comp(maxvols), dmag(maxvols)
      real*4 skew(maxvols), alpha(maxvols), beta(maxvols), gamma(maxvols)
      real*4 volGeomVars(maxvols,7)
      equivalence (volGeomVars(1,1), gmag(1)),(volGeomVars(1,2), comp(1))
      equivalence (volGeomVars(1,3), dmag(1)),(volGeomVars(1,4), skew(1))
      equivalence (volGeomVars(1,5), alpha(1)),(volGeomVars(1,6), beta(1))
      equivalence (volGeomVars(1,7), gamma(1))
c
c       Indices to where variables start in var, or 0 if no variable; flag for
c       whether stitchfunc should unload variables
      integer*4 mapGmag, mapComp, mapDmag, mapSkew, mapAlpha, mapBeta, mapGamma
      integer*4 mapArray(7), ifUnload
      equivalence (mapArray(1), mapGmag), (mapArray(2), mapComp)
      equivalence (mapArray(3), mapDmag), (mapArray(4), mapSkew)
      equivalence (mapArray(5), mapAlpha), (mapArray(6), mapBeta)
      equivalence (mapArray(7), mapGamma)

c       
c       The corresponding positions in lower and upper volumes
      real*4 poslo(3, maxvecs), posup(3, maxvecs)
c
c       Index to starting value and number of vectors in each overlap
      integer*4 istrVector(2*maxvols+1), numVectors(2*maxvols)
c       
c       Original and rotated vectors
      real*4 center(3,maxvecs), vector(3,maxvecs), cenRot(3,maxvecs)
      real*4 vecRot(3,maxvecs)
c       
c       Size of input volumes and of output volume
      integer*4 nxyzin(3, maxvols), nxyzout(3), nxout, nyout, nzout
      equivalence (nxout, nxyzout(1)), (nyout, nxyzout(2)), (nzout, nxyzout(3))
c       
c       Index from volume to upper one in each direction, 
c       index to overlap vector set in each direction; number of volumes loaded
      integer*4 iVolUpper(2, maxvols), indVector(2, maxvols)
      integer*4 iVolLower(2,maxvols), numVols
c       
c       intervals between volumes; indexes for getting between full set of
c       volumes and one in minimization
      integer*4  intervals(2,3), intervalX, intervalY
      equivalence (intervals(1,1), intervalX), (intervals(2,2), intervalY)
      integer*4 iVolSolve(0:maxvols), numSolve, iVolFull(0:maxvols)
c       
c       matrices in the fit or based on the final resolution of variables;
c       scale factor and scaling intervals, residuals from the fit
      real*4 fitMat(3,3,maxvols), spacings(2,3), scalexyz, resid(3,maxvecs)
      real*4 aa(3,3,maxvols)
      equivalence (aa, fitMat)
c       
c       matrices used to call gaussj
      real*8 daa(maxvols,maxvols), dbb(maxvols,7)
c       
c       Inverse of final full matrices
      real*4 dtor, finvMat(3,3,maxvols), finvDxyz(3,maxvols)
c       
c       Variables describing the original vector grid: number in each dimension
c       index to vectors, starting location in index array and free position
c       starting and delta coordinates of grid in each dimension
      integer*4 numVecGrid(3,2*maxvols), indGridToVec(maxvecs)
      integer*4 istrVecGrid(2*maxvols), indG2Vbase
      real*4 vecGridStart(3,2*maxvols), vecGridDelta(3,2*maxvols)
c       
c       Parameters set by countedges and used in findwarpvector
      integer*4 numEdges(2), numPieces, inPiece(4), inEdge(2,2)
      integer*4 inEdLower(2,2), inEdUpper(2,2), nIniExtend(2,2)
      real*4 xInPiece(4), yInPiece(4), edgefrac4(2,2)
c       
c       Overall min and max of edges, and min and max within bands along the
c       long dimension, for finding edge fractions
      real*4 edgeMin(3,2*maxvols), edgeMax(3,2*maxvols), bandDel(2*maxvols)
      real*4 bandMin(maxband,2*maxvols), bandMax(maxband,2*maxvols)
      integer*4 numBands(2*maxvols)
c       
c       Mean width of edges, mean spacing of vectors in each dimension
c       Effective limits of each volume in each corner in XY, used for finding
c       end fractions
      real*4 edgeWidthMean, delMean(3), volLimTR(2,maxvols)
      real*4 volLimBL(2,maxvols), volLimTL(2,maxvols), volLimBR(2,maxvols)
c
      end module stitchvars
