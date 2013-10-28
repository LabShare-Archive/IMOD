! The module tiltvars, with variabkes used extensively in tilt
!
! $Id$
!
! alpha              X tilt angles
! angles             Angles of tilt around tilt axis
! array              Main stack array
! axisXoffset        offset of tilt axis from center of input images
! baseForLog         Value to add before taking log
! baseOutAdd         Scaling factors to use adding to a base rec
! baseOutScale
! centerSlice        center slice for x-axis tilting
! compress           Compression (thinning) factors
! cosAlpha           Cosines of alpha, tilt, and reprojection angles
! cosBeta            
! cosReproj          
! cWarpAlpha         Cosines of local area alpha and tilt angles
! cWarpBeta          
! debug              Debug output flag
! delAlpha           Local alignment delta alpha and tilt values
! delBeta
! dmaxIn             Min/max/mean of input file
! dmeanIn            
! dminIn             
! dxWarpDelz         Interval in X between positions
! edgeFill           Value to fill edges with
! effectiveScale     A scaling factor with non-log scaling to use testing the GPU output
! exposeWeight       Exposure weights
! filterScale        The approximate implicit scaling caused by the default radial filter
! flatFrac           Fraction of flat filter to use in SIRT
! fwarp              Local area transformation
! idelSlice          Increment of slices to reconstruct
! idelXwarp          X and Y intervals between local areas
! idelYwarp          
! ifAlpha            Flag for whether there are X axis tilts
! ifDelAlpha         If there are local changes in alpha
! ifLog              Flag to take the log
! ifOutSirtProj      Flags to output reprojections and rec slices in internal SIRT
! ifOutSirtRec       
! indLoadBase        index of start of loaded input data
! indLoadEnd         index of end of loaded input data
! indNeededBase      Base index for start/end arrays
! indOutSlice        index of start of output slice
! indStretchLine     Index of start of stretched input line
! indWarp            Index to current local area data
! indWorkPlane       Index of plane of reprojected lines
! inPlaneSize        Size needed for input plane, possibly stretched
! interpFacStretch   interpolation factor for cosine stretched data, or 0
! interpOrdStretch   interpolation order for cosine stretching
! interpOrdXtilt     order for interpolating X-tilted slice
! ipExtraSize        size of extra buffer for loading for cosine stretching
! ireadBase          Index of start of ring of tilted slices
! isignConstraint    Sign constraint for internal SIRT
! isliceEnd          ending slice to reconstruct
! isliceSizeBP       size = thickness x width of output slice (space allowed)
! isliceStart        starting slice to reconstruct
! iterForReport      Starting iteration number for mean/sd reports
! ithickBP           thickness of back-projection slice made by PROJECT
! ithickOut          thickness of final tilted output slice
! ithickReproj       
! iviewSubtract      Array of views to subtract
! iwidth             Width of output slice
! ixStartWarp        X, Y coordinates of first local area
! iyStartWarp        
! ixUnmaskedSE       Starting and ending pixels on output line that are NOT masked
! limReproj          Size for reprojection-based arrays
! limView            Size of view-based arrays
! limWarp            # of local areas time # of views
! loadGpuEnd         Last slice loaded onto GPU
! loadGpuStart       First slice loaded onto GPU
! mapUsedView        Map from view index to original view
! maskEdges          Flag to mask edges of output
! maxRayPixels       Maximum number of pixels in a reprojection ray
! maxStack           Full size of data array
! maxTotSlice        Full range of reconstruction being done when doing a chunk
! minTotSlice        
! maxXload           Range of X loaded when reprojecting a rec
! minXload          
! maxXreproj         Limits of input in X, Y, Z for reprojection
! maxYreproj         
! maxZreproj         
! minXreproj         
! minYreproj         
! minZreproj         
! neededEnds         Ending slice number needed for each slice
! neededStarts       Starting slice number needed for each slice
! newMode            New output mode
! nprojXyz           Array for size of input
! numExtraMaskPix    # of extra pixels for the mask at edges
! numGpuPlanes       Number of planes available to be stored on GPU
! numNeedSE          Number of starting/ending slice numbers
! numPad             # of pixels to pad for 1D FFTs
! numPixInRay        # of pixels in each projection ray
! numPlanes          Number of input planes that can be loaded
! numReadNeed        # of tilted slices to read into ring
! numReproj          # of reprojection angles
! numSIRTiter        # of SIRT iterations
! numTiltIncWgt      # of weights for finding local tilt increment
! numVertNeeded      # of vertical slices needed to make tilted output slice
! numViewBase        Number of views in base rec
! numViews           number of input views, or number used
! numViewSubtract    # of views to subtract
! numWarpDelz        Number of positions in warpDelz array when reproj local
! numWarpPos         Number of local positions
! numWgtAngles       # of angles in wgtAngles array used for weighting by local increment
! nxProj             width of input (formerly nprj)
! nyProj             y-dimension of input (formerly mprj)
! nxStretched        Size of each stretched input line
! nxWarp             # of local positions in X and Y
! nyWarp             
! origLines          Original projection lines for subtracting from reprojection
! outAdd             Value to add to reconstruction before scaling
! outScale           Scaling factor to multiple by before output
! perpendicular      Flag to output XZ slices to file
! projLine           Line for reprojecting from BP
! projMean           Mean of input when reprojecting rec
! projSubtraction    Flag for doing subtraction of original projections
! readBaseRec        Flag to read in a base reconstruction
! recReproj          Flag to reproejct from a reconstruction file
! recSubtraction     Flag for doing subtraction of read-in slices for SIRT
! reportVals         array for mean, sd, and # of slices for each iteration
! reprojBP           Flag to reproject at zero tilt
! reprojLines        Lines into which reprojection from rec is done
! saveVertSlices     Save vertical slices at last SIRT iteration
! signConstraint     Positive or negative sign constraint in SIRT
! sinAlpha           Sines of alpha, tilt, and reprojection angles
! sinBeta            
! sinReproj          
! sirtFromZero       Do zero iteration of SIRT too
! stretchOffset      X offset of stretched line
! sWarpAlpha         Sines of local alpha and beta
! sWarpBeta          
! tiltIncWgts        Weight factors for computing local tilt increment
! title              Title for header
! useGPU             Flag to use the GPU
! vertSirtInput      Input for SIRT is vertical slices
! warpDelz           Delta Z steps when reprojecting from local alignment rec
! warpXZfac          Local z factors
! warpYZfac          
! wgtAngles          Full set of angles for computing weighting by local tilt increment
! xcenIn             center coordinate of input slice
! xcenOut            center x coordinate of output slice
! ycenOut            center y coordinate of output slice
! xprojfs            Projection factors for reprojection from local aligned rec
! yprojfs            
! xprojzs            
! yprojzs            
! xprojOffset        Projection offsets for getting from coordinate in reprojection
! yprojOffset        to coordinate in original projections
! xRayStart          Starting X and Y of reprojection ray
! yRayStart          
! xzfac              Z factors
! yzfac              
! ycenModProj        Center in Y for model projection
! yOffset            vertical shift of data in an output slice
! zeroWeight         Sum of weights at zero frequency, used to scale projection fill value
!
module tiltvars
  !
  integer*4 indLoadEnd, maxStack, indNeededBase, numNeedSE
  real*4, allocatable :: array(:), reprojLines(:), projLine(:), origLines(:)
  integer*4, allocatable :: neededStarts(:), neededEnds(:)
  !
  integer*4 nprojXyz(3), iwidth, ithickBP, isliceStart, isliceEnd, numPad, ithickOut
  integer*4 nxProj, nyProj, numViews, limView, limReproj
  real*4 title(20), dminIn, dmaxIn, dmeanIn
  equivalence (nxProj, nprojXyz(1)), (nyProj, nprojXyz(2)), (numViews, nprojXyz(3))
  !
  LOGICAL*4 maskEdges, perpendicular, reprojBP, recReproj, debug, readBaseRec, useGPU
  logical*4 recSubtraction, projSubtraction, vertSirtInput, saveVertSlices, sirtFromZero
  logical*4 rotateBy90
  !
  real*4 axisXoffset, xcenIn, centerSlice, xcenOut, ycenOut, baseForLog, yOffset
  real*4, allocatable ::  xzfac(:), yzfac(:), compress(:), exposeWeight(:), angles(:)
  real*4 edgeFill, zeroWeight, flatFrac, ycenModProj
  integer*4 indOutSlice, indLoadBase, isliceSizeBP , idelSlice, newMode, ifLog
  integer*4 inPlaneSize, ipExtraSize, numPlanes, interpFacStretch, interpOrdStretch
  integer*4 interpOrdXtilt, minTotSlice, maxTotSlice, numViewBase, numViewSubtract
  integer*4 numExtraMaskPix, numVertNeeded
  !
  integer*4, allocatable ::  nxStretched(:), indStretchLine(:), mapUsedView(:)
  real*4, allocatable ::  stretchOffset(:), wgtAngles(:)
  !
  integer*4 numTiltIncWgt, numWgtAngles
  real*4 tiltIncWgts(20)
  !
  integer*4, allocatable :: ixUnmaskedSE(:,:), iviewSubtract(:), maxRayPixels(:)
  !
  real*4 outAdd, outScale, baseOutAdd, baseOutScale, effectiveScale
  !
  real*4, allocatable :: sinBeta(:), cosBeta(:), sinAlpha(:), cosAlpha(:), alpha(:)
  integer*4 ifAlpha
  !
  integer*4 numWarpPos, limWarp
  integer*4 nxWarp, nyWarp, ixStartWarp, iyStartWarp, idelXwarp, idelYwarp, ifDelAlpha
  integer*4, allocatable :: indWarp(:), numPixInRay(:)
  real*4, allocatable :: delAlpha(:), cWarpBeta(:), sWarpBeta(:), cWarpAlpha(:)
  real*4, allocatable :: sWarpAlpha(:), fwarp(:,:,:), delBeta(:), warpXZfac(:)
  real*4, allocatable :: warpYZfac(:), warpDelz(:), xRayStart(:), yRayStart(:)
  real*4, allocatable :: xprojfs(:), xprojzs(:), yprojfs(:), yprojzs(:)
  !
  integer*4 numReproj, maxZreproj
  integer*4 minXreproj, maxXreproj, minYreproj, maxYreproj, minZreproj
  integer*4 ithickReproj, minXload, maxXload, numWarpDelz
  integer*4 numSIRTiter, numReadNeed, ireadBase, indWorkPlane
  integer*4 ifOutSirtProj, ifOutSirtRec, isignConstraint, iterForReport
  real*4 xprojOffset, yprojOffset, projMean, filterScale, dxWarpDelz
  real*4, allocatable :: reportVals(:,:), cosReproj(:), sinReproj(:)
  !
  integer*4 numGpuPlanes, loadGpuStart, loadGpuEnd
  !
end module tiltvars
