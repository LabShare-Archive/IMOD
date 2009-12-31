c       nxprj      width of input (formerly nprj)
c       nyprj      y-dimension of input (formerly mprj)
c       nviews     number of input views, or number used
c       islice     starting slice to reconstruct
c       jslice     ending slice to reconstruct
c       idelslice  increment of slices to reconstruct
c       iwide      width of output slice
c       ithick     thickness of output slice made by PROJECT
c       ithwid     size = thickness x width of output slice (space allowed)
c       ithickout  thickness of final tilted output slice
c       imap       address of start of output slice
c       nbase      address of start of loaded input data
c       nstack     address of end of loaded input data
c       maxstack   full size of data array
c       iplane     size needed for input plane, possibly stretched
c       ipextra    size of extra buffer for loading for cosine stretching
c       nplanes    Number of input planes that can be loaded
c       needStarts Starting slice number needed for each slice
c       needEnds   Ending slice number needed for each slice
c       numNeedSE  Number of starting/ending slice numbers
c       needBase   Base index for start/end arrays
c       numGpuPlanes  Number of planes available to be stored on GPU
c       loadGpuStart  First slice loaded onto GPU
c       loadGpuEnd    Last slice loaded onto GPU
c       delxx      offset of tilt axis from center of input images
c       xcenin     center coordinate of input slice
c       slicen     center slice for x-axis tilting
c       xcen       center x coordinate of output slice
c       ycen       center y coordinate of output slice
c       yoffset    vertical shift of data in an output slice
c       interpfac  interpolation factor for cosine stretched data, or 0
c       interpord  interpolation order for cosine stretching
c       nvertneed  # of vertical slices needed to make tilted output slice
c       intordxtilt  order for interpolating X-tilted slice
c       reproj     Flag to reproject at zero tilt
c       
      module tiltvars
      integer limmask, limview, limwidth
      integer limrays, limreproj
      parameter (limmask=20000)
      parameter (limview=720,limwidth=20000)
      parameter (limrays = 2 * limwidth, limreproj = limview)
c       
      integer*4 NSTACK,maxstack, needBase, numNeedSE
      real*4, allocatable :: ARRAY(:), reprojLines(:)
      integer*4, allocatable :: needStarts(:), needEnds(:)
c       
      integer*4 npxyz(3),IWIDE,ITHICK,ISLICE,JSLICE,npad,ithickout
      integer*4 nxprj, nyprj, nviews
      real*4 ANGLES(limview),TITLE(20),pmin,pmax,pmean
      equivalence (nxprj,npxyz(1)),(nyprj,npxyz(2)),(nviews,npxyz(3))
c       
      LOGICAL MASK,PERP,reproj,recReproj,debug,readBase,useGPU
c       
      real*4 DELXX,xcenin,slicen,XCEN,YCEN,baselog,compress(limview),yoffset
      real*4 xzfac(limview), yzfac(limview), edgeFill, zeroWeight, flatFrac
      real*4 expWeight(limview),ycenModProj
      integer*4 IMAP,nbase,ITHWID ,idelslice,newmode,mapuse(limview),
     &    iflog,iplane,ipextra,nplanes,interpfac,interpord, nvertneed,
     &    intordxtilt, minTotSlice, maxTotSlice, numViewBase, nViewSubtract,
     &    ivSubtract(limview)
c       
      integer*4 nstretch(limview),indstretch(limview)
      real*4 ofstretch(limview)
c
      integer*4 nweight, numWgtAngles
      real*4 wincr(20), wgtAngles(limview)
c       
      integer*4 masklft(limmask),maskrt(limmask)
      real*4 RMASK,TMASK
c       
      real*4 FLEVL,SCALE,baseFlevl, baseScale
c       
      real*4 sbet(limview),cbet(limview),SAL(limview),CAL(limview)
c       
      integer*4 ifalpha
      real*4 alpha(limview)
c       
      integer*4 limwpos, limwarp
      integer*4 nxwarp,nywarp,ixswarp,iyswarp,idxwarp,idywarp,ifdelalpha
      integer*4, allocatable :: indwarp(:)
      real*4, allocatable :: delalpha(:),cwarpb(:),swarpb(:), cwarpa(:),
     &    swarpa(:),fw(:,:,:),delbeta(:),warpXZfac(:),warpYZfac(:),warpDelz(:)
c       
      integer*4 nreproj, nraymax(limreproj), nrayinc(limrays), maxZreproj
      integer*4 minXreproj, maxXreproj, minYreproj, maxYreproj, minZreproj
      integer*4 ithickReproj, minXload, maxXload
      real*4 xprjOffset, yprjOffset, projMean, filterScale
      real*4 xraystr(limrays), yraystr(limrays), cosReproj(limreproj)
      real*4 sinReproj(limreproj)
c       
      integer*4 numGpuPlanes, loadGpuStart, loadGpuEnd
c
      end module tiltvars
