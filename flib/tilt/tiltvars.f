c       nprj       width of input
c       mprj       y-dimension of input
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
      integer limmask, limview, limwpos, limwidth, limwarp
      integer limrays, limreproj
      integer*4 limstack /200000000/
c       parameter (limstack=2200000)
      parameter (limmask=20000)
      parameter (limview=720,limwidth=20000)
      parameter (limwpos=1600,limwarp=192000)
      parameter (limrays = 2 * limwidth, limreproj = limview)
c       
      integer*4 NSTACK,maxstack
      real*4, allocatable :: ARRAY(:)
c       
      integer*4 npxyz(3),IWIDE,ITHICK,ISLICE,JSLICE,npad,ithickout
      integer*4 nprj, mprj, nviews
      real*4 ANGLES(limview),TITLE(20),pmin,pmax,pmean
      equivalence (nprj,npxyz(1)),(mprj,npxyz(2)),(nviews,npxyz(3))
c       
      LOGICAL MASK,PERP,fastbp,reproj,recReproj,debug,readBase
c       
      real*4 DELXX,xcenin,slicen,XCEN,YCEN,baselog,compress(limview),yoffset
      real*4 xzfac(limview), yzfac(limview), edgeFill, zeroWeight, flatFrac
      real*4 expWeight(limview),ycenModProj
      integer*4 IMAP,nbase,ITHWID ,idelslice,newmode,mapuse(limview),
     &    iflog,nreplic ,iplane,ipextra,interpfac,interpord ,ifbpwrk,
     &    ifbpzwrk,ifbpiw,ifbprw,ifbpzw,nprjfbp ,nvertneed,intordxtilt,
     &    minTotSlice, maxTotSlice, numViewBase, nViewSubtract,
     &    ivSubtract(limview)
c       
      integer*4 nstretch(limview),indstretch(limview)
      real*4 ofstretch(limview)
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
      integer*4 nxwarp,nywarp,ixswarp,iyswarp,idxwarp,idywarp,
     &    ifdelalpha,indwarp(limwpos)
      real*4 delalpha(limwarp),cwarpb(limwarp),swarpb(limwarp),
     &    cwarpa(limwarp),swarpa(limwarp),fw(2,3,limwarp),
     &    delbeta(limwarp),warpXZfac(limwarp),warpYZfac(limwarp)
c       
      integer*4 nreproj, nraymax(limreproj), nrayinc(limrays), maxZreproj
      integer*4 minXreproj, maxXreproj, minYreproj, maxYreproj, minZreproj
      integer*4 ithickReproj, minXload, maxXload
      real*4 xprjOffset, yprjOffset, projMean, filterScale
      real*4 xraystr(limrays), yraystr(limrays), cosReproj(limreproj)
      real*4 sinReproj(limreproj)
      end module tiltvars
