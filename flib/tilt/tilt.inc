c	  nprj       width of input
c	  mprj       y-dimension of input
c	  nviews     number of input views, or number used
c	  islice     starting slice to reconstruct
c	  jslice     ending slice to reconstruct
c	  idelslice  increment of slices to reconstruct
c	  iwide      width of output slice
c	  ithick     thickness of output slice made by PROJECT
c	  ithwid     size = thickness x width of output slice (space allowed)
c	  ithickout  thickness of final tilted output slice
c	  imap       address of start of output slice
c	  nbase      address of start of loaded input data
c	  nstack     address of end of loaded input data
c	  maxstack   full size of data array
c	  iplane     size needed for input plane, possibly stretched
c	  ipextra    size of extra buffer for loading for cosine stretching
c	  delxx      offset of tilt axis from center of input images
c	  xcenin     center coordinate of input slice
c	  slicen     center slice for x-axis tilting
c	  xcen       center x coordinate of output slice
c	  ycen       center y coordinate of output slice
c	  yoffset    vertical shift of data in an output slice
c         interpfac  interpolation factor for cosine stretched data, or 0
c	  interpord  interpolation order for cosine stretching
c	  nvertneed  # of vertical slices needed to make tilted output slice
c	  intordxtilt  order for interpolating X-tilted slice
c	  reproj     Flag to reproject at zero tilt
c	  
	integer limstack, limmask, limview, limwpos, limwidth, limwarp
        parameter (limstack=60000000)
c        parameter (limstack=2200000)
	parameter (limmask=4096)
	parameter (limview=720,limwidth=20000)
	parameter (limwpos=400,limwarp=40000)
c
	integer*4 NSTACK,maxstack
	real*4 ARRAY(limstack)
	COMMON /BUFFER/NSTACK,maxstack,ARRAY
c
	integer*4 npxyz(3),IWIDE,ITHICK,ISLICE,JSLICE,npad,ithickout
	integer*4 nprj, mprj, nviews
	real*4 ANGLES(limview),TITLE(20),pmin,pmax
	COMMON /HEADER/npxyz,pmin,pmax,IWIDE,ITHICK,ISLICE,JSLICE
     &      ,ANGLES,TITLE,npad,ithickout
	equivalence (nprj,npxyz(1)),(mprj,npxyz(2)),(nviews,npxyz(3))
c
	LOGICAL MASK,PERP,fastbp,reproj
c
	real*4 DELXX,xcenin,slicen,XCEN,YCEN,baselog,compress(limview),yoffset
	real*4 xzfac(limview), yzfac(limview)
	integer*4 IMAP,nbase,ITHWID ,idelslice,newmode,mapuse(limview),
     &	    iflog,nreplic ,iplane,ipextra,interpfac,interpord ,ifbpwrk,
     &	    ifbpzwrk,ifbpiw,ifbprw,ifbpzw,nprjfbp ,nvertneed,intordxtilt
	COMMON /MISCEL/PERP,DELXX,IMAP,nbase,ITHWID,xcenin,slicen,XCEN,YCEN
     &	    ,idelslice,newmode,mapuse,iflog,baselog,nreplic
     &	    ,compress,iplane,ipextra,interpfac,interpord,fastbp
     &      ,ifbpwrk,ifbpzwrk,ifbpiw,ifbprw,ifbpzw,nprjfbp,yoffset
     &      ,nvertneed,intordxtilt,reproj,xzfac,yzfac
c
	integer*4 nstretch(limview),indstretch(limview)
	real*4 ofstretch(limview)
	common /strtch/nstretch,indstretch,ofstretch
c
	integer*4 masklft(limmask),maskrt(limmask)
	real*4 RMASK,TMASK
	COMMON /MASK/MASK,RMASK,TMASK,masklft,maskrt
c
	real*4 FLEVL,SCALE
	COMMON /BPSCAL/FLEVL,SCALE
c
	real*4 sbet(limview),cbet(limview),SAL(limview),CAL(limview)
	COMMON /SINCOS/sbet,cbet,SAL,CAL
c	  
	integer*4 ifalpha
	real*4 alpha(limview)
	common /alfcor/ifalpha,alpha
c
	integer*4 nxwarp,nywarp,ixswarp,iyswarp,idxwarp,idywarp,
     &	    ifdelalpha,indwarp(limwpos)
	real*4 delalpha(limwarp),cwarpb(limwarp),swarpb(limwarp),
     &      cwarpa(limwarp),swarpa(limwarp),fw(2,3,limwarp),
     &	    delbeta(limwarp),warpXZfac(limwarp),warpYZfac(limwarp)
	common /warps/ nxwarp,nywarp,ixswarp,iyswarp,idxwarp,idywarp,
     &	    ifdelalpha,indwarp,delbeta, delalpha,cwarpb,swarpb,
     &      cwarpa,swarpa,fw,warpXZfac, warpYZfac
