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
c
        parameter (limstack=60000000)
c        parameter (limstack=2200000)
	parameter (limmask=4096)
	parameter (limview=720,limwidth=20000)
	parameter (limwpos=400,limwarp=40000)
	COMMON /BUFFER/NSTACK,maxstack,ARRAY(limstack)
	COMMON /HEADER/npxyz(3),IWIDE,ITHICK,ISLICE,JSLICE
     &      ,ANGLES(limview),TITLE(20),npad,ithickout
	equivalence (nprj,npxyz(1)),(mprj,npxyz(2)),(nviews,npxyz(3))
	LOGICAL MASK,PERP,fastbp
	COMMON /MISCEL/PERP,DELXX,IMAP,nbase,ITHWID,xcenin,slicen,XCEN,YCEN
     &	    ,idelslice,newmode,mapuse(limview),iflog,baselog,nreplic
     &	    ,compress(limview),iplane,ipextra,interpfac,interpord,fastbp
     &      ,ifbpwrk,ifbpzwrk,ifbpiw,ifbprw,ifbpzw,nprjfbp,yoffset
     &      ,nvertneed,intordxtilt
	common /strtch/nstretch(limview),indstretch(limview),ofstretch(limview)
	COMMON /MASK/MASK,RMASK,TMASK,masklft(limmask),maskrt(limmask)
	COMMON /BPSCAL/FLEVL,SCALE
	COMMON /SINCOS/sbet(limview),cbet(limview),SAL(limview),CAL(limview)
	common /alfcor/ifalpha,alpha(limview)
        real*4 fw(2,3,limwarp)
	common /warps/ nxwarp,nywarp,ixswarp,iyswarp,idxwarp,idywarp,
     &	    ifdelalpha,indwarp(limwpos),delbeta(limwarp),
     &      delalpha(limwarp),cwarpb(limwarp),swarpb(limwarp),
     &      cwarpa(limwarp),swarpa(limwarp),fw
