	integer maxsiz,limvar,ifastsiz,maxlinelength,limxypc,limnpc
	integer limedge,limsect,memlim,limedgbf,ixgdim,iygdim
	parameter (maxsiz=1024*1024*16)		!# of pixels
c	  
c	  set limvar to LESS THAN the square root of maxsiz
c	  
	parameter (limvar=3000)
c
	real*4 array(maxsiz)
	parameter (ifastsiz=32,maxlinelength=40960)
	real brray(ifastsiz*maxlinelength)
	common /bigarr/ array,brray
c
	parameter (limxypc=80,limnpc=50000,limedge=50000,limsect=1000)
	integer*4 nxyzin(3),nxyzout(3),nxin,nyin,nzin,nxout,nyout,nzout
	integer*4 noverlap(2),nedge(2),nxoverlap,nyoverlap
	integer*4 ixpclist(limnpc),iypclist(limnpc) !piece coords in x,y
	integer*4 izpclist(limnpc),neglist(limnpc) !section #, negative #
	integer*4 iedgelower(limnpc,2),iedgeupper(limnpc,2)
	integer*4 ipiecelower(limedge,2),ipieceupper(limedge,2)
	integer*4 indent(2)			!minimum indent short & long
	integer*4 intgrid(2)			!grid interval short & long
	integer*4 iboxsiz(2)			!box size short & long
	real*4 edgelonear(2),edgehinear(2)	!limits for values near edges
	equivalence (nxin,nxyzin(1))
	equivalence (nxout,nxyzout(1))
	equivalence (noverlap(1),nxoverlap)
	integer*4 npclist,minxpiece,minypiece,nxpieces,nypieces
	common /pclist/nxin,nyin,nzin,nxout,nyout,nzout,nxpieces,
     &	    nypieces ,nxoverlap,nyoverlap,npclist,minxpiece,minypiece,
     &	    nedge ,ixpclist,iypclist,izpclist,neglist,iedgelower,
     &	    iedgeupper ,ipiecelower,ipieceupper,indent,intgrid,iboxsiz,
     &	    edgelonear,edgehinear
c
	integer*4 inpiece(5)			!piece # of pieces point in
	integer*4 inedge(5,2)			!edge # of edges point is in
	integer*4 numedges(2)			!number of edges in x and y
	real*4 xinpiece(5),yinpiece(5)		!x,y coordinates within piece
	integer*4 inedlower(5,2),inedupper(5,2)	!list index of piece be&ab edge
	integer*4 mappiece(limxypc,limxypc)	!map of pieces in this section
	logical dogxforms,multng		!if doing g's, if negs in sect
	real*4 hxcen,hycen			!coord of center of input frame
	real*4 gxcen,gycen			!coord of output image center
	real*4 ginv(2,3),hinv(2,3,limnpc)	!inverse of g and h xforms
	integer*4 numpieces
	common /edgcnt/ inpiece,inedge,numedges,xinpiece,yinpiece,
     &	    inedlower, inedupper,mappiece,dogxforms,multng,numpieces,
     &	    hxcen,hycen, gxcen,gycen, ginv,hinv
c	  
	parameter (memlim=128)
	integer*4 izmemlist(memlim),lastused(memlim)
	integer*4 npixin,limsec,jusecount
	common /shuffl/ npixin,limsec,jusecount,izmemlist,lastused
c
	parameter (limedgbf=20,ixgdim=256,iygdim=256)
	integer*4 ibufedge(limedge,2),iedgbflist(limedgbf)
     &	    ,ixybflist(limedgbf),lasedguse(limedgbf)
	integer*4 iunedge(2)
	integer*4 nxgrbf(limedgbf),nygrbf(limedgbf)
     &	    ,ixgrdstbf(limedgbf),iygrdstbf(limedgbf)
     &	    ,ixofsbf(limedgbf),iyofsbf(limedgbf)
     &	    ,intxgrbf(limedgbf),intygrbf(limedgbf)
	integer*4 intgrcopy(2)
	real*4 dxgrbf(ixgdim,iygdim,limedgbf)
     &	    ,dygrbf(ixgdim,iygdim,limedgbf)
     &	    ,ddengrbf(ixgdim,iygdim,limedgbf)
	integer*4 jusedgct,needbyteswap
	common /edgebf/ iunedge,jusedgct,needbyteswap,ibufedge,
     &	    iedgbflist, ixybflist,
     &	    lasedguse, nxgrbf ,nygrbf,ixgrdstbf,iygrdstbf, ixofsbf,
     &	    iyofsbf,intxgrbf,intygrbf,dxgrbf,dygrbf ,ddengrbf,intgrcopy
