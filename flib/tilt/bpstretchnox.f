	subroutine bpstretchnox(array,index,iproj,npts,xfrac,interpfac)
	real*4 array(*)
	omxfrac=1.-xfrac
	ind=index
	nloop=npts/2
	if(npts.gt.nloop*2)then
	  ARRAY(IND)=ARRAY(IND)+
     &	      omxfrac*ARRAY(IPROJ) +XFRAC*ARRAY(IPROJ+1) 
	  iproj=iproj+interpfac
	  ind=ind+1
	endif
	DO i=1,nloop
	  ARRAY(IND)=ARRAY(IND)+
     &	      omxfrac*ARRAY(IPROJ) +XFRAC*ARRAY(IPROJ+1) 
	  iproj=iproj+interpfac
	  ind=ind+1
	  ARRAY(IND)=ARRAY(IND)+
     &	      omxfrac*ARRAY(IPROJ) +XFRAC*ARRAY(IPROJ+1) 
	  iproj=iproj+interpfac
	  ind=ind+1
	enddo
	return
	end
