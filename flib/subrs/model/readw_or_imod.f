c	  $Author$
c	  
c	  $Date$
c	  
c	  $Revision$
c
c	  $Log$
	logical function readw_or_imod(filename)
	include 'model.inc'
	include 'endian.inc'
	logical read_mod
	integer*4 getimod,int4(4)
	character*(*) filename
	real*4 flt(3)
	integer*2 int2(26)
	byte ptbyte
c
	readw_or_imod=.true.
	ierr=getimod(ibase_obj,npt_in_obj,p_coord,obj_color,n_point,
     &      n_object,filename)
	if(ierr.eq.0)then
	  do i=1,n_point
	    object(i)=i
	  enddo
	  do i=1,n_object
	    ndx_order(i)=i
	    obj_order(i)=i
	  enddo
	  max_mod_obj=n_object
	  ntot_in_obj=n_point
	  n_clabel=0
	  ibase_free=ibase_obj(n_object)+npt_in_obj(n_object)
	  nin_order=n_object
	else
	  readw_or_imod=.false.
	  open(20,file=filename,status='old',err=20)
	  call qopen(istrm,filename,'OLD')
	  call qseek(istrm,1,1,1)
c
	  call qread(istrm,int2,52,ier)
	  if(lowbyte.eq.2)call convert_shorts(int2,1)
	  if(ier.ne.0.or.int2(1).ne.3)go to 10
c	    
	  call qread(istrm,int2,2,ier)
	  if(lowbyte.eq.2)call convert_shorts(int2,1)
	  if(ier.ne.0.or.int2(1).ne.3)go to 10
c
	  call qread(istrm,int4,12,ierr)
	  if(ierr.ne.0)go to 10
	  if(lowbyte.eq.2)call convert_longs(int4,3)
c
	  n_point=int4(2)
	  n_object=0				!recount # of objects
	  ibase_free=0
	  ntot_in_obj=0
	  max_mod_obj=0
	  do i=1,max_obj_num
	    npt_in_obj(i)=0
	  enddo
100	  call qread(istrm,int2,2,ier)
	  if(lowbyte.eq.2)call convert_shorts(int2,1)
	  if(ier.ne.0.or.int2(1).ne.3)go to 10
c
	  call qread(istrm,int4,16,ier)
	  if(ier.ne.0)go to 10
	  if(lowbyte.eq.2)call convert_longs(int4,4)
c
	  if(int4(1).ne.0)then
	    i=int4(1)
	    ninobj=int4(2)
	    n_object=n_object+1
	    obj_color(1,i)=int4(3)
	    obj_color(2,i)=int4(4)
	    obj_order(n_object)=i
	    ndx_order(i)=n_object
	    npt_in_obj(i)=ninobj
	    ntot_in_obj=ntot_in_obj+ninobj
	    ibase_obj(i)=ibase_free
	    max_mod_obj=max(max_mod_obj,i)
	    do ii=1,ninobj
	      call qread(istrm,int2,2,ier)
	      if(lowbyte.eq.2)call convert_shorts(int2,1)
	      if(ier.ne.0.or.int2(1).ne.3)go to 10
c
	      call qread(istrm,int4,4,ier)
	      if(ier.ne.0)go to 10
	      if(lowbyte.eq.2)call convert_longs(int4,1)
	      ipt=int4(1)
c		
	      call qread(istrm,flt,12,ier)
	      if(ier.ne.0)go to 10
	      call fromvmsfloats(flt,3)
	      if(lowbyte.eq.1.)call convert_longs(flt,3)
c		
	      call qread(istrm,ptbyte,1,ier)
	      if(ier.ne.0)go to 10
c
	      if(ipt.gt.0) then
		if(ipt.gt.n_point) ipt=mod(ipt,10000) !in case of old models
		p_coord(1,ipt)=flt(1)
		p_coord(2,ipt)=flt(2)
		p_coord(3,ipt)=flt(3)
		pt_label(ipt)=ptbyte
	      endif
	      object(ii+ibase_free)=ipt
	    enddo
	    ibase_free=ibase_free+ninobj
	    go to 100
	  endif
	  n_clabel=0
	  nin_order=n_object
	  readw_or_imod=.true. 
	  go to 15
c
10	  readw_or_imod=read_mod()
15	  call qclose(istrm)
20	  close(20)
	endif
	return
	end
