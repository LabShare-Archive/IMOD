




c	  REPACK_MOD is a subroutine to repack a WIMP model.  It packs all
c	  true point coordinates downward into contiguous space in the P_COORD
c	  array, and changes all of the references to those points
c	  appropriately in the OBJECT array.  Point marks, text labels, and
c	  branch points should all be handled properly.
c
	subroutine repack_mod
	include 'model.inc'
	real*4 temp(max_pt)
	integer*4 ixref(max_pt)
	byte pt_temp(max_pt)
c	  
c	  go through model objects, look at each actual point encountered in
c	  turn; if it is not encountered before, move it to next spot in temp
c	  array and put a pointer to that spot in the cross-reference list;
c	  then change the reference to that spot in the OBJECT array
c
	do i=1,max_pt
	  ixref(i)=0
	enddo
	ipout=0
	do iobj=1,max_mod_obj
	  ibase=ibase_obj(iobj)
	  do ipnt=1,npt_in_obj(iobj)
	    ispot=abs(object(ipnt+ibase))
	    if(ixref(ispot).eq.0)then
	      ipout=ipout+1
	      temp(ipout)=p_coord(1,ispot)
c	      temp(2,ipout)=p_coord(2,ispot)
c	      temp(3,ipout)=p_coord(3,ispot)
	      pt_temp(ipout)=pt_label(ispot)
	      ixref(ispot)=ipout
	    endif
	    object(ipnt+ibase)=sign(ixref(ispot),object(ipnt+ibase))
	  enddo
	enddo
c	  
c	  move text label references
c
	ilabout=0
	do ilab=1,n_clabel
	  if(ixref(label_list(ilab)).ne.0)then
	    ilabout=ilabout+1
	    label_list(ilabout)=ixref(label_list(ilab))
	  endif
	enddo
	n_clabel=ilabout
c	  
c	  move the coordinates and point marks back into proper arrays
c
	do i=1,ipout
	  pt_label(i)=pt_temp(i)
	  p_coord(1,i)=temp(i)
c	  p_coord(2,i)=temp(2,i)
c	  p_coord(3,i)=temp(3,i)
	enddo

	do iyz=2,3
	  do i=1,max_pt
	    itmp=ixref(i)
	    if(itmp.gt.0)temp(itmp)=p_coord(iyz,i)
	  enddo
	  do i=1,ipout
	    p_coord(iyz,i)=temp(i)
	  enddo
	enddo

	n_point=ipout
	return
	end
