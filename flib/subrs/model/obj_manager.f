c       The OBJECT array is a list of pointers from each
c       element in each model object to the array of points P_COORD.
c       Each object occupies a contiguous area in OBJECT, but the different
c       objects are loosely packed in the array.  The current model object
c       is always the last one in the array so that it can be extended
c       freely; OBJECT_MOVER is called to move an object to end of the array.
c       When the array gets too sparse or the end of the array is approached,
c       OBJECT_PACKER is called to pack all of the objects down into
c       contiguous space again.
c       
      subroutine object_packer
      include 'model.inc'
c       
c       recompute total entries in OBJECT, total # of objects, max object #
c       
      ntotsave=ntot_in_obj
      nobjsave=n_object
      maxsave=max_mod_obj
      ntot_in_obj=0
      n_object=0
      do iobj=1,max_obj_num
        if(npt_in_obj(iobj).gt.0)then
          ntot_in_obj=ntot_in_obj+npt_in_obj(iobj)
          n_object=n_object+1
          max_mod_obj=iobj
        endif
      enddo
      if(ntotsave.ne.ntot_in_obj.or.nobjsave.ne.n_object.or.
     &    maxsave.lt.max_mod_obj)print *,'ntot,nobj,max',ntotsave,
     &    nobjsave,maxsave,ntot_in_obj,n_object,max_mod_obj
c       
c       if total entries in OBJECT is less than the current free pointer,
c       pack the array down
c       
      if(ntot_in_obj.lt.ibase_free)then
        ibase_free=0
        do iord=1,nin_order
          iobj=obj_order(iord)
          if(iobj.ne.0)then
            ninobj=npt_in_obj(iobj)
            ibasobj=ibase_obj(iobj)
            if(ninobj.ne.0)then
              if(ibasobj.ne.ibase_free)then     !move pointers down if needed
                ibasdown=ibase_free
                do ibase=1+ibasobj,ninobj+ibasobj
                  ibasdown=ibasdown+1
                  object(ibasdown)=object(ibase)
                enddo
              endif
              ibase_obj(iobj)=ibase_free        !in any case, reset these
              ibase_free=ibase_free+ninobj
            else                                !if object empty, clear spot
              obj_order(iord)=0
            endif
          endif
        enddo
      endif
c       
c       repack the object order array if # of objects is < # in order array
c       
      if(n_object.lt.nin_order)then
        indorder=0
        do iord=1,nin_order
          iobj=obj_order(iord)
          if(iobj.ne.0)then
            if(npt_in_obj(iobj).ne.0)then
              indorder=indorder+1
              obj_order(indorder)=iobj
              ndx_order(iobj)=indorder
            else
              ndx_order(iobj)=0
            endif
          endif
        enddo
        nin_order=indorder
      endif
c       
      return
      end


      subroutine object_mover(iobj,failed)
      logical failed
      include 'model.inc'
c       
c       just return if object already at top of list
c       
      failed=.false.
      if(nin_order.gt.0)then
        if(obj_order(nin_order).eq.iobj)return
      endif
c       
c       if the order array is full or there's not enough space in the object
c       array, first try to repack arrays, then give up if still too full
c       
      ninobj=npt_in_obj(iobj)
      failed=nin_order.ge.max_obj_order .or.
     &    ibase_free+ninobj+4.ge.len_object
      if(failed)call object_packer
      failed=nin_order.ge.max_obj_order .or.
     &    ibase_free+ninobj+4.ge.len_object
      if(failed)then
        print *,'insufficient space to make desired object current'
        return
      endif
c       
c       if object is not null, move to top of OBJECT and adjust things;
c       otherwise assume nothing is set up, not even base pointer
c       
      if(ninobj.gt.0)then
        ibase=ibase_obj(iobj)
        ibase_obj(iobj)=ibase_free
        do ind=ibase+1,ibase+ninobj
          ibase_free=ibase_free+1
          object(ibase_free)=object(ind)
        enddo
        nin_order=nin_order+1                   !add to end of order list
        obj_order(nin_order)=iobj
        obj_order(ndx_order(iobj))=0            !clear earlier spot in list
        ndx_order(iobj)=nin_order               !set up index to order list
      endif
c       
      return
      end
