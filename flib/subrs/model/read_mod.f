ccccccccccsubroutine read_mod ccccccccccccccccccccccccccccccccccccccccccccccccc
c       This routine reads model info from a disk file which is generated by
c       STORE_MOD.
c       Hans Chen	Aug 31, 1987
c       DNM 9/12/88 defined new parameters, fixed it so it could read in old
c       models with branch points marked by 10000 but eliminate the marker
c       DNM 9/13/88 changed format for reading point # from 2x,i5 to 1x,i6
c       DNM 12/21/88 changed to work with new format for dealing with text
c       labels and cleaned up declarations now that all model stuff is in .inc
c       file
c       DNM 7/20/89  changes for new model format
c
c       $Id$
c
c       $Log$
c
      logical function read_mod()
      include 'model.inc'
      
      character*50 model_file
      character*10 label_c
      character*39 dummy
      character*80 string

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>Execution starts here<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      n_clabel=0
      read_mod=.false.
      read(20,101,err=151)string
101   format(90a)
      if(string(1:5).eq.'Model')then
        indstr=1
      elseif(string(1:5).eq.' Mode')then
        indstr=2
      else
        return
      endif
      read(20,101,err=151)string
      read(string(indstr:),'(38x,i5)',err=151)max_mod_obj
      read(20,101,err=151)string
      read(string(indstr:),'(38x,i5)',err=151)n_point
      read(20,101,err=151)string
      read(string(indstr:),'(38x,i5)',err=151)n_object
      read(20,101,err=151) string               ! Object sequence :
      n_object=0
      ibase_free=0
      ntot_in_obj=0
      max_mod_obj=0
      do i=1,max_obj_num
        npt_in_obj(i)=0
      enddo
c	call zero(npt_in_obj,4*max_obj_num)
100   read(20,101,err=151)string
      if(string(indstr:3+indstr).eq.' Obj')then
        read(string(indstr:),'(10x,i15)',err=151)i
        read(20,101,err=151)string
        read(string(indstr:),'(11x,i15)',err=151)ninobj
        read(20,101,err=151)string
        read(string(indstr:),'(15x,i1,2x,i3)',err=151)
     &      obj_color(1,i),obj_color(2,i)
        read(20,101,err=151)string
        n_object=n_object+1
        obj_order(n_object)=i
        ndx_order(i)=n_object
        npt_in_obj(i)=ninobj
        ntot_in_obj=ntot_in_obj+ninobj
        ibase_obj(i)=ibase_free
        max_mod_obj=max(max_mod_obj,i)
        do ii=1,ninobj
c           DNM: change format to allow >100000 points
          read(20,101,err=151)string
          read(string(indstr:),'(i6,1x,3(f7.2,1x),2x,i1,2x,a10)',
     &        err=151)ipt, p1,p2,p3,imark,label_c
          irec=ipt
          if(ipt.gt.0) then
c             DNM: if index is > # of points, then it must be old model and
c             needs fixing
            if(ipt.gt.n_point)ipt=mod(ipt,10000)
            p_coord(1,ipt)=p1
            p_coord(2,ipt)=p2
            p_coord(3,ipt)=p3
            pt_label(ipt)=imark
c             DNM: if label is on a real point, add to list
            if(label_c.ne.' ')then
              n_clabel=n_clabel+1
              label_list(n_clabel)=ipt
              clabel(n_clabel)=label_c
            endif
          endif
          object(ii+ibase_free)=ipt
        enddo
        ibase_free=ibase_free+ninobj
        go to 100	 
      elseif(string(1:5).eq.'     ') then
        nin_order=n_object
        read_mod=.true. 
        return	 
      endif
151   close(20)
      write(*,'(a)') 
     &    ' Model file format incompatible,input a new one'
      print *,' ipt=',ii,i,irec
      
      return
      end
