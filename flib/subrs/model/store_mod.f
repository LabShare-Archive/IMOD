cccccccccSubroutine store_mod.forcccccccccccccccccccccccccccccccccccccccccccccc
c       This routine is used to store the model info into an ascii disk file.
c       
c       
c       Hans Chen       Aug 31, 1987
c       DNM 9/12/88 introduced parameter definitions and eliminated mod 10000
c       DNM 9/13/88 changed format for writing point # from 2x,i5 to 1x,i6
c       DNM 9/28/88 stop writing out null labels
c       DNM 12/21/88 changed to work with new format for dealing with text
c       labels and cleaned up declarations now that all model stuff is in .in
c       file
c       DNM 7/20/89  changes for new model format
c       
c       $Id$
c       
c       $Log$
c       
      subroutine store_mod(model_file)

      include 'model.inc'
      
      character*50 model_file
      character*39 dummy
      character*80 string,sttt

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>Execution starts here<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      if(n_point.le.0) return
      maxlen=1
      do i=1,max_mod_obj
        maxlen=max(maxlen,npt_in_obj(i))
      enddo
      needmax=max(max_mod_obj,min(max_obj_num,
     &    ifix(sqrt(len_object*float(n_object)/maxlen))))
      write(20,'(a80)',err=99)sttt              !to take care a new file
      rewind(20)
      dummy='Model file name........................'
      write(20,'(1x,a39,a30)',err=99)dummy,model_file(1:30)
      dummy='max # of object........................'
      write(20,'(1x,a38,i5)',err=99)dummy,needmax
      dummy='# of node..............................'
      write(20,'(1x,a38,i5)',err=99)dummy,n_point
      dummy='# of object............................'
      write(20,'(1x,a38,i5)',err=99)dummy,n_object

      dummy='  Object sequence :'
      write(20,'(a20)',err=99)dummy(1:20)
      do i=1,max_mod_obj
        if(npt_in_obj(i).gt.0) then
c           print*,' iobject=',i
          write(20,'(a11,i8)',err=99)'  Object #:',i
          write(20,'(a12,i10)',err=99)' # of point:',npt_in_obj(i)
          write(20,'(a16,i1,2x,i3)',err=99)' Display switch:',
     &        obj_color(1,i),obj_color(2,i)
          String='     #    X       Y       Z      Mark    Label '
          write(20,'(a47)',err=99)string(1:47)
          do ii=1,npt_in_obj(i)
            ipnt=object(ii+ibase_obj(i))
            it=abs(ipnt)
c             DNM: change format to allow >100000 points, pt_label 3 digits
            if(n_clabel.eq.0)then
              write(20,'(1x,i6,1x,3(f7.2,1x),i3)' ,err=99)ipnt,
     &            p_coord(1,it),p_coord(2,it),p_coord(3,it)
     &            ,pt_label(it)
            else
c               search for labels at "it"
              ilab=1
              indlab=0
              do while(ilab.le.n_clabel.and.indlab.eq.0)
                if(label_list(ilab).eq.it)indlab=ilab
                ilab=ilab+1
              enddo
              if(indlab.eq.0)then
                write(20,'(1x,i6,1x,3(f7.2,1x),i3)' ,err=99)ipnt,
     &              p_coord(1,it),p_coord(2,it),p_coord(3,it)
     &              ,pt_label(it)
              else
                write(20,'(1x,i6,1x,3(f7.2,1x),i3,2x,a10)'
     &              ,err=99)ipnt,
     &              p_coord(1,it),p_coord(2,it),p_coord(3,it),
     &              pt_label(it),clabel(indlab)    
              endif
            endif
          enddo
        endif
      enddo
      write(20,'(a)')' '
      write(20,'(a5)')'  END'
      return
99    print*,' format error, model file is truncated'
      return
      end
