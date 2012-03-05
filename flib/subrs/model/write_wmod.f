c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       ! Writes the current model as an IMOD model to the file [modelfile].
c       If [n_point] is the model common is negative, it will not put
c       contour data in the model arrays into the model before saving.
c       
      subroutine write_wmod(modelfile)
      implicit none
      character*(*) modelfile
      integer writeimod,imodBackupFile, ierr
c       
      ierr = imodBackupFile(modelfile)
      if(ierr.ne.0)write(6,*)'WARNING: write_wmod - Error ',
     &    'attempting to rename existing model file'

      call putModelObjects()
      ierr = writeimod(modelfile)
      if(ierr.ne.0)then
        write(6,*)'ERROR: write_wmod - writing output model file'
        call exit(1)
      endif
      return
      end

c       ! Puts contour data in the model arrays back into the model, unless 
c       [n_point] is the model common is negative.  In partial mode, only
c       the objects containing data in the arrays will be modified in the
c       IMOD model; otherwise any existing objects in the IMOD model will be
c       removed.
c       
      subroutine putModelObjects()
      implicit none
      include 'model.inc'
      integer putimod,ierr
c       
c       Set n_point to negative to signal that objects are already put out
c       
      if (n_point .ge. 0) then
        ierr=putimod(ibase_obj,npt_in_obj,p_coord,object,obj_color,
     &      n_point,max_mod_obj)
      else
        ierr = 0
      endif
      if (ierr.ne.0) then
        write(6,*)'ERROR: putModelObjects - putting objects back '//
     &      'into IMOD model'
        call exit(1)
      endif
      return
      end
