	subroutine write_wmod(modelfile)
	implicit none
	include 'model.inc'
	character*(*) modelfile
	integer putimod,imodBackupFile, ierr
c
	ierr = imodBackupFile(modelfile)
	if(ierr.ne.0)write(6,*)'WARNING: write_wmod - Error ',
     &	    'attempting to rename existing model file'

	ierr=putimod(ibase_obj,npt_in_obj,p_coord,object,obj_color,
     &	    n_point,max_mod_obj)
	if(ierr.ne.0)then
	  write(6,*)'ERROR: write_wmod - writing output model file'
	  call exit(1)
	else
	  call writeimod(modelfile)
	endif
	return
	end
