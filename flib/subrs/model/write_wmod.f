	subroutine write_wmod(modelfile)
	include 'model.inc'
	character*(*) modelfile
C	add definition of concat subroutine from hvem directory
	character*80 comline,tmpfile,rmline,concat
	logical exist
	integer rename,putimod
	inquire(file=modelfile,exist=exist)
	namlen=lnblnk(modelfile)
	if(exist)then
C	ierr=rename(modelfile,modelfile(1:namlen)//'~')
	ierr=rename(modelfile,concat(modelfile(1:namlen),'~'))
	  if(ierr.ne.0)write(6,*)
     &	      ' Error attempting to rename existing model file'
	endif
	ierr=putimod(ibase_obj,npt_in_obj,p_coord,object,obj_color,
     &	    n_point,max_mod_obj)
	if(ierr.ne.0)then
	  write(6,*)' Error attempting to output model file'
	else
	  call writeimod(modelfile)
	endif
	return
	end
