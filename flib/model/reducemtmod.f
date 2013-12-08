*       * * * * REDUCEMTMOD * * * * *
c       
c       REDUCEMTMOD will reduce the number of points in a simple model (i.e.
c       one with no branch points), retaining the starting and ending points
c       and every Nth point in between, where N is the reduction factor
c       specified by the user.
c       
c       Entries:
c       
c       Name of input model file
c       
c       Name of output file for reduced model
c       
c       Reduction factor.  2 will retain every 2nd point; 3, every 3rd point,
c       .   etc.  To retain only the start and end, enter a number greater
c       .   than the longest object length.
c       
c       David Mastronarde 2/9/90
c       
      include 'model.inc'
      integer*4 listz(1000)
      logical exist,readw_or_imod,inside,lastinside
      character*80 modelfile
      character*2 inex

10    write(*,'(1x,a,$)')'Name of input model file to reduce: '
      read(*,'(a)')modelfile
15    exist=readw_or_imod(modelfile)
      if(.not.exist)go to 10
c       
      write(*,'(1x,a,$)')'Name of output file for reduced model: '
      read(*,'(a)')modelfile
c       
      write(*,'(1x,a,$)')
     &    'Reduction factor (i.e., N to retain every Nth point): '
      read(*,*)nreduce
c       
      do iobj=1,max_mod_obj
        ninobj=npt_in_obj(iobj)
        if(ninobj.gt.0)then
          ninnew=(ninobj+2*(nreduce-1))/nreduce
          iptold=1
          ibase=ibase_obj(iobj)
          do iptnew=1,ninnew
            object(iptnew+ibase)=object(iptold+ibase)
            iptold=min(ninobj,iptold+nreduce)
          enddo
          npt_in_obj(iobj)=ninnew
        endif
      enddo
      call repack_mod
c       
c       write out result
c       
77    call write_wmod(modelfile)
      stop 'REDUCED MODEL REPACKED AND WRITTEN'
      end
