*       * * * * ENDMODEL * * * * *
c       
c       ENDMODEL will make a new model file containing only specified
c       starting and ending points of contours in a model.  The colors of
c       the new model objects may be completely different from the colors of
c       the original objects.
c       
c       For data coming from a WIMP model file, the IMOD object number is
c       the 256 minus the WIMP color (e.g., object 1 for color 255).  WIMP
c       objects that are turned off may be referred to by the negative of
c       this object number.
c       
c       Entries to the program:
c       
c       Name of input model file
c       
c       Name of output model file
c       
c       Number of different objects to create in the output
c       model.  For each such object, next enter the following two lines:
c       
c       .  Color of the object in the output model, and number of kinds of
c       .  ends to include in that object.  (For example, "250,4" to include
c       .  four kinds of ends in an object colored 250)
c       
c       .  A list of pairs of values: each pair consists of an object number
c       .  in the old model, and either a 0 to take the starting point or a 1
c       .  to take the ending point for each contour in that object.  (For
c       .  example, to take starting points of contours in objects 1 and 3,
c       .  and ending points of contours in objects 2 and 4, enter the list
c       .  "1,0,3,0,2,1,4,1" )
c       
c       Number of points to generate for each new model contour, and the
c       increment in Z between them.  If you need this model just to look at,
c       enter "1,0" to get just a single point.  If you intend to run an MTK
c       analysis of 3-D distances between these ends, you need at least 2
c       points; in this case enter "2,0.1"
c       
c       David Mastronarde  11/23/92; modified for IMOD 4/24/97
c       
      include 'model.inc'
      character*80 modelfile
      logical exist,readw_or_imod
      real*4 xyzmt(3,2,max_obj_num)
      integer*4 itype(max_obj_num),newtype(100),nmap(100)
     &    ,ioldtype(100,100),ifend(100,100)
c       
91    write(*,'(1x,a,$)')'Name of input model file: '
      read(5,'(a)')modelfile
c       
75    exist=readw_or_imod(modelfile)
      if(.not.exist)go to 91
c       
      write(*,'(1x,a,$)')'Name of model file to store points in: '
      read(5,'(a)')modelfile
c       
      call deleteiobj()
      nmt=0
      do iobj=1,max_mod_obj
        if(npt_in_obj(iobj).ge.2)then
          ibase=ibase_obj(iobj)
          ip1=abs(object(1+ibase))
          ip2=abs(object(npt_in_obj(iobj)+ibase))
          nmt=nmt+1
          itype(nmt)=256-obj_color(2,iobj)
          if(obj_color(1,iobj).eq.0)itype(nmt)=-itype(nmt)
          do i=1,3
            xyzmt(i,1,nmt)=p_coord(i,ip1)
            xyzmt(i,2,nmt)=p_coord(i,ip2)
          enddo
        endif
      enddo
c       
      write(*,'(1x,a,$)')'# of new objects to create: '
      read(5,*)nnew
c       
      do inew=1,nnew
        write(*,'(1x,a,i3,a,$)')'For new object #',inew,
     &      ', enter new color and # of objects mapping to it: '
        read(5,*)newtype(inew),nmap(inew)
        print *,'For each old object, enter the object # (- if ',
     &      'turned off in WIMP model),',
     &      ' and 0 for start or 1 for end'
        read(5,*)(ioldtype(inew,j),ifend(inew,j),j=1,nmap(inew))
      enddo
c       
      write(*,'(1x,a,$)')
     &    '# of points to store at each end, z increment: '
      read(5,*)nstore,zinc
c       
      do i=1,max_obj_num
        npt_in_obj(i)=0
      enddo
      n_object=0
      n_point=0
c       
      do imt=1,nmt
        do inew=1,nnew
          do imap=1,nmap(inew)
            if(itype(imt).eq.ioldtype(inew,imap))then
              n_object=n_object+1
              obj_color(1,n_object)=1
              obj_color(2,n_object)=newtype(inew)
              npt_in_obj(n_object)=nstore
              ibase_obj(n_object)=n_point
              isn=ifend(inew,imap)+1
              do inc=1,nstore
                n_point=n_point+1
                object(n_point)=n_point
                p_coord(1,n_point)=xyzmt(1,isn,imt)
                p_coord(2,n_point)=xyzmt(2,isn,imt)
                p_coord(3,n_point)=xyzmt(3,isn,imt)+(inc-1)*zinc
                pt_label(n_point)=0
              enddo
            endif
          enddo
        enddo
      enddo
c       
      max_mod_obj=n_object
      call write_wmod(modelfile)
      stop
      end
