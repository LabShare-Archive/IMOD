c       parameters defining the maximum size of a model - small model version
c       
      integer max_obj_num,max_pt,len_object,max_obj_order,max_clabel
      parameter (max_obj_num=75000)             !absolute max # of objects 
      parameter (max_pt=1000000)                !max # of real points with coordinates
      parameter (len_object=(max_pt+max_pt/5))  !len of pointer array "object"
      parameter (max_obj_order=(max_obj_num+max_obj_num/5))
      parameter (max_clabel=200)                !max # of text labels

c       Variable definition for the common /wmpmod/
      integer*4 object(len_object)              !pointer to p_coord array
      integer*4 npt_in_obj(max_obj_num)         !# of points in object
      integer*4 ibase_obj(max_obj_num)          !base index of object in OBJECT
      integer*4 obj_color(2,max_obj_num)        !ON/OFF and color indexes
      integer*4 obj_order(max_obj_order)        !list of objects in order
      integer*4 ndx_order(max_obj_num)          !index of object's entry in OBJ_ORDER
      real*4 p_coord(3,max_pt)                  !model coordinates of points
      integer*1 pt_label(max_pt)                !symbol number for point
      character*10 clabel(max_clabel)           !text labels
      integer*4 label_list(max_clabel)          !list of points with labels
      integer*4 n_point                         !highest point # in p_coord
      integer*4 n_object                        !total # of non-zero objects
      integer*4 ibase_free                      !base index of free area in OBJECT
      integer*4 ntot_in_obj                     !total entries in OBJECT
      integer*4 nin_order                       !# of entries in OBJ_ORDER
      integer*4 max_mod_obj                     !highest object # used so far
      integer*4 n_clabel                        !number of text labels
      integer*4 mod_window(9)                   !flag - display model in window
      integer*4 mod_changes                     !current # of changes to model
      logical display

      common/wmpmod/object,npt_in_obj,ibase_obj,obj_color,obj_order
     &    ,ndx_order,p_coord,pt_label,clabel,label_list
     &    ,n_point,n_object,ibase_free,ntot_in_obj,nin_order
     &    ,max_mod_obj,n_clabel,mod_window,mod_changes,display
