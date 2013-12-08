*****   SCALEMODEL.FOR*********************************************
*       Will rescale a model for changes in image origin, delta or tilt
*       

      include 'model.inc'

      real*4 tilt(3,2),orig(3,2),delt(3,2),freinp(10),tmp(3),tmp2(3)
      real*4 postnew(3),preold(3),scrat(3,3),mold(3,3),mnew(3,3)
      character*80 modelfile(2),dummy
      character*3 oldnew(2)
      data oldnew/'old','new'/
      logical exist,readw_or_imod
c       
c       get parameters
c       
      do ion=1,2
c         read model file name
91      write(*,'(1x,a,a,$)')oldnew(ion),' model file: '
        read(*,'(a)')modelfile(ion)
c         
c         read in the model or try to open new file right off
c         
        if(ion.eq.1)then
          print *,'reading model . . .'
          exist=readw_or_imod(modelfile(ion))
          if(.not.exist)then
93          print *,'file does not exist or is not in proper format'
            go to 91
          endif
        endif
c         
c         set up defaults: old = 0 orig, tilt, 1 delta; new=old
c         
        do j=1,3
          if(ion.eq.1)then
            orig(j,1)=0.
            delt(j,1)=1.
            tilt(j,1)=0.
          else
            orig(j,2)=orig(j,1)
            delt(j,2)=delt(j,1)
            tilt(j,2)=tilt(j,1)
          endif              
        enddo

c         read origin

        write(*,'(1x,a,a,3g11.4,a,$)')oldnew(ion),' origin x,y,z [',
     &      (orig(j,ion),j=1,3),']: '
        read(*,'(a)')dummy
        call frefor(dummy,freinp,ninp)
        do j=1,3
          if(ninp.ge.j)orig(j,ion)=freinp(j)
        enddo

c         read delta

        write(*,'(1x,a,a,3g11.4,a,$)')oldnew(ion),' delta x,y,z [',
     &      (delt(j,ion),j=1,3),']: '
        read(*,'(a)')dummy
        call frefor(dummy,freinp,ninp)
        do j=1,3
          if(ninp.ge.j)delt(j,ion)=freinp(j)
        enddo

c         read tilt

        write(*,'(1x,a,a,3f6.1,a,$)')oldnew(ion),
     &      ' x,y,z tilt angles [',(tilt(j,ion),j=1,3),']: '
        read(*,'(a)')dummy
        call frefor(dummy,freinp,ninp)
        do j=1,3
          if(ninp.ge.j)tilt(j,ion)=freinp(j)
        enddo
      enddo

c       find out if tilt series

      write(*,'(1x,a,$)')'1 for model on tilt series [0]: '
      read(*,'(a)')dummy
      call frefor(dummy,freinp,ninp)

c       set up arrays for pre and post rotation adjustment - use these to set
c       origin values into if the origin is to be considered on the "wrong" side
c       of the rotations
      do j=1,3
        preold(j)=0.
        postnew(j)=0.
      enddo

      if((ninp.ge.1) .and. (freinp(1).eq.1.))then
c         if tilt, add old z origin before rotating, subtract new z after rotate back
        preold(3)=orig(3,1)
        orig(3,1)=0.
        postnew(3)=orig(3,2)
        orig(3,2)=0.
      endif

c       get matrixes for old and new rotations

      call icalc_matrix(tilt(1,1),mold)
      call icalc_matrix(tilt(1,2),scrat)

      call inv_matrix(scrat,mnew)

c       do the transformations

      do i=1,n_point

c         get coords plus pre-old values
        do j=1,3
          tmp(j)=p_coord(j,i)+preold(j)
        enddo

c         rotate, add old origin, scale to new delta, subtract new origin
        do j=1,3
          tmp2(j)=(tmp(1)*mold(j,1)+tmp(2)*mold(j,2)+tmp(3)*mold(j,3)
     &        +orig(j,1))*delt(j,2)/delt(j,1)-orig(j,2)
        enddo

c         rotate back, subtract post-new values
        do j=1,3
          p_coord(j,i)=tmp2(1)*mnew(j,1)+tmp2(2)*mnew(j,2)
     &        +tmp2(3)*mnew(j,3)-postnew(j)
        enddo
      enddo

c       store the model
      call write_wmod(modelfile(2))
      close(20)

c       output what was done
      write(*,'(23x,a,36x,a)')'OLD','NEW'
      write(*,'(a,3g11.4,6x,3g11.4)')' ORIGIN:'
     &    ,((orig(j,i),j=1,3),i=1,2)
      write(*,'(a,3g11.4,6x,3g11.4)')' DELTA :'
     &    ,((delt(j,i),j=1,3),i=1,2)
      write(*,'(a,f7.1,2f11.1,6x,3f11.1)')' TILT  :'
     &    ,((tilt(j,i),j=1,3),i=1,2)
      end

c       DNM 10/30/88: need to declare LEN a real
      SUBROUTINE inv_matrix(M1,M2)

      REAL*4 M1(3,3),M2(3,3),LEN

C>>>>>>>>>>>>>>>>>>>>>>>>EXECUTION STARTS HERE<<<<<<<<<<<<<<<<<<<<<<<<<<<
      Len = m1(1,1)*m1(2,2)*m1(3,3)+m1(2,1)*m1(3,2)*m1(1,3)+
     &    m1(1,2)*m1(2,3)*m1(3,1)-m1(1,3)*m1(2,2)*m1(3,1)-m1(2,1)*
     &    m1(1,2)*m1(3,3)-m1(1,1)*m1(3,2)*m1(2,3)

      M2(1,1) = (M1(2,2)*M1(3,3)-M1(3,2)*M1(2,3))/len
      M2(2,1) = (M1(3,1)*M1(2,3)-M1(2,1)*M1(3,3))/len
      M2(3,1) = (M1(2,1)*M1(3,2)-M1(3,1)*M1(2,2))/len
      M2(1,2) = (M1(3,2)*M1(1,3)-M1(1,2)*M1(3,3))/len
      M2(2,2) = (M1(1,1)*M1(3,3)-M1(3,1)*M1(1,3))/len
      M2(3,2) = (M1(3,1)*M1(1,2)-M1(3,2)*M1(1,1))/len
      M2(1,3) = (M1(1,2)*M1(2,3)-M1(2,2)*M1(1,3))/len
      M2(2,3) = (M1(2,1)*M1(1,3)-M1(1,1)*M1(2,3))/len
      M2(3,3) = (M1(2,2)*M1(1,1)-M1(2,1)*M1(1,2))/len
      RETURN                          
      END

