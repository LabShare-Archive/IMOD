c       FILL_LISTZ examines the list of z values in a piece list (NCPLIST
c       values in array IZPCLIST) and returns an ordered list of the unique
c       values (NLISTZ values in array LISTZ)
c       
      subroutine fill_listz(izpclist,npclist,listz,nlistz)
c       
      integer*4 izpclist(*),listz(*)
c       
      nlistz=0
      do i=1,npclist
        ifound=0
        do il=1,nlistz
          if(listz(il).eq.izpclist(i))ifound=-1
          if(ifound.ge.0.and.listz(il).lt.izpclist(i))ifound=il
        enddo
        if(ifound.ge.0)then
          do it=nlistz,ifound+1,-1
            listz(it+1)=listz(it)
          enddo
          listz(ifound+1)=izpclist(i)
          nlistz=nlistz+1
        endif
      enddo
      return
      end
