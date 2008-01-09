*       * * * * * EDPIECEPOINT * * * * *
*       
c       This program allows one to "edit" a list of piece coordinates, or a
c       list of point coordinates, in two ways:
c       1) The set of Z values may be mapped, one-to-one, to any arbitrary
c       new set of Z values; and
c       2) The X, Y or Z coordinates of all pieces or points may be shifted
c       by a constant.
c       In addition, one can make up a new piece list for single or multiple
c       frames.
c       
c       See man page for details
c       
c       David Mastronarde  5/8/89
c
c       $Id$
c       
c       $Log$
c       
      implicit none
      integer limpcl, limsec
      parameter (limpcl=1000000,limsec=1000000)
      character*160 pclfil
      integer*4 ixpclist(limpcl),iypclist(limpcl),izpclist(limpcl)
      integer*4 listz(limsec),newlist(limsec),indxzlst(limsec)
      integer*4 npclist, nxframe, nyframe, nxdelta, nydelta,nzdummy, ix, iy, iz
      integer*4 i, nlistz,nnew,ixadd,iyadd,izadd,ipc,newzval,indmov
c       
      nxframe = 1
      nyframe = 1
      nxdelta = 0
      nydelta = 0
      ixadd = 0
      iyadd = 0
      izadd = 0
c
      write(*,'(1x,a)')'Enter name of input piece or point'//
     &    ' list file (Return if none)'
      read(*,'(a)')pclfil
      call read_piece_list(pclfil,ixpclist,iypclist,izpclist,npclist)
c       
      if(npclist.eq.0)then
        write(*,'(1x,a,$)')'# of sections to make new piece list for: '
        read(*,*)nzdummy
        write(*,'(1x,a,$)')'# of montage pieces in X and in Y (/ for 1,1): '
        read(5,*)nxframe, nyframe
        write(*,'(1x,a,$)')
     &      'Spacing between pieces in X and Y (size minus overlap): '
        read(5,*)nxdelta, nydelta
        i = 0
        do iz=1,nzdummy
          do iy = 1, nyframe
            do ix = 1, nxframe
              i = i + 1
              ixpclist(i)=(ix - 1) * nxdelta
              iypclist(i)=(iy - 1) * nydelta
              izpclist(i)=iz-1
            enddo
          enddo
        enddo
        npclist = i
      endif
c       
      write(*,'(1x,a,$)')
     &    'Name of output piece or point list file: '
      read(*,'(a)')pclfil
      call fill_listz(izpclist,npclist,listz,nlistz)
c       
c       get list of ranges
c       
      print *,'The current list of Z values in input list is:'
      call wrlist(listz,nlistz)
c       
c       get new list of z values for remapping
c       
20    write(*,'(/,a,i4,a,/,a,/,a,/,a)')' Enter new list of',nlistz,
     &    ' Z values to remap these values to.',
     &    ' Enter ranges just as above, or / to leave list alone,',
     &    '   or -1 to replace each Z value with its negative.',
     &    ' Use numbers from -999 to -990 to remove points with a'//
     &    ' particular Z value.'
      do i=1,nlistz
        newlist(i)=listz(i)
      enddo
      nnew=nlistz
      call rdlist(5,newlist,nnew)
c       
      if(nnew.eq.1.and.newlist(1).eq.-1)then
        do i=1,nlistz
          newlist(i)=-listz(i)
        enddo
      elseif(nnew.ne.nlistz)then
        print *,'Number of Z values does not correspond'
        go to 20
      endif
      do i=1,nlistz
        indxzlst(listz(i)+1-listz(1))=i
      enddo
c       
      write(*,'(1x,a,$)')
     &    'Amounts to ADD to ALL X, Y and Z coordinates (/ for 0,0,0): '
      read(*,*)ixadd,iyadd,izadd
c       
      ipc=1
      do while(ipc.le.npclist)
        newzval=newlist(indxzlst(izpclist(ipc)+1-listz(1)))
        if(newzval.lt.-999.or.newzval.gt.-990)then
          izpclist(ipc)=newzval+izadd
          ixpclist(ipc)=ixpclist(ipc)+ixadd
          iypclist(ipc)=iypclist(ipc)+iyadd
          ipc=ipc+1
        else
          npclist=npclist-1
          do indmov=ipc,npclist
            ixpclist(indmov)=ixpclist(indmov+1)
            iypclist(indmov)=iypclist(indmov+1)
            izpclist(indmov)=izpclist(indmov+1)
          enddo
        endif
      enddo
      call dopen(3,pclfil,'new','f')
      write(3,'(3i6)')(ixpclist(i),iypclist(i),izpclist(i) ,i=1,npclist)
      close(3)
      call exit(0)
      end
