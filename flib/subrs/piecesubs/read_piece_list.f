c	  READ_PIECE_LIST will open the file whose name is in FILPCL, and
c	  read the piece coordinates into IXPCLIST, IYPCLIST, IZPCLIST until
c	  end of file.  NPCLIST has the number of coordinates, or 0 if the
c	  file name is blank.
c
      subroutine read_piece_list(filpcl,ixpclist,iypclist,izpclist, npclist)
      implicit none
      character*(*) filpcl
      integer*4 ixpclist(*),iypclist(*),izpclist(*), npclist
      call read_piece_list2(filpcl,ixpclist,iypclist,izpclist, npclist, 0)
      return
      end

c       A safe version that will only fill the array up to LIMPCL and issues a warning
c       if pieces don't fit
c
      subroutine read_piece_list2(filpcl,ixpclist,iypclist,izpclist, npclist, limpcl)
      implicit none
      character*(*) filpcl
      integer*4 ixpclist(*),iypclist(*),izpclist(*), npclist, limpcl, ixt, iyt, izt, i
c
      npclist=0
      if(filpcl.eq.' ')return
      call dopen(2,filpcl,'ro','f')
10    i=npclist+1
      read(2,*,end=20,err=30)ixt, iyt, izt
      if (limpcl .gt. 0 .and. i .gt. limpcl) then
        write(*,'(/,a)')'WARNING: read_piece_list - Too many piece coordinates for arrays'
        return
      endif
      ixpclist(i) = ixt
      iypclist(i) = iyt
      izpclist(i) = izt
      npclist=i
      go to 10
20    close(2)
      return
30    write(*,'(/,a)')'ERROR: read_piece_list - reading piece list file'
      call exit(1)
      end
