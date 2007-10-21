c	  READ_PIECE_LIST will open the file whose name is in FILPCL, and
c	  read the piece coordinates into IXPCLIST, IYPCLIST, IZPCLIST until
c	  end of file.  NPCLIST has the number of coordinates, or 0 if the
c	  file name is blank.
c
	subroutine read_piece_list(filpcl,ixpclist,iypclist,izpclist,
     &	    npclist)
	character*(*) filpcl
c
	integer*4 ixpclist(*),iypclist(*),izpclist(*)
c
	npclist=0
	if(filpcl.eq.' ')return
	call dopen(2,filpcl,'ro','f')
10	i=npclist+1
	read(2,*,end=20,err=30)ixpclist(i),iypclist(i),izpclist(i)
	npclist=i
	go to 10
20	close(2)
	return
30      write(*,'(/,a)')'ERROR: read_piece_list - reading piece list file'
        call exit(1)
	end
