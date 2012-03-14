c       LOOKUP_PIECE takes a list of NPCLIST piece coordinates in
c       I[XYZ]PCLIST, the piece dimensions NX and NY, and index coordinates
c       in the montaged image IND[XYZ], and finds the piece that those
c       coordinates are in (IPCZ) and the coordinates IPCX, IPCY of the point
c       in that piece
c       
      subroutine lookup_piece(ixpclist,iypclist,izpclist,
     &    npclist,nx,ny,indx,indy,indz,ipcx,ipcy,ipcz)
c       
      integer*4 ixpclist(*),iypclist(*),izpclist(*)
c       
      ipcx=indx
      ipcy=indy
      ipcz=indz
      if(npclist.eq.0)return
      ipcx=-1
      ipcy=-1
      ipcz=-1
      do ipc=1,npclist
        if(indz.eq.izpclist(ipc).and.
     &      indx.ge.ixpclist(ipc).and.indx.lt.ixpclist(ipc)+nx .and.
     &      indy.ge.iypclist(ipc).and.indy.lt.iypclist(ipc)+ny)then
          ipcz=ipc-1
          ipcx=indx-ixpclist(ipc)
          ipcy=indy-iypclist(ipc)
          return
        endif
      enddo
      return
      end
