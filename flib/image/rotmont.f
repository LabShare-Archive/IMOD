*       * * * * * * * * ROTMONT.FOR * * * * * * *
*       
c       ROTMONT will rotate selected sections from a montage by plus or minus
c       90 degrees and allow one to shift the coordinates by a fixed amount.
*       
*       Entries are as follows:
*       
*       Input image file
*       Name of input file with list of piece coordinates
*       Output image file
*       Name of new file for list of coordinates of pieces in the output file
c       
c       0 to rotate images by +90 degrees (counterclockwise) or 1 to rotate
c       .  by -90 degrees (clockwise)
c       
c       A list of sections to rotate, or Return for all sections (ranges may
c       .  be entered).
c       
c       Amounts to ADD to all X, Y and Z piece coordinates
*       
*       David Mastronarde, July, 1996

      parameter (maxsiz=2100*2100)              !# of pixels
      real*4 array(maxsiz),brray(maxsiz)
      character*80 filnam
      integer*4 mxyzin(3),nxyzst(3)/0,0,0/
      real*4 cell(6)/1.,1.,1.,0.,0.,0./
c       
      parameter (limxypc=50,limnpc=100000,limsect=1000)
      integer*4 nxyzin(3),nxyzout(3),nxin,nyin,nzin,nxout,nyout,nzout
      integer*4 nxoverlap,nyoverlap
      integer*4 ixpclist(limnpc),iypclist(limnpc) !piece coords in x,y
      integer*4 izpclist(limnpc)                !section #,
      equivalence (nxin,nxyzin(1))
      equivalence (nxout,nxyzout(1))
      integer*4 mappiece(limxypc,limxypc)       !map of pieces in this section
      integer*4 listz(limsect)
      common /pclist/nxin,nyin,nzin,nxout,nyout,nzout,nxpieces,
     &    nypieces ,nxoverlap,nyoverlap,npclist,minxpiece,minypiece,
     &    maxxpiece,maxypiece,ixpclist,iypclist,izpclist,mappiece
c       
      character dat*9, tim*8
c       
c       7/7/00 CER: remove the encode's; titlech is the temp space
c       
      character*80 titlech
c       
      real*4 title(20)
c       
      write(*,'(1x,a,$)')'Input image file: '
      read(5,'(a)')filnam
      call imopen(1,filnam,'ro')
      call irdhdr(1,nxyzin,mxyzin,modein,dmin,dmax,dmean)
c       
      write(*,'(1x,a,$)') 'Piece list file: '
      read(*,'(a)')filnam
      call read_piece_list(filnam,ixpclist,iypclist,izpclist,
     &    npclist)
c       
c       if no pieces, set up mocklist
      if(npclist.eq.0)stop 'error'
c       
c       now check lists and get basic properties of overlap etc
c       
      call checklist(ixpclist,npclist,1,nxin,minxpiece,nxpieces,
     &    nxoverlap)
      call checklist(iypclist,npclist,1,nyin,minypiece,nypieces,
     &    nyoverlap)
      if(nxpieces.le.0. or. nypieces.le.0)stop 'PIECE LIST NOT GOOD'
c       
c       
      write(*,'(1x,a,$)')'Output image file: '
      read(5,'(a)')filnam
      call imopen(2,filnam,'new')
      call itrhdr(2,1)

c       
30    write(*,'(1x,a,$)')'Name of output piece list file: '
      read(5,'(a)')filnam
      call dopen(3,filnam,'new','f')
c       
      write(*,'(1x,a,$)')
     &    '0 to rotate +90 (ccw) or 1 to rotate -90 (cw): '
      read(5,*)ifcw
      if(ifcw.eq.0)then
        maxrotxy=(nypieces-1)*(nyin-nyoverlap)+minypiece
        call rotpclist(ixpclist,iypclist,npclist,maxrotxy)
      else
        maxrotxy=(nxpieces-1)*(nxin-nxoverlap)+minxpiece
        call rotpclist(iypclist,ixpclist,npclist,maxrotxy)
      endif
c       
      print *,'Enter list of sections to rotate (ranges OK, Return',
     &    ' for all)'
      call rdlist(5,listz,nlistz)
c       
      write(*,'(1x,a,$)')
     &    'Amounts to add to X, Y and Z for all piece coordinates: '
      read(5,*)ixadd,iyadd,izadd
      dminout=1.e10
      dmaxout=-1.e10
      grandsum=0.
      call date(dat)
      call time(tim)
c       
c       7/7/00 CER: remove the encodes
c       
c       encode(80,90,title) dat,tim
      write(titlech,90) dat,tim
      read(titlech,'(20a4)')(title(kti),kti=1,20)
90    format( 'ROTMONT: Montage rotated by 90 degrees',
     &    t57, a9, 2x, a8 )
      nxout=nyin
      nyout=nxin
      nzout=0
      do iz=1,nzin
        ifrot=0
        if(nlistz.eq.0)then
          ifrot=1
        else
          do il=1,nlistz
            if(izpclist(iz).eq.listz(il))ifrot=1
          enddo
        endif
        if(ifrot.eq.1)then
          nzout=nzout+1
          call imposn(1,iz-1,0)
          call irdsec(1,array,*98)
          call rotarr(array,brray,nxin,nyin,ifcw)
          call iwrsec(2,brray)
          call iclden(brray,nxout,nyout,1,nxout,1,nyout,tmin,tmax,tmean)
          grandsum=grandsum+tmean
          dminout=min(dminout,tmin)
          dmaxout=max(dmaxout,tmax)
          ixpclist(nzout)=ixpclist(iz)+ixadd
          iypclist(nzout)=iypclist(iz)+iyadd
          izpclist(nzout)=izpclist(iz)+izadd
        endif
      enddo
      call ialsiz(2,nxyzout,nxyzst)
      call ialsam(2,nxyzout)
      cell(1)=nxout
      cell(2)=nyout
      cell(3)=nzout
      call ialcel(2,cell)
      call iwrhdr(2,title,1,dminout,dmaxout,grandsum/nzout)
      write(3,'(2i6,i4)')(ixpclist(i),iypclist(i),izpclist(i),i=1,
     &    nzout)
      close(3)
      call imclose(2)
      call exit(0)
98    stop 'error reading image'
      end


      subroutine rotpclist(ixpclist,iypclist,npclist,maxrotxy)
      integer*4 ixpclist(*),iypclist(*)
      do i=1,npclist
        itmp=ixpclist(i)
        ixpclist(i)=maxrotxy-iypclist(i)
        iypclist(i)=itmp
      enddo
      return
      end

      subroutine rotarr(array,brray,nx,ny,ifcw)
      real*4 array(nx,ny),brray(ny,nx)
      if(ifcw.eq.0)then
        do iy=1,ny
          ixo=ny+1-iy
          do ix=1,nx
            brray(ixo,ix)=array(ix,iy)
          enddo
        enddo
      else
        do ix=1,nx
          iyo=nx+1-ix
          do iy=1,ny
            brray(iy,iyo)=array(ix,iy)
          enddo
        enddo
      endif
      return
      end
