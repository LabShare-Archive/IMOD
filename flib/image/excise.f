************EXCISE**************
*       
*       EXCISE will chop out pieces from an image file, at positions
c       specified in a file of coordinates.  The file consists of three
c       integer index coordinates per position (x, y, z), such as is put out
c       by the POINT routine in WIMP.  The centers of the excised pieces can
c       be offset from the point coordinates by specified amounts.  If a
c       piece extends beyond the edge of the original image, it will be
c       padded with the mean intensity of the input file.
c       
c       The inputs are:
c       
c       Input image file
c       Name of piece list file if image is a montage; otherwise Return
c       Name of file of point coordinates
c       Name of output file for pieces
c       X and Y dimensions of the output file
c       X and Y offsets, which will be added to the point coordinates to
c       .  obtain the coordinates of the center of each output piece
c       
c       The program can extract pieces from a montaged image file, but it
c       will not do so properly if a piece falls in the overlap zone between
c       montage frames.  It would have to be modified to do this properly.
c       
c       David Mastronarde 3/4/91
*       
      parameter (ixdim=2100,iydim=2100,maxshift=3)
      parameter (limpnt=10000,limpcl=50000)
      COMMON //NX,NY,NZ
C       
      DIMENSION NXYZ(3),MXYZ(3),NXYZST(3),
     &    ARRAY(ixdim*iydim),BRRAY(ixdim*iydim),TITLE(20),
     &    NXYZ2(3),MXYZ2(3),CELL2(6)
C       
      integer*4 ixpclist(limpcl),iypclist(limpcl),izpclist(limpcl)
      CHARACTER*80 FILIN,FILOUT,filpoint,plfile
      character*9 dat
      character*8 tim
c       
c       7/7/00 CER: remove the encode's; titlech is the temp space
c       
      character*80 titlech
C       
      EQUIVALENCE (NX,NXYZ)
C       
      integer*4 ipx(limpnt),ipy(limpnt),ipz(limpnt)
      dimension listsect(300)
      DATA NXYZST/0,0,0/
c       
      write(*,'(1x,a,$)')'Image input file: '
      READ(5,101)FILIN
101   format(a)
      CALL IMOPEN(1,FILIN,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN2,DMAX2,DMEAN2)
C       
      IF ((NX*NY.GT.ixdim*iydim)) THEN
        PRINT *,' INPUT ARRAY TOO LARGE.'
        STOP
      ENDIF
c       
      write(*,'(1x,a,$)')'Piece list file if image is montage,'//
     &    ' otherwise Return: '
      read(*,101)plfile
      call read_piece_list(plfile,ixpclist,iypclist,izpclist,npclist)
c       
      write(*,'(1x,a,$)')'File with coordinates to excise: '
      read(5,101)filpoint
      call dopen(3,filpoint,'ro','f')
      write(*,'(1x,a,$)')'Output file for excised pieces: '
      READ(5,101)FILOUT
c       
c       read in the points in point file
c       
      npoint=0
10    j=npoint+1
      read(3,*,end=15)ixtm,iytm,iztm
      call lookup_piece(ixpclist,iypclist,izpclist,npclist,
     &    nx,ny,ixtm,iytm,iztm,ipx(j),ipy(j),ipz(j))
      npoint=j
      go to 10
c       
15    write(*,'(1x,a,$)')'X and Y dimensions of output file: '
      read(*,*)nx3,ny3
c       
      write(*,'(1x,a,$)')'X and Y offsets from coordinates to'//
     &    ' center of output piece: '
      read(5,*)ixofset,iyofset
c       
      newmode=mode
c       write(*,'(1x,a,i2,a,$)')
c       &      'Mode of output file [',newmode,'=default, same as input]: '
c       read(*,*)newmode
c       if(newmode.lt.0.or.newmode.gt.15.or.
c       &           (newmode.gt.2.and.newmode.lt.9))go to 17
c       
C       
C       Create output header.  Just assume all points will exist in file.
C       
      CALL IMOPEN(2,FILOUT,'NEW')
      NXYZ2(1)=NX3
      NXYZ2(2)=NY3
      NXYZ2(3)=npoint
      MXYZ2(1)=NX3
      MXYZ2(2)=NY3
      MXYZ2(3)=npoint
      CELL2(1)=NX3
      CELL2(2)=NY3
      CELL2(3)=npoint
      CELL2(4)=90.
      CELL2(5)=90.
      CELL2(6)=90.
C       
      call time(tim)
      call date(dat)
c       
c       7/7/00 CER: remove the encodes
c       
c       ENCODE(80,301,TITLE)dat,tim
      write(titlech,301)
301   FORMAT('EXCISE: pieces excised from image file',t57,a9,2x,a8)
      read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
      CALL ICRHDR(2,NXYZ2,MXYZ2,newmode,TITLE,0)
      call itrlab(2,1)
      CALL IALCEL(2,CELL2)
      dmin=1.e10
      dmax=-1.e10
      tmean=0.
c       
c       make list of z values
c       
      nzlist=0
      do j=1,npoint
        notonlist=1
        do list=1,nzlist
          izpnt=ipz(j)
          if(izpnt.eq.listsect(list).or.izpnt.lt.0)notonlist=0
        enddo
        if(notonlist.eq.1)then
          nzlist=nzlist+1
          listsect(nzlist)=ipz(j)
        endif
      enddo
c       
c       loop over the sections in the list
c       
      do list=1,nzlist
c         
c         read section if it exists
c         
        iz=listsect(list)
        if(iz.ge.0.and.iz.lt.nz)then
          call imposn(1,iz,0)
          call irdsec(1,array,*99)
c           
c           look for points in that section
c           
          do jpoint=1,npoint
            if(ipz(jpoint).eq.iz)then
              ix1=ipx(jpoint)+ixofset-nx3/2
              ix2=ix1+nx3-1
              iy1=ipy(jpoint)+iyofset-ny3/2
              iy2=iy1+ny3-1
c               
              call irepak2(brray,array,nx,ny,ix1,ix2,iy1,iy2,dmean2)
c               
              call iclden(brray,nx3,ny3,1,nx3,1,ny3,tmin,tmax,tmpmn)
              dmin=min(dmin,tmin)
              dmax=max(dmax,tmax)
              tmean=tmean+tmpmn
              call iwrsec(2,brray)
            endif
          enddo
        else
          print *,'SECTION #',IZ,' DOES NOT EXIST IN FILE'
          stop
        endif
      enddo
c       
      dmean=tmean/npoint
C       
      CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
      CALL IMCLOSE(2)
C       
      print *,npoint,' pieces excised'
      STOP
99    WRITE(6,450)
450   FORMAT(' END OF IMAGE WHILE READING')
      STOP
      END


c       
c       not so simple routine to repack an image from a portion of a 2-d
c       array sequentially into a 1-d array (which should not be the same
c       array).  Pixels outside the range of the original array will be
c       fill with the supplied value of DMEAN.  BRRAY is the repacked array,
c       everything else follows definition of iwrpas
c       
      subroutine irepak2(brray,array,mx,my,nx1,nx2,ny1,ny2,dmean)
      dimension brray(*),array(mx,my)
      ind=1
      do iy=ny1+1,ny2+1
        if(iy.ge.1.and.iy.le.my)then
          do ix=nx1+1,nx2+1
            if(ix.ge.1.and.ix.le.mx)then
              brray(ind)=array(ix,iy)
            else
              brray(ind)=dmean
            endif
            ind=ind+1
          enddo
        else
          do ix=nx1+1,nx2+1
            brray(ind)=dmean
            ind=ind+1
          enddo
        endif
      enddo
      return
      end
