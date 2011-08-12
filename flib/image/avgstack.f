c	-------------------------------------------------------------------
c	--- This program reads in an image file containing multiple	---
c	--- sections and averages the sections into a single output	---
c	--- image.							---
c	--- Written: 27-Jan-1989 - sjm					---
c	--- Updates: 19-Dec-1989 - dnm: fixed cell setting in header	---
c	-------------------------------------------------------------------
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 3.5  2011/02/23 20:00:12  mast
c       Used memory allocation
c
c       Revision 3.4  2011/02/23 19:30:27  mast
c       Allow filenames longer than 50 characters
c	
c       Revision 3.3  2006/03/23 20:25:11  mast
c       Fixed 0,0 being interpreted as do all sections
c	
c       Revision 3.2  2005/04/14 00:35:45  mast
c       Rewrote to read in chunks, put big array in common, redimensioned
c	
c       
      implicit none
      integer*4 maxarr, limarr

      real*4, allocatable :: array(:), brray(:)
      real*4 dmax, dmin, dmean, title(20), cell(6)
      integer*4 nxyz(3), mxyz(3), nxyzst(3),nx,ny,nz,mode
      equivalence (nxyz,nx)
      common //nx,ny,nz
      character*320 input, output
      character dat*9, tim*8
      data nxyzst / 0.,0.,0. /
      character*80 titlech
      integer*4 ifst,ilst,ix,iy,maxLines, numChunks,isec,i,kti
      integer*4 numsec,numLines,iChunk
      
c       
c	---------------------------------
c	--- initialize and input info ---

      write ( *, 1000 ) ' Input'
      read  ( *, 2000 ) input
      write ( *, 1000 ) 'Output'
      read  ( *, 2000 ) output

      call imopen(1,input,'ro')
      call imopen(2,output,'new')

      call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean) !--- Get Image Header ---!
      maxarr = nx*ny
      limarr = max(nx, min(5000*5000, maxarr))
      allocate(array(maxarr), brray(limarr), stat = ifst)
      call memoryError(ifst, 'ARRAYS FOR IMAGE DATA')

      nz = nz - 1                               !--- first section is 0, so nz = total # sections - 1
      ifst = 0
      ilst = nz
      write ( *, * ) ' Sections to average [First,Last] (/ for all):'
      read  ( *, * ) ifst, ilst

      ilst = min(ilst,nz)                       !--- Check end limit ---!
      ifst = min(ifst,nz)                       !--- Check beginning limit ---!
      ifst = min(ifst,ilst)                     !--- Check beginning/end order ---!


c	-------------------------------
c	--- Clear the storage array ---

      do iy = 1 , ny*nx
        array(iy) = 0.0
      end do

      call itrhdr(2,1)
      call ialmod(2,2)
      call ialnbsym(2,0)
      call ialsymtyp(2,0,0)

      nxyz(3) = 1
      mxyz(3) = 1
      call ialsam(2,mxyz)
      call ialsiz(2,nxyz,nxyzst)
      call irtcel(2,cell)
      cell(3) = cell(1) / mxyz(1)
      call ialcel(2,cell)

      call date(dat)
      call time(tim)
      numsec = ilst - ifst + 1

c       7/7/00 CER: remove the encodes
c       
C       encode ( 80, 3000, title ) numsec, dat, tim
      write(titlech,3000) numsec,dat,tim
      read(titlech,'(20a4)')(title(kti),kti=1,20)

      maxLines = limarr / nx
      numChunks = (ny + maxLines - 1) / maxLines

c	------------------------------------------------------------
c	--- Add all requested sections from the input image file ---

      do isec = ifst, ilst
        write ( *, * ) ' Adding section ', isec
        iy = 1
        call imposn(1,isec,0)
        do iChunk = 1, numChunks
          numLines = min(maxLines, ny - (iChunk - 1) * maxLines)
          call irdsecl(1,brray, numLines, *100) 
          do ix = 1, nx*numLines
            array(iy) = array(iy) + brray(ix)
            iy = iy + 1
          enddo
        enddo
      enddo

      call imclose(1)


c	--------------------------------
c	--- Average the output image ---
      write ( *, * ) ' Averaging stack...'
      do iy = 1 , nx*ny
        array(iy) = array(iy) / float(numsec)
      end do


c	-----------------------------
c	--- Write out the average ---


      call iclden(array,nx,ny,1,nx,1,ny,dmin,dmax,dmean)

      call iwrhdr(2,title,1,dmin,dmax,dmean)

      call iwrsec(2,array)

      call imclose(2)

      call exit(0)

1000  format ( 1x, a6, ' file : ', $ )
2000  format ( a )
3000  format ( 'AVGSTACK: ', i4, ' sections averaged.', t57, a9, 2x, a8 )
100   call errorexit('READING FILE')
      end

      subroutine errorexit(message)
      character*(*) message
      print *
      print *,'ERROR: AVGSTACK - ',message
      call exit(1)
      end
