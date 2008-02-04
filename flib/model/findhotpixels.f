c       FINDHOTPIXELS
c       
c         Findhotpixels will analyze a set of CCD camera dark reference images
c       to identify relatively hot pixels in the CCD chip and will output a
c       list of the pixels in a format suitable for placing in the property
c       file for SerialEM.  See man page and SerialEM help manual for more
c       details.
c       
c       David Mastronarde, 2003.
c       
c       $Id$
c       
c       $Log$
c       Revision 3.2  2006/10/18 14:41:05  mast
c       Fixed option line for all those other compilers
c
c       Revision 3.1  2006/10/17 22:57:31  mast
c       Added to package
c
c
      implicit none
      integer idim,limsec
      parameter (idim=20480,limsec=10)
      integer*4 NX,NY,NZ,NXYZ(3),MXYZ(3),NXYZST(3)
      COMMON //NX,NY,NZ
      include 'smallmodel.inc'
C       
      real*4 ARRAY(idim,limsec),TITLE(20)
C       
      CHARACTER*120 FILIN,FILOUT
      character*512 colstring
C       
      integer*4 listcols(5000)
      EQUIVALENCE (NX,NXYZ)
      integer*4 nhotcols, ierr, nzdo, mode, ix, iy, iz, ifonlist, j, nsum, i
      real*4 thresh, dmin,dmax,dmean, sum, avg
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetString,PipGetFloat
      integer*4 PipGetInOutFile, PipParseInput, PipGetBoolean
      integer numOptions
      parameter (numOptions = 5)
      character*(120 * numOptions) options(1) 
      options(1) = 
     &    'input:InputFile:FN:Name of input image file@'//
     &    'output:OutputFile:FN:Name of output model file@'//
     &    'columns:HotColumns:LI:List of hot columns to exclude '//
     &    '(numbered from 0)@threshold:ThresholdDifference:F:'//
     &    'Threshold difference from mean value for hot pixels@'//
     &    'help:usage:B:Print help output'
c       
      nhotcols = 0
      call PipExitOnError(0, 'ERROR: FINDHOTPIXELS - ')
      call PipAllowCommaDefaults(1)
      ierr = PipParseInput(options, numOptions, '@', numOptArg, numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0
      if ((pipinput .and. numOptArg + numNonOptArg .lt. 3)  .or.
     &    PipGetBoolean('help', j) .eq. 0) then
        call PipPrintHelp('findhotpixels', 0, 1, 1)
        call exit(0)
      endif
c       
      if (PipGetInOutFile('InputFile', 1, 'Name of input image file', filin)
     &    .ne. 0) call exitError('NO INPUT FILE SPECIFIED')
      if (PipGetInOutFile('OutputFile', 2, 'Name of output model file', filout)
     &    .ne. 0) call exitError('NO OUTPUT FILE SPECIFIED')
      if (pipinput) then
        if (PipGetString('HotColumns', colstring) .eq. 0)
     &      call parselist(colstring, listcols, nhotcols)
        if (PipGetFloat('ThresholdDifference', thresh) .ne. 0)
     &      call exitError('YOU MUST ENTER A THRESHOLD DIFFERENCE FROM MEAN')
      else
        print *,'Enter list of hot columns to exclude (numbered from',
     &      ' 0), or Return if none'
        call rdlist(5, listcols, nhotcols)
c         
        write(*,'(1x,a,$)')'Threshold difference from mean: '
        read(5,*)thresh
      endif
C       
      call imopen(1,filin,'ro')
      call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
      nzdo = min(nz,limsec)

      print *
      thresh = thresh + dmean
      max_mod_obj = 1
      ibase_obj(1) = 0
      obj_color(1,1)=1
      obj_color(2,1)=255
      n_point = 0
      do iy = 1, ny
        do iz=1,nzdo
          call imposn(1, iz - 1, iy - 1)
          call irdlin(1, array(1,iz), *99)
        enddo
        
        do ix = 1, nx

          ifonlist = 0
          do j = 1, nhotcols
            if (ix-1 .eq. listcols(j)) ifonlist = 1
          enddo

          if (ifonlist .eq. 0) then
            sum = 0.
            nsum = 0
            do iz = 1, nzdo
              if (array(ix,iz).gt.thresh)then
                nsum=nsum+1
                sum=sum+array(ix,iz)-dmean
              endif
            enddo
            if (nsum.eq.nzdo)then
              avg=sum/nsum  
              n_point=n_point+1
              if (n_point .gt. max_pt) call exitError(
     &            'TOO MANY POINTS ABOVE THRESHOLD; RAISE THE THRESHOLD')
              p_coord(1,n_point) = ix - 0.55
              p_coord(2,n_point) = iy - 0.55
              p_coord(3,n_point) = nzdo / 2
              object(n_point)=n_point
              write(*,'(2i6,f7.0)') ix,iy,avg
            endif
          endif
        enddo
      enddo

      npt_in_obj(1) = n_point
      call putimodflag(1, 2)
      call putsymtype(1, 0)
      call putsymsize(1, 5)
      call write_wmod(filout)

      print *
      PRINT *,'Total pixels: ',n_point
      print *
      do i = 1,n_point
        if (mod(i,5) .eq. 1) write(*,'(a,$)') 'HotPixels '
        write(*,'(2i6,$)') nint(p_coord(1,i) + 0.5),nint(p_coord(2,i) + 0.5)
        if (mod(i,5).eq.0) print *
      enddo
      print *
      print *
      do i = 1, nhotcols
        if (mod(i,13) .eq. 1) write(*,'(a,$)') 'HotColumns '
        write(*,'(i5,$)') listcols(i)
        if (mod(i,13).eq.0) print *
      enddo
      print *
c
      call imclose(1)
      call exit(0)
99    call exitError('READING FILE')
      end


