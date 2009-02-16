*       PROGRAM TO ALTER HEADER INFORMATION WITHOUT REWRITING FILE
c       For each header entry selected by the user, the program prints the
c       current values and asks for new values, checking for legality in some
c       cases.  See man page for details.
c       
c       David Mastronarde 10/24/88
c       
c       $Id$
c       Log at end of file
C       
      implicit none
      integer nfunc,idim,nx,ny,nz,maxextra
      parameter (nfunc=20)
      parameter (idim=2100, maxextra = idim*idim * 4)
      integer*4 NXYZ(3),MXYZ(3),NXYZST(3),mcrs(3),listdel(1000)
      real*4 delt(3),tilt(3),
     &    TITLE(20,10),cell(6),array(idim*idim)
      equivalence (nxyz(1),nx),(nxyz(2),ny),(nxyz(3),nz)
      common /bigimg/ array
C       
      CHARACTER*320 FILIN
      character*20 funcin,funcup
      character*9 param(nfunc)
      data param/'ORG','CEL','DAT','DEL','MAP','SAM','TLT'
     &    ,'TLT_ORIG','TLT_ROT','LAB','MMM','RMS','FIXPIXEL',
     &    'FIXEXTRA','FIXMODE','SETMMM','FEIPIXEL','INVERTORG','HELP','DONE'/
C       
C       
      DATA NXYZST/0,0,0/
      integer*4 mode,iwhich,i,itype,lens,n1,n2,n3,ntitle,ndel
      integer*4 newtitle,iold,ifdel,id,j,nbytex,iflag,ifok,iz,nbsym,nint,nreal
      real*4 dmin,dmax,dmean,origx,origy,origz,v1,v2
      real*4 dmins,dmaxs,sd,rms,pixel
      real*8 dmeans,sums,sumsqs,totn,tsum,sumsq
      integer*4 maxLines,numChunks, iChunk,numLines
      logical*4 nbytes_and_flags
C       
C       Read in input file
C       
      call getinout(1,filin,filin)
C       
      call ialbrief(0)
      CALL IMOPEN(2,FILIN,'OLD')
      call irdhdr(2,nxyz,mxyz,mode,dmin,dmax,dmean)
C       
      print *,'If you make a mistake, interrupt with Ctrl-C instead'
     &    ,' of exiting with DONE'
30    write(*,102)
102   format(1x,'Options: org, cel, dat, del, map, sam, tlt, ',
     &    'tlt_orig, tlt_rot, lab, mmm,',/, ' rms, fixpixel, feipixel, ',
     &    'fixextra, fixmode, invertorg, setmmm, help, OR done')
      write(*,'(1x,a,$)')'Enter option: '
      read(5,101)funcin
101   FORMAT(A)
      call strupcase(funcup,funcin)
      iwhich=0
      do i=1,nfunc
        if(funcup.eq.param(i))iwhich=i
      enddo
      go to(1,2,3,4,5,6,7,8,9,10,11,16,12,13,17,18,19,20,14,15),iwhich
      print *,'Not a legal entry, try again'
      go to 30
C       
c       ORIGIN
c       
1     call irtorg(2,origx,origy,origz)
      write(*,111)origx,origy,origz
111   format(' Alter origin.  The origin is the offset FROM the',
     &    ' first point in the image',/,' file TO the center of',
     &    ' the coordinate system, expressed in true coordinates.',
     &    /,' Current x, y, z:',3g11.4,/,'New x, y, z: ',$)
      read(5,*)origx,origy,origz
      call ialorg(2,origx,origy,origz)
      go to 30
c       
c       CELL
c       
2     call irtcel(2,cell)
      write(*,112)(cell(i),i=1,6)
112   format(' Alter cell.  Current size and angles:',/,3g11.4,3f9.3,
     &    /,'New size and angles: ',$)
      read(5,*)(cell(i),i=1,6)
      if(cell(1).gt.0.and.cell(2).gt.0.and.cell(3).gt.0)then
        call ialcel(2,cell)
      else
        print *,'No good, cell(1-3) must be positive'
      endif
      go to 30
c       
c       data type etc
c       
3     call irtdat(2,itype,lens,n1,n2,v1,v2)
      write(*,113)itype,lens,n1,n2,v1,v2
113   format(' Alter data type.  Current type, lens, n1, n2, v1, v2:'
     &    ,/,4i5,2f10.3,/,' Enter new type (0 regular serial sections'
     &    ', 1 tilt series, 2 serial stereo',/,' pairs, 3 averaged',
     &    ' serial sections, 4 averaged serial stereo pairs): ',$)
      read(5,*)itype
      if(itype.lt.0.or.itype.gt.4)then
        print *,'No good, type must be 0-4'
        go to 30
      endif
      if(itype.eq.0)then
        print *,'You do not need to change any of the other parameters'
      elseif(itype.eq.1)then
        n2=0
        write(*,'(1x,a,$)')
     &      '1, 2, or 3 if the tilt is around the X, Y or Z axis: '
        read(5,*)n1
        if(n1.le.0.or.n1.gt.3)then
          print *,'Value no good'
          go to 30
        endif
        write(*,'(1x,a,$)')'Increment in tilt angle between views,'
     &      //' tilt angle of first view: '
        read(5,*)v1,v2
      endif
      if(itype.ge.3)then
        write(*,'(1x,a,$)')
     &      'Number of original sections averaged into one section: '
        read(5,*)n1
        write(*,'(1x,a,a,/,a,$)')'Enter the spacing between the',
     &      ' original section numbers contributing',
     &      '    to successive sections in this file: '
        read(5,*)n2
      endif
      if(itype.eq.2.or.itype.eq.4)then
        write(*,'(1x,a,$)')'Tilt angles of left and right eye views: '
        read(5,*)v1,v2
      endif
      write(*,213)itype,lens,n1,n2,v1,v2
213   format(' Proposed new type, lens, n1, n2, v1, v2:',/,4i5,2f10.3,
     &    /,' Enter / to accept, or a new type,',
     &    ' lens, n1, n2, v1, v2: ',$)
      read(5,*)itype,lens,n1,n2,v1,v2
      if(itype.ge.0.and.itype.le.4)then
        call ialdat(2,itype,lens,n1,n2,v1,v2)
      else
        print *,'No good, type must be 0-4'
      endif
      go to 30
c       
c       DELTA
c       
4     call irtdel(2,delt)
      call irtsam(2,mxyz)
      call irtcel(2,cell)
c       
c       DNM 12/25/00: this used to call ialdel, which set mxyz to 1,1,1
c       and cell size to the desired deltas; decided to change it to
c       preserve mxyz instead
c       
      write(*,114)(delt(i),i=1,3)
114   format(' Alter delta - changes cell sizes to achieve desired',
     &    ' pixel spacing',/,' Current delta x, y, z:',3G11.4,
     &    /,'New delta x, y, z: ',$)
      read(5,*)(delt(i),i=1,3)
      if(delt(1).gt.0.and.delt(2).gt.0.and.delt(3).gt.0)then
c         call ialdel(2,delt)
        do i=1,3
          cell(i)=mxyz(i)*delt(i)
        enddo
        call ialcel(2,cell)
      else
        print *,'No good, must be positive'
      endif
      go to 30
c       
c       MAPPING
c       
5     call irtmap(2,mcrs)
      write(*,115)(mcrs(i),i=1,3)
115   format(' Alter mapping.  Current mapping constants:',3i3,/
     &    ,'New constants: ',$)
      read(5,*)(mcrs(i),i=1,3)
      n1=0
      n2=0
      n3=0
      do i=1,3
        if(mcrs(i).eq.1)n1=n1+1
        if(mcrs(i).eq.2)n2=n2+1
        if(mcrs(i).eq.3)n3=n3+1
      enddo
      if(n1.eq.1.and.n2.eq.1.and.n3.eq.1)then
        call ialmap(2,mcrs)
      else
        print *,'No good, must be permutation of 1, 2, 3'
      endif
      go to 30
c       
c       SAMPLING
c       
6     call irtsam(2,mxyz)
      write(*,116)(mxyz(i),i=1,3)
116   format(' Alter sampling (mxyz).  Current x, y, z:',3i5,/
     &    ,'New x, y, z: ',$)
      read(5,*)(mxyz(i),i=1,3)
      if(mxyz(1).gt.0.and.mxyz(2).gt.0.and.mxyz(3).gt.0)then
        call ialsam(2,mxyz)
      else
        print *,'No good, must be positive'
      endif
      go to 30
c       
c       TILT - current angles
c       
7     call irttlt(2,tilt)
      write(*,117)(tilt(i),i=1,3)
117   format(' Alter current tilt angles.  Current angles:',6f6.1,
     &    /,'New current angles: ',$)
      read(5,*)(tilt(i),i=1,3)
      call ialtlt(2,tilt)
      go to 30
c       
c       TILT_ORIG - original angles
c       
8     call irttlt_orig(2,tilt)
      write(*,118)(tilt(i),i=1,3)
118   format(' Alter original tilt angles.  Current angles:',6f6.1,
     &    /,'New original angles: ',$)
      read(5,*)(tilt(i),i=1,3)
      call ialtlt_orig(2,tilt)
      go to 30
c       
c       TILT_ROT - rotate current angles
c       
9     write(*,119)
119   format(' Rotate current tilt angles.',
     &    /,'Angles to rotate by: ',$)
      read(5,*)(tilt(i),i=1,3)
      call ialtlt_rot(2,tilt)
      go to 30
c       
c       LAB - delete selected labels
c       
10    call irtlab(2,title,ntitle)
      write(*,'(a,/)')' Delete labels.  Current labels are:'
      write(*,'(i3,1x,19a4)')(i,(title(j,i),j=1,19),i=1,ntitle)
      write(*,'(/,a)')
     &    ' Enter numbers of labels to delete (ranges ok)'
      call rdlist(5,listdel,ndel)
      newtitle=0
      do iold=1,ntitle
        ifdel=0
        do id=1,ndel
          if(iold.eq.listdel(id))ifdel=1
        enddo
        if(ifdel.eq.0)then
          newtitle=newtitle+1
          do j=1,20
            title(j,newtitle)=title(j,iold)
          enddo
        endif
      enddo
      newtitle=max(1,newtitle)
      write(*,'(a,/)')' New label list would be:'
      write(*,'(i3,1x,19a4)')(i,(title(j,i),j=1,19),i=1,newtitle)
      write(*,'(/,1x,a,$)')
     &    '1 to confirm changing to this label list, 0 not to: '
      read(5,*)ifok
      if(ifok.ne.0)call iallab(2,title,newtitle)
      go to 30
c       
c       MMM - recompute min/max/mean
c       
11    maxLines = idim**2 / nx
      numChunks = (ny + maxLines - 1) / maxLines
      write(*,121)
121   format(' Recomputing min/max/mean of images - takes a while...')
      call imposn(2,0,0)
      dmin=1.e10
      dmax=-1.e10
      tsum=0.
      sumsq=0.
      totn = 0.
      do iz=1,nz
        do iChunk = 1, numChunks
          numLines = min(maxLines, ny - (iChunk - 1) * maxLines)
          call irdsecl(2,array, numLines, *99) 
          call iclavgsd(array,nx,numLines,1,nx,1,numLines,dmins,dmaxs,sums,
     &        sumsqs, dmean,sd)
          dmin=min(dmin,dmins)
          dmax=max(dmax,dmaxs)
          tsum=tsum+sums
          sumsq=sumsq+sumsqs
          totn = totn + nx * numLines
        enddo
      enddo
      dmeans=tsum/totn
      rms=sqrt((sumsq - totn * dmeans**2) / totn)
      dmean = dmeans
      call ialrms(2,rms)
      if(iwhich.eq.12)write(*,162)rms
162   format(' New RMS value = ', g13.5)
      go to 30
c       
c       RMS - first inform of current RMS value
c       
16    call irtrms(2,rms)
      write(*,161)rms
161   format(' Current RMS value = ', g13.5)
      go to 11
c       
12    write(*,122)
122   format(' Changing sample and cell sizes to match image size, '
     &    ,/,' which will make pixel spacing be 1.0 1.0 1.0.')
      call irtcel(2,cell)
      cell(1)=nx
      cell(2)=ny
      cell(3)=nz
      call ialcel(2,cell)
      call ialsam(2,nxyz)
      go to 30
c       
c       FIXPIECES - Remove flag for piece coordinates from header
c       
13    write(*,123)
123   format(' Marking header as not containing any ',
     &    'piece coordinates.')
      call irtsymtyp(2,nbytex,iflag)
      if (mod(iflag/2,2).gt.0)iflag = iflag-2
      call ialsymtyp(2,nbytex,iflag)
      go to 30
c       
c       FIXMODE - change between 1 and 6  
c       
17    if (mode .ne. 6 .and. mode .ne. 1) then
        print *,'Only mode 6 can be changed to mode 1, or 1 to 6'
        go to 30
      endif
c       
      mode = 7 - mode
      write(*,1124)mode
1124  format(/,'Changing mode to',i2)
      call ialmod(2,mode)
      if (dmax .gt. 32767 .and. mode .eq. 1) write(*,124) dmax
124   format(/,'The file maximum is', f12.1, ' and numbers bigger than',
     &    ' 32767 will not be',/,
     &    ' represented correctly in this mode.')
      if (dmin .lt. 0 .and. mode .eq. 6) write(*,2124) dmin
2124  format(/,'The file minimum is', f12.1, ' and negative numbers',
     &    ' will not be',/, ' represented correctly in this mode.')
      go to 30
c       
c       SETMMM - set the min,max,mean
18    write(*,218)dmin, dmax, dmean
218   format(' Alter min/max/mean.  Current values:',6g15.5,
     &    /,'New min, max, mean: ',$)
      read(5,*)dmin, dmax, dmean
      go to 30
c       
c       FEIPIXEL - use the pixel size in extra header to set pixel spacing
19    call irtnbsym(1,nbsym)
      if (nbsym .le. 0) then
        print *,'No extended header information in this file'
        go to 30
      endif
      if (nbsym.gt.maxextra) then
        print *,'Extended header data too large for array'
        go to 30
      endif
      call irtsym(1,nbsym,array)
      call irtsymtyp(1,nint,nreal)
      if (nbytes_and_flags(nint, nreal)) then
        print *,'The extended header is not in Agard/FEI format'
        go to 30
      endif
      if (nreal .lt. 12) then
        print *,'There is no pixel size in this extended header (too few'//
     &      ' values per section)'
        go to 30
      endif
      pixel = array(nint + 12) * 1.e10
      if (pixel .le. 0) then
        print *,'Pixel size in extended header is not a usable value:', pixel
        go to 30
      endif
      write(*,219)pixel
219   format('Pixel size in extended header is',g11.4,' Angstroms',/, 'Enter',
     &    ' 1 to set the pixel spacing to this value, 0 not to: ',$)
      read(5,*)ifok
      if (ifok .eq. 0) go to 30
      call irtsam(2,mxyz)
      call irtcel(2,cell)
      do i=1,3
        cell(i)=mxyz(i)*delt(i)
      enddo
      call ialcel(2,cell)
      go to 30
c       
c       INVERTORG  - invert the sign of the origin
20    call ialorg(2, origx, origy, origz)
      origx = -origx
      origy = -origy
      origz = -origz
      write(*,120)origx, origy, origz
120   format('Inverting sign of origin: new origin = ',3g13.6)
      call ialorg(2, origx, origy, origz)
      go to 30
c
14    write(*,201)
201   format(/,' org = change x,y,z origin',
     &    /,' cel = change cell size',
     &    /,' dat = change data type (for tilt series, stereo',
     &    ' pairs, or averaged sections)',
     &    /,' del = change delta (pixel size) directly',
     &    /,' map = change x,y,z mapping to rows, columns, sections',
     &    /,' sam = change mxyz sampling',
     &    /,' tlt = change alpha, beta, gamma current tilt angles',
     &    /,' tlt_orig = change original tilt angles',
     &    /,' tlt_rot = rotate current tilt angles',
     &    /,' lab = delete selected labels',
     &    /,' mmm = fix min/max/mean and set RMS value by ',
     &    'reading all images'
     &    /,' rms = set RMS value, does same actions as mmm',
     &    /,' fixpixel = fix pixel spacing by setting cell and',
     &    ' sample sizes to image size',
     &    /,' feipixel = set pixel spacing from pixel size in ',
     &    'Agard/FEI extended header',
     &    /,'            sample sizes to image size',
     &    /,' fixextra = fix extra header so that file',
     &    ' does not look like a montage'
     &    /,' fixmode = change mode from 6 to 1 (unsigned to signed ',
     &    'integer) or 1 to 6',
     &    /,' setmmm = set min, max, mean to entered values',
     &    /,' help = type this again',
     &    /,' done = exit',/)
      go to 30
c       
15    CALL IWRHDR(2,TITLE,-1,dmin,dmax,dmean)
      call irdhdr(2,nxyz,mxyz,mode,dmin,dmax,dmean)
      CALL IMCLOSE(2)
      call exit(0)
99    print *
      print *,'ERROR: ALTERHEADER - reading file'
      call exit(1)
      END
c       
c       $Log$
c       Revision 3.7  2006/09/28 21:44:30  mast
c       Turn off brief header, read data in chunks for mmm/rms
c       
c       Revision 3.6  2006/03/25 06:10:04  mast
c       Added option to set min/max/mean
c       
c       Revision 3.5  2005/11/11 22:53:01  mast
c       Added switch between 1 and 6 modes (not really needed now)
c       
c       Revision 3.4  2005/05/26 04:35:58  mast
c       Used double precision to get rms correct
c       
c       Revision 3.3  2002/08/18 23:13:28  mast
c       Changed to cal iclavgsd in library
c       
c       Revision 3.2  2002/08/17 05:45:50  mast
c       Moved big array to common to avoid stack size problem on SGI
c       
c       Revision 3.1  2002/08/17 05:37:29  mast
c       Made mmm compute rms, and added rms option to do same thing.
c       Also made declarations for implicit none, standardized error exit
c       
