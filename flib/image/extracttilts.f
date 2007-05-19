*************EXTRACTTILTS.F**********************************************
c       
c       EXTRACTTILTS will extract tilt angles or other per-section 
c       information from the header of an image file, if they are present,
c       and produce a file with a list of the values.
c       
c       See man page for more details
c       
c       David Mastronarde, 1/2/00
c       
c       $Id$
c       Log at end of file
c       
      implicit none
      integer maxextra, maxtilts, maxpiece
      parameter (maxextra = 4000000, maxtilts = 4000, maxpiece=100000)
      integer*4 NXYZ(3),MXYZ(3)
      real*4 tilt(maxtilts), val2(maxtilts)
      real*4 array(maxextra/4)
      integer*4 ixpiece(maxpiece),iypiece(maxpiece),izpiece(maxpiece)
C       
      CHARACTER*160 FILIN,filout
      character*16 typeText(6) /'tilt angle', ' ', 'stage position',
     &    'magnification', 'intensity value', 'exposure dose'/
C       
      integer*4 nz, ierr, iftilt, ifmag, ifstage, npiece, i, nbyte, iflags
      integer*4 maxz, iunout, lenText, mode, itype, ifc2, ntilt, nbsym
      integer*4 ntiltout, lnblnk, ifdose
      real*4 dmin, dmax, dmean
      EQUIVALENCE (Nz,NXYZ(3))
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetBoolean, PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  extracttilts
c       
      integer numOptions
      parameter (numOptions = 8)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@output:OutputFile:FN:@tilts:TiltAngles:B:@'//
     &    'stage:StagePositions:B:@mag:Magnifications:B:@'//
     &    'intensities:Intensities:B:@exp:ExposureDose:B:@help:usage:B:'
C       
      filout = ' '
      ifmag = 0
      ifstage = 0
      ifC2 = 0
      iftilt = 0
      ifdose = 0
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'extracttilts',
     &    'ERROR: EXTRACTTILTS - ', .true., 1, 1, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0

      if (PipGetInOutFile('InputFile', 1, 'Image input file', filin)
     &    .ne. 0) call exitError('NO INPUT FILE SPECIFIED')
      ierr = PipGetInOutFile('OutputFile', 2,
     &    'Name of output file, or return to print out values', filout)
c       
      if (pipinput) then
        ierr = PipGetBoolean('TiltAngles', iftilt)
        ierr = PipGetBoolean('Magnifications', ifmag)
        ierr = PipGetBoolean('StagePositions', ifstage)
        ierr = PipGetBoolean('Intensities', ifC2)
        ierr = PipGetBoolean('ExposureDose', ifdose)
        if (iftilt + ifmag + ifstage + ifC2 + ifdose .eq. 0) iftilt = 1
        if (iftilt + ifmag + ifstage + ifC2 + ifdose .ne. 1) call exitError(
     &      'YOU MUST ENTER ONLY ONE OPTION FOR DATA TO EXTRACT')
        if (iftilt .ne. 0) itype = 1
        if (ifstage .ne. 0) itype = 3
        if (ifmag .ne. 0) itype = 4
        if (ifC2 .ne. 0) itype = 5
        if (ifdose .ne. 0) itype = 6
      else
        iftilt = 1
        itype = 1
      endif

      call PipDone()

      CALL IMOPEN(1,FILIN,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C       
      call irtnbsym(1,nbsym)
      if(nbsym.gt.maxextra) call exitError(
     &    'ARRAYS NOT LARGE ENOUGH FOR EXTRA HEADER DATA')

      call irtsym(1,nbsym,array)
      call irtsymtyp(1,nbyte,iflags)
      call get_extra_header_pieces (array,nbsym,nbyte,iflags,nz,
     &    ixpiece,iypiece,izpiece,npiece,maxpiece)
      if(npiece.eq.0)then
        do i=1,nz
          izpiece(i)=i-1
        enddo
        maxz=nz
      else
        maxz=0
        do i=1,npiece
          maxz=max(maxz,izpiece(i))
        enddo
      endif
c       
c       set up a marker value for empty slots
c       
      do i=1,maxz
        tilt(i)=-999.
      enddo
c       
      lenText = lnblnk(typeText(itype))
      call get_extra_header_items (array,nbsym,nbyte,iflags,nz,itype,
     &    tilt,val2,ntilt,maxtilts,izpiece)
      if (ntilt.eq.0) then
        print *
        print *,'ERROR: EXTRACTTILTS - No ',typeText(itype)(1:lenText),
     &      ' information in this image file'
        call exit(1)
      endif
c       
c       pack the tilts down
c       
      ntiltout=0
      do i=1,ntilt
        if(tilt(i).ne.-999.)then
          ntiltout=ntiltout+1
          tilt(ntiltout)=tilt(i)
          val2(ntiltout)=val2(i)
        endif
      enddo
c       
      iunout = 6
      if (filout .ne. ' ') then
        call dopen(1,filout,'new','f')
        iunout = 1
      endif
      
      if (iftilt .ne. 0) write(iunout,'(f7.2)')(tilt(i),i=1,ntiltout)
      if (ifmag .ne. 0) write(iunout,'(i7)')(nint(tilt(i)),i=1,ntiltout)
      if (ifC2 .ne. 0) write(iunout,'(f8.5)')(tilt(i),i=1,ntiltout)
      if (ifstage .ne. 0)  write(iunout,'(2f9.2)')
     &    (tilt(i), val2(i), i=1,ntiltout)
      if (ifdose .ne. 0) write(iunout,'(f13.5)')(tilt(i),i=1,ntiltout)

      if (iunout.eq. 1) then
        close(1)
        print *,ntiltout,' ',typeText(itype)(1:lenText),'s output to file'
      endif

      CALL IMCLOSE(1)
c       
      call exit(0)
      END

c       $Log$
c       Revision 3.6  2006/07/08 13:44:57  mast
c       Raised limites, switchet to exitError
c
c       Revision 3.5  2006/02/24 22:11:15  mast
c       Exit with ERROR if no items found in header
c
c       Revision 3.4  2004/04/02 00:50:25  mast
c       Had to initialize iftilt
c	
c       Revision 3.3  2004/03/19 04:34:19  mast
c       had to declare lnblnk
c	
c       Revision 3.2  2004/03/18 16:37:44  mast
c       Converted to PIP and added options for extracting other info
c	
c       Revision 3.1  2003/06/05 00:11:14  mast
c       Change STOP to standardized ERROR exit
c	
