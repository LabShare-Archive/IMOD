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
      integer*4 maxextra, maxtilts, maxpiece
      integer*4 NXYZ(3),MXYZ(3)
      real*4, allocatable :: tilt(:), val2(:), array(:)
      integer*4, allocatable :: ixpiece(:),iypiece(:),izpiece(:)
C       
      CHARACTER*320 FILIN,filout,metafile
      character*16 typeText(9) /'tilt angle', ' ', 'stage position',
     &    'magnification', 'intensity value', 'exposure dose', 'pixel spacing',
     &    'defocus', 'exposure time'/
C       
      integer*4 nz, ierr, iftilt, ifmag, ifstage, npiece, i, nbyte, iflags
      integer*4 maxz, iunout, lenText, mode, itype, ifc2, ntilt, nbsym
      integer*4 ntiltout, ifdose, ifwarn, ifall, ifexp, ifdef, ifpix, nfound
      integer*4 ifAddMdoc, indAdoc, iTypeAdoc, numSect, montage, ifMdoc
      integer*4 AdocOpenImageMetadata
      real*4 dmin, dmax, dmean
      EQUIVALENCE (Nz,NXYZ(3))
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetBoolean, PipGetInOutFile, PipGetString
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  extracttilts
c       
      integer numOptions
      parameter (numOptions = 15)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@output:OutputFile:FN:@'//
     &    'mdoc:MdocMetadataFile:B:@other:OtherMetadataFile:FN:@'//
     &    'tilts:TiltAngles:B:@stage:StagePositions:B:@'//
     &    'mag:Magnifications:B:@intensities:Intensities:B:@'//
     &    'exp:ExposureDose:B:@camera:CameraExposure:B:@'//
     &    'pixel:PixelSpacing:B:@defocus:Defocus:B:@'//
     &    'warn:WarnIfTiltsSuspicious:B:@all:AllPieces:B:@help:usage:B:'
C       
      filout = ' '
      ifmag = 0
      ifstage = 0
      ifC2 = 0
      iftilt = 0
      ifdose = 0
      ifexp = 0
      ifpix = 0
      ifdef = 0
      ifwarn = 0
      npiece = 0
      ifall = 0
      ifmdoc = 0
      metafile = ' '
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'extracttilts',
     &    'ERROR: EXTRACTTILTS - ', .true., 1, 1, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0

      if (PipGetInOutFile('InputFile', 1, 'Image input file', filin)
     &    .ne. 0) call exitError('NO INPUT IMAGE FILE SPECIFIED')
      ierr = PipGetInOutFile('OutputFile', 2,
     &    'Name of output file, or return to print out values', filout)
c       
      if (pipinput) then
        ierr = PipGetBoolean('AllPieces', ifall)
        ierr = PipGetBoolean('TiltAngles', iftilt)
        ierr = PipGetBoolean('Magnifications', ifmag)
        ierr = PipGetBoolean('StagePositions', ifstage)
        ierr = PipGetBoolean('Intensities', ifC2)
        ierr = PipGetBoolean('ExposureDose', ifdose)
        ierr = PipGetBoolean('CameraExposure', ifexp)
        ierr = PipGetBoolean('Defocus', ifdef)
        ierr = PipGetBoolean('PixelSpacing', ifpix)
        ierr = PipGetBoolean('MdocMetadataFile', ifmdoc)
        ierr = PipGetBoolean('WarnIfTiltsSuspicious', ifwarn)
        ifAddMdoc = PipGetString('OtherMetadataFile', metafile)
        ierr = iftilt + ifmag + ifstage + ifC2 + ifdose + ifdef + ifexp + ifpix
        if (ierr .eq. 0) iftilt = 1
        if (ierr .gt. 1) call exitError(
     &      'YOU MUST ENTER ONLY ONE OPTION FOR DATA TO EXTRACT')
        if (iftilt .ne. 0) itype = 1
        if (ifstage .ne. 0) itype = 3
        if (ifmag .ne. 0) itype = 4
        if (ifC2 .ne. 0) itype = 5
        if (ifdose .ne. 0) itype = 6
        if (ifpix .ne. 0) itype = 7
        if (ifdef .ne. 0) itype = 8
        if (ifexp .ne. 0) itype = 9
        if (itype .gt. 6 .and. ifmdoc .eq. 0 .and. metafile .eq. ' ')
     &      call exitError('THIS KIND OF INFORMATION CAN '//
     &      'BE OBTAINED ONLY FROM A METADATA FILE')
      else
        iftilt = 1
        itype = 1
      endif

      call PipDone()

      CALL IMOPEN(1,FILIN,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C       
      call irtnbsym(1,nbsym)
      maxextra = nbsym + 1024
      maxpiece = nz + 1024
      maxtilts = maxpiece
      allocate(tilt(maxtilts), val2(maxtilts), array(maxextra/4), 
     &    ixpiece(maxpiece),iypiece(maxpiece),izpiece(maxpiece), stat = ierr)

      call memoryError(ierr, 'ARRAYS FOR EXTRA HEADER DATA')

      call irtsym(1,nbsym,array)
      call irtsymtyp(1,nbyte,iflags)
c
c       Open the metafile and check for errors
      if (ifmdoc .ne. 0 .or. metafile .ne. ' ') then
        if (metafile .eq. ' ') metafile = filin
        indAdoc = AdocOpenImageMetadata(metafile, ifAddMdoc, montage,
     &      numSect, itypeAdoc)
        if (indAdoc .eq. -1) call exitError(
     &      'OPENING OR READING THE METADATA FILE')
        if (indAdoc .eq. -2) call exitError(
     &      'THE METADATA FILE DOES NOT EXIST')
        if (indAdoc .eq. -3) call exitError('THE AUTODOC FILE IS NOT '//
     &      'A RECOGNIZED FORM OF IMAGE METADATA FILE')
        if (numSect .ne. nz) call exitError('THE IMAGE AND METADATA FILES'//
     &      ' DO NOT HAVE THE SAME NUMBER OF SECTIONS')
      endif
c
c       Get piece coordinates unless doing "all"
      if (ifall .eq. 0) then
        if (ifmdoc .eq. 0 .and. metafile .eq. ' ') then
          call get_extra_header_pieces (array,nbsym,nbyte,iflags,
     &        nz,ixpiece,iypiece,izpiece,npiece,maxpiece)
        else
          if (montage .ne. 0) then
            call get_metadata_pieces(indAdoc, itypeAdoc, nz, ixpiece,iypiece,
     &          izpiece, maxpiece, npiece)
            if (npiece .lt. nz) call exitError('THE METADATA FILE DOES NOT '//
     &          'HAVE PIECE COORDINATES FOR EVERY SECTION')
          endif
        endif
      endif
      if(npiece.eq.0)then
        do i=1,nz
          izpiece(i)=i-1
        enddo
        maxz=nz
      else
        maxz=0
        do i=1,npiece
          maxz=max(maxz,izpiece(i)+1)
        enddo
      endif
c       
c       set up a marker value for empty slots
c       
      do i=1,maxz
        tilt(i)=-999.
      enddo
c       
      if (ifmdoc .eq. 0 .and. metafile .eq. ' ') then
        call get_extra_header_items (array,nbsym,nbyte,iflags,nz,itype,
     &      tilt,val2,ntilt,maxtilts,izpiece)
        if (ntilt.eq.0) then
          write(*,'(a,a,a)')'ERROR: EXTRACTTILTS - No ',trim(typeText(itype)),
     &        ' information in this image file'
          call exit(1)
        endif
      else
        call get_metadata_items(indAdoc, iTypeAdoc, nz, itype, tilt, val2,
     &      ntilt, nfound, maxtilts,izpiece)
        if (nfound .ne. nz) then
          write(*,'(a,a,a)')'ERROR: EXTRACTTILTS - ',trim(typeText(itype)),
     &        ' is missing for all or some sections in metadata file'
          call exit(1)
        endif
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
      if (ifdef .ne. 0 .or. ifpix .ne. 0 .or. ifexp .ne. 0)
     &    write(iunout,'(f11.3)')(tilt(i),i=1,ntiltout)

      if (iunout.eq. 1) then
        close(1)
        print *,ntiltout,' ',trim(typeText(itype)),'s output to file'
      endif

      if (iftilt .gt. 0 .and. ifwarn .gt. 0) then
        ntilt = 0
        do i = 1, ntiltout
          if (abs(tilt(i)) .lt. 0.1) ntilt = ntilt + 1
          if (abs(tilt(i)) .gt. 95.) ifmag = ifmag + 1
        enddo
        if (ntiltout .gt. 2 .and. ntilt .gt. ntiltout / 2)
     &      write(*,103) ntilt,'near zero'
103     format('WARNING: extracttilts - ',i4,
     &      ' of the extracted tilt angles are ',a)
        if (ifmag .gt. 0)
     &      write(*,103) ifmag,'greater than 95 degrees'
      endif

      CALL IMCLOSE(1)
c       
      call exit(0)
      END

c       $Log$
c       Revision 3.11  2009/10/14 23:50:41  mast
c       allocate arrays for extra header data
c
c       Revision 3.10  2009/05/08 02:19:10  mast
c       Add option to get items at all frames
c
c       Revision 3.9  2007/12/27 20:57:15  mast
c       Added option to test for bad tilt angles and give warning
c
c       Revision 3.8  2007/07/15 21:21:47  mast
c       Made usage of maxz consistent between montage and single frame
c
c       Revision 3.7  2007/05/19 00:04:12  mast
c       Added exposure dose
c
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
