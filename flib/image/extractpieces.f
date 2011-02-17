*************EXTRACTPIECES.FOR**********************************************
c       
c       EXTRACTPIECES will extract piece coordinates from the header of
c       an image file, if they are present, and produce a file with those
c       coordinates (a piece list file).
c       
c       The Unix version will take the file names either on the command line
c       or as entries to the program.  If there are no file names on the
c       command line, the program asks for both the input and output file
c       names.  If there is one file name on the command line, the program
c       takes that as the input file and asks for the output file name.
c       
c       Entries to program:
c       
c       Image file
c       Output file for piece coordinates
c       
c       David Mastronarde, 1/2/00
c       
      implicit none
      integer*4 NXYZ(3),MXYZ(3), nz, maxpiece, maxextra, nbsym,nbyte,iflags
      real*4, allocatable :: array(:)
      integer*4, allocatable :: ixpiece(:),iypiece(:),izpiece(:)
      integer*4 MODE,npiece,i, ierr, ifAddMdoc, indAdoc, iTypeAdoc, numSect
      integer*4 montage
      real*4 DMIN,DMAX,DMEAN
      logical useMdoc
C       
      CHARACTER*320 FILIN,filout, metafile
C       
      EQUIVALENCE (Nz,NXYZ(3))

      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetString, AdocOpenImageMetadata
      integer*4 PipGetInOutFile, PipParseInput, PipGetBoolean,PipGetLogical
      integer numOptions
      parameter (numOptions = 5)
      character*(120 * numOptions) options(1) 
      options(1) = 
     &    'input:InputFile:FN:Name of input image file@'//
     &    'output:OutputFile:FN:Name of output piece list file@'//
     &    'mdoc:MdocMetadataFile:B:Take coordinates from metadata file named'//
     &    ' inputfile.mdoc)@'//
     &    'other:OtherMetadataFile:FN:Name of other metadata file to take '//
     &    'coordinates from@'//
     &    'help:usage:B:Print help output'

      metafile = ' '
      filin = ' '
      useMdoc = .false.
      nz = 0
c
      call PipExitOnError(0, 'ERROR: EXTRACTPIECES - ')
      call PipAllowCommaDefaults(1)
      ierr = PipParseInput(options, numOptions, '@', numOptArg, numNonOptArg)
      if (PipGetBoolean('help', ierr) .eq. 0) then
        call PipPrintHelp('extractpieces', 0, 1, 1)
        call exit(0)
      endif
      ifAddMdoc = PipGetString('OtherMetadataFile', metafile)
      ierr = PipGetLogical('MdocMetadataFile', useMdoc)
      if (useMdoc .and. ifAddMdoc .eq. 0) call exitError(
     &    'YOU CANNOT ENTER BOTH -usemdoc AND -metafile')
      if (PipGetInOutFile('InputFile', 1, 'Name of input image file', filin)
     &    .ne. 0 .and. ifAddMdoc .ne. 0)
     &    call exitError('NO INPUT FILE SPECIFIED')
      if (PipGetInOutFile('OutputFile', 2, 'Name of output piece list file',
     &    filout) .ne. 0) call exitError('NO OUTPUT FILE SPECIFIED')
      call PipDone()
c       
      if (filin .ne. ' ') then
        CALL IMOPEN(1,FILIN,'RO')
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
      endif
C         
      if (.not.useMdoc .and. ifAddMdoc .ne. 0) then
c
c         Get data from the image header
        call irtnbsym(1,nbsym)
        call irtsymtyp(1,nbyte,iflags)
        maxextra = nbsym + 1024
        maxpiece = nz + 1024
        allocate(array(maxextra/4), ixpiece(maxpiece), iypiece(maxpiece),
     &      izpiece(maxpiece), stat = ierr)
        call memoryError(ierr, 'ARRAYS FOR EXTRA HEADER OR PIECE DATA')
        call irtsym(1,nbsym,array)
        call get_extra_header_pieces (array,nbsym,nbyte,iflags,nz,
     &      ixpiece,iypiece,izpiece,npiece,maxpiece)
        if (npiece .eq. 0)
     &      print *,'There are no piece coordinates in this image file'
      else
c         
c         Or get data from the autodoc file
        if (metafile .eq. ' ') metafile = filin
        indAdoc = AdocOpenImageMetadata(metafile, ifAddMdoc, montage, numSect,
     &      itypeAdoc)
        if (indAdoc .gt. 0 .and. filin .eq. ' ') nz = numSect
        npiece = 0
c         
c         Thanks to a bug in SerialEM, Montage flag may be missing.  So just plow
c         ahead regardless
        if (indAdoc .gt. 0 .and. numSect .eq. nz) then
          maxpiece = nz + 1024
          allocate(ixpiece(maxpiece), iypiece(maxpiece), izpiece(maxpiece),
     &        stat = ierr)
          call memoryError(ierr, 'ARRAYS FOR PIECE DATA')
          call get_metadata_pieces(indAdoc, itypeAdoc, nz, ixpiece,iypiece,
     &        izpiece, maxpiece, npiece)
        endif
c         
c         Give lots of different error messages
        if (indAdoc .lt. 0 .or. npiece .lt. nz) then
          if (indAdoc .eq. -2) then
            print *,'The metadata file does not exist'
          else if (indAdoc .eq. -3) then
            print *,'The autodoc file is not a recognized type of image'//
     &          ' metadata file'
          else if (indAdoc .eq. -1) then
            print *,'There was an error opening or reading the metadata file'
          else if (npiece .gt. 0 .or. numSect .ne. nz)
     &          then
            print *,'The metadata file does not have piece coordinates for'//
     &          ' every image in the file'
          else
            print *,'There are no piece coordinates in the metadata file'
          endif
        endif
      endif
      if (npiece .eq. nz) then
        call dopen(1,filout,'new','f')
        write(1,'(2i7,i5)')(ixpiece(i),iypiece(i),izpiece(i),i=1,npiece)
        close(1)
        print *,npiece,' piece coordinates output to file'
      endif

      CALL IMCLOSE(1)
c       
      call exit(0)
      END

