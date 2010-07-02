c       PipParseInput provides a simple front end to the "pip" package
c       for parsing input parameters
c       
c       "options" is an array of character strings, one for each option,
c       listing the short name, long name, type code, and help string,
c       separated by commas.  In this case "separator" should be a space.
c       Alternatively, options can be an array consisting of one long
c       character string, with each option separated from the next by
c       the character in "separator"
c       "numOptions" is the number of options in the array or string
c       The routine returns numOptArg, the number of option arguments
c       on the command line; and numNonOptArg, the number of non-option
c       arguments.
c       
c       $Id$
c       Log at end
c       

      integer*4 function PipParseInput(options, numOptions, separator,
     &    numOptArg, numNonOptArg)
      implicit none
      character*(*) options(*)
      character separator
      integer*4 numOptArg, numOptions, numNonOptArg
      integer*4 PipInitialize, PipAddOption, PipParseEntries
      integer*4 i, lnblnk, j, indStr, indEnd, lenAll
c       
c       initialize then pass the options one by one
c       
      PipParseInput = PipInitialize(numOptions)
      if (PipParseInput .ne. 0) return
      if (separator .eq. ' ') then
        do i = 1, numOptions
          PipParseInput = PipAddOption(options(i))
          if (PipParseInput .ne. 0) return
        enddo
      else
c         
c         if options are all in one string with a separator
c         
        indStr = 1
        lenAll = lnblnk(options(1))
        do i = 1, numOptions
          j = indStr
          indEnd = 0
          do while (j .le. lenAll .and. indEnd .eq. 0)
            if (options(1)(j:j) .eq. separator) indEnd = j - 1
            j = j + 1
          enddo
          if (j .gt. lenAll) indEnd = lenAll
          if (indStr .gt. indEnd) then
            call PipSetError('Too few options in string')
            PipParseInput = -1
            return
          endif
          PipParseInput = PipAddOption(options(1)(indStr:indEnd))
          if (PipParseInput .ne. 0) return
          indStr = j
        enddo
      endif

      PipParseInput = PipParseEntries(numOptArg, numNonOptArg)
      return
      end


c       Function to parse the entries to the program after options have
c       been defined
c       
      integer*4 function PipParseEntries(numOptArg, numNonOptArg)
      implicit none
      integer bufferSize
      parameter (bufferSize = 1024)
      integer*4 numOptArg, numNonOptArg
      integer*4 iargc, i, lnblnk
      integer*4 PipNextArg,PipReadStdinIfSet
      character*(bufferSize) string
c       
c       pass the arguments in one by one
c       
      do i = 1, iargc()
        call getarg(i, string)
        if (lnblnk(string) .eq. bufferSize) then
          call PipSetError(
     &        'Input argument too long for buffer in PipParseEntries')
          PipParseEntries = -1
          return
        endif
        PipParseEntries = PipNextArg(string)
        if (PipParseEntries .lt. 0) return
        if (PipParseEntries .gt. 0 .and. i .eq. iargc()) then
          call PipSetError('A value was expected but not found for'//
     &        ' the last option on the command line')
          PipParseEntries = -1
          return
        endif
      enddo
c       
c       Or read stdin if the flag is set to do it when no arguments
c
      if (iargc() .eq. 0) then
        PipParseEntries = PipReadStdinIfSet()
        if (PipParseEntries .ne. 0) return
      endif
c       
c       get numbers to return
c       
      call PipNumberOfArgs(numOptArg, numNonOptArg)
      call PipPrintEntries()
      PipParseEntries = 0
      return
      end


c       PipGetLogical: Function to get a Fortran logical directly
c       
      integer*4 function PipGetLogical(option, value)
      implicit none
      character*(*) option
      logical value
      integer*4 intval, ierr
      integer*4 PipGetBoolean

      intval = 0
      PipGetLogical = 0
      ierr = PipGetBoolean(option, intval)
      if (ierr .ne. 0) then
        PipGetLogical = ierr
      else
        value = intval .ne. 0
      endif
      return
      end


c       PipReadOrParseOptions will first try to read an options file,
c       then fallback to a list of options supplied to it
c       
      subroutine PipReadOrParseOptions(options, numOptions, progName,
     &    exitString, interactive, minArgs, numInFiles, numOutFiles, 
     &    numOptArg, numNonOptArg)
      implicit none
      character*(*) options(*)
      character*(*) progName
      character*(*) exitString
      logical interactive
      integer*4 minArgs, numInFiles, numOutFiles
      integer*4 numNonOptArg, numOptArg, numOptions, ierr
      character*240 errString
      integer*4 PipGetError,PipParseInput,PipReadOptionFile,PipParseEntries
      integer*4 PipGetBoolean, PipPrintHelp
c       
c       First try to read autodoc file
c       
      call PipAllowCommaDefaults(1)
      ierr = PipReadOptionFile(progName, 1, 0)
      call PipExitOnError(0, exitString)
      call setExitPrefix(exitString)
c       
c       If that is OK, go parse the entries;
c       otherwise print error message and use fallback option list
c       
      if (ierr .eq. 0) then
        ierr = PipParseEntries(numOptArg, numNonOptArg)
      else
        ierr = PipGetError(errString)
        print *,'PIP WARNING:', errString,
     &      'Using fallback options in Fortran code'
        ierr = PipParseInput(options, numOptions, '@', numOptArg,
     &      numNonOptArg)
      endif
c       
c       Process help input
c       
      if (interactive .and. numOptArg + numNonOptArg .eq. 0) return
      if (numOptArg + numNonOptArg .lt. minArgs .or.
     &    PipGetBoolean('help', ierr) .eq. 0) then
        ierr = PipPrintHelp(progName, 0, numInFiles, numOutFiles)
        call exit(0)
      endif
      return
      end


c       PipGetInOutFile gets one of the input or output files specified by
c       "option", or found at the "nonOptArgNo' non-option argument position
c       The name is returned in "filename"
c       If there no such entry, it returns with an error
c       If PIP input is not being used, it used "prompt" to ask for the name
c       
      integer*4 function PipGetInOutFile(option, nonOptArgNo, prompt,
     &    filename)
      implicit none
      character*(*) option
      character*(*) prompt
      character*(*) filename
      integer*4 nonOptArgNo, numOptArg, numNonOptArg
      integer*4 PipGetString, PipGetNonOptionArg
c       
      PipGetInOutFile = 0
      call PipNumberOfArgs(numOptArg, numNonOptArg)
c       
c       if there is PIP input, first look for explicit option by the name
c       then get the given non-option argument if there are enough
c       
      if (numOptArg + numNonOptArg .gt. 0) then
        PipGetInOutFile = PipGetString(option, filename)
        if (PipGetInOutFile .ne. 0) then
          if (numNonOptArg .lt. nonOptArgNo) return
          PipGetInOutFile = PipGetNonOptionArg(nonOptArgNo, filename)
        endif
      else
c         
c         Otherwise get interactive input with the prompt
c         
        write(*,'(1x,a,a,$)')prompt,': '
        read(5,'(a)')filename
      endif
      return
      end


c       Exits with error status after issuing the given message, with the
c       prefix set by calling setExitPrefix

      subroutine exiterror(message)
      implicit none
      character*(*) message
      character*32 prefix
      common /exitprefix/ prefix
      integer*4 lnblnk
      write(*,'(/,a,a,a)')prefix(1:lnblnk(prefix)),' ',message
      call exit(1)
      end


      subroutine setExitPrefix(message)
      character*(*) message
      character*32 prefix
      common /exitprefix/ prefix
      prefix = message
      return
      end

      subroutine memoryError(ierr, message)
      implicit none
      integer*4 ierr
      character*(*) message
      if (ierr .ne. 0) call exitError('FAILURE TO ALLOCATE '//message)
      return
      end

      
c       
c       $Log$
c       Revision 3.12  2009/12/04 20:31:12  mast
c       Add call to print entries
c
c       Revision 3.11  2007/06/22 05:02:53  mast
c       Enabled read from stdin if no args
c
c       Revision 3.10  2006/05/05 14:33:38  mast
c       Removed bad initialization of exit prefix
c
c       Revision 3.9  2006/05/04 21:11:28  mast
c       Added exiterror and subroutine to set error, prefix, called from
c       read_or_parse_options
c
c       Revision 3.8  2003/10/11 04:22:36  mast
c       Fix declaration of options as array in PipReadOrParse
c	
c       Revision 3.7  2003/10/11 00:04:56  mast
c       Fixed bug in getting filename interactively
c	
c       Revision 3.6  2003/10/10 20:43:41  mast
c       Added subroutine for getting input/output files
c	
c       Revision 3.5  2003/10/08 17:20:16  mast
c       Changes to work with autodoc files
c	
c       Revision 3.4  2003/08/09 17:03:28  mast
c       Fix bug (add declarations) in new getlogical function
c	
c       Revision 3.3  2003/08/08 16:23:18  mast
c       Added function to get a boolean into a logical variable directly
c	
c       Revision 3.2  2003/06/20 23:11:39  mast
c       Added capability to process options all in one string
c	
c       Revision 3.1  2003/06/05 00:14:22  mast
c       Addition to package
c	
