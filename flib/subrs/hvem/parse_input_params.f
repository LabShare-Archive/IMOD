c	  PipParseInput provides a simple front end to the "pip" package
c	  for parsing input parameters
c	  
c	  "options" is an array of character strings, one for each option,
c	  listing the short name, long name, type code, and help string,
c	  separated by commas.  In this case "separator" should be a space.
c	  Alternatively, options can be an array consisting of one long
c	  character string, with each option separated from the next by
c	  the character in "separator"
c	  "numOptions" is the number of options in the array or string
c	  The routine returns numOptArg, the number of option arguments
c	  on the command line; and numNonOptArg, the number of non-option
c	  arguments.
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	  Revision 3.3  2003/08/08 16:23:18  mast
c	  Added function to get a boolean into a logical variable directly
c	
c	  Revision 3.2  2003/06/20 23:11:39  mast
c	  Added capability to process options all in one string
c	
c	  Revision 3.1  2003/06/05 00:14:22  mast
c	  Addition to package
c	

	integer function PipParseInput(options, numOptions, separator,
     &	    numOptArg, numNonOptArg)
	implicit none
	integer bufferSize
	parameter (bufferSize = 1024)
	character*(*) options(*)
	character separator
	integer*4 numOptArg, numOptions, numNonOptArg
	integer*4 PipInitialize, PipAddOption
	integer*4 PipNextArg
	integer*4 iargc, i, lnblnk, j, indStr, indEnd, lenAll
	character*(bufferSize) string
c	  
c	  initialize then pass the options one by one
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
c	    if options are all in one string with a separator
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
c	  
c	  pass the arguments in one by one
c
	do i = 1, iargc()
	  call getarg(i, string)
	  if (lnblnk(string) .eq. bufferSize) then
	    call PipSetError(
     &		'Input argument too long for buffer in PipParseInput')
	    PipParseInput = -1
	    return
	  endif
	  PipParseInput = PipNextArg(string)
	  if (PipParseInput .lt. 0) return
	  if (PipParseInput .gt. 0 .and. i .eq. iargc()) then
	    call PipSetError('A value was expected but not found for'//
     &		' the last option on the command line')
	    PipParseInput = -1
	    return
	  endif
	enddo
c	  
c	  get numbers to return
c
	call PipNumberOfArgs(numOptArg, numNonOptArg)
	PipParseInput = 0
	return
	end

	integer function PipGetLogical(option, value)
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
