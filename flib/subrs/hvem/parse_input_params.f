c	  PipParseInput provides a simple front end to the "pip" package
c	  for parsing input parameters
c	  
c	  options is an array of character strings, one for each option,
c	  listing the short name, long name, type code, and help string,
c	  separated by commas.
c	  numOptions is the number of options in the array.
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

	integer function PipParseInput(options, numOptions, numOptArg,
     &	    numNonOptArg)
	implicit none
	integer bufferSize
	parameter (bufferSize = 1024)
	character*(*) options(*)
	integer*4 numOptArg, numOptions, numNonOptArg
	integer*4 PipInitialize, PipAddOption
	integer*4 PipNextArg
	integer*4 iargc, i, lnblnk
	character*(bufferSize) string
c	  
c	  initialize then pass the options one by one
c
	PipParseInput = PipInitialize(numOptions)
	if (PipParseInput .ne. 0) return
	do i = 1, numOptions
	  PipParseInput = PipAddOption(options(i))
	  if (PipParseInput .ne. 0) return
	enddo
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
