#!/usr/bin/python
#
# Parse Input Params module, translated from C
#
# Author: David Mastronarde
#
# $Id$
# Log at end
#
# The approach in translating was to retain most of the flags and capabilities
# in the C code, even when the calls for setting the flags were not included:
# i.e., the special flags, help output level and type.  However, manpage output
# is not provided for as that was not needed.  Default value entries for
# general arrays was also not included as this would have involved changing the
# calls for getting arrays defined in the previous extension module.  The code
# order is the same as in parse_params.c.  Obvious simplifications were made
# but much of the string processing logic was retained as the safest and
# easiest approach - so some of this will seem rather un-Python like.
# All global variables are declared in the functions that use them, although
# only the ones being assigned to need to be declared.

import re, sys, os

# The global defines, tables and variables
NON_OPTION_STRING = "NonOptionArgument"
STANDARD_INPUT_STRING = "StandardInput"
STANDARD_INPUT_END =  "EndInput"
LOOKUP_NOT_FOUND = -1
LOOKUP_AMBIGUOUS = -2
OPTFILE_DIR = "autodoc"
OPTFILE_EXT = "adoc"
OPTDIR_VARIABLE = "AUTODOC_DIR"
OPEN_DELIM = "["
CLOSE_DELIM = "]"
VALUE_DELIM = "="
PATH_SEPARATOR = os.sep
PIP_INTEGER = 1
PIP_FLOAT = 2

types = ("B","PF","LI","I", "F", "IP", "FP", "IT", "FT", "IA", "FA", "CH","FN")
typeForUsage = ("Boolean", "File", "List", "Int", "Float", "2 ints", \
  "2 floats", "3 ints", "3 floats", "Ints", "Floats", "String", "File", \
  "Unknown argument type")
numTypes = 13
tokenSep = re.compile("[= \t]")
nonWhite = re.compile('[^ \t]')
valueSep = re.compile('[ ,\t/]')
pipErrno = 0



# The options "structure"; initialize all the values
class pipOption:
    def __init__(self):
        self.shortName = ''
        self.longName = ''
        self.type = ''
        self.helpString = ''
        self.values = []
        self.multiple = 0
        self.count = 0
        
optTable = None
tableSize = 0
numOptions = 0
nonOptInd = 0
errorString = None
exitPrefix = None
errorDest = 0
nextOption = 0
nextArgBelongsTo = -1
numOptionArguments = 0
allowDefaults = 0
outputManpage = 0
defaultDelim = VALUE_DELIM
valueDelim = defaultDelim
noCase = 0
doneEnds = 0
takeStdIn = 0
nonOptLines = 0
noAbbrevs = 0
notFoundOK = False


# Initialize for given number of options
#
def PipInitialize(numOpts):
    global numOptions, tableSize, nonOptInd, optTable
    numOptions = numOpts
    tableSize = numOpts + 2
    nonOptInd = numOptions
    optTable = []

    # Initialize the table
    for i in range(tableSize):
        inst = pipOption()
        optTable.append(inst)

    # In the last slots, put non-option arguments, and also put the
    #  name for the standard input option for easy checking on duplication
    optTable[nonOptInd].longName = NON_OPTION_STRING
    optTable[nonOptInd + 1].shortName = STANDARD_INPUT_STRING
    optTable[nonOptInd + 1].longName = STANDARD_INPUT_END
    optTable[nonOptInd].multiple = 1
    return 0


# Free all allocated memory and set state back to initial state (sort of)
#
def PipDone():
    global numOptions, tableSize, nonOptInd, optTable, errorString, nextOption
    global nextArgBelongsTo, numOptionArguments, allowDefaults
    del optTable
    optTable = None
    tableSize = 0
    numOptions = 0
    errorString = None
    nextOption = 0
    nextArgBelongsTo = -1
    numOptionArguments = 0
    allowDefaults = 0


# Set up for Pip to handle exiting on error, with a prefix string
#
def PipExitOnError(useStdErr, prefix):
    global errorDest, exitPrefix
    errorDest = useStdErr
    exitPrefix = prefix
    return 0


# Just set the exit prefix
#
def setExitPrefix(prefix):
    global exitPrefix
    exitPrefix = prefix


# Return the error number
#
def PipGetErrNo():
    global pipErrno
    return pipErrno

 
# Add an option, with short and long name, type, and help string
#
def PipAddOption(optionString):
    global nextOption, numOptions, optTable, tableSize, nonOptInd

    if (nextOption >= numOptions):
        PipSetError("Attempting to add more options than were originally"
                    " specified")
        return -1

    parts = optionString.split(':', 3)
    if len(parts) < 4:
        PipSetError("Option does not have three colons in it:  " + \
                    optionString)
        return -1

    optTable[nextOption].shortName = parts[0]
    optTable[nextOption].longName = parts[1]

    # If type ends in M, set multiple flag and strip M
    if parts[2].endswith('M'):
        optTable[nextOption].multiple = 1
        if len(parts[2]) > 1: 
            parts[2] = parts[2][0:len(parts[2]) - 1]
        else:
            parts[2] = ''

    optTable[nextOption].type = parts[2]
    optTable[nextOption].helpString = parts[3]

    for ind in range(tableSize):
        # after checking existing ones, skip to NonOptionArg and 
        # StandardInput entries
        if (ind >= nextOption and ind < nonOptInd):
            continue

        oldShort = optTable[ind].shortName
        oldLong = optTable[ind].longName
        if (PipStartsWith(parts[0], oldShort) or \
            PipStartsWith(oldShort, parts[0]) or \
            PipStartsWith(oldLong, parts[0]) or \
            PipStartsWith(parts[0], oldLong) or \
            PipStartsWith(oldShort, parts[1]) or \
            PipStartsWith(parts[1], oldShort) or \
            PipStartsWith(oldLong, parts[1]) or \
            PipStartsWith(parts[1], oldLong)):
            tempStr = "Option %s  %s is ambiguous with option %s  %s" % \
                      (parts[0], parts[1], oldShort, oldLong)
            PipSetError(tempStr)
            print tempStr
            return -1

    nextOption += 1
    return 0


# Call this to process the next argument
def PipNextArg(argString):
    global nextArgBelongsTo, optTable, STANDARD_INPUT_STRING
    global numOptionArguments, nonOptInd, nextOption

    # If we are expecting a value for an option, add string to the option 
    if (nextArgBelongsTo >= 0):
        err = AddValueString(nextArgBelongsTo, argString)

        # Check whether this option was for reading from parameter file
        if (not err and optTable[nextArgBelongsTo].type == 'PF'):
            try:
                paramFile = open(argString, "r")
            except:
                tempStr = "Error opening parameter file %s" % argString
                PipSetError(tempStr)
                return -1

            err = ReadParamFile(paramFile)
            try:
                close(paramFile)
            except:
                pass
        nextArgBelongsTo = -1
        return err

    # Is it a legal option starting with - or -- ?
    if (argString[0] == '-'):
        indStart = 1
        if (len(argString) > 1 and argString[1] == '-'):
            indStart = 2
        if (len(argString) == indStart):
            PipSetError("Illegal argument: - or --")
            return -1

        # First check for StandardInput
        if (STANDARD_INPUT_STRING.startswith(argString[indStart:])):
            err = ReadParamFile(sys.stdin)
            return err

        numOptionArguments += 1

        # Lookup the option among true defined options 
        err = LookupOption(argString[indStart:], nextOption)
        if (err < 0):
            return err

        # For an option with value, setup to get argument next time and
        # return an indicator that there had better be another
        if (optTable[err].type != 'B'):
            nextArgBelongsTo = err
            return 1
        else:
          
            # for a boolean option, set the argument with a 1
            return AddValueString(err, '1')

    # A non-option argument
    return AddValueString(nonOptInd, argString)


# return number of option arguments (approximate) and number of non-option
# arguments 
#
def PipNumberOfArgs():
    global optTable, numOptionArguments, pipErrno
    pipErrno = 0
    return (numOptionArguments, optTable[nonOptInd].count)

# Get a non-option argument, index numbered from 0 here
#
def PipGetNonOptionArg(argNo):
    global optTable, pipErrno
    pipErrno = 0
    if (argNo >= optTable[nonOptInd].count):
        PipSetError("Requested a non-option argument beyond the number" + \
                    "available")
        pipErrno =  -1
        return None
    return optTable[nonOptInd].values[argNo]


# Get a string option
#
def PipGetString(option, string):
    global pipErrno
    pipErrno = 0
    retval = GetNextValueString(option)
    if pipErrno < 0:
        return None
    if pipErrno > 0:
        return string
    return retval


# Get a boolean (binary) option; make sure it has a legal specification
#
def PipGetBoolean(option, val):
    global pipErrno
    pipErrno = 0
    strPtr = GetNextValueString(option)
    if pipErrno < 0:
        return None
    if pipErrno > 0:
        return val
    if (strPtr == "1" or strPtr == "T" or strPtr == "TRUE" or strPtr == "ON" \
        or strPtr == "t" or strPtr == "true" or strPtr == "on"):
        return 1
    elif (strPtr == "0" or strPtr == "F" or strPtr == "FALSE" or \
             strPtr == "OFF" or strPtr == "f" or strPtr == "false" or \
             strPtr == "off"):
        return 0
    else:
        tempStr = "Illegal entry for boolean option %s: %s" % (option, strPtr)
        PipSetError(tempStr)
        pipErrno =  -1
        return None


# Get single integer and float just call to get an array with one element
#
def PipGetInteger(option, val):
    global pipErrno
    num = 1
    retval = PipGetIntegerArray(option, num)
    if (pipErrno < 0):
        return None
    if (pipErrno > 0):
        return val
    return retval[0]

def PipGetFloat(option, val):
    global pipErrno
    num = 1
    retval = PipGetFloatArray(option, num)
    if (pipErrno < 0):
        return None
    if (pipErrno > 0):
        return val
    return retval[0]


# Get two integers or floats - move into array, get array with two elements
#
def PipGetTwoIntegers(option, val1, val2):
    global pipErrno
    num = 2
    retval = PipGetIntegerArray(option, num)
    if (pipErrno < 0):
        return None
    if (pipErrno > 0):
        return (val1, val2)
    return (retval[0], retval[1])

def PipGetTwoFloats(option, val1, val2):
    global pipErrno
    num = 2
    retval = PipGetFloatArray(option, num)
    if (pipErrno < 0):
        return None
    if (pipErrno > 0):
        return (val1, val2)
    return (retval[0], retval[1])


# Get three integers or floats - move into array, get array with two elements
#
def PipGetThreeIntegers(option, val1, val2, val3):
    global pipErrno
    num = 3
    retval = PipGetIntegerArray(option, num)
    if (pipErrno < 0):
        return None
    if (pipErrno > 0):
        return (val1, val2, val3)
    return (retval[0], retval[1], retval[2])

def PipGetThreeFloats(option, val1, val2, val3):
    global pipErrno
    num = 3
    retval = PipGetFloatArray(option, num)
    if (pipErrno < 0):
        return None
    if (pipErrno > 0):
        return (val1, val2, val3)
    return (retval[0], retval[1], retval[2])


# Getting an array of integers or floats calls the general routine for
# getting a line of values 
#
def PipGetIntegerArray(option, numToGet):
    global PIP_INTEGER
    return OptionLineOfValues(option, PIP_INTEGER, numToGet)

def PipGetFloatArray(option, numToGet):
    global PIP_FLOAT
    return OptionLineOfValues(option, PIP_FLOAT, numToGet)


# Print a complete usage statement only
#
def PipPrintHelp(progName, useStdErr, inputFiles, outputFiles):
    global numOptions, optTable, outputManpage, typeForUsage, types, numTypes
    numOut = 0
    numReal = 0
    helplim = 74
    out = sys.stdout
    if useStdErr:
        out = sys.stderr
    indent4 = "    "
    linePos = 11
#    descriptions = typeDescriptions

    for i in range(numOptions):
        if (len(optTable[i].shortName) or len(optTable[i].longName)):
            numReal += 1
    
    if (not outputManpage):
        out.write("Usage: %s " % progName)
        if (numOptions):
            out.write("[Options]")
        if (inputFiles):
            out.write(" input_file")
        if (inputFiles > 1):
            out.write("s...")
        if (outputFiles):
            out.write(" output_file")
        if (outputFiles > 1):
            out.write("s...")
        out.write("\n")

        if (not numReal):
            return 0
        out.write("Options:\n")
        descriptions = typeForUsage

    for i in range(numOptions):
        
        sname = optTable[i].shortName
        lname = optTable[i].longName
        indentStr = ""
        if (len(lname) or len(sname)):
            if (outputManpage <= 0):
                indentStr = indent4

            out.write(" ")
            if len(sname):
                out.write("-%s" % sname)
            if len(sname) or len(lname):
                out.write(" OR ")
            if len(lname):
                out.write("-%s" % lname)
            for j in range(numTypes):
                jj = j
                if (optTable[i].type == types[j]):
                    break
                
            if (optTable[i].type != "B"):
                out.write("   " + descriptions[jj])

        out.write("\n")

        # Print help string, breaking up line as needed
        if len(optTable[i].helpString):
            sname = optTable[i].helpString
            optLen = len(sname)
            newLinePt = sname.find('\n')
            while (optLen > helplim or newLinePt >= 0):

                # Break string at newline
                # Or break string at last space before limit
                if (newLinePt >= 0 and newLinePt <= helplim):
                    j = newLinePt
                else:
                    j = sname.rfind(' ', 1, helplim)
                    if (j < 0):
                        j = 0

                out.write("%s%s\n" % (indentStr, sname[0:j]))
                sname = sname[j+1:]
                newLinePt = sname.find('\n')
                optLen -= j + 1

            out.write("%s%s\n" % (indentStr, sname))

        if (optTable[i].multiple):
            out.write("%s(Successive entries accumulate)\n" % indentStr)

    return 0


# Return the error string, or an empty string and an error if there is none
#
def PipGetError():
    global errorString, pipErrno
    pipErrno = 0
    if (not errorString):
        pipErrno =  -1
        return ''
    return errorString


# Set the error string.
# If exitPrefix is set, then output an error message to stderr or stdout
# and exit. 
def PipSetError(errString):
    global errorString, exitPrefix
    outFile = sys.stdout
    if errorDest:
        outFile = stderr
    errorString = errString
    if not errorString and not exitPrefix:
        pipErrno =  -1
        return None

    if exitPrefix:
        if not errorString:
            errorString = "Unspecified error"
        outFile.write(exitPrefix + errorString + "\n")
        sys.exit(1)

    return 0


# Simple function to call with error string to append to prefix and exit
#
def exitError(errorMess):
    PipSetError(errorMess)
    sys.exit(1)


# Return the number of entries for a particular option
#
def PipNumberOfEntries(option):
    global nonOptInd, pipErrno
    pipErrno = 0
    err = LookupOption(option, nonOptInd + 1)
    if (err < 0):
        pipErrno =  err
        return None
     
    return optTable[err].count


# Top level routine to be called to process options and arguments
#
def PipParseInput(argv, options):

    # Initialize
    numOpts = len(options)
    err = PipInitialize(numOpts)
    if err:
        pipErrno =  err
        return None

    # add the options
    for i in range(numOpts):
        err = PipAddOption(options[i])
        if err:
            pipErrno =  err
            return None

    return (PipParseEntries(argv))


# Alternative routine to have options read from a file
#
def PipReadOptionFile(progName, helpLevel, localDir):
    global OPTDIR_VARIABLE, PATH_SEPARATOR, OPTFILE_DIR, OPTFILE_EXT
    global optTable, valueDelim
    optFile = None
    
    # If local directory not set, look for environment variable pointing
    # directly to where the file should be
    if (not localDir):
        pipDir = os.getenv(OPTDIR_VARIABLE)
        if (pipDir):
            bigStr = "%s%c%s.%s" % (pipDir, PATH_SEPARATOR, progName, \
                                    OPTFILE_EXT)
            try:
                # print "Looking for file " + bigStr
                optFile = open(bigStr, "r")
            except:
                optFile = None

        if (not optFile):
            pipDir = os.getenv("IMOD_DIR")
            if (pipDir):
                bigStr = "%s%c%s%c%s.%s" % (pipDir, PATH_SEPARATOR, \
                         OPTFILE_DIR, PATH_SEPARATOR, progName, OPTFILE_EXT)
                try:
                    #print "Looking for file " + bigStr
                    optFile = open(bigStr, "r")
                except:
                    optFile = None
  
    # If local directory set, set up name with ../ as many times as specified
    # and look for file there
    elif (localDir > 0):
        bigStr = ""
        for i in range(localDir):
            bigStr += ".." + PATH_SEPARATOR
            
            bigStr += "%s%c%s.%s" % (OPTFILE_DIR, PATH_SEPARATOR, progName, \
                                     OPTFILE_EXT)
            try:
                # print "Looking for file " + bigStr
                optFile = open(bigStr, "r")
            except:
                optFile = None

    
    # If there is still no file, look in current directory
    if (not optFile):
        bigStr = "%s.%s" % (progName, OPTFILE_EXT)
        try:
            # print "Looking for file " + bigStr
            optFile = open(bigStr, "r")
        except:
            optFile = None

        if (not optFile):
            bigStr = "Autodoc file " + progName + "." + OPTFILE_EXT + \
                     " was not found or not readable.\n" +\
                     "Check environment variable settings of " + \
                     OPTDIR_VARIABLE + " and " + \
                     "IMOD_DIR\nor place autodoc file in current directory"
            PipSetError(bigStr)
            return -1

    numOpts = 0
    optList = []
    longName = shortName = type = ''
    helpStr = ['','','']
    readingOpt = 0

    while (1):
        (lineLen, bigStr, indst) = PipReadNextLine(optFile, '#', 0, 0)
        if (lineLen == -2):
            PipSetError("Error reading autodoc file")
            return -1

        # Count up option entries
        textStr = bigStr[indst:]
        isOption = LineIsOptionToken(textStr)
        if (isOption):
            numOpts += 1

        # Look for new keyword-value delimiter before any options
        if (not numOpts):
            (newDelim, lastInd) = CheckKeyword(textStr, "KeyValueDelimiter", 0)
            if (newDelim):
                valueDelim = newDelim

        if (readingOpt and (lineLen == -3 or isOption)):
      
            # If we were reading options, it is time to add them if we are at
            # end of file or if we have reached a new token of any kind

            # Pick the closest help string that was read in if the given one
            # does not match (there has got to be an easier way!)
            if (helpLevel <= 1):
                if (helpStr[0]):
                    helpInd = 0
                elif (helpStr[1]):
                    helpInd = 1
                else:
                    helpInd = 2

            elif (helpLevel == 2):
                if (helpStr[1]):
                    helpInd = 1
                elif (helpStr[0]):
                    helpInd = 0
                else:
                    helpInd = 2

            else:
                if (helpStr[2]):
                    helpInd = 2
                elif (helpStr[1]):
                    helpInd = 1
                else:
                    helpInd = 0

            # If it is a section header, get rid of the names
            if (isSection):
                longName = shortName = ''

            optStr = "%s:%s:%s:%s" % (shortName, longName, type,
                                      helpStr[helpInd])
            optList.append(optStr)

            # Clean up 
            longName = shortName = type = ''
            helpStr = ['','','']
            readingOpt = 0

        if (lineLen == -3):
            break

        # If reading options, look for the various keywords
        if (readingOpt):

            # If the last string gotten was a help string and the line does not
            #   contain the value delimiter, then append it to the last string
            if (lastInd and textStr.find(valueDelim) < 0):
                if helpStr[lastInd-1].endswith('.'):
                    helpStr[lastInd-1] += '  '
                else:
                    helpStr[lastInd-1] += ' '

                # Replace leading ^ with a newline
                if (textStr[0] == '^'):
                    textStr = textStr.replace('^', '\n', 1)
                helpStr[lastInd-1] += textStr

            # Otherwise look for each keyword of interest, but zero last index
            else:
                lastInd = 0
                retval = CheckKeyword(textStr, "short", 0)
                if retval[0]:
                    (shortName, lastInd) = retval
                retval = CheckKeyword(textStr, "long", 0)
                if retval[0]:
                    (longName, lastInd) = retval
                retval = CheckKeyword(textStr, "type", 0)
                if retval[0]:
                    (type, lastInd) = retval

                # Check for usage if at help level 1 or if we haven't got
                # either of the other strings yet
                if (helpLevel <= 1 or not (helpStr[1] or helpStr[2])):
                    retval = CheckKeyword(textStr, "usage", 1)
                    if retval[0]:
                        (helpStr[0], lastInd) = retval
        
                # Check for tooltip if at level 2 or if at level 1 and haven't 
                # got usage, or at level 3 and haven't got manpage
                if (helpLevel == 2 or (helpLevel <= 1 and not helpStr[0]) or
                    (helpLevel >= 3 and not helpStr[2])):
                    retval = CheckKeyword(textStr, "tooltip", 2)
                    if retval[0]:
                        (helpStr[1], lastInd) = retval
        
                # Check for manpage if at level 3 or if at level 2 and haven't 
                # got tip, or at level 1 and haven't got tip or usage
                if (helpLevel >= 3 or (helpLevel == 2 and not helpStr[1]) or
                    (helpLevel <= 1 and not (helpStr[1] or helpStr[0]))):
                    retval = CheckKeyword(textStr, "manpage", 3)
                    if retval[0]:
                        (helpStr[2], lastInd) = retval

        # But if not reading options, check for a new option token and start 
        # reading if one is found.  But first take a Field value as default 
        # long option name
        elif (isOption > 0):
            lastInd = 0
            readingOpt = 1
            isSection = isOption - 1
            if (not isSection):
                retval = CheckKeyword(textStr[len(OPEN_DELIM):], "Field", 0)
                if retval[0]:
                    longName = retval[0]
                    longName = longName[:len(longName)-1].rstrip()


    # Initialize and process option strings
    PipInitialize(numOpts)
    # print "Initialized for %d options" % numOpts

    # add the options
    for i in range(numOpts):
        err = PipAddOption(optList[i])
        if err:
            return err

    return 0
                                       

# Routine to parse the entries in command line after options have been
# defined one way or another
#
def PipParseEntries(argv):
    global takeStdIn, pipErrno
    pipErrno = 0
    argc = len(argv)

    # Special case: no arguments and flag set to take stdin automatically
    if (not argc and takeStdIn):
        err = ReadParamFile(sys.stdin)
        if err:
            pipErrno =  err
            return None
    else:

        # parse the arguments
        for i in range(1,argc):
            err = PipNextArg(argv[i])
            if (err < 0):
                pipErrno =  err
                return None
            if (err and i == argc - 1):
                PipSetError("A value was expected but not found for" + \
                            " the last option on the command line")
                pipErrno =  -1
                return None

    return PipNumberOfArgs()


# High-level routine to initialize from autodoc with optional fallback options
# Set exit string and output to stdout, print usage if not enough arguments
#
def PipReadOrParseOptions(argv, options, progName, minArgs, numInFiles,
                          numOutFiles):
    global exitPrefix
    
    # Startup with fallback
    ierr = PipReadOptionFile(progName, 0, 0)
    PipExitOnError(0, "ERROR: %s - " % progName)
    if (not ierr):
        (numOptArgs, numNonOptArgs) = PipParseEntries(argv)
    else:
        errString = PipGetError()
        if (not options or not len(options)):
            PipSetError(errString)
        if (errString):
            print "PIP WARNING: %s\nUsing fallback options in script\n" % \
                  errString

        (numOptArgs, numNonOptArgs) = PipParseInput(argv, options)


    # Output usage and exit if not enough arguments
    exitSave = exitPrefix
    exitPrefix = None
    help = PipGetBoolean('help', 0)
    exitPrefix = exitSave
    if (help or numOptArgs + numNonOptArgs < minArgs):
        PipPrintHelp(progName, 0, numInFiles, numOutFiles)
        sys.exit(0)

    return (numOptArgs, numNonOptArgs)


# Routine to get input/output file from parameter or non-option args
#
def PipGetInOutFile(option, nonOptArgNo):
    global optTable, pipErrno
    retval = PipGetString(option, '')
    if not pipErrno:
        return retval
    if (nonOptArgNo >= optTable[nonOptInd].count):
        pipErrno =  1
        return None
    return PipGetNonOptionArg(nonOptArgNo)


# Read successive lines from a parameter file or standard input, and 
# store as options and values 
#
def ReadParamFile(pFile):
    global notFoundOK, numOptionArguments, optTable, nonOptLines, tokenSep
    global STANDARD_INPUT_STRING, STANDARD_INPUT_END, numOptions
    while (1):

        # If non-option lines are allowed, set flag that it is OK for
        # LookupOption to not find the option, but only for the given number
        # of lines at the start of the input
        notFoundOK = not numOptionArguments and \
                      (optTable[nonOptInd].count < nonOptLines)
        (lineLen, lineStr, indst) = PipReadNextLine(pFile,  '#', 0, 1)
        if (lineLen == -3):
            break
        if (lineLen == -2):
            PipSetError("Error reading parameter file or " + \
                        STANDARD_INPUT_STRING)
            return -1


        # Find token
        matchObj = re.search(tokenSep, lineStr[indst:])
        indnd = lineLen
        if (matchObj):
            indnd = matchObj.start()
        token = lineStr[indst:indnd]

        # Done if it matches end of input string
        if (token == STANDARD_INPUT_END) or (doneEnds and token == "DONE"):
            break

        # Look up option
        optNum = LookupOption(token, numOptions)
        if (optNum < 0):

            # If no option, process special case if in-line non-options allowed
            # or error out
            if (notFoundOK):
                err = AddValueString(nonOptInd, lineStr[indst:])
                if err:
                    return err
                continue
            else:
                return optNum

        if (optTable[optNum].type == "PF"):
            PipSetError("Trying to open a parameter file while reading a " +
                        "parameter file or " + STANDARD_INPUT_STRING)
            return -1

        # Find first non-white space, passing over at most one equals sign
        indst = indnd + 1
        gotEquals = 0
        while (indst < lineLen):
            if (lineStr[indst] == '='):
                if (gotEquals):
                    PipSetError("Two = signs in input line:  " + lineStr)
                    return -1
                gotEquals = 1

            elif (lineStr[indst] != ' ' and lineStr[indst] != '\t'):
                break
            indst += 1

        # If there is a string, get one; if not, get a "1" for boolean,
        # otherwise it is an error
        if (indst < lineLen):
            token = lineStr[indst:]
        elif (optTable[optNum].type == 'B'):
            token = "1"
        else:
            PipSetError("Missing a value on the input line:  " + lineStr)
            return -1

        # Add the token as a value string and increment argument number
        err = AddValueString(optNum, token)
        if err:
            return err
        numOptionArguments += 1

    notFoundOK = 0
    return 0


# Reads a line from the file [pFile], stripping white space at the end of the
# line and in-line comments starting with [comment] if [inLineComments] is 
# non-zero.  Discards the line and reads another if it is blank or if the 
# first non-blank character is [comment], unless [keepComments] is nonzero.
# Returns a tuple of: the length of the line, or -3 for end of 
# file, or -2 for error reading file; the line; and the index of the first
# non-white space character
#
def PipReadNextLine(pFile, comment, keepComments, inLineComments):
    global nonWhite

    while (1):
        try:
            lineStr = pFile.readline()
            lineLen = len(lineStr)
            if not lineLen:
                return (-3, '', 0)
        except:
            return (-2, '', 0)

        # Get first non-white space
        matchObj = re.search(nonWhite, lineStr)
        if (not matchObj):
            continue
        indst = matchObj.start()

        # If it is a comment, skip or strip line ending and return
        if (lineStr[indst] == comment):
            if (keepComments):
                lineStr = lineStr.rstrip()
                lineLen = len(lineStr)
                break
            else:
                continue

        # adjust line length to remove comment, if we have in-line comments
        if inLineComments:
            strPtr = lineStr.find(comment)
            if strPtr >= 0:
                lineLen = strPtr

        # adjust line length back further to remove white space and newline
        lineStr = lineStr[0:lineLen].rstrip()
        lineLen = len(lineStr)

        # Return if something is on line or we are keeping comments 
        if (indst < lineLen or keepComments):
            break
  
    return (lineLen, lineStr, indst)



# Parse a line of values for an option and return them into an array
#
def OptionLineOfValues(option, valType, numToGet):
    global pipErrno
    
    # Get string  and save pointer to it for error messages
    strPtr = GetNextValueString(option)
    if pipErrno:
        return None
    return PipGetLineOfValues(option, strPtr, valType, numToGet)


# Parses a line of values from the string in [strPtr] and returns them as an
# array.  The type is indicated in
# [valType] as PIP_INTEGER (1) for integer or PIP_FLOAT (2) for float.
# The number of values to get is set in [numToGet], where a value of zero
# indicates all values should be returned, in which case the number gotten is 
# returned in [numToGet].  The return value is -1 for errors in parsing or
# too few values on the line.
# NOTE THAT DEFAULTS CANNOT BE ALLOWED UNLESS AN ARRAY CAN BE SUPPLIED
#
def PipGetLineOfValues(option, strPtr, valType, numToGet):
    global valueSep, allowDefaults, PIP_INTEGER, PIP_FLOAT
    global pipErrno
    pipErrno = 0
    fullStr = strPtr
    array = []
    numGot = 0

    while (len(strPtr)):
        matchObj = re.search(valueSep, strPtr)
        if (not matchObj):

            # no object means read a number to end of string
            endPtr = len(strPtr)
        elif (matchObj.start() == 0):

            # separator at start means advance by one byte and continue
            # if defaults allowed and a specific number are expected,
            # / means stop processing and mark all values as received
            if (strPtr[0] == '/'):
                if (allowDefaults and numToGet):
                    numGot = numToGet
                    break
        
                tempStr = "Default entry with a / is not allowed in value entry:  %s  %s" % \
                          (option, fullStr)
                PipSetError(tempStr)
                pipErrno =  -1
                return None

            # special handling of commas to allow default values
            if (strPtr[0] == ','):

                # If already have a comma, skip an array value if defaults
                # allowed
                if (gotComma):
                    if (allowDefaults and numToGet):
                        numGot += 1
                        if (numGot >= numToGet):
                            break
                    else:
                        tempStr = "Default entries with commas are not allowed in value entry:  %s  %s" % \
                                  (option, fullStr)
                        PipSetError(tempStr)
                        pipErrno =  -1
                        return None
                gotComma = 1
      
            strPtr = strPtr[1:]
            continue

        else:
            # otherwise, this should be the end index for the conversion
            endPtr = matchObj.start()
    

        # convert number in a try block
        try:
            if (valType == PIP_INTEGER):
                array.append(int(strPtr[0:endPtr]))
            else:
                array.append(float(strPtr[0:endPtr]))
            numGot += 1
        except:
            tempStr = "Illegal character in value entry:  %s  %s" % \
                      (option, fullStr)
            PipSetError(tempStr)
            pipErrno =  -1
            return None
    
        # Mark that there is no comma after we have a number
        gotComma = 0

        # Done if at end of line, or if count is fulfilled
        if (endPtr == len(strPtr) or (numToGet and numGot >= numToGet)):
            break
    
        # Otherwise advance to separator and continue
        strPtr = strPtr[endPtr:]
        

    # If not enough values found, return error
    if (numToGet > 0 and numGot < numToGet):
        tempStr = "%d values expected but only %d values found in value entry:  %s %s" % \
                  (numToGet, numGot, option, fullStr)
        PipSetError(tempStr)
        pipErrno =  -1
        return None
  
    return array


# Get a pointer to the value string for the given option.
# Return < 0 if the option is invalid, 1 if the option was not entered.
# If the option allows multiple values, advance the multiple counter
#
def GetNextValueString(option):
    global optTable, nonOptInd, pipErrno
    pipErrno = 0
    err = LookupOption(option, nonOptInd + 1)
    optp = optTable[err]
    if (err < 0):
        pipErrno =  err
        return None
    if (not optp.count):
        pipErrno =  1
        return None

    index = 0
    if (optp.multiple):
        index = optp.multiple - 1
    if (optp.multiple and optp.multiple < optp.count):
        optp.multiple += 1
    return optp.values[index]


# Add a string to the set of values for an option whose index is optInd
#
def AddValueString(optInd, strPtr):
    global optTable
    optp = optTable[optInd]
    
    # If we accept multiple values or have none yet, append;
    # otherwise set first element
    if optp.multiple or not optp.count:
        optp.values.append(strPtr)
        optp.count += 1
    else:
        optp.values[0] = strPtr
        optp.count = 1
    return 0


# Lookup an option in the table, up to the range in maxLookup
#
def LookupOption(option, maxLookup):
    global optTable, LOOKUP_NOT_FOUND, LOOKUP_AMBIGUOUS, notFoundOK, noAbbrevs
    lenopt = 0
    found = LOOKUP_NOT_FOUND

    if (noAbbrevs):
        lenopt = len(option)
        
    # Look at all of the options specified by maxLookup
    for i in range(maxLookup):
        sname = optTable[i].shortName
        lname = optTable[i].longName
        if (PipStartsWith(sname, option) and \
            (not noAbbrevs or lenopt == len(sname))) or \
            (PipStartsWith(lname, option) and \
            (not noAbbrevs or lenopt == strlen(lname))):
          
            # If it is found, it's an error if one has already been found
            if (found == LOOKUP_NOT_FOUND):
                found = i
            else:
                tempStr = "An option specified by \"%s\" is ambiguous between option %s -  %s  and option %s -  %s" % \
                         (option, sname, lname, optTable[found].shortName, \
                          optTable[found].longName)
                PipSetError(tempStr)
                return LOOKUP_AMBIGUOUS
            
    # Set error string unless flag set that non-options are OK 
    if (found == LOOKUP_NOT_FOUND and not notFoundOK):
        tempStr = "Illegal option: %s" % option
        PipSetError(tempStr)
    return found


# Test for whether str1 starts with str2, returns false if either is empty
#
def PipStartsWith(str1, str2):
    if (not str1 or str1 == "" or not str2 or str2 == ""):
        return False
    return str1.startswith(str2)


# Determines whether the line contains the token for an option inside the
# opening and closing delimiters and returns 1 if it does, or -1 if it is
# another token
#
def LineIsOptionToken(line):

    # It is not a token unless it starts with open delim and contains close
    if (not PipStartsWith(line, OPEN_DELIM) or line.find(CLOSE_DELIM) < 0):
        return 0

    # It must then contain "Field" right after delim to be an option
    openlen = len(OPEN_DELIM)
    if (PipStartsWith(line[openlen:], "Field")):
        return 1
    if (PipStartsWith(line[openlen:], "SectionHeader")):
        return 2

    return -1


#
# Checks for whether a keyword occurs at the beginning of the line and
# is followed by the keyword-value delimiter, and if so returns the
# value string and the supplied index number, otherwise blank string and 0
#
def CheckKeyword(line, keyword, index):
    global valueDelim
    lineLen = len(line)
    
    # First make sure line starts with it
    if (not PipStartsWith(line, keyword)):
        return ('', 0)

    # Now look for delimiter 
    valStart = line.find(valueDelim)
    if (valStart < 0):
        return ('', 0)

    # Eat spaces after the delimiter and return if nothing left
    # In other words, a key with no value is the same as having no key at all 
    valStart += len(valueDelim)
    while (valStart < lineLen and (line[valStart] == ' ' or
           line[valStart] == '\t')):
        valStart += 1
    if (valStart >= lineLen):
        return ('', 0)

    return (line[valStart:], index)

# $Log$
# Revision 1.1  2008/01/05 17:19:09  mast
# Added to package
#
#
