#!/usr/bin/python
#
# Parse Input Params module, translated from C
#
# Author: David Mastronarde
#
# $Id$
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
PRINTENTRY_VARIABLE = "PIP_PRINT_ENTRIES"
OPEN_DELIM = "["
CLOSE_DELIM = "]"
VALUE_DELIM = "="
PATH_SEPARATOR = os.sep
PIP_INTEGER = 1
PIP_FLOAT = 2

sTypes = ("B","PF","LI","I", "F", "IP", "FP", "IT", "FT", "IA", "FA", "CH","FN")
sTypeForUsage = ("Boolean", "File", "List", "Int", "Float", "2 ints", \
  "2 floats", "3 ints", "3 floats", "Ints", "Floats", "String", "File", \
  "Unknown argument type")
sNumTypes = 13
tokenSep = re.compile("[= \t]")
nonWhite = re.compile('[^ \t]')
valueSep = re.compile('[ ,\t/]')
pipErrno = 0
sQuoteTypes = """'"`"""


# The options "structure"; initialize all the values
class pipOption:
   def __init__(self):
      self.shortName = ''
      self.longName = ''
      self.type = ''
      self.helpString = ''
      self.format = ''
      self.values = []
      self.multiple = 0
      self.count = 0
      self.lenShort = 0
      self.nextLinked = []
      self.linked = False
sOptTable = None
sTableSize = 0
sNumOptions = 0
sNonOptInd = 0
sErrorString = None
sExitPrefix = None
sProgramName  = ""
sErrorDest = 0
sNextOption = 0
sNextArgBelongsTo = -1
sNumOptionArguments = 0
sAllowDefaults = 0
sOutputManpage = 0
sPrintEntries = -1
sDefaultDelim = VALUE_DELIM
sValueDelim = sDefaultDelim
sNoCase = 0
sDoneEnds = 0
sTakeStdIn = 0
sNonOptLines = 0
sNoAbbrevs = 0
sNotFoundOK = False
sLinkedOption = None
sTestAbbrevForUsage = False

# Initialize for given number of options
#
def PipInitialize(numOpts):
   global sNumOptions, sTableSize, sNonOptInd, sOptTable
   sNumOptions = numOpts
   sTableSize = numOpts + 2
   sNonOptInd = sNumOptions
   sOptTable = []

   # Initialize the table
   for i in range(sTableSize):
      inst = pipOption()
      sOptTable.append(inst)

   # In the last slots, put non-option arguments, and also put the
   #  name for the standard input option for easy checking on duplication
   sOptTable[sNonOptInd].longName = NON_OPTION_STRING
   sOptTable[sNonOptInd + 1].shortName = STANDARD_INPUT_STRING
   sOptTable[sNonOptInd + 1].longName = STANDARD_INPUT_END
   sOptTable[sNonOptInd].multiple = 1
   return 0


# Free all allocated memory and set state back to initial state (sort of)
#
def PipDone():
   global sNumOptions, sTableSize, sNonOptInd, sOptTable, sErrorString, sNextOption
   global sNextArgBelongsTo, sNumOptionArguments, sAllowDefaults
   del sOptTable
   sOptTable = None
   sTableSize = 0
   sNumOptions = 0
   sErrorString = None
   sNextOption = 0
   sNextArgBelongsTo = -1
   sNumOptionArguments = 0
   sAllowDefaults = 0
   sLinkedOption = None


# Set up for Pip to handle exiting on error, with a prefix string
#
def PipExitOnError(useStdErr, prefix):
   global sErrorDest, sExitPrefix
   sErrorDest = useStdErr
   sExitPrefix = prefix
   return 0


# Just set the exit prefix
#
def setExitPrefix(prefix):
   global sExitPrefix
   sExitPrefix = prefix

# Enable the entry ouput
#
def PipEnableEntryOutput(val):
   global sPrintEntries
   sPrintEntries = val

# Set a linked option
#
def PipSetLinkedOption(option):
   global sLinkedOption
   sLinkedOption = option

# Return the error number
#
def PipGetErrNo():
   global pipErrno
   return pipErrno

 
# Add an option, with short and long name, type, and help string
#
def PipAddOption(optionString):
   global sNextOption, sNumOptions, sOptTable, sTableSize, sNonOptInd

   if (sNextOption >= sNumOptions):
      PipSetError("Attempting to add more options than were originally"
                  " specified")
      return -1

   parts = optionString.split(':', 3)
   if len(parts) < 4:
      PipSetError("Option does not have three colons in it:  " + optionString)
      return -1

   sOptTable[sNextOption].shortName = parts[0]
   newSlen = len(parts[0])
   sOptTable[sNextOption].lenShort = newSlen
   sOptTable[sNextOption].longName = parts[1]

   # If type ends in M or L, set multiple flag and strip M or L
   if parts[2].endswith('M') or parts[2].endswith('L'):
      sOptTable[sNextOption].multiple = 1
      sOptTable[sNextOption].linked = parts[2].endswith('L')
      if len(parts[2]) > 1:
         parts[2] = parts[2][0:len(parts[2]) - 1]
      else:
         parts[2] = ''

   sOptTable[sNextOption].type = parts[2]
   sOptTable[sNextOption].helpString = parts[3]

   for ind in range(sTableSize):
      # after checking existing ones, skip to NonOptionArg and 
      # StandardInput entries
      if (ind >= sNextOption and ind < sNonOptInd):
         continue

      oldShort = sOptTable[ind].shortName
      oldLong = sOptTable[ind].longName
      oldSlen = sOptTable[ind].lenShort
      if (((PipStartsWith(parts[0], oldShort) or PipStartsWith(oldShort, parts[0])) and
           ((newSlen > 1 and oldSlen > 1) or (newSlen == 1 and oldSlen == 1))) or \
         PipStartsWith(oldLong, parts[0]) or PipStartsWith(parts[0], oldLong) or \
         PipStartsWith(oldShort, parts[1]) or PipStartsWith(parts[1], oldShort) or \
          PipStartsWith(oldLong, parts[1]) or PipStartsWith(parts[1], oldLong)):
         sTempStr = "Option " + parts[0] + "  " + parts[1] + \
                   " is ambiguous with option " + oldShort + "  " + oldLong
         PipSetError(sTempStr)
         sys.stdout.write(sTempStr + "\n")
         return -1

   sNextOption += 1
   return 0


# Call this to process the next argument
def PipNextArg(argString):
   global sNextArgBelongsTo, sOptTable, STANDARD_INPUT_STRING
   global sNumOptionArguments, sNonOptInd, sNextOption, sNotFoundOK

   # If we are expecting a value for an option, add string to the option 
   if (sNextArgBelongsTo >= 0):
      err = AddValueString(sNextArgBelongsTo, argString)

      # Check whether this option was for reading from parameter file
      if (not err and sOptTable[sNextArgBelongsTo].type == 'PF'):
         try:
            paramFile = open(argString, "r")
         except:
            sTempStr = "Error opening parameter file " + argString
            PipSetError(sTempStr)
            return -1

         err = ReadParamFile(paramFile)
         try:
            close(paramFile)
         except:
            pass
      sNextArgBelongsTo = -1
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

      # Next check if it is a potential numeric non-option arg
      sNotFoundOK = 1
      for ch in argString[indStart:]:
         if ch != '-' and ch != ','  and ch != '.' and ch != ' ' and not ch.isdigit():
           sNotFoundOK = 0
           break

      # Lookup the option among true defined options 
      err = LookupOption(argString[indStart:], sNextOption)

      # Process as an option unless it could be numeric and was not found
      if  not (sNotFoundOK and err == LOOKUP_NOT_FOUND):
         sNotFoundOK = 0
         if err < 0:
            return err

         sNumOptionArguments += 1

         # For an option with value, setup to get argument next time and
         # return an indicator that there had better be another
         if (sOptTable[err].type != 'B'):
            sNextArgBelongsTo = err
            return 1
         else:
           
            # for a boolean option, set the argument with a 1
            return AddValueString(err, '1')

   # A non-option argument
   sNotFoundOK = 0
   return AddValueString(sNonOptInd, argString)


# return number of option arguments (approximate) and number of non-option
# arguments 
#
def PipNumberOfArgs():
   global sOptTable, sNumOptionArguments, pipErrno
   pipErrno = 0
   return (sNumOptionArguments, sOptTable[sNonOptInd].count)

# Get a non-option argument, index numbered from 0 here
#
def PipGetNonOptionArg(argNo):
   global sOptTable, pipErrno
   pipErrno = 0
   if (argNo >= sOptTable[sNonOptInd].count):
      PipSetError("Requested a non-option argument beyond the number" + \
                  " available")
      pipErrno =  -1
      return None
   return sOptTable[sNonOptInd].values[argNo]


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
      sTempStr = "Illegal entry for boolean option " + option + ": " + strPtr
      PipSetError(sTempStr)
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
   global sNumOptions, sOptTable, sOutputManpage, sTypeForUsage, sTypes, sNumTypes
   global sTestAbbrevForUsage
   numOut = 0
   numReal = 0
   helplim = 74
   out = sys.stdout
   if useStdErr:
      out = sys.stderr
   indent4 = "    "
   linePos = 11
#    descriptions = sTypeDescriptions

   for i in range(sNumOptions):
      if (len(sOptTable[i].shortName) or len(sOptTable[i].longName)):
         numReal += 1
   
   if (not sOutputManpage):
      out.write("Usage: " + progName + " ")
      if (sNumOptions):
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
      out.write("Options can be abbreviated, current short name abbreviations are in " +\
            "parentheses\n")
      out.write("Options:\n")
      descriptions = sTypeForUsage

   sTestAbbrevForUsage = True
   for i in range(sNumOptions):
      
      sname = sOptTable[i].shortName
      lname = sOptTable[i].longName
      indentStr = ""

      # Try to look up an abbreviation of the short name
      abbrev = None
      if len(sname):
         for j in range(1, len(sname)):
            if LookupOption(sname[:j], sNumOptions) == i:
               abbrev = sname[:j]
               break
                               
      if (len(lname) or len(sname)):
         if (sOutputManpage <= 0):
            indentStr = indent4

         out.write(" ")
         if len(sname):
            out.write("-" + sname)
         if abbrev:
            out.write(" (-" + abbrev + ")")
         if len(sname) and len(lname):
            out.write("  OR  ")
         if len(lname):
            out.write("-" + lname)
         for j in range(sNumTypes):
            jj = j
            if (sOptTable[i].type == sTypes[j]):
               break
            
         if (sOptTable[i].type != "B"):
            format = descriptions[jj]
            if sOptTable[i].format:
               format = sOptTable[i].format.replace("\\fR", '').replace("\\fI", '').\
                   replace("\\fB", '')
            out.write("   " + format)

      out.write("\n")

      # Print help string, breaking up line as needed
      if len(sOptTable[i].helpString):
         sname = sOptTable[i].helpString
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

            out.write(indentStr + sname[0:j] + "\n")
            sname = sname[j+1:]
            newLinePt = sname.find('\n')
            optLen -= j + 1

         out.write(indentStr + sname + "\n")

      if (sOptTable[i].multiple):
         out.write(indentStr + "(Successive entries accumulate)\n")

   sTestAbbrevForUsage = False
   return 0


# Print the entries
def PipPrintEntries():
   global sNumOptions, sOptTable, sPrintEntries
   if sPrintEntries < 0:
      sPrintEntries = 0
      val = os.getenv(PRINTENTRY_VARIABLE)
      if val != None:
         try:
            sPrintEntries = int(val)
         except:
            sPrintEntries = 0
   if not sPrintEntries:
      return
   sys.stdout.write("\n*** Entries to program " + sProgramName + " ***" + "\n")
   for i in range(sNumOptions):
      sname = sOptTable[i].shortName
      lname = sOptTable[i].longName
      if (len(lname) or len(sname)) and sOptTable[i].count:
         name = lname
         if not len(lname):
            name = sname
         for j in range(sOptTable[i].count):
            sys.stdout.write("  " + name + " = " + sOptTable[i].values[j] + \
                             "\n")
   if sOptTable[sNonOptInd].count:
      sys.stdout.write("  Non-option arguments:")
      for j in range(sOptTable[sNonOptInd].count):
         sys.stdout.write("   " +  sOptTable[sNonOptInd].values[j])
      sys.stdout.write('\n')
   sys.stdout.write("*** End of entries ***\n\n")
   

# Return the error string, or an empty string and an error if there is none
#
def PipGetError():
   global sErrorString, pipErrno
   pipErrno = 0
   if (not sErrorString):
      pipErrno =  -1
      return ''
   return sErrorString


# Set the error string.
# If sExitPrefix is set, then output an error message to stderr or stdout
# and exit. 
def PipSetError(errString):
   global sErrorString, sExitPrefix
   outFile = sys.stdout
   if sErrorDest:
      outFile = stderr
   sErrorString = errString
   if not sErrorString and not sExitPrefix:
      pipErrno =  -1
      return None

   if sExitPrefix:
      if not sErrorString:
         sErrorString = "Unspecified error"
      outFile.write(sExitPrefix + sErrorString + "\n")
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
   global sNonOptInd, pipErrno
   pipErrno = 0
   err = LookupOption(option, sNonOptInd + 1)
   if (err < 0):
      pipErrno =  err
      return None
    
   return sOptTable[err].count


# Return the index of the next non-option arg or linked option that was entered after
# this option
#
def PipLinkedIndex(option):
   global sNumOptions, sLinkedOption, pipErrno, sNonOptInd
   pipErrno = 0
   err = LookupOption(option, sNumOptions)
   if (err < 0):
      pipErrno =  err
      return None

   ind = 0
   if sOptTable[err].multiple:
      ind = sOptTable[err].multiple - 1

   # Set up to use count from non-option args, but use the count from the linked option
   # instead if it was entered at all.  This allows other non-option args to be used
   which = 0
   if sLinkedOption != None:
      ilink = LookupOption(sLinkedOption, sNumOptions)
      if (ilink < 0):
         pipErrno =  ilink
         return None
      if sOptTable[ilink].count:
         which = 1
   return sOptTable[err].nextLinked[2 * ind + which]
                     

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


# Try to open an autodoc at AUTODOC_DIR or IMOD_DIR/autodoc
#
def PipOpenInstalledAdoc(progName):
   global OPTDIR_VARIABLE, PATH_SEPARATOR, OPTFILE_DIR, OPTFILE_EXT
   optFile = None
   pipDir = os.getenv(OPTDIR_VARIABLE)
   if (pipDir):
      bigStr = pipDir + PATH_SEPARATOR + progName + "." + OPTFILE_EXT
      try:
         # print "Looking for file " + bigStr
         optFile = open(bigStr, "r")
      except:
         optFile = None

   if (not optFile):
      pipDir = os.getenv("IMOD_DIR")
      if (pipDir):
         bigStr = pipDir + PATH_SEPARATOR + OPTFILE_DIR +\
                  PATH_SEPARATOR + progName + "." + OPTFILE_EXT
         try:
            #print "Looking for file " + bigStr
            optFile = open(bigStr, "r")
         except:
            optFile = None

   return optFile


# Alternative routine to have options read from a file
#
def PipReadOptionFile(progName, helpLevel, localDir):
   global PATH_SEPARATOR, OPTFILE_DIR, OPTFILE_EXT
   global sOptTable, sValueDelim, sProgramName
   optFile = None
   sProgramName = progName
   
   # If local directory not set, look for environment variable pointing
   # directly to where the file should be
   if (not localDir):
      optFile = PipOpenInstalledAdoc(progName)
  
   # If local directory set, set up name with ../ as many times as specified
   # and look for file there
   elif (localDir > 0):
      bigStr = ""
      for i in range(localDir):
         bigStr += ".." + PATH_SEPARATOR
         
         bigStr += OPTFILE_DIR + PATH_SEPARATOR + progName + "." + OPTFILE_EXT
         try:
            # print "Looking for file " + bigStr
            optFile = open(bigStr, "r")
         except:
            optFile = None

   
   # If there is still no file, look in current directory
   if (not optFile):
      bigStr = progName + "." + OPTFILE_EXT
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
   formatList = []
   longName = shortName = type = ''
   helpStr = ['','','']
   formatStr = ''
   readingOpt = 0
   inQuoteIndex = -1
   
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
         (newDelim, lastInd, inQuoteIndex) = CheckKeyword(textStr, "KeyValueDelimiter", 0)
         if (newDelim):
            sValueDelim = newDelim

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

         optStr = shortName + ":" + longName + ":" + type + ":" + \
                  helpStr[helpInd]
         optList.append(optStr)
         formatList.append(formatStr)

         # Clean up 
         longName = shortName = type = ''
         helpStr = ['','','']
         formatStr = ''
         readingOpt = 0

      if (lineLen == -3):
         break

      # If reading options, look for the various keywords
      if (readingOpt):

         # If the last string gotten was a help string and the line does not contain the
         # value delimiter or we are in a quote, then append it to the last string
         if lastInd and (inQuoteIndex >= 0 or textStr.find(sValueDelim) < 0):
            if helpStr[lastInd-1].endswith('.'):
               helpStr[lastInd-1] += '  '
            else:
               helpStr[lastInd-1] += ' '

            # Replace leading ^ with a newline
            if (textStr[0] == '^'):
               textStr = textStr.replace('^', '\n', 1)

            # If inside quotes, look for quote at end and say it is the end of accepting
            # continuation lines, and as a protection, also say a blank line ends it
            lentx = len(textStr)
            if inQuoteIndex >= 0 and \
                   (not lentx or textStr[lentx - 1] == sQuoteTypes[inQuoteIndex]):
               if lentx:
                  helpStr[lastInd-1] += textStr[:lentx - 1]
               lastInd = 0
               inQuoteIndex = -1
            else:
               helpStr[lastInd-1] += textStr

         # Otherwise look for each keyword of interest, but zero last index
         else:
            lastInd = 0
            retval = CheckKeyword(textStr, "short", 0)
            if retval[0]:
               (shortName, lastInd, inQuoteIndex) = retval
            retval = CheckKeyword(textStr, "long", 0)
            if retval[0]:
               (longName, lastInd, inQuoteIndex) = retval
            retval = CheckKeyword(textStr, "type", 0)
            if retval[0]:
               (type, lastInd, inQuoteIndex) = retval
            retval = CheckKeyword(textStr, "format", 0)
            if retval[0]:
               (formatStr, lastInd, inQuoteIndex) = retval

            # Check for usage if at help level 1 or if we haven't got
            # either of the other strings yet
            if (helpLevel <= 1 or not (helpStr[1] or helpStr[2])):
               retval = CheckKeyword(textStr, "usage", 1, 1)
               if retval[0]:
                  (helpStr[0], lastInd, inQuoteIndex) = retval
      
            # Check for tooltip if at level 2 or if at level 1 and haven't 
            # got usage, or at level 3 and haven't got manpage
            if (helpLevel == 2 or (helpLevel <= 1 and not helpStr[0]) or
               (helpLevel >= 3 and not helpStr[2])):
               retval = CheckKeyword(textStr, "tooltip", 2, 1)
               if retval[0]:
                  (helpStr[1], lastInd, inQuoteIndex) = retval
      
            # Check for manpage if at level 3 or if at level 2 and haven't 
            # got tip, or at level 1 and haven't got tip or usage
            if (helpLevel >= 3 or (helpLevel == 2 and not helpStr[1]) or
               (helpLevel <= 1 and not (helpStr[1] or helpStr[0]))):
               retval = CheckKeyword(textStr, "manpage", 3, 1)
               if retval[0]:
                  (helpStr[2], lastInd, inQuoteIndex) = retval

            # If that was a line with quoted string, check for quote at end of line and
            # close out the help string if so */
            if inQuoteIndex >= 0 and lastInd:
               lentx = len(helpStr[lastInd - 1])
               if lentx and helpStr[lastInd - 1][lentx - 1] == sQuoteTypes[inQuoteIndex]:
                  helpStr[lastInd - 1] = helpStr[lastInd - 1][:lentx - 1]
                  inQuoteIndex = -1
                  lastInd = 0
                  
      # But if not reading options, check for a new option token and start 
      # reading if one is found.  But first take a Field value as default 
      # long option name
      elif (isOption > 0):
         lastInd = 0
         inQuoteIndex = -1
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
      sOptTable[sNextOption - 1].format = formatList[i]

   return 0
                              

# Routine to parse the entries in command line after options have been
# defined one way or another
#
def PipParseEntries(argv):
   global sTakeStdIn, pipErrno
   pipErrno = 0
   argc = len(argv)

   # Special case: no arguments and flag set to take stdin automatically
   if (not argc and sTakeStdIn):
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

   PipPrintEntries()
   return PipNumberOfArgs()


# High-level routine to initialize from autodoc with optional fallback options
# Set exit string and output to stdout, print usage if not enough arguments
#
def PipReadOrParseOptions(argv, options, progName, minArgs, numInFiles,
                          numOutFiles):
   global sExitPrefix
   
   # Startup with fallback
   ierr = PipReadOptionFile(progName, 0, 0)
   PipExitOnError(0, "ERROR: " + progName + " - ")
   if (not ierr):
      (numOptArgs, numNonOptArgs) = PipParseEntries(argv)
   else:
      errString = PipGetError()
      if (not options or not len(options)):
         PipSetError(errString)
      if (errString):
          sys.stdout.write("PIP WARNING: " + errString + \
                           "\nUsing fallback options in main program\n\n")

      (numOptArgs, numNonOptArgs) = PipParseInput(argv, options)


   # Output usage and exit if not enough arguments
   exitSave = sExitPrefix
   sExitPrefix = None
   help = PipGetBoolean('help', 0)
   sExitPrefix = exitSave
   if (help or numOptArgs + numNonOptArgs < minArgs):
      PipPrintHelp(progName, 0, numInFiles, numOutFiles)
      sys.exit(0)

   return (numOptArgs, numNonOptArgs)


# Routine to get input/output file from parameter or non-option args
#
def PipGetInOutFile(option, nonOptArgNo):
   global sOptTable, pipErrno
   retval = PipGetString(option, '')
   if not pipErrno:
      return retval
   if (nonOptArgNo >= sOptTable[sNonOptInd].count):
      pipErrno =  1
      return None
   return PipGetNonOptionArg(nonOptArgNo)


# Read successive lines from a parameter file or standard input, and 
# store as options and values 
#
def ReadParamFile(pFile):
   global sNotFoundOK, sNumOptionArguments, sOptTable, sNonOptLines, tokenSep
   global STANDARD_INPUT_STRING, STANDARD_INPUT_END, sNumOptions
   while (1):

      # If non-option lines are allowed, set flag that it is OK for
      # LookupOption to not find the option, but only for the given number
      # of lines at the start of the input
      sNotFoundOK = not sNumOptionArguments and \
                   (sOptTable[sNonOptInd].count < sNonOptLines)
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
      if (token == STANDARD_INPUT_END) or (sDoneEnds and token == "DONE"):
         break

      # Look up option
      optNum = LookupOption(token, sNumOptions)
      if (optNum < 0):

         # If no option, process special case if in-line non-options allowed
         # or error out
         if (sNotFoundOK):
            err = AddValueString(sNonOptInd, lineStr[indst:])
            if err:
               return err
            continue
         else:
            return optNum

      if (sOptTable[optNum].type == "PF"):
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
      elif (sOptTable[optNum].type == 'B'):
         token = "1"
      else:
         PipSetError("Missing a value on the input line:  " + lineStr)
         return -1

      # Add the token as a value string and increment argument number
      err = AddValueString(optNum, token)
      if err:
         return err
      sNumOptionArguments += 1

   sNotFoundOK = 0
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
   global valueSep, sAllowDefaults, PIP_INTEGER, PIP_FLOAT
   global pipErrno
   pipErrno = 0
   fullStr = strPtr
   array = []
   numGot = 0
   gotComma = 0

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
            if (sAllowDefaults and numToGet):
               numGot = numToGet
               break
      
            sTempStr = "Default entry with a / is not allowed in value entry" +\
                      ":  "+ option + "  " + fullStr
            PipSetError(sTempStr)
            pipErrno =  -1
            return None

         # special handling of commas to allow default values
         if (strPtr[0] == ','):

            # If already have a comma, skip an array value if defaults
            # allowed
            if (gotComma):
               if (sAllowDefaults and numToGet):
                  numGot += 1
                  if (numGot >= numToGet):
                     break
               else:
                  sTempStr = "Default entries with commas are not allowed " +\
                            "in value entry:  " + option + "  " + fullStr
                  PipSetError(sTempStr)
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
         sTempStr = "Illegal character in value entry:  " + \
                   option + "  " + fullStr
         PipSetError(sTempStr)
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
      sTempStr = str(numToGet) + " values expected but only " + str(numGot) + \
                " values found in value entry:  " + option + "  " + fullStr
      PipSetError(sTempStr)
      pipErrno =  -1
      return None
  
   return array


# Get a pointer to the value string for the given option.
# Return < 0 if the option is invalid, 1 if the option was not entered.
# If the option allows multiple values, advance the multiple counter
#
def GetNextValueString(option):
   global sOptTable, sNonOptInd, pipErrno
   pipErrno = 0
   err = LookupOption(option, sNonOptInd + 1)
   optp = sOptTable[err]
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
   global sOptTable, pipErrno
   optp = sOptTable[optInd]

   # Add the index of the next non-option arg and the index of a linked option if
   # one is defined to the array for these
   if optp.linked:
      optp.nextLinked.append(sOptTable[sNonOptInd].count)
      ind = 0
      if sLinkedOption:
         err = LookupOption(sLinkedOption, sNumOptions)
         if (err < 0):
            pipErrno =  err
            return None
         ind = sOptTable[err].count
      optp.nextLinked.append(ind)
   
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
   global sOptTable, LOOKUP_NOT_FOUND, LOOKUP_AMBIGUOUS, sNotFoundOK, sNoAbbrevs
   lenopt = len(option)
   found = LOOKUP_NOT_FOUND

   # Look at all of the options specified by maxLookup
   for i in range(maxLookup):
      sname = sOptTable[i].shortName
      lname = sOptTable[i].longName
      lenShort = sOptTable[i].lenShort
      starts = PipStartsWith(sname, option)

      # First test for single letter short name match - if it passes, skip ambiguity test
      if lenopt == 1 and starts and lenShort == 1:
         found = i;
         break
      
      if (starts and  (not sNoAbbrevs or lenopt == lenShort)) or \
             (PipStartsWith(lname, option) and (not sNoAbbrevs or lenopt == len(lname))):
        
         # If it is found, it's an error if one has already been found
         if (found == LOOKUP_NOT_FOUND):
            found = i
         else:
            if not sTestAbbrevForUsage:
               sTempStr = "An option specified by \"" + option + \
                   "\" is ambiguous between option " + sname + " -  " + \
                   lname + "  and option " +  sOptTable[found].shortName + \
                   " -  " + sOptTable[found].longName
               PipSetError(sTempStr)
            return LOOKUP_AMBIGUOUS
         
   # Set error string unless flag set that non-options are OK 
   if (found == LOOKUP_NOT_FOUND and not sNotFoundOK):
      sTempStr = "Illegal option: " + option
      PipSetError(sTempStr)
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
# value string and the supplied index number, otherwise blank string and 0.
# If quoteOK is non-zero, it sees if the string starts with a quote character in
# sQuoteTypes, strips that, and returns the index in the third element
#
def CheckKeyword(line, keyword, index, quoteOK = 0):
   global sValueDelim
   lineLen = len(line)
   
   # First make sure line starts with it
   if (not PipStartsWith(line, keyword)):
      return ('', 0, -1)

   # Now look for delimiter 
   valStart = line.find(sValueDelim)
   if (valStart < 0):
      return ('', 0, -1)

   # Eat spaces after the delimiter and return if nothing left
   # In other words, a key with no value is the same as having no key at all 
   valStart += len(sValueDelim)
   while (valStart < lineLen and (line[valStart] == ' ' or
         line[valStart] == '\t')):
      valStart += 1
   if (valStart >= lineLen):
      return ('', 0, -1)

   # Look for quote if directed to
   indQuote = -1
   if quoteOK and line[valStart] in sQuoteTypes:
      indQuote = sQuoteTypes.find(line[valStart])
      valStart += 1

   return (line[valStart:], index, indQuote)
