/*   parse_params.cpp  -  the PIP package for parsing input parameters
 *
 *   Copyright (C) 2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.3  2003/06/10 23:21:03  mast
Add File name type

Revision 3.2  2003/06/05 03:01:48  mast
Adding return values  - error caught on SGI

Revision 3.1  2003/06/05 00:22:49  mast
Addition to IMOD

*/

#include "parse_params.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>


#define NON_OPTION_STRING "NonOptionArgument"
#define STANDARD_INPUT_STRING "StandardInput"
#define STANDARD_INPUT_END  "EndInput"
#define PARAM_FILE_STRING  "PF"
#define BOOLEAN_STRING    "B"
#define LOOKUP_NOT_FOUND -1
#define LOOKUP_AMBIGUOUS -2
#define INTEGER_TYPE  1
#define FLOAT_TYPE    2
#define TEMP_STR_SIZE  1024
#define LINE_STR_SIZE  10240

/* The structure for storing the options and the arguments as they are
   parsed */
typedef struct pipOptions {
  char *shortName;      /* Short option name */
  char *longName;       /* Long option name */
  char *type;           /* Type string */
  char *helpString;     /* Help string */
  char **valuePtr;      /* pointer to array of string pointers with values */
  int multiple;         /* 0 if single value allowed, or number of next one
			   being returned (numbered from 1) */
  int count;            /* Number of values accumulated */
} PipOptions;

static char *types[] = {BOOLEAN_STRING, PARAM_FILE_STRING,
			"LI", "I", "F", "IA", "FA", "CH", "FN"};
static char *typeDescriptions[] = {
  "Boolean",
  "Parameter file",
  "List of integer ranges",
  "Integer",
  "Floating point",
  "Multiple integers",
  "Multiple floats",
  "Text string",
  "File name",
  "Unknown argument type"
};
static char numTypes = 8;

static char nullChar = 0x00;
static char *nullString = &nullChar;

/* declarations of local functions */
static int ReadParamFile(FILE *pFile);
static int GetLineOfValues(char *option, void *array, int valType, 
			   int *numToGet, int arraySize);
static int GetNextValueString(char *option, char **strPtr);
static int AddValueString(int option, char *strPtr);
static int LookupOption(char *option, int maxLookup);
static char *PipSubStrDup(char *s1, int i1, int i2);
static void AppendToErrorString(char *str);
static int StartsWith(char *fullStr, char *subStr);


/* The static tables and variables */
static PipOptions *optTable = NULL;
static int tableSize = 0;
static int numOptions = 0;
static int nonOptInd;
static char *errorString = NULL;
static char *exitPrefix = NULL;
static int errorDest = 0;
static int nextOption = 0;
static int nextArgBelongsTo = -1;
static int numOptionArguments = 0;
static char *tempStr;
static char *lineStr;
static int allowDefaults = 0;

int PipInitialize(int numOpts)
{
  int i;

  tempStr = (char *)malloc(TEMP_STR_SIZE);
  lineStr = (char *)malloc(LINE_STR_SIZE);

  /* Make the table big enough for extra entries (NonOptionArgs) */
  numOptions = numOpts;
  tableSize = numOpts + 2;
  nonOptInd = numOptions;
  optTable = (PipOptions *)malloc(tableSize * sizeof(PipOptions));
  if (!tempStr || !lineStr || !optTable) {
    PipMemoryError(NULL, "PipInitialize");
    return -1;
  }

  /* Initialize the table */
  for (i = 0; i < tableSize; i++) {
    optTable[i].shortName = NULL;
    optTable[i].longName = NULL;
    optTable[i].type = NULL;
    optTable[i].helpString = NULL;
    optTable[i].valuePtr = NULL;
    optTable[i].multiple = 0;
    optTable[i].count = 0;
  }

  /* In the last slots, put non-option arguments, and also put the
     name for the standard input option for easy checking on duplication */
  optTable[nonOptInd].longName = strdup(NON_OPTION_STRING);
  optTable[nonOptInd + 1].shortName = strdup(STANDARD_INPUT_STRING);
  optTable[nonOptInd + 1].longName = strdup(STANDARD_INPUT_END);
  if (!optTable[nonOptInd].longName || !optTable[nonOptInd + 1].shortName ||
      !optTable[nonOptInd + 1].longName) {
    PipMemoryError(NULL, "PipInitialize");
    return -1;
  }
  optTable[nonOptInd].multiple = 1;

  return 0;
}

/* Free all allocated memory and set state back to initial state */
void PipDone(void)
{
  int i, j;
  PipOptions *optp;
  for (i = 0; i < tableSize; i++) {
    optp = &optTable[i];
    if (optp->shortName)
      free(optp->shortName);
    if (optp->longName)
      free(optp->longName);
    if (optp->type)
      free(optp->type);
    if (optp->helpString)
      free(optp->helpString);
    if (optp->valuePtr) {
      for (j = 0; j < optp->count; j++)
	if (optp->valuePtr[j])
	  free(optp->valuePtr[j]);
      free(optp->valuePtr);
    }
  }
  if (optTable)
    free(optTable);
  optTable = NULL;
  tableSize = 0;
  numOptions = 0;
  if (errorString)
    free(errorString);
  errorString = NULL;
  if (exitPrefix)
    free(exitPrefix);
  exitPrefix = NULL;
  nextOption = 0;
  nextArgBelongsTo = -1;
  numOptionArguments = 0;
  allowDefaults = 0;
  if (tempStr)
    free(tempStr);
  tempStr = NULL;
  if (lineStr)
    free(lineStr);
  lineStr = NULL;
}

/* Set up for Pip to handle exiting on error, with a prefix string */
int PipExitOnError(int useStdErr, char *prefix)
{
  /* Get rid of existing string; and if called with null string,
     then cancel an existing exit on error */
  if (exitPrefix)
    free(exitPrefix);
  exitPrefix = NULL;
    
  if (!prefix || !*prefix)
    return 0;
    
  errorDest = useStdErr;
  exitPrefix = strdup(prefix);
  if (!exitPrefix) {
    fprintf(useStdErr ? stderr : stdout, "Failure to get memory in "
	    "PipExitOnError");
    exit(1);
  }
  return 0;
}

void PipAllowCommaDefaults(int val)
{
  allowDefaults = val;
}

/* Add an option, with short and long name, type, and help string */
int PipAddOption(char *optionString)
{
  int ind, indEnd;
  PipOptions *optp = &optTable[nextOption];
  char *colonPtr;
  char *oldShort;
  char *oldLong;
  char *newShort;
  char *newLong;
  char *subStr = optionString;

  if (nextOption >= numOptions) {
    PipSetError("Attempting to add more options than were originally"
		 " specified");
    return -1;
  }

  /* In the following, if there is ever not another :, skip to error */
  /* get the short name */
  colonPtr = strchr(subStr, ':');
  if (colonPtr) {

    indEnd = colonPtr - subStr;
    if (indEnd > 0)
      optp->shortName = PipSubStrDup(subStr, 0, indEnd - 1);
    else
      optp->shortName = strdup(nullString);
    if (PipMemoryError(optp->shortName, "PipAddOption"))
      return -1;
    subStr += indEnd + 1;
    
    /* Get the long name */
    colonPtr = strchr(subStr, ':');
    if (colonPtr) {
      if (PipMemoryError(colonPtr, "PipAddOption"))
	return -1;
      indEnd = colonPtr - subStr;
      if (indEnd > 0)
	optp->longName = PipSubStrDup(subStr, 0, indEnd - 1);
      else
	optp->longName = strdup(nullString);
      if (PipMemoryError(optp->longName, "PipAddOption"))
	return -1;
      subStr += indEnd + 1;

      /* Get the type and if there is M at the end, trim it off and set
	 multiple flag to 1 */
      colonPtr = strchr(subStr, ':');
      if (colonPtr) {
	if (PipMemoryError(colonPtr, "PipAddOption"))
	  return -1;
	indEnd = colonPtr - subStr;
	if (indEnd > 0) {
	  ind = indEnd - 1;
	  if (subStr[ind] == 'M') {
	    optp->multiple = 1;
	    ind--;
	  }
	  optp->type = PipSubStrDup(subStr, 0, ind);
	} else
	  optp->type = strdup(nullString);
	if (PipMemoryError(optp->type, "PipAddOption"))
	  return -1;
	subStr += indEnd + 1;

	/* Now if there is anything left, it is the help string */
	optp->helpString = strdup(subStr);
	if (PipMemoryError(optp->helpString, "PipAddOption"))
	  return -1;

	/* Need to check the short and long names against all previous
	   names and special names */
	newShort = optp->shortName;
	newLong = optp->longName;
	for (ind = 0; ind < tableSize; ind++) {

	  /* after checking existing ones, skip to NonOptionArg and 
	     StandardInput entries */
	  if (ind >= nextOption && ind < nonOptInd)
	    continue;

	  oldShort = optTable[ind].shortName;
	  oldLong = optTable[ind].longName;
	  if (StartsWith(newShort, oldShort) || 
	      StartsWith(oldShort, newShort) ||
	      StartsWith(oldLong, newShort) ||
	      StartsWith(newShort, oldLong) ||
	      StartsWith(oldShort, newLong) ||
	      StartsWith(newLong, oldShort) ||
	      StartsWith(oldLong, newLong) ||
	      StartsWith(newLong, oldLong)) {
	    sprintf(tempStr, "Option %s  %s is ambiguous with option %s"
		    "  %s", newShort, newLong, oldShort, oldLong);
	    PipSetError(tempStr);
	    return -1;
	  }
	}	      

	nextOption++;
	return 0;
      }
    }
  }

  sprintf(tempStr, "Option does not have three colons in it:\n");
  AppendToErrorString(optionString);
  return -1;
}

/* Call this to process the next argument */
int PipNextArg(char *argString)
{
  char *argCopy;
  int err;
  FILE *paramFile;
  int indStart;

  /* If we are expecting a value for an option, duplicate string and add
     it to the option */
  if (nextArgBelongsTo >= 0) {
    argCopy = strdup(argString);
    if (PipMemoryError(argCopy, "PipNextArg"))
      return -1;
    err = AddValueString(nextArgBelongsTo, argCopy);

    /* Check whether this option was for reading from parameter file */
    if (!err && !strcmp(optTable[nextArgBelongsTo].type, PARAM_FILE_STRING)) {
      paramFile = fopen(argCopy, "r");
      if (paramFile) {
	err = ReadParamFile(paramFile);
	fclose(paramFile);
      } else {
	sprintf(tempStr, "Error opening parameter file %s", argCopy);
	PipSetError(tempStr);
	err = -1;
      }		   
    }

    nextArgBelongsTo = -1;
    return err;
  }

  /* Is it a legal option starting with - or -- ? */
  if (argString[0] == '-') {
    indStart = 1;
    if (strlen(argString) > 1 && argString[1] == '-')
      indStart = 2;
    if (strlen(argString) == indStart) {
      PipSetError("Illegal argument: - or --");
      return -1;
    }
    numOptionArguments++;

    /* First check for StandardInput */
    if (StartsWith(STANDARD_INPUT_STRING, argString + indStart)) {
      err = ReadParamFile(stdin);
      return err;
    }

    /* Lookup the option among true defined options */
    err = LookupOption(argString + indStart, nextOption);
    if (err < 0)
      return err;

    /* For an option with value, setup to get argument next time and
       return an indicator that there had better be another */
    if (strcmp(BOOLEAN_STRING, optTable[err].type)) {
      nextArgBelongsTo = err;
      return 1;
    } else {
      
      /* for a boolean option, set the argument with a 1 */
      argCopy = strdup("1");
      if (PipMemoryError(argCopy, "PipNextArg"))
	return -1;
      return AddValueString(err, argCopy);
    }
  }

  /* A non-option argument */
  argCopy = strdup(argString);
  if (PipMemoryError(argCopy, "PipNextArg"))
    return -1;
  return AddValueString(nonOptInd, argCopy);
}

/* return number of option arguments (approximate) and number of non-option
   arguments */
void PipNumberOfArgs(int *numOptArgs, int *numNonOptArgs)
{
  *numOptArgs = numOptionArguments;
  *numNonOptArgs = optTable[nonOptInd].count;
}

/* Get a non-option argument, index numbered from 0 here */
int PipGetNonOptionArg(int argNo, char **arg)
{
  if (argNo >= optTable[nonOptInd].count) {
    PipSetError("Requested a non-option argument beyond the number available");
    return -1;
  }
  *arg = strdup(optTable[nonOptInd].valuePtr[argNo]);
  return PipMemoryError(*arg, "PipGetNonOptionArg");
}

/* Get a string option */
int PipGetString(char *option, char **string)
{
  char *strPtr;
  int err;
  if ((err = GetNextValueString(option, &strPtr)))
    return err;
  *string  = strdup(strPtr);
  return PipMemoryError(*string, "PipGetString");
}

/* Get a boolean (binary) option; make sure it has a legal specification */
int PipGetBoolean(char *option, int *val)
{
  char *strPtr;
  int err;
  if ((err = GetNextValueString(option, &strPtr)))
    return err;
  if (!strcmp(strPtr, "1") || !strcasecmp(strPtr, "T") || 
      !strcasecmp(strPtr, "TRUE") || !strcasecmp(strPtr, "ON"))
    *val = 1;
  else if (!strcmp(strPtr, "0") || !strcasecmp(strPtr, "F") || 
	   !strcasecmp(strPtr, "FALSE") || !strcasecmp(strPtr, "OFF"))
    *val = 0;
  else {
    sprintf(tempStr, "Illegal entry for boolean option %s: %s", option, 
	    strPtr);
    PipSetError(tempStr);
    return -1;
  }
  return 0;
}

/* Get single integer and float just call to get an array with one element */
int PipGetInteger(char *option, int *val)
{
  int num = 1;
  return PipGetIntegerArray(option, val, &num, 1);
}

int PipGetFloat(char *option, float *val)
{
  int num = 1;
  return PipGetFloatArray(option, val, &num, 1);
}

/* Getting an array of integers or floats calls the general routine for
   getting a line of values */
int PipGetIntegerArray(char *option, int *array, int *numToGet, int arraySize)
{
  return GetLineOfValues(option, (void *)array, INTEGER_TYPE, numToGet,
			 arraySize);
}

int PipGetFloatArray(char *option, float *array, int *numToGet, int arraySize)
{
  return GetLineOfValues(option, (void *)array, FLOAT_TYPE, numToGet,
			 arraySize);
}

/* Print a complete usage statement */
int PipPrintHelp(char *progName, int useStdErr, int inputFiles, 
		 int outputFiles)
{
  int i, j;
  int helplim = 74;
  char *sname, *lname;
  FILE *out = useStdErr ? stderr : stdout;
  fprintf(out, "Usage: %s ", progName);
  if (numOptions)
    fprintf(out, "[Options]");
  if (inputFiles)
    fprintf(out, " input_file");
  if (inputFiles > 1)
    fprintf(out, "s...");
  if (outputFiles)
    fprintf(out, " output_file");
  if (outputFiles > 1)
    fprintf(out, "s...");
  fprintf(out,"\n");
  if (!numOptions)
    return 0;
  fprintf(out, "Options:\n");
  for (i = 0; i < numOptions; i++) {
    sname = optTable[i].shortName;
    lname = optTable[i].longName;
    if (!lname && !sname)
      continue;
    /*   if (i)
	 fprintf(out, "\n"); */
    fprintf(out, " ");
    if (sname && *sname)
      fprintf(out, "-%s", sname);
    if (sname && *sname && lname && *lname)
      fprintf(out," OR ");
    if (lname && *lname)
      fprintf(out, "-%s", lname);
    for (j = 0; j < numTypes; j++)
      if (!strcmp(optTable[i].type, types[j]))
	break;
    if (!strcmp(optTable[i].type, BOOLEAN_STRING))
      fprintf(out, "   (%s entry, no value expected)", typeDescriptions[j]);
    else
      fprintf(out, "   %s", typeDescriptions[j]);
    fprintf(out, "\n");

    /* Print help string, breaking up line as needed */
    if (optTable[i].helpString && *optTable[i].helpString) {
      lname = strdup(optTable[i].helpString);
      if (PipMemoryError(lname, "PipPrintHelp"))
        return -1;
      sname = lname;
      while (strlen(sname) > helplim) {
        for (j = helplim; j >= 1; j--)
          if (sname[j] == ' ')
            break;
        sname[j] = 0x00;
        fprintf(out, "    %s\n", sname);
        sname += j + 1;
      }
      fprintf(out, "    %s\n", sname);
      free(lname);
    }

    if (optTable[i].multiple)
      fprintf(out, "    (Successive entries accumulate)\n");
  }
  return 0;
}

/* Return the error string, or an empty string and an error if there is none */
int PipGetError(char **errString)
{
  if (!errorString) {
    *errString = strdup(nullString);
    PipMemoryError(*errString, "PipGetError");
    return -1;
  }
  *errString = strdup(errorString);
  return PipMemoryError(*errString, "PipGetError");
}

/* Set the error string.
   If exitPrefix is set, then output an error message to stderr or stdout
   and exit. */
int PipSetError(char *errString)
{
  FILE *outFile = errorDest ? stderr : stdout;
  if (errorString)
     free(errorString);
  errorString = strdup(errString);
  if (!errorString && !exitPrefix)
    return -1;

  if (exitPrefix) {
    fprintf(outFile, "%s ", exitPrefix);
    fprintf(outFile, "%s\n", errorString ? errorString : "Unspecified error");
    exit(1);
  }
  return 0;
}

/* Return the number of entries for a particular option */
int PipNumberOfEntries(char *option, int *numEntries)
{
  int err;
  if ((err = LookupOption(option, nonOptInd + 1)) < 0)
    return err;
  *numEntries = optTable[err].count;
  return 0;
}

/* Top level routine to be called to process options and arguments */
int PipParseInput(int argc, char *argv[], char *options[], int numOptions,
		  int *numOptArgs, int *numNonOptArgs)
{
  int i, err;

  /* Initialize */
  if ((err = PipInitialize(numOptions)))
    return err;

  /* add the options */
  for (i = 0; i < numOptions; i++)
    if ((err = PipAddOption(options[i])))
      return err;

  /* parse the arguments */
  for (i = 1; i < argc; i++) {
    if ((err = PipNextArg(argv[i])) < 0)
      return err;
    if (err && i == argc - 1) {
      PipSetError("A value was expected but not found for"
		  " the last option on the command line");
      return -1;
    }
  }
  PipNumberOfArgs(numOptArgs, numNonOptArgs);
  return 0;
}

/* Read successive lines from a parameter file or standard input, and 
   store as options and values */
static int ReadParamFile(FILE *pFile)
{
  int lineLen;
  int indst, indnd, optNum, gotEquals, err;
  char *strPtr;
  char *token;
  char ch;
  
  while (1) {

    /* read a line, or as much as fits in lineStr */
    if (fgets(lineStr, LINE_STR_SIZE, pFile) == NULL) {

      /* If error, it's OK if it's an EOF, or an error otherwise */
      if (feof(pFile))
	break;
      PipSetError("Error reading parameter file or "STANDARD_INPUT_STRING);
      return -1;
    }

    /* check for line too long */
    lineLen = strlen(lineStr);
    if (lineLen == LINE_STR_SIZE - 1) {
      PipSetError("Line too long for buffer while reading parameter file or "
		  STANDARD_INPUT_STRING);
      return -1;
    }

    /* adjust line length if there is a comment sign, then back further
     if there is white space (also remove newline) */
    strPtr = strchr(lineStr, '#');
    if (strPtr)
      lineLen = strPtr - lineStr;

    while (lineLen > 0) {
      ch = lineStr[lineLen - 1]; 
      if (ch != ' ' && ch != '\t' && ch != '\n')
	break;
      lineLen--;
    }

    /* Get first non-white space */
    indst = 0;
    while (indst < lineLen) {
      if (lineStr[indst] != ' ' && lineStr[indst] != '\t')
	break;
      indst++;
    }

    /* Nothing left on line? skip */
    if (indst == lineLen)
      continue;
      
    /* Find token and make a copy */
    strPtr = strpbrk(lineStr + indst, "= \t");
    indnd = strPtr ? (strPtr - lineStr) - 1 : lineLen - 1;
    if (indnd >= lineLen)
      indnd = lineLen - 1;

    token = PipSubStrDup(lineStr, indst, indnd);
    if (PipMemoryError(token, "ReadParamFile"))
      return -1;

    /* Done if it matches end of input string */
    if (!strcmp(token, STANDARD_INPUT_END))
      break;

    /* Look up option and free the token string */
    if ((optNum = LookupOption(token, numOptions)) < 0)
      return optNum;
    free(token);

    if (!strcmp(optTable[optNum].type, PARAM_FILE_STRING)) {
      PipSetError("Trying to open a parameter file while reading a parameter"
		  " file or "STANDARD_INPUT_STRING);
      return -1;
    }

    /* Find first non-white space, passing over at most one equals sign */
    indst = indnd + 1;
    gotEquals = 0;
    while (indst < lineLen) {
      if (lineStr[indst] == '=') {
	if (gotEquals) {
	  sprintf(tempStr, "Two = signs in input line:\n");
	  AppendToErrorString(lineStr);
	  return -1;
	}
	gotEquals = 1;
      } else if (lineStr[indst] != ' ' && lineStr[indst] != '\t')
	break;
      indst++;
    }

    /* If there is a string, get one; if not, get a "1" for boolean, otherwise
       it is an error */
    if (indst < lineLen)
      token = PipSubStrDup(lineStr, indst, lineLen - 1);
    else if (!strcmp(optTable[optNum].type, BOOLEAN_STRING))
      token = strdup("1");
    else {
      sprintf(tempStr, "Missing a value on the input line:\n");
      AppendToErrorString(lineStr);
      return -1;
    }
    if (PipMemoryError(token, "ReadParamFile"))
      return -1;

    /* Add the token as a value string */
    if ((err = AddValueString(optNum, token)))
      return err;
  }
  return 0;
}

/* Parse a line of values and return them into an array */
static int GetLineOfValues(char *option, void *array, int valType, 
			   int *numToGet, int arraySize)
{
  char *strPtr;
  char *sepPtr;
  char *endPtr;
  char *invalid;
  char *fullStr;
  int *iarray = (int *)array;
  float *farray = (float *)array;
  int err, nChar;
  int numGot = 0;
  int gotComma = 1;
  char sepStr[] = ",\t /";

  /* Get string  and save pointer to it for error messages */
  if ((err = GetNextValueString(option, &strPtr)))
    return err;
  fullStr = strPtr;

  while (strlen(strPtr)) {
    sepPtr = strpbrk(strPtr, sepStr);
    if (!sepPtr) {

      /* null pointer means read a number to end of string */
      endPtr = strPtr + strlen(strPtr);

    } else if (sepPtr == strPtr) {

      /* separator at start means advance by one byte and continue */
      /* if defaults allowed and a specific number are expected,
	 / means stop processing and mark all values as received */
      if (strPtr[0] == '/') {
	if (allowDefaults && *numToGet) {
	  numGot = *numToGet;
	  break;
	}
	sprintf(tempStr, "Default entry with a / is not allowed"
		" in value entry:\n");
	AppendToErrorString(fullStr);
	return -1;
      }

      /* special handling of commas to allow default values */
      if (strPtr[0] == ',') {

	/* If already have a comma, skip an array value if defaults allowed */
	if (gotComma) {
	  if (allowDefaults && *numToGet) {
	    numGot++;
	    if (numGot >= *numToGet)
	      break;
	  } else {
	    sprintf(tempStr, "Default entries with commas are not allowed"
		    " in value entry:\n");
	    AppendToErrorString(fullStr);
	    return -1;
	  }
	}
	gotComma = 1;
      }
      strPtr++;
      continue;

    } else {
      /* otherwise, this should be the end pointer in the strto[ld] call */
      endPtr = sepPtr;
    }

    /* If we are already full, then it is an error */
    if (numGot >= arraySize) {
      sprintf(tempStr, "Too many values for input array in value entry:\n");
      AppendToErrorString(fullStr);
      return -1;
    }

    /* convert number, get pointer  to first invalid char */
    if (valType == INTEGER_TYPE)
      iarray[numGot++] = strtol(strPtr, &invalid, 10);
    else
      farray[numGot++] = (float)strtod(strPtr, &invalid);

    /* If invalid character is before end character, it is an error */
    if (invalid != endPtr) {
      sprintf(tempStr, "Illegal character in value entry:\n");
      AppendToErrorString(fullStr);
      return -1;
    }

    /* Mark that there is no comma after we have a number */
    gotComma = 0;

    /* Done if at end of line, or if count is fulfilled */
    if (!*endPtr || (*numToGet && numGot >= *numToGet))
      break;
    
    /* Otherwise advance to separator and continue */
    strPtr = endPtr;
  }

  /* return number actually gotten if it was left open */
  if (!*numToGet)
    *numToGet = numGot;

  /* If not enough values found, return error */
  if (numGot < *numToGet) {
    sprintf(tempStr, "%d values expected but only %d values found in value "
	    "entry\n", *numToGet, numGot);
    AppendToErrorString(fullStr);
    return -1;
  }

  return 0;
}

/* Get a pointer to the value string for the given option.
   Return < 0 if the option is invalid, 1 if the option was not entered.
   If the option allows multiple values, advance the multiple counter */
static int GetNextValueString(char *option, char **strPtr)
{
  int err;
  int index = 0;
  if ((err = LookupOption(option, nonOptInd + 1)) < 0)
    return err;
  if (!optTable[err].count)
    return 1;
  if (optTable[err].multiple) {
    index = optTable[err].multiple - 1;
    if (optTable[err].multiple < optTable[err].count)
      optTable[err].multiple++;
  }
  *strPtr = optTable[err].valuePtr[index];
  return 0;
}

/* Add a string to the set of values for an option */
static int AddValueString(int option, char *strPtr)
{
  PipOptions *optp = &optTable[option];

  /* If the count is zero or we accept multiple values, need to allocate
     array for address of string */
  if (!optp->count || optp->multiple) {
    if (optp->count)
      optp->valuePtr = (char **)realloc(optp->valuePtr, (optp->count + 1) *
					sizeof(char *));
    else
      optp->valuePtr = (char **)malloc(sizeof(char *));
    if (PipMemoryError(optp->valuePtr, "AddValueString"))
      return -1;
  } else {

    /* otherwise, need to free existing value */
    if (*optp->valuePtr)
      free (*optp->valuePtr);
    optp->count = 0;
  }
  /* save address that was passed in.  The caller had to make a duplicate */

  optp->valuePtr[optp->count++] = strPtr;
  return 0;
}

/* Look up an option in the table, issue an error message if the option does
   not exist or is ambiguous; return index of option or an error code */
static int LookupOption(char *option, int maxLookup)
{
  int i;
  int err;
  int found = LOOKUP_NOT_FOUND;
  char *sname, *lname;

  /* Look at all of the options specified by maxLookup */
  for (i = 0; i < maxLookup; i++) {
    sname = optTable[i].shortName;
    lname = optTable[i].longName;
    if ((sname && *sname && strstr(sname, option) == sname) ||
	(lname && *lname && strstr(lname, option) == lname)) {

      /* If it is found, it's an error if one has already been found */
      if (found == LOOKUP_NOT_FOUND)
	found = i;
      else {
	sprintf(tempStr, "An option specified by \"%s\" is ambiguous between "
		"option %s -  %s  and option %s -  %s", option, sname, lname, 
		optTable[found].shortName, optTable[found].longName);
	PipSetError(tempStr);
	return LOOKUP_AMBIGUOUS;
      }
    }
  }
  if (found == LOOKUP_NOT_FOUND) {
    sprintf(tempStr, "Illegal option: %s", option);
    PipSetError(tempStr);
  }
  return found;
}

/* Duplicate a substring into a new string */
static char *PipSubStrDup(char *s1, int i1, int i2)
{
  int i;
  int size = i2 + 2 - i1;
  char *s2 = (char *)malloc(size);
  if (PipMemoryError(s2, "PipSubStrDup"))
    return NULL;

  for (i = i1; i <= i2; i++)
    s2[i - i1] = s1[i];
  s2[size - 1] = 0x00;
  return s2;
}

/* Test for whether the pointer is valid and give memory error if not */
int PipMemoryError(void *ptr, char *routine)
{
  if (ptr)
    return 0;
  sprintf(tempStr, "Failed to get memory for string in %s", routine);
  PipSetError(tempStr);
  return -1;
}

/* Add as much of a string as fits to the tempStr and use to set error */
static void AppendToErrorString(char *str)
{
  int len = strlen(tempStr);
  tempStr[TEMP_STR_SIZE - 1] = 0x00;
  strncpy(&tempStr[len], str, TEMP_STR_SIZE - len - 1);
  PipSetError(tempStr);
}

/* returns 1 if fullStr starts with subStr, with tests for NULL strings */
static int StartsWith(char *fullStr, char *subStr)
{
  if (!fullStr || !subStr)
    return 0;
  if (!*fullStr || !*subStr)
    return 0;
  if (strstr(fullStr, subStr) == fullStr)
    return 1;
  return 0;
}

