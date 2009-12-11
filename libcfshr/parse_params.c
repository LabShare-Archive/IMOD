/*   parse_params.cpp  -  the PIP package for parsing input parameters
 *
 *   Copyright (C) 2003-2006 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See dist/COPYRIGHT for full notice.
 *
 *  $Id$
 *  Log at end of file
 */                                                                           

#include "parse_params.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>

#define NON_OPTION_STRING "NonOptionArgument"
#define STANDARD_INPUT_STRING "StandardInput"
#define STANDARD_INPUT_END  "EndInput"
#define PARAM_FILE_STRING  "PF"
#define BOOLEAN_STRING    "B"
#define LOOKUP_NOT_FOUND -1
#define LOOKUP_AMBIGUOUS -2
#define TEMP_STR_SIZE  1024
#define LINE_STR_SIZE  10240
#define PREFIX_SIZE    64
#ifdef _WIN32
#define PATH_SEPARATOR '\\'
#else
#define PATH_SEPARATOR '/'
#endif
#define OPTFILE_DIR "autodoc"
#define OPTFILE_EXT "adoc"
#define OPTDIR_VARIABLE "AUTODOC_DIR"
#define PRINTENTRY_VARIABLE  "PIP_PRINT_ENTRIES"
#define OPEN_DELIM "["
#define CLOSE_DELIM "]"
#define VALUE_DELIM "="

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
                        "LI", "I", "F", "IP", "FP", "IT", "FT", "IA", "FA",
                        "CH", "FN"};
static char *typeDescriptions[] = {
  "Boolean",
  "Parameter file",
  "List of integer ranges",
  "Integer",
  "Floating point",
  "Two integers",
  "Two floats",
  "Three integers",
  "Three floats",
  "Multiple integers",
  "Multiple floats",
  "Text string",
  "File name",
  "Unknown argument type"
};

static char *typeForUsage[] = {
  "Boolean",
  "File",
  "List",
  "Int",
  "Float",
  "2 ints",
  "2 floats",
  "3 ints",
  "3 floats",
  "Ints",
  "Floats",
  "String",
  "File",
  "Unknown argument type"
};



static char numTypes = 13;

static char nullChar = 0x00;
static char *nullString = &nullChar;

/* declarations of local functions */
static int ReadParamFile(FILE *pFile);
static int OptionLineOfValues(char *option, void *array, int valType, 
                           int *numToGet, int arraySize);
static int GetNextValueString(char *option, char **strPtr);
static int AddValueString(int option, char *strPtr);
static int LookupOption(char *option, int maxLookup);
static char *PipSubStrDup(char *s1, int i1, int i2);
static void AppendToErrorString(char *str);
static int LineIsOptionToken(char *line);
static int CheckKeyword(char *line, char *keyword, char **copyto, int *gotit,
                        char ***lastCopied);


/* The static tables and variables */
static PipOptions *optTable = NULL;
static int tableSize = 0;
static int numOptions = 0;
static int nonOptInd;
static char *errorString = NULL;
static char exitPrefix[PREFIX_SIZE] = "";
static int errorDest = 0;
static int nextOption = 0;
static int nextArgBelongsTo = -1;
static int numOptionArguments = 0;
static char *tempStr = NULL;
static char *lineStr = NULL;
static int allowDefaults = 0;
static int outputManpage = 0;
static int printEntries = -1;
static char defaultDelim[] = VALUE_DELIM;
static char *valueDelim = defaultDelim;
static char *programName = &nullChar;
static int noCase = 0;
static int doneEnds = 0;
static int takeStdIn = 0;
static int nonOptLines = 0;
static int noAbbrevs = 0;
static int notFoundOK = 0;

/*
 * Initialize option tables for given number of options
 */
int PipInitialize(int numOpts)
{
  int i;

  if (!tempStr)
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

/*
 * Free all allocated memory and set state back to initial state
 */
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
  if (programName != nullString)
    free(programName);
  programName = nullString;
}

/*
 * Set up for Pip to handle exiting on error, with a prefix string
 */
int PipExitOnError(int useStdErr, char *prefix)
{
  /* Get rid of existing string; and if called with null string,
     this cancels an existing exit on error */
  exitPrefix[0] = 0x00;
    
  if (!prefix || !*prefix)
    return 0;
    
  errorDest = useStdErr;
  strncpy(exitPrefix, prefix, PREFIX_SIZE - 1);
  exitPrefix[PREFIX_SIZE - 1] = 0x00;
  return 0;
}

/*
 * Function for compatibility with Fortran routines, so setExitPrefix and
 * exitError can be used without using PIP
 */
void setExitPrefix(char *prefix)
{
  PipExitOnError(0, prefix);
}

void PipAllowCommaDefaults(int val)
{
  allowDefaults = val;
}

void PipSetManpageOutput(int val)
{
  outputManpage = val;
}

void PipEnableEntryOutput(int val)
{
  printEntries = val;
}

/*
 * Set noxious special flags for Tilt program 
 */
void PipSetSpecialFlags(int inCase, int inDone, int inStd, int inLines,
                        int inAbbrevs)
{
  noCase = inCase;
  doneEnds = inDone;
  takeStdIn = inStd;
  nonOptLines = inLines;
  noAbbrevs = inAbbrevs;
}

/*
 * Add an option, with short and long name, type, and help string
 */
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
          if (PipStartsWith(newShort, oldShort) || 
              PipStartsWith(oldShort, newShort) ||
              PipStartsWith(oldLong, newShort) ||
              PipStartsWith(newShort, oldLong) ||
              PipStartsWith(oldShort, newLong) ||
              PipStartsWith(newLong, oldShort) ||
              PipStartsWith(oldLong, newLong) ||
              PipStartsWith(newLong, oldLong)) {
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

  sprintf(tempStr, "Option does not have three colons in it:  ");
  AppendToErrorString(optionString);
  return -1;
}

/*
 * Call this to process the next argument
 */
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

    /* First check for StandardInput */
    if (PipStartsWith(STANDARD_INPUT_STRING, argString + indStart)) {
      err = ReadParamFile(stdin);
      return err;
    }
    numOptionArguments++;

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

/*
 * return number of option arguments (approximate) and number of non-option
 * arguments 
 */
void PipNumberOfArgs(int *numOptArgs, int *numNonOptArgs)
{
  *numOptArgs = numOptionArguments;
  *numNonOptArgs = optTable[nonOptInd].count;
}

/*
 * Get a non-option argument, index numbered from 0 here
 */
int PipGetNonOptionArg(int argNo, char **arg)
{
  if (argNo >= optTable[nonOptInd].count) {
    PipSetError("Requested a non-option argument beyond the number available");
    return -1;
  }
  *arg = strdup(optTable[nonOptInd].valuePtr[argNo]);
  return PipMemoryError(*arg, "PipGetNonOptionArg");
}

/*
 * Get a string option
 */
int PipGetString(char *option, char **string)
{
  char *strPtr;
  int err;
  if ((err = GetNextValueString(option, &strPtr)))
    return err;
  *string  = strdup(strPtr);
  return PipMemoryError(*string, "PipGetString");
}

/*
 * Get a boolean (binary) option; make sure it has a legal specification
 */
int PipGetBoolean(char *option, int *val)
{
  char *strPtr;
  int err;
  if ((err = GetNextValueString(option, &strPtr)))
    return err;
  if (!strcmp(strPtr, "1") || !strcmp(strPtr, "T") || 
      !strcmp(strPtr, "TRUE") || !strcmp(strPtr, "ON") || 
      !strcmp(strPtr, "t") || 
      !strcmp(strPtr, "true") || !strcmp(strPtr, "on"))
    *val = 1;
  else if (!strcmp(strPtr, "0") || !strcmp(strPtr, "F") || 
           !strcmp(strPtr, "FALSE") || !strcmp(strPtr, "OFF")
           || !strcmp(strPtr, "f") || 
           !strcmp(strPtr, "false") || !strcmp(strPtr, "off"))
    *val = 0;
  else {
    sprintf(tempStr, "Illegal entry for boolean option %s: %s", option, 
            strPtr);
    PipSetError(tempStr);
    return -1;
  }
  return 0;
}

/*
 * Get single integer and float just call to get an array with one element
 */
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

/*
 * Get two integers or floats - move into array, get array with two elements
 */
int PipGetTwoIntegers(char *option, int *val1, int *val2)
{
  int err;
  int num = 2;
  int tmp[2];
  tmp[0] = *val1;
  tmp[1] = *val2;
  if ((err = PipGetIntegerArray(option, tmp, &num, 2)) != 0)
    return err;
  *val1 = tmp[0];
  *val2 = tmp[1];
  return 0;
}

int PipGetTwoFloats(char *option, float *val1, float *val2)
{
  int err;
  int num = 2;
  float tmp[2];
  tmp[0] = *val1;
  tmp[1] = *val2;
  if ((err = PipGetFloatArray(option, tmp, &num, 2)) != 0)
    return err;
  *val1 = tmp[0];
  *val2 = tmp[1];
  return 0;
}

/*
 * Get three integers or floats - move into array, get array with two elements
 */
int PipGetThreeIntegers(char *option, int *val1, int *val2, int *val3)
{
  int err;
  int num = 3;
  int tmp[3];
  tmp[0] = *val1;
  tmp[1] = *val2;
  tmp[2] = *val3;
  if ((err = PipGetIntegerArray(option, tmp, &num, 3)) != 0)
    return err;
  *val1 = tmp[0];
  *val2 = tmp[1];
  *val3 = tmp[2];
  return 0;
}

int PipGetThreeFloats(char *option, float *val1, float *val2, float *val3)
{
  int err;
  int num = 3;
  float tmp[3];
  tmp[0] = *val1;
  tmp[1] = *val2;
  tmp[2] = *val3;
  if ((err = PipGetFloatArray(option, tmp, &num, 3)) != 0)
    return err;
  *val1 = tmp[0];
  *val2 = tmp[1];
  *val3 = tmp[2];
  return 0;
}

/*
 * Getting an array of integers or floats calls the general routine for
 * getting a line of values 
 */
int PipGetIntegerArray(char *option, int *array, int *numToGet, int arraySize)
{
  return OptionLineOfValues(option, (void *)array, PIP_INTEGER, numToGet,
                         arraySize);
}

int PipGetFloatArray(char *option, float *array, int *numToGet, int arraySize)
{
  return OptionLineOfValues(option, (void *)array, PIP_FLOAT, numToGet,
                         arraySize);
}

/*
 * Print a complete usage statement, man page entry, or Fortran fallback
 */
int PipPrintHelp(char *progName, int useStdErr, int inputFiles, 
                 int outputFiles)
{
  int i, j, lastOpt, optLen, numOut = 0, numReal = 0;
  int helplim = 74;
  char *sname, *lname, *newLinePt;
  FILE *out = useStdErr ? stderr : stdout;
  char indent4[] = "    ";
  char *indentStr;
  int linePos = 11;
  char **descriptions = &typeDescriptions[0];

  /* Get correct number of options for Fortran fallback */
  for (i = 0; i < numOptions; i++) {
    sname = optTable[i].shortName;
    lname = optTable[i].longName;
    if ((lname && *lname) || (sname && *sname))
      numReal++;
  }

  if (!outputManpage) {
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

    if (!numReal)
      return 0;
    fprintf(out, "Options:\n");
    descriptions = &typeForUsage[0];
  }

  for (i = 0; i < numOptions; i++) {
    sname = optTable[i].shortName;
    lname = optTable[i].longName;
    indentStr = nullString;
    if ((lname && *lname) || (sname && *sname)) {
      if (outputManpage <= 0)
        indentStr = indent4;

      /* Output Fortran fallback code (-2) */
      if (outputManpage == -2) {
        lastOpt = (i == numOptions - 1);
        if (!numOut) fprintf(out, 
                        "      integer numOptions\n"
                        "      parameter (numOptions = %d)\n"
                        "      character*(40 * numOptions) options(1)\n"
                        "      options(1) =\n     &    '", numReal);
        
        optLen = strlen(sname) + strlen(lname) + strlen(optTable[i].type) + 4 +
          optTable[i].multiple;
        
        if (linePos + optLen + (lastOpt ? 0 : 3) > 79) {
          fprintf(out, "'//\n     &    '");
          linePos = 11;
        }
        fprintf(out, "%s:%s:%s%s%s", sname, lname, optTable[i].type, 
                optTable[i].multiple ? "M:" : ":", lastOpt ? "'\n" : "@");
        linePos += optLen;
        numOut++;
        continue;
      }
      
      /* Fallback output for C code (2) or Python code (3) */
      if (outputManpage == 2 || outputManpage == 3) {
        lastOpt = (i == numOptions - 1);
        if (!numOut) {
          if (outputManpage == 2) {
            fprintf(out, "  int numOptions = %d;\n"
                    "  char *options[] = {\n    ", numReal);
            linePos = 5;
          } else {
            fprintf(out, "options = [");
            linePos = 12;
          }
        }
        optLen = strlen(sname) + strlen(lname) + strlen(optTable[i].type) + 7 +
          optTable[i].multiple;
        if (linePos + optLen > 79) {
          if (outputManpage == 2) {
            fprintf(out, "\n    ");
            linePos = 5;
          } else {
            fprintf(out, "\n           ");
            linePos = 12;
          }
        }
        fprintf(out, "\"%s:%s:%s%s\"%s", sname, lname, optTable[i].type, 
                optTable[i].multiple ? "M:" : ":", 
                lastOpt ? (outputManpage == 2 ? "};\n" : "]\n"): ", ");
        linePos += optLen;
        numOut++;
        continue;
      }

      if (i && outputManpage < 0)
        fprintf(out, "\n");
      if (outputManpage > 0)
        fprintf(out, ".TP\n.B ");
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
      if (strcmp(optTable[i].type, BOOLEAN_STRING))
        fprintf(out, "%s%s", outputManpage > 0 ? " \t " : "   ", 
                descriptions[j]);
      fprintf(out, "\n");
    } else if (outputManpage == -2 || outputManpage >= 2)
      continue;
    else if (outputManpage == 1)
      fprintf(out, ".SS ");
    else
      fprintf(out, "\n");

    /* Print help string, breaking up line as needed */
    if (optTable[i].helpString && *optTable[i].helpString) {
      lname = strdup(optTable[i].helpString);
      if (PipMemoryError(lname, "PipPrintHelp"))
        return -1;
      sname = lname;
      optLen = strlen(sname);
      newLinePt = strchr(sname, '\n');
      while (optLen > helplim || newLinePt) {

        /* Break string at newline */
        if (newLinePt && newLinePt - sname <= helplim) {
          j = newLinePt - sname;
          newLinePt = strchr(sname + j + 1, '\n');
        } else {

          /* Or break string at last space before limit */
          for (j = helplim; j >= 1; j--)
            if (sname[j] == ' ')
              break;
        }

        /* Replace break point with null, print and reset pointer and count */
        sname[j] = 0x00;
        fprintf(out, "%s%s\n", indentStr, sname);
        sname += j + 1;
        optLen -= j + 1;
      }
      fprintf(out, "%s%s\n", indentStr, sname);
      free(lname);
    }

    if (optTable[i].multiple)
      fprintf(out, "%s(Successive entries accumulate)\n", indentStr);
  }
  return 0;
}

/*
 * Print all the option entries if enabled by program call and/or environment
 * variable
 */
void PipPrintEntries()
{
  char *name, *sname, *lname;
  int i, j;
  if (printEntries < 0) {
    printEntries = 0;
    name = getenv(PRINTENTRY_VARIABLE);
    if (name)
      printEntries = atoi(name);
  }
  if (!printEntries)
    return;

  /* Count up entries */
  j = 0;
  for (i = 0; i < numOptions; i++)
    j += optTable[i].count;
  if (!(j + optTable[nonOptInd].count))
    return;

  printf("\n*** Entries to program %s ***\n", programName);
  for (i = 0; i < numOptions; i++) {
    sname = optTable[i].shortName;
    lname = optTable[i].longName;
    if ((lname && *lname) || (sname && *sname)) {
      name = (lname && *lname) ? lname : sname;
      for (j = 0; j < optTable[i].count; j++)
        printf("  %s = %s\n", name, optTable[i].valuePtr[j]);
    }
  }
  if (optTable[nonOptInd].count) {
    printf("  Non-option arguments:");
    for (j = 0; j < optTable[nonOptInd].count; j++)
      printf("   %s", optTable[nonOptInd].valuePtr[j]);
    printf("\n");
  }
  printf("*** End of entries ***\n\n");

  /* Windows needed two flushes here */
  fflush(stdout);
  fflush(stdout);
}


/*
 * Return the error string, or an empty string and an error if there is none
 */
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

/*
 * Set the error string.
 * If exitPrefix is set, then output an error message to stderr or stdout
 * and exit. 
 */
int PipSetError(char *errString)
{
  FILE *outFile = errorDest ? stderr : stdout;
  if (errorString)
    free(errorString);
  errorString = strdup(errString);
  if (!errorString && !exitPrefix[0])
    return -1;

  if (exitPrefix[0]) {
    fprintf(outFile, "%s ", exitPrefix);
    fprintf(outFile, "%s\n", errorString ? errorString : "Unspecified error");
    exit(1);
  }
  return 0;
}

void exitError(char *format, ...)
{
  char errorMess[512];
  va_list args;
  va_start(args, format);
  vsprintf(errorMess, format, args);
  PipSetError(errorMess);
  exit(1);
}

/*
 * Return the number of entries for a particular option
 */
int PipNumberOfEntries(char *option, int *numEntries)
{
  int err;
  if ((err = LookupOption(option, nonOptInd + 1)) < 0)
    return err;
  *numEntries = optTable[err].count;
  return 0;
}

/*
 * Top level routine to be called to process options and arguments
 */
int PipParseInput(int argc, char *argv[], char *options[], int numOpts,
                  int *numOptArgs, int *numNonOptArgs)
{
  int i, err;

  /* Initialize */
  if ((err = PipInitialize(numOpts)))
    return err;

  /* add the options */
  for (i = 0; i < numOpts; i++)
    if ((err = PipAddOption(options[i])))
      return err;


  return (PipParseEntries(argc, argv, numOptArgs, numNonOptArgs));
}

/*
 * Alternative routine to have options read from a file
 */
int PipReadOptionFile(char *progName, int helpLevel, int localDir)
{
  int i, ind, indst, lineLen, err, needSize, isOption, isSection = 0;
  FILE *optFile = NULL;
  char *bigStr;
  char *pipDir;
  char *textStr;
  char *helpStr;
  int numOpts = 0;
  int bigSize = LINE_STR_SIZE;
  int readingOpt = 0;
  char *longName, *shortName, *type, *usageStr, *tipStr, *manStr;
  int gotLong, gotShort, gotType, gotUsage, gotTip, gotMan;
  int gotDelim = 0;
  char *optStr = NULL;
  int optStrSize = 0;
  char **lastGottenStr = NULL;

#ifdef PATH_MAX
  if (bigSize < PATH_MAX)
    bigSize = PATH_MAX;
#endif

  /* Set up temp string for error processing and big string for lines */
  if (!tempStr)
    tempStr = (char *)malloc(TEMP_STR_SIZE);
  bigStr = (char *)malloc(bigSize);
  if (!bigStr || !tempStr) {
    PipMemoryError(NULL, "PipReadOptionFile");
    return -1;
  }

  /* Save the program name for entry output */
  programName = strdup(progName);
  if (!programName)
    programName = nullString;

  /* If local directory not set, look for environment variable pointing
     directly to where the file should be */
  if (!localDir) {
    pipDir = getenv(OPTDIR_VARIABLE);
    if (pipDir) {
      sprintf(bigStr, "%s%c%s.%s", pipDir, PATH_SEPARATOR, progName, 
              OPTFILE_EXT);
      /* fprintf(stderr, "Looking for file %s\n", bigStr); */
      optFile = fopen(bigStr, "r");
    }

    if (!optFile) {
      pipDir = getenv("IMOD_DIR");
      if (pipDir) {
        sprintf(bigStr, "%s%c%s%c%s.%s", pipDir, PATH_SEPARATOR, OPTFILE_DIR,
                PATH_SEPARATOR, progName, 
                OPTFILE_EXT);
        optFile = fopen(bigStr, "r");
      }
    }
  }
  
  /* If local directory set, set up name with ../ as many times as specified
     and look for file there */
  else if (localDir > 0) {
    ind = 0;
    for (i = 0; i < localDir && i < 20; i++) {
      bigStr[ind++] = '.';
      bigStr[ind++] = '.';
      bigStr[ind++] = PATH_SEPARATOR;
    }
    sprintf(bigStr + ind, "%s%c%s.%s", OPTFILE_DIR, PATH_SEPARATOR, progName, 
            OPTFILE_EXT);
    optFile = fopen(bigStr, "r");
  }
    
  /* If there is still no file, look in current directory */
  if (!optFile) {
    sprintf(bigStr, "%s.%s", progName, OPTFILE_EXT);
    optFile = fopen(bigStr, "r");

    if (!optFile) {
      sprintf(bigStr, "Autodoc file %s.%s was not found or not readable.\n"
              "Check environment variable settings of "OPTDIR_VARIABLE" and "
              "IMOD_DIR\nor place autodoc file in current directory",
              progName, OPTFILE_EXT);
      PipSetError(bigStr);
      return -1;
    }
  }

  /* Count up the options */
  while (1) {
    lineLen = PipReadNextLine(optFile, bigStr, bigSize, '#', 0, 0, &indst);
    if (lineLen == -3)
      break;
    if (lineLen == -2) {
      PipSetError("Error reading option file");
      return -1;
    }
    
    /* If the string was not long enough, get a bigger string and start over */
    if (lineLen == -1) {
      bigSize += LINE_STR_SIZE;
      free(bigStr);
      bigStr = (char *)malloc(bigSize);
      if (PipMemoryError(bigStr, "PipReadOptionFile"))
        return -1;
      numOpts = 0;
      rewind(optFile);
      continue;
    }

    /* Look for new keyword-value delimiter before any options */
    if (!numOpts)
      CheckKeyword(bigStr + indst, "KeyValueDelimiter", &valueDelim, &gotDelim,
                 &lastGottenStr);

    /* Look for options */
    if (LineIsOptionToken(bigStr + indst) > 0)
      numOpts++;
  }

  /* Initialize */
  if ((err = PipInitialize(numOpts)))
    return err;

  /* fprintf(stderr, "Initialized for %d options\n", numOpts); */

  /* rewind file and process the options */
  rewind(optFile);
  longName = shortName = type = usageStr = tipStr = manStr = nullString;
  gotLong = gotShort = gotType = gotUsage = gotTip = gotMan = 0;

  while (1) {
    lineLen = PipReadNextLine(optFile, bigStr, bigSize, '#', 0, 0, &indst);
    if (lineLen == -2) {
      PipSetError("Error reading autodoc file");
      return -1;
    }

    textStr = bigStr + indst;
    isOption = LineIsOptionToken(textStr);
    if (readingOpt && (lineLen == -3 || isOption)) {
      
      /* If we were reading options, it is time to add them if we are at
         end of file or if we have reached a new token of any kind

         Pick the closest help string that was read in if the given one
         does not match (there has got to be an easier way!) */
      if (helpLevel <= 1) {
        if (gotUsage)
          helpStr = usageStr;
        else if (gotTip)
          helpStr = tipStr;
        else
          helpStr = manStr;
      }
      else if (helpLevel == 2) {
        if (gotTip)
          helpStr = tipStr;
        else if (gotUsage)
          helpStr = usageStr;
        else
          helpStr = manStr;
      }
      else {
        if (gotMan)
          helpStr = manStr;
        else if (gotTip)
          helpStr = tipStr;
        else
          helpStr = usageStr;
      }

      /* If it is a section header, get rid of the names */
      if (isSection) {
        if (gotShort)
          free(shortName);
        if (gotLong)
          free(longName);
        longName = shortName = nullString;
        gotLong = gotShort = 0;
      }

      needSize = strlen(shortName) + strlen(longName) + strlen(helpStr) + 15;
      if (optStrSize < needSize) {
        if (optStrSize)
          free(optStr);
        optStrSize = needSize;
        optStr = (char *)malloc(optStrSize);
        if (PipMemoryError(optStr, "PipReadOptionFile"))
          return -1;
      }

      sprintf(optStr, "%s:%s:%s:%s", shortName, longName, type, helpStr);

      if ((err = PipAddOption(optStr)))
        return err;

      /* Clean up memory and reset flags */
      if (gotShort)
        free(shortName);
      if (gotLong)
        free(longName);
      if (gotType)
        free(type);
      if (gotUsage)
        free(usageStr);
      if (gotTip)
        free(tipStr);
      if (gotMan)
        free(manStr);
      longName = shortName = type = usageStr = tipStr = manStr = nullString;
      gotLong = gotShort = gotType = gotUsage = gotTip = gotMan = 0;
      readingOpt = 0;
    }

    if (lineLen == -3)
      break;

    /* If reading options, look for the various keywords */
    if (readingOpt) {

      /* If the last string gotten was a help string and the line does not
         contain the value delimiter, then append it to the last string */
      if ((lastGottenStr == &usageStr || lastGottenStr == &tipStr ||
           lastGottenStr == &manStr) && !strstr(textStr, valueDelim)) {
        ind = strlen(*lastGottenStr);
        needSize = ind + strlen(textStr) + 3;
        *lastGottenStr = (char *)realloc(*lastGottenStr, needSize);
        strcat(*lastGottenStr, (*lastGottenStr)[ind - 1] == '.' ? "  " : " ");

        /* Replace leading ^ with a newline */
        if (textStr[0] == '^')
          textStr[0] = '\n';
        strcat(*lastGottenStr, textStr);
      }

      /* Otherwise look for each keyword of interest, but null out the pointer
         to last gotten one so that it will only be valid on the next line */
      else {
        lastGottenStr = NULL;
        if ((err = CheckKeyword(textStr, "short", &shortName, &gotShort,
                                &lastGottenStr)))
          return err;
        if ((err = CheckKeyword(textStr, "long", &longName, &gotLong,
                 &lastGottenStr)))
          return err;
        if ((err = CheckKeyword(textStr, "type", &type, &gotType,
                                &lastGottenStr)))
          return err;
        
        /* Check for usage if at help level 1 or if we haven't got either of
           the other strings yet */
        if (helpLevel <= 1 || !(gotTip || gotMan))
          if ((err = CheckKeyword(textStr, "usage", &usageStr, &gotUsage,
                                  &lastGottenStr)))
            return err;
        
        /* Check for tooltip if at level 2 or if at level 1 and haven't got
           usage, or at level 3 and haven't got manpage */
        if (helpLevel == 2 || (helpLevel <= 1 && !gotUsage) ||
            (helpLevel >= 3 && !gotMan))
          if ((err = CheckKeyword(textStr, "tooltip", &tipStr, &gotTip,
                                  &lastGottenStr)))
            return err;
        
        /* Check for manpage if at level 3 or if at level 2 and haven't got
           tip, or at level 1 and haven't got tip or usage */
        if (helpLevel >= 3 || (helpLevel == 2 && !gotTip) ||
            (helpLevel <= 1 && !(gotTip || gotUsage)))
          if ((err = CheckKeyword(textStr, "manpage", &manStr, &gotMan,
                                  &lastGottenStr)))
            return err;
      }
    }

    /* But if not reading options, check for a new option token and start 
       reading if one is found.  But first take a Field value as default 
       long option name */
    else if (isOption > 0) {
      lastGottenStr = NULL;
      readingOpt = 1;
      isSection = isOption - 1;
      if (!isSection) {
        if ((err = CheckKeyword(textStr + strlen(OPEN_DELIM), "Field", 
                                &longName, &gotLong, &lastGottenStr)))
          return err;
        if (gotLong)
          longName[strlen(longName) - 1] = nullChar;
      }
    }
  }
  free (bigStr);
  if (optStrSize)
    free(optStr);
  return 0;
}

/*
 * Routine to parse the entries in command line after options have been
 * defined on way or another
 */
int PipParseEntries(int argc, char *argv[], int *numOptArgs,
                    int *numNonOptArgs)
{
  int i, err;

  /* Special case: no arguments and flag set to take stdin automatically */
  if (!argc && takeStdIn) {
    if ((err = ReadParamFile(stdin)))
      return err;
  } else {

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
  }
  PipNumberOfArgs(numOptArgs, numNonOptArgs);
  PipPrintEntries();
  return 0;
}

/*
 * High-level routine to initialize from autodoc with optional fallback options
 * Set exit string and output to stdout, print usage if not enough arguments
 */
void PipReadOrParseOptions(int argc, char *argv[], char *options[], 
                           int numOpts, char *progName, int minArgs, 
                           int numInFiles, int numOutFiles, int *numOptArgs,
                           int *numNonOptArgs, void (headerFunc)(char *))
{
  int ierr;
  char *errString;
  char *prefix;
  prefix = (char *)malloc(strlen(progName) + 12);
  sprintf(prefix, "ERROR: %s -", progName);

  /* Startup with fallback */
  ierr = PipReadOptionFile(progName, 0, 0);
  PipExitOnError(0, prefix);
  free(prefix);
  if (!ierr) {
    PipParseEntries(argc, argv, numOptArgs, numNonOptArgs);
  } else {
    PipGetError(&errString);
    if (!options || !numOpts)
      PipSetError(errString);
    if (errString) {
      printf("PIP WARNING: %s\nUsing fallback options in C code\n", errString);
      free(errString);
    }
    PipParseInput(argc, argv, options, numOpts, numOptArgs,
                  numNonOptArgs);
  }

  /* Output usage and exit if not enough arguments */
  if (*numOptArgs + *numNonOptArgs < minArgs) {
    if (headerFunc != NULL)
      headerFunc(progName);
    PipPrintHelp(progName, 0, numInFiles, numOutFiles);
    exit(0);
  }
}

/*
 * Routine to get input/output file from parameter or non-option args
 */
int PipGetInOutFile(char *option, int nonOptArgNo, char **filename)
{
  if (PipGetString(option, filename)) {
    if (nonOptArgNo >= optTable[nonOptInd].count)
      return 1;
    PipGetNonOptionArg(nonOptArgNo, filename);
  }
  return 0;
}

/*
 * Read successive lines from a parameter file or standard input, and 
 * store as options and values 
 */
static int ReadParamFile(FILE *pFile)
{
  int lineLen;
  int indst, indnd, optNum, gotEquals, err;
  char *strPtr;
  char *token;
 
  while (1) {

  /* If non-option lines are allowed, set flag that it is OK for LookupOption
     to not find the option, but only for the given number of lines at the
     start of the input */
    notFoundOK = (!numOptionArguments && optTable[nonOptInd].count < 
                  nonOptLines) ? 1 : 0;
    lineLen = PipReadNextLine(pFile, lineStr, LINE_STR_SIZE, '#', 0, 1, 
                              &indst);
    if (lineLen == -3)
        break;
    if (lineLen == -2) {
      PipSetError("Error reading parameter file or "STANDARD_INPUT_STRING);
      return -1;
    }

    if (lineLen == -1) {
      PipSetError("Line too long for buffer while reading parameter file or "
                  STANDARD_INPUT_STRING);
      return -1;
    }

    /* Find token and make a copy */
    strPtr = strpbrk(lineStr + indst, "= \t");
    indnd = strPtr ? (strPtr - lineStr) - 1 : lineLen - 1;
    if (indnd >= lineLen)
      indnd = lineLen - 1;

    token = PipSubStrDup(lineStr, indst, indnd);
    if (PipMemoryError(token, "ReadParamFile"))
      return -1;

    /* Done if it matches end of input string */
    if (!strcmp(token, STANDARD_INPUT_END) || 
        (doneEnds && strlen(token) == 4 && PipStartsWith("DONE", token)))
      break;

    /* Look up option and free the token string */
    optNum = LookupOption(token, numOptions);
    free(token);
    if (optNum < 0) {

      /* If no option, process special case if in-line non-options allowed,
         or error out */
      if (notFoundOK) {
        token = PipSubStrDup(lineStr, indst, lineLen - 1);
        if (PipMemoryError(token, "ReadParamFile"))
          return -1;
        if ((err = AddValueString(nonOptInd, token)))
          return err;
        continue;
      } else
        return optNum;
    }

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
          sprintf(tempStr, "Two = signs in input line:  ");
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
      sprintf(tempStr, "Missing a value on the input line:  ");
      AppendToErrorString(lineStr);
      return -1;
    }
    if (PipMemoryError(token, "ReadParamFile"))
      return -1;

    /* Add the token as a value string and increment argument number */
    if ((err = AddValueString(optNum, token)))
      return err;
    numOptionArguments++;
  }
  notFoundOK = 0;
  return 0;
}

/*
 * Call from fortran to read stdin if the flag is set to take from stdin
 */
int PipReadStdinIfSet(void)
{
  if (takeStdIn)
    return ReadParamFile(stdin);
  return 0;
}

/*!
 * Reads a line from the file [pFile], stripping white space at the end of the
 * line and in-line comments starting with [comment] if [inLineComments] is 
 * non-zero.  Discards the line and reads another if it is blank or if the 
 * first non-blank character is [comment], unless [keepComments] is nonzero.
 * Returns the line in [lineStr] and the index of the first
 * non-white space character in [firstNonWhite].  The size of [lineStr] is
 * provided in [strSize].  Returns the length of the line, or -3 for end of 
 * file, -1 if the line is too long, or -2 for error reading file.
 */
int PipReadNextLine(FILE *pFile, char *lineStr, int strSize, char comment, 
                    int keepComments, int inLineComments, int *firstNonWhite)
{
  int indst, lineLen;
  char ch;
  char *strPtr;

  while (1) {

    /* read a line, or as much as fits in lineStr */
    if (fgets(lineStr, strSize, pFile) == NULL) {

      /* If error, it's OK if it's an EOF, or an error otherwise */
      if (feof(pFile))
        return -3;
      return -2;
    }

    /* check for line too long */
    lineLen = strlen(lineStr);
    if (lineLen == strSize - 1) {
      return -1;
    }

    /* Get first non-white space */
    indst = 0;
    while (indst < lineLen) {
      if (lineStr[indst] != ' ' && lineStr[indst] != '\t')
        break;
      indst++;
    }

    /* If it is a comment, skip or strip line ending and return */
    if (lineStr[indst] == comment) {
      if (keepComments) {
        while (lineStr[lineLen - 1] == '\n' || lineStr[lineLen - 1] == '\r')
          lineLen--;
        lineStr[lineLen] = 0x00;
        break;
      } else
        continue;
    }

    /* adjust line length to remove comment, if we have in-line comments */
    strPtr = strchr(lineStr, comment);
    if (strPtr && inLineComments)
      lineLen = strPtr - lineStr;

    /* adjust line length back further to remove white space and newline */
    while (lineLen > 0) {
      ch = lineStr[lineLen - 1]; 
      if (ch != ' ' && ch != '\t' && ch != '\n' && ch != '\r')
        break;
      lineLen--;
    }
    lineStr[lineLen] = 0x00;

    /* Return if something is on line or we are keeping comments */
    if (indst < lineLen || keepComments)
      break;
  }
  *firstNonWhite = indst;
  return lineLen;
}

/*
 * Parse a line of values for an option and return them into an array
 */
static int OptionLineOfValues(char *option, void *array, int valType, 
                              int *numToGet, int arraySize)
{
  char *strPtr;
  int err;

  /* Get string  and save pointer to it for error messages */
  if ((err = GetNextValueString(option, &strPtr)))
    return err;
  return PipGetLineOfValues(option, strPtr, array, valType, numToGet, 
                            arraySize);
}

/*!
 * Parses a line of values from the string in [strPtr] and returns them into
 * [array], whose size is given by [arraySize].  The type is indicated in
 * [valType] as PIP_INTEGER (1) for integer or PIP_FLOAT (2) for float.
 * The number of values to get is set in [numToGet], where a value of zero
 * indicates all values should be returned, in which case the number gotten is 
 * returned in [numToGet].  The return value is -1 for errors in parsing,
 * too few values on the line, or not enough space in the array.
 */
int PipGetLineOfValues(char *option, char *strPtr, void *array, int valType, 
                       int *numToGet, int arraySize)
{
  char *sepPtr;
  char *endPtr;
  char *invalid;
  char *fullStr;
  int *iarray = (int *)array;
  float *farray = (float *)array;
  int numGot = 0;
  int gotComma = 1;
  char sepStr[] = ",\t /";

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
                " in value entry:  %s  ", option);
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
                    " in value entry:  %s  ", option);
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
      sprintf(tempStr, "Too many values for input array in value entry:  %s  ",
              option);
      AppendToErrorString(fullStr);
      return -1;
    }
    
    /* convert number, get pointer  to first invalid char */
    if (valType == PIP_INTEGER)
      iarray[numGot++] = strtol(strPtr, &invalid, 10);
    else
      farray[numGot++] = (float)strtod(strPtr, &invalid);

    /* If invalid character is before end character, it is an error */
    if (invalid != endPtr) {
      sprintf(tempStr, "Illegal character in value entry:  %s  ", option);
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
            "entry:  %s  ", *numToGet, numGot, option);
    AppendToErrorString(fullStr);
    return -1;
  }

  return 0;
}

/*
 * Get a pointer to the value string for the given option.
 * Return < 0 if the option is invalid, 1 if the option was not entered.
 * If the option allows multiple values, advance the multiple counter
 */
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

/*
 * Add a string to the set of values for an option
 */
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

/*
 * Look up an option in the table, issue an error message if the option does
 * not exist or is ambiguous; return index of option or an error code
 */
static int LookupOption(char *option, int maxLookup)
{
  int i, lenopt = 0;
  int found = LOOKUP_NOT_FOUND;
  char *sname, *lname;

  if (noAbbrevs)
    lenopt = strlen(option);

  /* Look at all of the options specified by maxLookup */
  for (i = 0; i < maxLookup; i++) {
    sname = optTable[i].shortName;
    lname = optTable[i].longName;
    if ((PipStartsWith(sname, option) && 
         (!noAbbrevs || lenopt == strlen(sname))) ||
        (PipStartsWith(lname, option) && 
         (!noAbbrevs || lenopt == strlen(lname)))) {

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

  /* Set error string unless flag set that non-options are OK */
  if (found == LOOKUP_NOT_FOUND && !notFoundOK) {
    sprintf(tempStr, "Illegal option: %s", option);
    PipSetError(tempStr);
  }
  return found;
}

/*
 * Duplicate a substring into a new string
 */
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

/*
 * Test for whether the pointer is valid and give memory error if not
 */
int PipMemoryError(void *ptr, char *routine)
{
  if (ptr)
    return 0;
  if (!tempStr)
    tempStr = (char *)malloc(TEMP_STR_SIZE);
  if (!tempStr) {
    PipSetError("Failed to get memory for string in PipMemoryError");
    return -1;
  }
    
  sprintf(tempStr, "Failed to get memory for string in %s", routine);
  PipSetError(tempStr);
  return -1;
}

/*
 * Add as much of a string as fits to the tempStr and use to set error
 */
static void AppendToErrorString(char *str)
{
  int len = strlen(tempStr);
  tempStr[TEMP_STR_SIZE - 1] = 0x00;
  strncpy(&tempStr[len], str, TEMP_STR_SIZE - len - 1);
  PipSetError(tempStr);
}

/*!
 * Returns 1 if [fullStr] starts with [subStr], where either strings can be 
 * NULL.
 */
int PipStartsWith(char *fullStr, char *subStr)
{
  if (!fullStr || !subStr)
    return 0;
  if (!*fullStr || !*subStr)
    return 0;
  if (noCase) {
    while (*fullStr && *subStr) {
      if (toupper(*fullStr++) != toupper(*subStr++))
        return 0;
    }
    if (!*subStr)
      return 1;
  }  else if (strstr(fullStr, subStr) == fullStr)
    return 1;
  return 0;
}

/* 
 * Determines whether the line contains the token for an option inside the
 * opening and closing delimiters and returns 1 if it does, or -1 if it is
 * another token
 */
static int LineIsOptionToken(char *line)
{
  char *token;

  /* It is not a token unless it starts with open delim and contains close */
  if (!PipStartsWith(line, OPEN_DELIM) || !strstr(line, CLOSE_DELIM))
    return 0;

  /* It must then contain "Field" right after delim to be an option */
  token = line + strlen(OPEN_DELIM);
  if (PipStartsWith(token, "Field"))
    return 1;
  if (PipStartsWith(token, "SectionHeader"))
    return 2;

  return -1;
}

/*
 * Checks for whether a keyword occurs at the beginning of the line and
 * is followed by the keyword-value delimiter, and if so duplicates the
 * value string and sets the flag
 * Watch out for the lastCopied variable, which points to the variable holding
 * the address of the string
 */
static int CheckKeyword(char *line, char *keyword, char **copyto, int *gotit,
                        char ***lastCopied)
{
  char *valStart;
  char *copyStr;

  /* First make sure line starts with it */
  if (!PipStartsWith(line, keyword))
    return 0;

  /* Now look for delimiter */
  valStart = strstr(line, valueDelim);
  if (!valStart)
    return 0;

  /* Free previous entry if there was one, and mark that it was not gotten,
     so that an empty entry can supercede a non-empty one */
  if (*gotit && *copyto != nullString) {
    free (*copyto);
    *copyto = nullString;
    *gotit = 0;
  }

  /*Eat spaces after the delimiter and return if nothing left*/
  /* In other words, a key with no value is the same as having no key at all */
  valStart += strlen(valueDelim);
  while (*valStart == ' ' || *valStart == '\t')
    valStart++;
  if (!*valStart)
    return 0;

  /* Copy string and return address, set flag that it was gotten */
  copyStr = strdup(valStart);
  if (PipMemoryError(copyStr, "CheckKeyword"))
    return -1;

  *gotit = 1;
  *copyto = copyStr;
  *lastCopied = copyto;
  return 0;
}

/*
$Log$
Revision 1.5  2009/12/04 20:27:11  mast
Added automatic printing of entries

Revision 1.4  2009/02/17 02:54:20  mast
Keep error reports on one line

Revision 1.3  2008/12/04 05:49:18  mast
Put a .SS in front of section headers for manpage output

Revision 1.2  2007/10/18 22:22:47  mast
Made exir prefix a static array so exitError works after PipDone

Revision 1.1  2007/09/20 02:43:08  mast
Moved to new library

Revision 3.29  2007/08/03 16:40:15  mast
Fixes for clean compile

Revision 3.28  2007/07/15 20:56:25  mast
Try abbreviations for the types in the usage stagements

Revision 3.27  2007/06/22 05:01:24  mast
Changes to get Tilt to be able to use PIP

Revision 3.26  2007/04/04 23:18:23  mast
Fixed fallback output for C/Python when there are section headers

Revision 3.25  2006/11/22 18:54:29  mast
Eliminate a warning in VC6

Revision 3.24  2006/10/17 18:15:11  mast
Made the line reading function able to pass comments and blank lines back

Revision 3.23  2006/10/16 16:17:27  mast
Made some functions global for autodoc reader

Revision 3.22  2006/10/03 14:41:46  mast
Added python option output

Revision 3.21  2006/09/20 23:04:06  mast
Made the call to copyright be a callback function to decouple from utils

Revision 3.20  2006/09/19 16:58:25  mast
Clean up warnings

Revision 3.19  2006/09/12 15:21:14  mast
Added include

Revision 3.18  2006/06/08 03:11:31  mast
Added higher level C functions and provided fallback option string
output for C.

Revision 3.17  2006/02/27 05:56:24  mast
Modified to put out Fortran fixed format line to column 79

Revision 3.16  2005/11/18 20:25:52  mast
Fixed problem with section headers giving wrong number of options in
Fortran fallback output

Revision 3.15  2005/10/11 17:49:03  mast
Fixed fallback output to add back M to type when multiple set

Revision 3.14  2005/05/14 00:59:29  mast
Implemented field value as default long option, allowed multiple entries of
keyword to override previous entry, restricted to Field and SectionHeader

Revision 3.13  2005/04/16 00:08:21  mast
Added ability to have section headers for usage and man page output

Revision 3.12  2005/02/12 01:38:27  mast
Added ability to use ^ at start of line as a break

Revision 3.11  2004/06/09 22:55:32  mast
Added option to error messages about problems with value entries

Revision 3.10  2003/10/24 03:03:47  mast
unrecalled change for Windows/Intel

Revision 3.9  2003/10/11 04:22:02  mast
Remove \/, a bad combination

Revision 3.8  2003/10/10 20:38:49  mast
Made it count real arguments properly and had it eat \r from line ends
for Windows

Revision 3.7  2003/10/08 17:20:04  mast
Changes to work with autodoc files

Revision 3.6  2003/08/09 17:01:00  mast
Fix bug in new functions

Revision 3.5  2003/08/08 16:21:59  mast
Add functions for getting two numbers

Revision 3.4  2003/06/20 23:56:37  mast
Add ability to break output of help strings into limited-length lines

Revision 3.3  2003/06/10 23:21:03  mast
Add File name type

Revision 3.2  2003/06/05 03:01:48  mast
Adding return values  - error caught on SGI

Revision 3.1  2003/06/05 00:22:49  mast
Addition to IMOD

*/
