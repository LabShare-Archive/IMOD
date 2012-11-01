/*   parse_params.cpp  -  the PIP package for parsing input parameters
 *
 *   Copyright (C) 2003-2006 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See dist/COPYRIGHT for full notice.
 *
 *  $Id$
 */                                                                           

#include "parse_params.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include "b3dutil.h"

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
  int lenShort;         /* Length of short name */
  int *nextLinked;      /* Array of indexes of next non-option argument or linked option,
                           for associating an entry with another one */
  int linked;           /* Flag that it is a linked option (needed for usage/manpage) */
} PipOptions;

static char *sTypes[] = {BOOLEAN_STRING, PARAM_FILE_STRING,
                        "LI", "I", "F", "IP", "FP", "IT", "FT", "IA", "FA",
                        "CH", "FN"};
static char *sTypeDescriptions[] = {
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

static char *sTypeForUsage[] = {
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



static char sNumTypes = 13;

static char sNullChar = 0x00;
static char *sNullString = &sNullChar;
static char *sQuoteTypes = "\"'`";

/* declarations of local functions */
static int ReadParamFile(FILE *pFile);
static int OptionLineOfValues(const char *option, void *array, int valType, 
                           int *numToGet, int arraySize);
static int GetNextValueString(const char *option, char **strPtr);
static int AddValueString(int option, const char *strPtr);
static int LookupOption(const char *option, int maxLookup);
static char *PipSubStrDup(const char *s1, int i1, int i2);
static void AppendToErrorString(const char *str);
static int LineIsOptionToken(const char *line);
static int CheckKeyword(const char *line, const char *keyword, char **copyto, int *gotit,
                        char ***lastCopied, int *quoteInd);


/* The static tables and variables */
static PipOptions *sOptTable = NULL;
static int sTableSize = 0;
static int sNumOptions = 0;
static int sNonOptInd;
static char *sErrorString = NULL;
static char *sUsageString = NULL;
static char sExitPrefix[PREFIX_SIZE] = "";
static int sErrorDest = 0;
static int sNextOption = 0;
static int sNextArgBelongsTo = -1;
static int sNumOptionArguments = 0;
static char *sTempStr = NULL;
static char *sLineStr = NULL;
static int sAllowDefaults = 0;
static int sOutputManpage = 0;
static int sPrintEntries = -1;
static char sDefaultDelim[] = VALUE_DELIM;
static char *sValueDelim = sDefaultDelim;
static char *sProgramName = &sNullChar;
static int sNoCase = 0;
static int sDoneEnds = 0;
static int sTakeStdIn = 0;
static int sNonOptLines = 0;
static int sNoAbbrevs = 0;
static int sNotFoundOK = 0;
static char *sLinkedOption = NULL;
static int sTestAbbrevForUsage = 0;

/*
 * Initialize option tables for given number of options
 */
int PipInitialize(int numOpts)
{
  int i;

  if (!sTempStr)
    sTempStr = (char *)malloc(TEMP_STR_SIZE);
  sLineStr = (char *)malloc(LINE_STR_SIZE);

  /* Make the table big enough for extra entries (NonOptionArgs) */
  sNumOptions = numOpts;
  sTableSize = numOpts + 2;
  sNonOptInd = sNumOptions;
  sOptTable = (PipOptions *)malloc(sTableSize * sizeof(PipOptions));
  if (!sTempStr || !sLineStr || !sOptTable) {
    PipMemoryError(NULL, "PipInitialize");
    return -1;
  }

  /* Initialize the table */
  for (i = 0; i < sTableSize; i++) {
    sOptTable[i].shortName = NULL;
    sOptTable[i].longName = NULL;
    sOptTable[i].type = NULL;
    sOptTable[i].helpString = NULL;
    sOptTable[i].valuePtr = NULL;
    sOptTable[i].multiple = 0;
    sOptTable[i].count = 0;
    sOptTable[i].nextLinked = NULL;
    sOptTable[i].linked = 0;
  }

  /* In the last slots, put non-option arguments, and also put the
     name for the standard input option for easy checking on duplication */
  sOptTable[sNonOptInd].longName = strdup(NON_OPTION_STRING);
  sOptTable[sNonOptInd + 1].shortName = strdup(STANDARD_INPUT_STRING);
  sOptTable[sNonOptInd + 1].longName = strdup(STANDARD_INPUT_END);
  if (!sOptTable[sNonOptInd].longName || !sOptTable[sNonOptInd + 1].shortName ||
      !sOptTable[sNonOptInd + 1].longName) {
    PipMemoryError(NULL, "PipInitialize");
    return -1;
  }
  sOptTable[sNonOptInd].multiple = 1;

  return 0;
}

/*
 * Free all allocated memory and set state back to initial state
 */
void PipDone(void)
{
  int i, j;
  PipOptions *optp;
  for (i = 0; i < sTableSize; i++) {
    optp = &sOptTable[i];
    B3DFREE(optp->shortName);
    B3DFREE(optp->longName);
    B3DFREE(optp->type);
    B3DFREE(optp->helpString);
    if (optp->valuePtr) {
      for (j = 0; j < optp->count; j++) {
        B3DFREE(optp->valuePtr[j]);
      }
      free(optp->valuePtr);
    }
    B3DFREE(optp->nextLinked);
  }
  B3DFREE(sOptTable);
  sOptTable = NULL;
  sTableSize = 0;
  sNumOptions = 0;
  B3DFREE(sErrorString);
  sErrorString = NULL;
  B3DFREE(sUsageString);
  sUsageString = NULL;
  B3DFREE(sLinkedOption);
  sLinkedOption = NULL;
  sNextOption = 0;
  sNextArgBelongsTo = -1;
  sNumOptionArguments = 0;
  sAllowDefaults = 0;
  B3DFREE(sTempStr);
  sTempStr = NULL;
  B3DFREE(sLineStr);
  sLineStr = NULL;
  if (sProgramName != sNullString)
    free(sProgramName);
  sProgramName = sNullString;
}

/*
 * Set up for Pip to handle exiting on error, with a prefix string
 */
int PipExitOnError(int useStdErr, const char *prefix)
{
  /* Get rid of existing string; and if called with null string,
     this cancels an existing exit on error */
  sExitPrefix[0] = 0x00;
    
  if (!prefix || !*prefix)
    return 0;
    
  sErrorDest = useStdErr;
  strncpy(sExitPrefix, prefix, PREFIX_SIZE - 1);
  sExitPrefix[PREFIX_SIZE - 1] = 0x00;
  return 0;
}

/*
 * Function for compatibility with Fortran routines, so setExitPrefix and
 * exitError can be used without using PIP
 */
void setExitPrefix(const char *prefix)
{
  PipExitOnError(0, prefix);
}

void PipAllowCommaDefaults(int val)
{
  sAllowDefaults = val;
}

void PipSetManpageOutput(int val)
{
  sOutputManpage = val;
}

int PipSetUsageString(const char *usage)
{
  B3DFREE(sUsageString);
  sUsageString = strdup(usage);
  if (PipMemoryError(sUsageString, "PipSetUsageString"))
    return -1;
  return 0;
}

void PipEnableEntryOutput(int val)
{
  sPrintEntries = val;
}

int PipSetLinkedOption(const char *option)
{
  B3DFREE(sLinkedOption);
  sLinkedOption = strdup(option);
  if (PipMemoryError(sLinkedOption, "PipSetLinkedOption"))
    return -1;
  return 0;
}

/*
 * Set noxious special flags for Tilt program 
 */
void PipSetSpecialFlags(int inCase, int inDone, int inStd, int inLines,
                        int inAbbrevs)
{
  sNoCase = inCase;
  sDoneEnds = inDone;
  sTakeStdIn = inStd;
  sNonOptLines = inLines;
  sNoAbbrevs = inAbbrevs;
}

/*
 * Add an option, with short and long name, type, and help string
 */
int PipAddOption(const char *optionString)
{
  int ind, indEnd, oldSlen, newSlen;
  PipOptions *optp = &sOptTable[sNextOption];
  char *colonPtr;
  char *oldShort;
  char *oldLong;
  char *newShort;
  char *newLong;
  const char *subStr = optionString;

  if (sNextOption >= sNumOptions) {
    PipSetError("Attempting to add more options than were originally"
                " specified");
    return -1;
  }

  /* In the following, if there is ever not another :, skip to error */
  /* get the short name */
  colonPtr = strchr(subStr, ':');
  if (colonPtr) {

    indEnd = colonPtr - subStr;
    
    if (indEnd > 0) {
      optp->shortName = PipSubStrDup(subStr, 0, indEnd - 1);
      optp->lenShort = indEnd;
    } else {
      optp->shortName = strdup(sNullString);
      optp->lenShort = 0;
    }
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
        optp->longName = strdup(sNullString);
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
          if (subStr[ind] == 'M' || subStr[ind] == 'L') {
            optp->multiple = 1;
            if (subStr[ind] == 'L')
              optp->linked = 1;
            ind--;
          }
          optp->type = PipSubStrDup(subStr, 0, ind);
        } else
          optp->type = strdup(sNullString);
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
        newSlen = optp->lenShort;
        for (ind = 0; ind < sTableSize; ind++) {

          /* after checking existing ones, skip to NonOptionArg and 
             StandardInput entries */
          if (ind >= sNextOption && ind < sNonOptInd)
            continue;

          oldShort = sOptTable[ind].shortName;
          oldLong = sOptTable[ind].longName;
          oldSlen = sOptTable[ind].lenShort;
          if (((PipStartsWith(newShort, oldShort) || PipStartsWith(oldShort, newShort)) &&
               ((newSlen > 1 && oldSlen > 1) || (newSlen == 1 && oldSlen == 1))) ||
              PipStartsWith(oldLong, newShort) || PipStartsWith(newShort, oldLong) ||
              PipStartsWith(oldShort, newLong) || PipStartsWith(newLong, oldShort) ||
              PipStartsWith(oldLong, newLong) || PipStartsWith(newLong, oldLong)) {
            sprintf(sTempStr, "Option %s  %s is ambiguous with option %s"
                    "  %s", newShort, newLong, oldShort, oldLong);
            PipSetError(sTempStr);
            return -1;
          }
        }         

        sNextOption++;
        return 0;
      }
    }
  }

  sprintf(sTempStr, "Option does not have three colons in it:  ");
  AppendToErrorString(optionString);
  return -1;
}

/*
 * Call this to process the next argument
 */
int PipNextArg(const char *argString)
{
  char *argCopy;
  int err, i, lenarg;
  char ch;
  FILE *paramFile;
  int indStart;

  /* If we are expecting a value for an option, duplicate string and add
     it to the option */
  if (sNextArgBelongsTo >= 0) {
    argCopy = strdup(argString);
    if (PipMemoryError(argCopy, "PipNextArg"))
      return -1;
    err = AddValueString(sNextArgBelongsTo, argCopy);

    /* Check whether this option was for reading from parameter file */
    if (!err && !strcmp(sOptTable[sNextArgBelongsTo].type, PARAM_FILE_STRING)) {
      paramFile = fopen(argCopy, "r");
      if (paramFile) {
        err = ReadParamFile(paramFile);
        fclose(paramFile);
      } else {
        sprintf(sTempStr, "Error opening parameter file %s", argCopy);
        PipSetError(sTempStr);
        err = -1;
      }        
    }

    sNextArgBelongsTo = -1;
    return err;
  }

  /* Is it a legal option starting with - or -- ? */
  if (argString[0] == '-') {
    indStart = 1;
    lenarg = strlen(argString);
    if (lenarg > 1 && argString[1] == '-')
      indStart = 2;
    if (lenarg == indStart) {
      PipSetError("Illegal argument: - or --");
      return -1;
    }

    /* First check for StandardInput */
    if (PipStartsWith(STANDARD_INPUT_STRING, argString + indStart)) {
      err = ReadParamFile(stdin);
      return err;
    }

    /* Next check if it is a potential numeric non-option arg */
    sNotFoundOK = 1;
    for (i = indStart; i < lenarg; i++) {
      ch = argString[i];
      if (ch != '-' && ch != ','  && ch != '.' && ch != ' ' && (ch < '0' || ch > '9')) {
        sNotFoundOK = 0;
        break;
      }
    }

    /* Lookup the option among true defined options */
    err = LookupOption(argString + indStart, sNextOption);

    /* Process as an option unless it could be numeric and was not found */
    if (!(sNotFoundOK && err == LOOKUP_NOT_FOUND)) {
      sNotFoundOK = 0;
      if (err < 0)
        return err;
      
      sNumOptionArguments++;
      
      /* For an option with value, setup to get argument next time and
         return an indicator that there had better be another */
      if (strcmp(BOOLEAN_STRING, sOptTable[err].type)) {
        sNextArgBelongsTo = err;
        return 1;
      } else {
        
        /* for a boolean option, set the argument with a 1 */
        argCopy = strdup("1");
        if (PipMemoryError(argCopy, "PipNextArg"))
          return -1;
        return AddValueString(err, argCopy);
      }
    }
    sNotFoundOK = 0;
  }

  /* A non-option argument */
  argCopy = strdup(argString);
  if (PipMemoryError(argCopy, "PipNextArg"))
    return -1;
  return AddValueString(sNonOptInd, argCopy);
}

/*
 * return number of option arguments (approximate) and number of non-option
 * arguments 
 */
void PipNumberOfArgs(int *numOptArgs, int *numNonOptArgs)
{
  *numOptArgs = sNumOptionArguments;
  *numNonOptArgs = sOptTable[sNonOptInd].count;
}

/*
 * Get a non-option argument, index numbered from 0 here
 */
int PipGetNonOptionArg(int argNo, char **arg)
{
  if (argNo >= sOptTable[sNonOptInd].count) {
    PipSetError("Requested a non-option argument beyond the number available");
    return -1;
  }
  *arg = strdup(sOptTable[sNonOptInd].valuePtr[argNo]);
  return PipMemoryError(*arg, "PipGetNonOptionArg");
}

/*
 * Get a string option
 */
int PipGetString(const char *option, char **string)
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
int PipGetBoolean(const char *option, int *val)
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
    sprintf(sTempStr, "Illegal entry for boolean option %s: %s", option, 
            strPtr);
    PipSetError(sTempStr);
    return -1;
  }
  return 0;
}

/*
 * Get single integer and float just call to get an array with one element
 */
int PipGetInteger(const char *option, int *val)
{
  int num = 1;
  return PipGetIntegerArray(option, val, &num, 1);
}

int PipGetFloat(const char *option, float *val)
{
  int num = 1;
  return PipGetFloatArray(option, val, &num, 1);
}

/*
 * Get two integers or floats - move into array, get array with two elements
 */
int PipGetTwoIntegers(const char *option, int *val1, int *val2)
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

int PipGetTwoFloats(const char *option, float *val1, float *val2)
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
int PipGetThreeIntegers(const char *option, int *val1, int *val2, int *val3)
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

int PipGetThreeFloats(const char *option, float *val1, float *val2, float *val3)
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
int PipGetIntegerArray(const char *option, int *array, int *numToGet, int arraySize)
{
  return OptionLineOfValues(option, (void *)array, PIP_INTEGER, numToGet,
                         arraySize);
}

int PipGetFloatArray(const char *option, float *array, int *numToGet, int arraySize)
{
  return OptionLineOfValues(option, (void *)array, PIP_FLOAT, numToGet,
                         arraySize);
}

/*
 * Print a complete usage statement, man page entry, or program fallback code
 */
int PipPrintHelp(const char *progName, int useStdErr, int inputFiles, 
                 int outputFiles)
{
  int i, j, abbrevOK, lastOpt, jlim, optLen, numOut = 0, numReal = 0;
  int helplim = 74;
  char *sname, *lname, *newLinePt;
  FILE *out = useStdErr ? stderr : stdout;
  char indent4[] = "    ";
  char *indentStr;
  int linePos = 11;
  int fort90 = (sOutputManpage == -3) ? 1 : 0;
  int fort77 = (sOutputManpage == -2) ? 1 : 0;
  int cCode = (sOutputManpage == 2) ? 1 : 0;
  int python = (sOutputManpage == 3) ? 1 : 0;
  char *fortCont = fort90 ? " &\n      '" : "\n     &    '";
  char **descriptions = &sTypeDescriptions[0];

  /* Get correct number of options for Fortran fallback */
  for (i = 0; i < sNumOptions; i++) {
    sname = sOptTable[i].shortName;
    lname = sOptTable[i].longName;
    if ((lname && *lname) || (sname && *sname))
      numReal++;
  }

  if (!sOutputManpage) {
    if (sUsageString) {
      fprintf(out, "%s", sUsageString);
    } else {
      fprintf(out, "Usage: %s ", progName);
      if (sNumOptions)
        fprintf(out, "[Options]");
      if (inputFiles)
        fprintf(out, " input_file");
      if (inputFiles > 1)
        fprintf(out, "s...");
      if (outputFiles)
        fprintf(out, " output_file");
      if (outputFiles > 1)
        fprintf(out, "s...");
    }
    fprintf(out,"\n");

    if (!numReal)
      return 0;
    fprintf(out, "Options can be abbreviated, current short name abbreviations are in "
            "parentheses\n");
    fprintf(out, "Options:\n");
    descriptions = &sTypeForUsage[0];
  }

  sTestAbbrevForUsage = 1;
  for (i = 0; i < sNumOptions; i++) {
    sname = sOptTable[i].shortName;
    lname = sOptTable[i].longName;
    indentStr = sNullString;

    /* Try to look up an abbreviation of the short name */
    abbrevOK = 0;
    if (sname && *sname) {
      jlim = strlen(sname) - 1;
      if (jlim > TEMP_STR_SIZE - 10)
        jlim = TEMP_STR_SIZE - 10;
      for (j = 0; j < jlim; j++) {
        sTempStr[j] = sname[j];
        sTempStr[j + 1] = 0x00;
        if (LookupOption(sTempStr, sNumOptions) == i) {
          abbrevOK = 1;
          break;
        }
      }
    }

    if ((lname && *lname) || (sname && *sname)) {
      if (sOutputManpage <= 0 && !fort90)
        indentStr = indent4;

      /* Output Fortran fallback code (-2) */
      if (fort77 || fort90) {
        lastOpt = (i == sNumOptions - 1);
        if (!numOut) fprintf(out, 
                             "%s  integer numOptions\n"
                             "%s  parameter (numOptions = %d)\n"
                             "%s  character*(40 * numOptions) options(1)\n"
                             "%s  options(1) =%s", indentStr, indentStr, numReal, 
                             indentStr, indentStr, fortCont);
        
        optLen = strlen(sname) + strlen(lname) + strlen(sOptTable[i].type) + 4 +
          sOptTable[i].multiple;
        
        if (linePos + optLen + (lastOpt ? 0 : (fort90 ? 5 : 3)) > 90) {
          fprintf(out, "'//%s", fortCont);
          linePos = fort90 ? 7 : 11;
        }
        fprintf(out, "%s:%s:%s%s%s", sname, lname, sOptTable[i].type, 
                sOptTable[i].multiple ? (sOptTable[i].linked ? "L:" : "M:") : ":", 
                lastOpt ? "'\n" : "@");
        linePos += optLen;
        numOut++;
        continue;
      }
      
      /* Fallback output for C code (2) or Python code (3) */
      if (cCode || python) {
        lastOpt = (i == sNumOptions - 1);
        if (!numOut) {
          if (cCode) {
            fprintf(out, "  int numOptions = %d;\n"
                    "  const char *options[] = {\n    ", numReal);
            linePos = 5;
          } else {
            fprintf(out, "options = [");
            linePos = 12;
          }
        }
        optLen = strlen(sname) + strlen(lname) + strlen(sOptTable[i].type) + 7 +
          sOptTable[i].multiple;
        if (linePos + optLen > 90) {
          if (cCode) {
            fprintf(out, "\n    ");
            linePos = 5;
          } else {
            fprintf(out, "\n           ");
            linePos = 12;
          }
        }
        fprintf(out, "\"%s:%s:%s%s\"%s", sname, lname, sOptTable[i].type, 
                sOptTable[i].multiple ? (sOptTable[i].linked ? "L:" : "M:") : ":", 
                lastOpt ? (cCode ? "};\n" : "]\n"): ", ");
        linePos += optLen;
        numOut++;
        continue;
      }

      if (i && sOutputManpage < 0)
        fprintf(out, "\n");
      if (sOutputManpage > 0)
        fprintf(out, ".TP\n.B ");
      fprintf(out, " ");
      if (sname && *sname)
        fprintf(out, "-%s", sname);
      if (abbrevOK)
        fprintf(out, " (-%s)", sTempStr);
      if (sname && *sname && lname && *lname)
        fprintf(out,"  OR  ");
      if (lname && *lname)
        fprintf(out, "-%s", lname);
      for (j = 0; j < sNumTypes; j++)
        if (!strcmp(sOptTable[i].type, sTypes[j]))
          break;
      if (strcmp(sOptTable[i].type, BOOLEAN_STRING))
        fprintf(out, "%s%s", sOutputManpage > 0 ? " \t " : "   ", 
                descriptions[j]);
      fprintf(out, "\n");
    } else if (fort77 || fort90 || cCode || python)
      continue;
    else if (sOutputManpage == 1)
      fprintf(out, ".SS ");
    else
      fprintf(out, "\n");

    /* Print help string, breaking up line as needed */
    if (sOptTable[i].helpString && *sOptTable[i].helpString) {
      lname = strdup(sOptTable[i].helpString);
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

    if (sOptTable[i].linked)
      fprintf(out, "%s(Multiple entries linked to a different option)\n",
              indentStr);
    else if (sOptTable[i].multiple)
      fprintf(out, "%s(Successive entries accumulate)\n", indentStr);
  }
  sTestAbbrevForUsage = 0;
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
  if (sPrintEntries < 0) {
    sPrintEntries = 0;
    name = getenv(PRINTENTRY_VARIABLE);
    if (name)
      sPrintEntries = atoi(name);
  }
  if (!sPrintEntries)
    return;

  /* Count up entries */
  j = 0;
  for (i = 0; i < sNumOptions; i++)
    j += sOptTable[i].count;
  if (!(j + sOptTable[sNonOptInd].count))
    return;

  printf("\n*** Entries to program %s ***\n", sProgramName);
  for (i = 0; i < sNumOptions; i++) {
    sname = sOptTable[i].shortName;
    lname = sOptTable[i].longName;
    if ((lname && *lname) || (sname && *sname)) {
      name = (lname && *lname) ? lname : sname;
      for (j = 0; j < sOptTable[i].count; j++)
        printf("  %s = %s\n", name, sOptTable[i].valuePtr[j]);
    }
  }
  if (sOptTable[sNonOptInd].count) {
    printf("  Non-option arguments:");
    for (j = 0; j < sOptTable[sNonOptInd].count; j++)
      printf("   %s", sOptTable[sNonOptInd].valuePtr[j]);
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
  if (!sErrorString) {
    *errString = strdup(sNullString);
    PipMemoryError(*errString, "PipGetError");
    return -1;
  }
  *errString = strdup(sErrorString);
  return PipMemoryError(*errString, "PipGetError");
}

/*
 * Set the error string.
 * If sExitPrefix is set, then output an error message to stderr or stdout
 * and exit. 
 */
int PipSetError(const char *errString)
{
  FILE *outFile = sErrorDest ? stderr : stdout;
  if (sErrorString)
    free(sErrorString);
  sErrorString = strdup(errString);
  if (!sErrorString && !sExitPrefix[0])
    return -1;

  if (sExitPrefix[0]) {
    fprintf(outFile, "%s ", sExitPrefix);
    fprintf(outFile, "%s\n", sErrorString ? sErrorString : "Unspecified error");
    exit(1);
  }
  return 0;
}

void exitError(const char *format, ...)
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
int PipNumberOfEntries(const char *option, int *numEntries)
{
  int err;
  if ((err = LookupOption(option, sNonOptInd + 1)) < 0)
    return err;
  *numEntries = sOptTable[err].count;
  return 0;
}

/*
 * Return the index of the next non-option arg or linked option that was entered after 
 * this option
 */
int PipLinkedIndex(const char *option, int *index)
{
  int err, ind, ilink, which = 0;
  if ((err = LookupOption(option, sNonOptInd + 1)) < 0)
    return err;
  if (!sOptTable[err].linked) {
    sprintf(sTempStr, "Trying to get a linked index for option %s, which is not "
            "identified as linked", option);
    PipSetError(sTempStr);
    return -1;
  }
  ind = 0;
  if (sOptTable[err].multiple)
    ind = sOptTable[err].multiple - 1;

  /* Use count from non-option args, but use the count from the linked option
     instead if it was entered at all.  This allows other non-option args to be used */
  if (sLinkedOption) {
    if ((ilink = LookupOption(sLinkedOption, sNumOptions)) < 0)
      return ilink;
    if (sOptTable[ilink].count)
      which = 1;
  }
  *index = sOptTable[err].nextLinked[2 * ind + which];
  return 0;
}

/*
 * Top level routine to be called to process options and arguments
 */
int PipParseInput(int argc, char *argv[], const char *options[], int numOpts,
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
int PipReadOptionFile(const char *progName, int helpLevel, int localDir)
{
  int i, ind, len, indst, lineLen, err, needSize, isOption, isSection = 0;
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
  int inQuoteIndex = -1;

#ifdef PATH_MAX
  if (bigSize < PATH_MAX)
    bigSize = PATH_MAX;
#endif

  /* Set up temp string for error processing and big string for lines */
  if (!sTempStr)
    sTempStr = (char *)malloc(TEMP_STR_SIZE);
  bigStr = (char *)malloc(bigSize);
  if (!bigStr || !sTempStr) {
    PipMemoryError(NULL, "PipReadOptionFile");
    return -1;
  }

  /* Save the program name for entry output */
  sProgramName = strdup(progName);
  if (!sProgramName)
    sProgramName = sNullString;

  /* If local directory not set, look for environment variable pointing
     directly to where the file should be */
  if (!localDir) {
    pipDir = getenv(OPTDIR_VARIABLE);
    if (pipDir) {
      if (strlen(pipDir) > bigSize - 100) {
        PipSetError(OPTDIR_VARIABLE" is suspiciously long");
        return -1;
      } 
      sprintf(bigStr, "%s%c%s.%s", pipDir, PATH_SEPARATOR, progName, 
              OPTFILE_EXT);
      /* fprintf(stderr, "Looking for file %s\n", bigStr); */
      optFile = fopen(bigStr, "r");
    }

    if (!optFile) {
      pipDir = getenv("IMOD_DIR");
      if (pipDir) {
        if (strlen(pipDir) > bigSize - 100) {
          PipSetError("IMOD_DIR is suspiciously long");
          return -1;
        }
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
      CheckKeyword(bigStr + indst, "KeyValueDelimiter", &sValueDelim, &gotDelim,
                   &lastGottenStr, NULL);

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
  longName = shortName = type = usageStr = tipStr = manStr = sNullString;
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
        longName = shortName = sNullString;
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
      longName = shortName = type = usageStr = tipStr = manStr = sNullString;
      gotLong = gotShort = gotType = gotUsage = gotTip = gotMan = 0;
      readingOpt = 0;
    }

    if (lineLen == -3)
      break;

    /* If reading options, look for the various keywords */
    if (readingOpt) {

      /* If the last string gotten was a help string and the line does not contain the 
         value delimiter or we are in a quote, then append it to the last string */
      if ((lastGottenStr == &usageStr || lastGottenStr == &tipStr ||
           lastGottenStr == &manStr) && 
          (inQuoteIndex >= 0 || !strstr(textStr, sValueDelim))) {
        ind = strlen(*lastGottenStr);
        len = strlen(textStr);
        needSize = ind + len + 3;
        *lastGottenStr = (char *)realloc(*lastGottenStr, needSize);
        strcat(*lastGottenStr, (*lastGottenStr)[ind - 1] == '.' ? "  " : " ");

        /* Replace leading ^ with a newline */
        if (textStr[0] == '^')
          textStr[0] = '\n';

        /* If inside quotes, look for quote at end and say it is the end of accepting
           continuation lines, and as a protection, also say a blank line ends it */
        if (inQuoteIndex >= 0 && 
            (!len || textStr[len - 1] == sQuoteTypes[inQuoteIndex])) {
          if (len) {
            textStr[len - 1] = 0x00;
            strcat(*lastGottenStr, textStr);
          }
          inQuoteIndex = -1;
          lastGottenStr = NULL;
        } else {
          strcat(*lastGottenStr, textStr);
        }
      }

      /* Otherwise look for each keyword of interest, but null out the pointer
         to last gotten one so that it will only be valid on the next line */
      else {
        lastGottenStr = NULL;
        inQuoteIndex = -1;
        if ((err = CheckKeyword(textStr, "short", &shortName, &gotShort,
                                &lastGottenStr, NULL)))
          return err;
        if ((err = CheckKeyword(textStr, "long", &longName, &gotLong,
                                &lastGottenStr, NULL)))
          return err;
        if ((err = CheckKeyword(textStr, "type", &type, &gotType,
                                &lastGottenStr, NULL)))
          return err;
        
        /* Check for usage if at help level 1 or if we haven't got either of
           the other strings yet */
        if (helpLevel <= 1 || !(gotTip || gotMan))
          if ((err = CheckKeyword(textStr, "usage", &usageStr, &gotUsage,
                                  &lastGottenStr, &inQuoteIndex)))
            return err;
        
        /* Check for tooltip if at level 2 or if at level 1 and haven't got
           usage, or at level 3 and haven't got manpage */
        if (helpLevel == 2 || (helpLevel <= 1 && !gotUsage) ||
            (helpLevel >= 3 && !gotMan))
          if ((err = CheckKeyword(textStr, "tooltip", &tipStr, &gotTip,
                                  &lastGottenStr, &inQuoteIndex)))
            return err;
        
        /* Check for manpage if at level 3 or if at level 2 and haven't got
           tip, or at level 1 and haven't got tip or usage */
        if (helpLevel >= 3 || (helpLevel == 2 && !gotTip) ||
            (helpLevel <= 1 && !(gotTip || gotUsage)))
          if ((err = CheckKeyword(textStr, "manpage", &manStr, &gotMan,
                                  &lastGottenStr, &inQuoteIndex)))
            return err;
        
        /* If that was a line with quoted string, check for quote at end of line and
           close out the help string if so */
        if (inQuoteIndex >= 0 && lastGottenStr) {
          len = strlen(*lastGottenStr);
          if (len && (*lastGottenStr)[len - 1] == sQuoteTypes[inQuoteIndex]) {
            (*lastGottenStr)[len - 1] = 0x00;
            inQuoteIndex = -1;
            lastGottenStr = NULL;
          }
        }
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
                                &longName, &gotLong, &lastGottenStr, NULL)))
          return err;
        if (gotLong)
          longName[strlen(longName) - 1] = sNullChar;
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
  if (!argc && sTakeStdIn) {
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
void PipReadOrParseOptions(int argc, char *argv[], const char *options[], 
                           int numOpts, const char *progName, int minArgs, 
                           int numInFiles, int numOutFiles, int *numOptArgs,
                           int *numNonOptArgs, void (headerFunc)(const char *))
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
      printf("PIP WARNING: %s\nUsing fallback options in main program\n", errString);
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
int PipGetInOutFile(const char *option, int nonOptArgNo, char **filename)
{
  if (PipGetString(option, filename)) {
    if (nonOptArgNo >= sOptTable[sNonOptInd].count)
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
    sNotFoundOK = (!sNumOptionArguments && sOptTable[sNonOptInd].count < 
                  sNonOptLines) ? 1 : 0;
    lineLen = PipReadNextLine(pFile, sLineStr, LINE_STR_SIZE, '#', 0, 1, 
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
    strPtr = strpbrk(sLineStr + indst, "= \t");
    indnd = strPtr ? (strPtr - sLineStr) - 1 : lineLen - 1;
    if (indnd >= lineLen)
      indnd = lineLen - 1;

    token = PipSubStrDup(sLineStr, indst, indnd);
    if (PipMemoryError(token, "ReadParamFile"))
      return -1;

    /* Done if it matches end of input string */
    if (!strcmp(token, STANDARD_INPUT_END) || 
        (sDoneEnds && strlen(token) == 4 && PipStartsWith("DONE", token)))
      break;

    /* Look up option and free the token string */
    optNum = LookupOption(token, sNumOptions);
    free(token);
    if (optNum < 0) {

      /* If no option, process special case if in-line non-options allowed,
         or error out */
      if (sNotFoundOK) {
        token = PipSubStrDup(sLineStr, indst, lineLen - 1);
        if (PipMemoryError(token, "ReadParamFile"))
          return -1;
        if ((err = AddValueString(sNonOptInd, token)))
          return err;
        continue;
      } else
        return optNum;
    }

    if (!strcmp(sOptTable[optNum].type, PARAM_FILE_STRING)) {
      PipSetError("Trying to open a parameter file while reading a parameter"
                  " file or "STANDARD_INPUT_STRING);
      return -1;
    }

    /* Find first non-white space, passing over at most one equals sign */
    indst = indnd + 1;
    gotEquals = 0;
    while (indst < lineLen) {
      if (sLineStr[indst] == '=') {
        if (gotEquals) {
          sprintf(sTempStr, "Two = signs in input line:  ");
          AppendToErrorString(sLineStr);
          return -1;
        }
        gotEquals = 1;
      } else if (sLineStr[indst] != ' ' && sLineStr[indst] != '\t')
        break;
      indst++;
    }

    /* If there is a string, get one; if not, get a "1" for boolean, otherwise
       it is an error */
    if (indst < lineLen)
      token = PipSubStrDup(sLineStr, indst, lineLen - 1);
    else if (!strcmp(sOptTable[optNum].type, BOOLEAN_STRING))
      token = strdup("1");
    else {
      sprintf(sTempStr, "Missing a value on the input line:  ");
      AppendToErrorString(sLineStr);
      return -1;
    }
    if (PipMemoryError(token, "ReadParamFile"))
      return -1;

    /* Add the token as a value string and increment argument number */
    if ((err = AddValueString(optNum, token)))
      return err;
    sNumOptionArguments++;
  }
  sNotFoundOK = 0;
  return 0;
}

/*
 * Call from fortran to read stdin if the flag is set to take from stdin
 */
int PipReadStdinIfSet(void)
{
  if (sTakeStdIn)
    return ReadParamFile(stdin);
  return 0;
}

/*!
 * Reads a line from the file [pFile], stripping white space at the end of the
 * line and in-line comments starting with [comment] if [inLineComments] is 
 * non-zero.  Discards the line and reads another if it is blank or if the 
 * first non-blank character is [comment], unless [keepComments] is nonzero.
 * Returns the line in [sLineStr] and the index of the first
 * non-white space character in [firstNonWhite].  The size of [sLineStr] is
 * provided in [strSize].  Returns the length of the line, or -3 for end of 
 * file, -1 if the line is too long, or -2 for error reading file.
 */
int PipReadNextLine(FILE *pFile, char *sLineStr, int strSize, char comment, 
                    int keepComments, int inLineComments, int *firstNonWhite)
{
  int indst, lineLen;
  char ch;
  char *strPtr;

  while (1) {

    /* read a line, or as much as fits in sLineStr */
    if (fgets(sLineStr, strSize, pFile) == NULL) {

      /* If error, it's OK if it's an EOF, or an error otherwise */
      if (feof(pFile))
        return -3;
      return -2;
    }

    /* check for line too long */
    lineLen = strlen(sLineStr);
    if (lineLen == strSize - 1) {
      return -1;
    }

    /* Get first non-white space */
    indst = 0;
    while (indst < lineLen) {
      if (sLineStr[indst] != ' ' && sLineStr[indst] != '\t')
        break;
      indst++;
    }

    /* If it is a comment, skip or strip line ending and return */
    if (sLineStr[indst] == comment) {
      if (keepComments) {
        while (sLineStr[lineLen - 1] == '\n' || sLineStr[lineLen - 1] == '\r')
          lineLen--;
        sLineStr[lineLen] = 0x00;
        break;
      } else
        continue;
    }

    /* adjust line length to remove comment, if we have in-line comments */
    strPtr = strchr(sLineStr, comment);
    if (strPtr && inLineComments)
      lineLen = strPtr - sLineStr;

    /* adjust line length back further to remove white space and newline */
    while (lineLen > 0) {
      ch = sLineStr[lineLen - 1]; 
      if (ch != ' ' && ch != '\t' && ch != '\n' && ch != '\r')
        break;
      lineLen--;
    }
    sLineStr[lineLen] = 0x00;

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
static int OptionLineOfValues(const char *option, void *array, int valType, 
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
int PipGetLineOfValues(const char *option, const char *strPtr, void *array, int valType, 
                       int *numToGet, int arraySize)
{
  char *sepPtr;
  const char *endPtr;
  char *invalid;
  const char *fullStr;
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
        if (sAllowDefaults && *numToGet) {
          numGot = *numToGet;
          break;
        }
        sprintf(sTempStr, "Default entry with a / is not allowed"
                " in value entry:  %s  ", option);
        AppendToErrorString(fullStr);
        return -1;
      }

      /* special handling of commas to allow default values */
      if (strPtr[0] == ',') {

        /* If already have a comma, skip an array value if defaults allowed */
        if (gotComma) {
          if (sAllowDefaults && *numToGet) {
            numGot++;
            if (numGot >= *numToGet)
              break;
          } else {
            sprintf(sTempStr, "Default entries with commas are not allowed"
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
      sprintf(sTempStr, "Too many values for input array in value entry:  %s  ",
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
      sprintf(sTempStr, "Illegal character in value entry:  %s  ", option);
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
    sprintf(sTempStr, "%d values expected but only %d values found in value "
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
static int GetNextValueString(const char *option, char **strPtr)
{
  int err;
  int index = 0;

  if ((err = LookupOption(option, sNonOptInd + 1)) < 0)
    return err;
  if (!sOptTable[err].count)
    return 1;

  if (sOptTable[err].multiple) {
    index = sOptTable[err].multiple - 1;
    if (sOptTable[err].multiple < sOptTable[err].count)
      sOptTable[err].multiple++;
  }
  *strPtr = sOptTable[err].valuePtr[index];
  return 0;
}

/*
 * Add a string to the set of values for an option
 */
static int AddValueString(int option, const char *strPtr)
{
  int err;
  PipOptions *optp = &sOptTable[option];

  /* If the count is zero or we accept multiple values, need to allocate
     array for address of string, and array for next non option index */
  if (!optp->count || optp->multiple) {
    if (optp->count) {
      optp->valuePtr = (char **)realloc(optp->valuePtr, (optp->count + 1) *
                                        sizeof(char *));
      if (optp->linked)
        optp->nextLinked = (int *)realloc(optp->nextLinked, (optp->count + 1) * 2 *
                                          sizeof(int));
    } else {
      optp->valuePtr = (char **)malloc(sizeof(char *));
      if (optp->linked)
        optp->nextLinked = (int *)malloc(2 * sizeof(int));
    }
    if (PipMemoryError(optp->valuePtr, "AddValueString") || 
        (optp->linked && PipMemoryError(optp->nextLinked, "AddValueString")))
      return -1;
  } else {

    /* otherwise, need to free existing value */
    if (*optp->valuePtr)
      free (*optp->valuePtr);
    optp->count = 0;
  }
  /* save address that was passed in.  The caller had to make a duplicate */

  if (optp->linked) {
    optp->nextLinked[2 * optp->count] = sOptTable[sNonOptInd].count;
    optp->nextLinked[2 * optp->count + 1] = 0;
    if (sLinkedOption) {
      err = LookupOption(sLinkedOption, sNumOptions);
      if (err < 0)
        return err;
      optp->nextLinked[2 * optp->count + 1] = sOptTable[err].count;
    }
  }
  optp->valuePtr[optp->count++] = (char *)strPtr;
  return 0;
}

/*
 * Look up an option in the table, issue an error message if the option does
 * not exist or is ambiguous; return index of option or an error code
 */
static int LookupOption(const char *option, int maxLookup)
{
  int starts, i, lenopt, lenShort;
  int found = LOOKUP_NOT_FOUND;
  char *sname, *lname;

  lenopt = strlen(option);

  /* Look at all of the options specified by maxLookup */
  for (i = 0; i < maxLookup; i++) {
    sname = sOptTable[i].shortName;
    lname = sOptTable[i].longName;
    lenShort = sOptTable[i].lenShort;
    starts = PipStartsWith(sname, option);

    /* First test for single letter short name match - if passes, skip ambiguity test */
    if (lenopt == 1 && starts && lenShort == 1) {
      found = i;
      break;
    }
    if ((starts && (!sNoAbbrevs || lenopt == lenShort)) ||
        (PipStartsWith(lname, option) && (!sNoAbbrevs || lenopt == strlen(lname)))) {

      /* If it is found, it's an error if one has already been found */
      if (found == LOOKUP_NOT_FOUND)
        found = i;
      else {
        if (!sTestAbbrevForUsage) {
          sprintf(sTempStr, "An option specified by \"%s\" is ambiguous between "
                  "option %s -  %s  and option %s -  %s", option, sname, lname, 
                  sOptTable[found].shortName, sOptTable[found].longName);
          PipSetError(sTempStr);
        }
        return LOOKUP_AMBIGUOUS;
      }
    }
  }

  /* Set error string unless flag set that non-options are OK */
  if (found == LOOKUP_NOT_FOUND && !sNotFoundOK) {
    sprintf(sTempStr, "Illegal option: %s", option);
    PipSetError(sTempStr);
  }
  return found;
}

/*
 * Duplicate a substring into a new string
 */
static char *PipSubStrDup(const char *s1, int i1, int i2)
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
int PipMemoryError(void *ptr, const char *routine)
{
  if (ptr)
    return 0;
  if (!sTempStr)
    sTempStr = (char *)malloc(TEMP_STR_SIZE);
  if (!sTempStr) {
    PipSetError("Failed to get memory for string in PipMemoryError");
    return -1;
  }
    
  sprintf(sTempStr, "Failed to get memory for string in %s", routine);
  PipSetError(sTempStr);
  return -1;
}

/*
 * Add as much of a string as fits to the sTempStr and use to set error
 */
static void AppendToErrorString(const char *str)
{
  int len = strlen(sTempStr);
  sTempStr[TEMP_STR_SIZE - 1] = 0x00;
  strncpy(&sTempStr[len], str, TEMP_STR_SIZE - len - 1);
  PipSetError(sTempStr);
}

/*!
 * Returns 1 if [fullStr] starts with [subStr], where either strings can be 
 * NULL.
 */
int PipStartsWith(const char *fullStr, const char *subStr)
{
  if (!fullStr || !subStr)
    return 0;
  if (!*fullStr || !*subStr)
    return 0;
  if (sNoCase) {
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
static int LineIsOptionToken(const char *line)
{
  const char *token;

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
 * value string and sets the flag.  If quoteInd is non-null, it sees if the string starts
 * with a quote character in sQuoteTypes, strips that, and returns the index in quoteInd.
 * Watch out for the lastCopied variable, which points to the variable holding
 * the address of the string
 */
static int CheckKeyword(const char *line, const char *keyword, char **copyto, int *gotit,
                        char ***lastCopied, int *quoteInd)
{
  char *valStart, *quoteStart;
  char *copyStr;

  /* First make sure line starts with it */
  if (!PipStartsWith(line, keyword))
    return 0;

  /* Now look for delimiter */
  valStart = strstr(line, sValueDelim);
  if (!valStart)
    return 0;

  /* Free previous entry if there was one, and mark that it was not gotten,
     so that an empty entry can supercede a non-empty one */
  if (*gotit && *copyto != sNullString) {
    free (*copyto);
    *copyto = sNullString;
    *gotit = 0;
  }

  /* Eat spaces after the delimiter and return if nothing left */
  /* In other words, a key with no value is the same as having no key at all */
  valStart += strlen(sValueDelim);
  while (*valStart == ' ' || *valStart == '\t')
    valStart++;
  if (!*valStart)
    return 0;

  /* Look for quote if directed to */
  if (quoteInd) {
    quoteStart = strchr(sQuoteTypes, *valStart);
    if (quoteStart) {
      *quoteInd = quoteStart - sQuoteTypes;
      valStart++;
    } else
      *quoteInd = -1;
  }

  /* Copy string and return address, set flag that it was gotten */
  copyStr = strdup(valStart);
  if (PipMemoryError(copyStr, "CheckKeyword"))
    return -1;

  *gotit = 1;
  *copyto = copyStr;
  *lastCopied = copyto;
  return 0;
}
