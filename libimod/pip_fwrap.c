/*   pip_fwrap.c  -   Fortran wrapper for Pip function calls
 *
 *   Copyright (C) 2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.3  2003/08/08 16:22:26  mast
Add functiond for getting two numbers

Revision 3.2  2003/06/10 23:21:54  mast
Avoid freeing strings that were never allocated

Revision 3.1  2003/06/05 00:24:02  mast
Addition to IMOD

*/

#include "parse_params.h"
#include <string.h>

/* Use these defines in case they need to be defined otherwise */
#define pipinitialize pipinitialize_
#define pipexitonerror pipexitonerror_
#define pipaddoption pipaddoption_
#define pipnextarg pipnextarg_
#define pipnumberofargs pipnumberofargs_
#define pipgetnonoptionarg pipgetnonoptionarg_
#define pipgetstring pipgetstring_
#define pipgetinteger pipgetinteger_
#define pipgettwointegers pipgettwointegers_
#define pipgettwofloats pipgettwofloats_
#define pipgetthreeintegers pipgetthreeintegers_
#define pipgetthreefloats pipgetthreefloats_
#define pipgetfloat pipgetfloat_
#define pipgetboolean pipgetboolean_
#define pipgetintegerarray pipgetintegerarray_
#define pipgetfloatarray pipgetfloatarray_
#define pipprinthelp pipprinthelp_
#define pipgeterror pipgeterror_
#define pipseterror pipseterror_
#define pipnumberofentries pipnumberofentries_
#define pipdone pipdone_
#define pipparseinput pipparseinput_
#define pipallowcommadefaults pipallowcommadefaults_
#define pipreadoptionfile pipreadoptionfile_
#define pipsetmanpageoutput pipsetmanpageoutput_


static int c2fString(char *cStr, char *fStr, int fSize);
static char *f2cString(char *str, int strSize);


int pipinitialize(int *numOptions)
{
  return PipInitialize(*numOptions);
}

int pipexitonerror(int *useStdErr, char *prefix, int stringSize)
{
  char *cStr;
  int err;
  if (!(cStr = f2cString(prefix, stringSize)))
    return -1;
  err = PipExitOnError(*useStdErr, cStr);
  free(cStr);
  return err;
}

void pipallowcommadefaults(int *val)
{
  PipAllowCommaDefaults(*val);
}

void pipsetmanpageoutput(int *val)
{
  PipSetManpageOutput(*val);
}

int pipreadoptionfile(char *progName, int *helpLevel, int *localDir, 
                      int stringSize)
{
  char *cStr;
  int err;
  if (!(cStr = f2cString(progName, stringSize)))
    return -1;
  err = PipReadOptionFile(cStr, *helpLevel, *localDir);
  free(cStr);
  return err;
}

int pipaddoption(char *optionString, int stringSize)
{
  char *cStr;
  int err;
  if (!(cStr = f2cString(optionString, stringSize)))
    return -1;
  err = PipAddOption(cStr);
  free(cStr);
  return err;
}

int pipnextarg(char *argString, int stringSize)
{
  char *cStr;
  int err;
  if (!(cStr = f2cString(argString, stringSize)))
    return -1;
  err = PipNextArg(cStr);
  free(cStr);
  return err;
}

void pipnumberofargs(int *numOptArgs, int *numNonOptArgs)
{
  PipNumberOfArgs(numOptArgs, numNonOptArgs);
}

int pipgetnonoptionarg(int *argNo, char *arg, int stringSize)
{
  char *argPtr = NULL;
  int err;
  err = PipGetNonOptionArg(*argNo - 1, &argPtr);
  if (!err && c2fString(argPtr, arg, stringSize)) {
    PipSetError("Non-option argument too long for character variable");
    err = -1;
  }

  /* Are we supposed to free it? */
  if (argPtr)
    free(argPtr);
  return err;
}

int pipgetstring(char *option, char *string, int optionSize, int stringSize)
{
  char *strPtr = NULL;
  char *cStr;
  int err;
  if (!(cStr = f2cString(option, optionSize)))
    return -1;
  
  err = PipGetString(cStr, &strPtr);
  if (!err && c2fString(strPtr, string, stringSize)) {
    PipSetError("In PipGetString, string is too long for character variable");
    err = -1;
  }

  /* Are we supposed to free it? */
  if (strPtr)
    free(strPtr);
  free(cStr);
  return err;
}

int pipgetinteger(char *option, int *val, int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = f2cString(option, optionSize)))
    return -1;
  err = PipGetInteger(cStr, val);
  free(cStr);
  return err;
}

int pipgetfloat(char *option, float *val, int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = f2cString(option, optionSize)))
    return -1;
  err = PipGetFloat(cStr, val);
  free(cStr);
  return err;
}

int pipgettwointegers(char *option, int *val1, int *val2, int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = f2cString(option, optionSize)))
    return -1;
  err = PipGetTwoIntegers(cStr, val1, val2);
  free(cStr);
  return err;
}

int pipgettwofloats(char *option, float *val1, float *val2, int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = f2cString(option, optionSize)))
    return -1;
  err = PipGetTwoFloats(cStr, val1, val2);
  free(cStr);
  return err;
}

int pipgetthreeintegers(char *option, int *val1, int *val2, int *val3,
                        int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = f2cString(option, optionSize)))
    return -1;
  err = PipGetThreeIntegers(cStr, val1, val2, val3);
  free(cStr);
  return err;
}

int pipgetthreefloats(char *option, float *val1, float *val2, float *val3,
                      int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = f2cString(option, optionSize)))
    return -1;
  err = PipGetThreeFloats(cStr, val1, val2, val3);
  free(cStr);
  return err;
}

int pipgetboolean(char *option, int *val, int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = f2cString(option, optionSize)))
    return -1;
  err = PipGetBoolean(cStr, val);
  free(cStr);
  return err;
}

int pipgetintegerarray(char *option, int *array, int *numToGet, int *arraySize,
		       int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = f2cString(option, optionSize)))
    return -1;
  err = PipGetIntegerArray(cStr, array, numToGet, *arraySize);
  free(cStr);
  return err;
}

int pipgetfloatarray(char *option, float *array, int *numToGet, int *arraySize,
		     int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = f2cString(option, optionSize)))
    return -1;
  err = PipGetFloatArray(cStr, array, numToGet, *arraySize);
  free(cStr);
  return err;
}

int pipprinthelp(char *string, int *useStdErr, int *inputFiles, 
		 int *outputFiles, int stringSize)
{
  char *cStr;
  int err;
  if (!(cStr = f2cString(string, stringSize))) {
    PipSetError("Memory error in pipgethelp_");
    return -1;
  }
  err = PipPrintHelp(cStr, *useStdErr, *inputFiles, *outputFiles);
  free(cStr);
  return err;
}

int pipgeterror(char *errString, int stringSize)
{
  char *strPtr = NULL;
  int err;
  int copyLen = stringSize - 1;

  /* Set up string as null, and with a null at end in case it is too long */
  err = PipGetError(&strPtr);
  if (!strPtr)
    return -1;

  err = c2fString(strPtr, errString, stringSize);

  /* Are we supposed to free it? */
  free(strPtr);
  return err;
}

int pipseterror(char *errString, int stringSize)
{
  char *cStr;
  int err;
  if (!(cStr = f2cString(errString, stringSize))) {
    PipSetError("Memory error in pipseterror_");
    return -1;
  }
  err = PipSetError(cStr);
  free(cStr);
  return err;
}

int pipnumberofentries(char *option, int *numEntries, int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = f2cString(option, optionSize)))
    return -1;
  err = PipNumberOfEntries(cStr, numEntries);
  free(cStr);
  return err;
}

void pipdone(void)
{
  PipDone();
}

/* Create a C string with a copy of a Fortran string */
static char *f2cString(char *str, int strSize)
{
  int i;
  char *newStr;

  /* find last non-blank character */
  for (i = strSize - 1; i >= 0; i--)
    if (str[i] != ' ')
      break;

  newStr = (char *)malloc(i + 2);
  if (!newStr) {
    PipSetError("Memory error converting string from Fortran to C");
    return NULL;
  }

  /* copy string if non-null, then put terminator at end */
  if (i >= 0)
    strncpy(newStr, str, i + 1);
  newStr[i + 1] = 0x00;
  return newStr;
}

/* Return a C string into a Fortran string, return error if it won't fit */
static int c2fString(char *cStr, char *fStr, int fSize)
{
  int i;
  while (*cStr && fSize > 0) {
    *fStr++ = *cStr++;
    fSize--;
  }

  /* Return error if there is still a non-null character */
  if (*cStr)
    return -1;

  /* Blank-pad */
  while (fSize > 0) {
    *fStr++ = ' ';
    fSize--;
  }
  return 0;
}
